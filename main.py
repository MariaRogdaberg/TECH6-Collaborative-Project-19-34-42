# =============================================================================
# main.py : Python entry point -- Extension 2, step 1 (predictions)
# =============================================================================
# WHAT THIS SCRIPT DOES
#   1. Load the data and pick the pre-1994 features
#   2. Set up OLS, Lasso and a random forest
#   3. Predict every town out of sample, training on secular towns only,
#      and measure the fit (R2)
#   4. Check whether the same features can predict who won a close
#      election, compared with always guessing the more common winner
#   5. Save the results in output/extension2/, with the actual outcome next
#      to the predictions so R can compare them in the same towns
# =============================================================================

import numpy as np
import pandas as pd
from sklearn.model_selection import GroupKFold, GridSearchCV, cross_val_score
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression, LassoCV, LogisticRegression
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.metrics import r2_score

# ---- 1. Data and features ---------------------------------------------------
# The data is the paper's Stata file, so it is read with read_stata.
df = pd.read_stata('data/regdata0.dta', convert_categoricals=False)

# Set A: 1990 census, Islamic mayor in 1989, 1990 buildings and density
features_a = [
    'c90lpop', 'c90ageshr19', 'c90ageshr60', 'c90sexr', 'c90mrf', 'c90empf',
    'i89',
    'lareapre', 'eduarshr90', 'lpopdens90',
]
# Set B: set A plus the 1990 high school shares for women and men
features_b = features_a + ['c90hischshr1520f', 'c90hischshr1520m']
feature_sets = {'A': features_a, 'B': features_b}

# Keep towns with every feature, the outcome and the RD variables
df = df.dropna(subset=features_b + ['hischshr1520f', 'iwm94', 'i94', 'pcode'])
df = df.reset_index(drop=True)

y = df['hischshr1520f'].values        # female high school share, 2000
secular = (df['i94'] == 0).values     # towns without an Islamic mayor
province = df['pcode'].values         # province, used to form the folds

print(f'Number of towns: {len(df):,d} (secular: {secular.sum():,d})')

# ---- 2. Models --------------------------------------------------------------
# Each model chooses its own tuning inside the training folds:
# LassoCV the penalty, GridSearchCV the forest's settings.
models = {
    'OLS': LinearRegression(),
    'Lasso': make_pipeline(StandardScaler(), LassoCV(cv=5)),
    'Forest': GridSearchCV(
        estimator=RandomForestRegressor(n_estimators=200, random_state=1234),
        param_grid={'min_samples_leaf': [5, 20], 'max_features': ['sqrt', 1.0]},
        cv=5,  # Number of folds for cross-validation
        refit=True,  # Refit with the best settings on all training towns
    ),
}

# ---- 3. Out-of-sample predictions -------------------------------------------
# Five folds, provinces kept together. Train on the secular towns of four
# folds, predict every town in the fifth, and repeat for each fold.
cv_splitter = GroupKFold(n_splits=5)


def predict_out_of_sample(X, model):
    prediction = np.zeros(len(df))
    for train, test in cv_splitter.split(X, y, groups=province):
        train = train[secular[train]]  # secular towns only
        model.fit(X[train], y[train])
        prediction[test] = model.predict(X[test])
    return prediction


predictions = df[['hischshr1520f', 'iwm94', 'i94', 'pcode']].copy()
benchmark = []

for set_name, features in feature_sets.items():
    X = df[features].values
    for model_name, model in models.items():
        column = f'{set_name}_{model_name}'
        predictions[column] = predict_out_of_sample(X, model)

        # Out-of-sample fit on the secular towns
        r2 = r2_score(y[secular], predictions.loc[secular, column])
        benchmark.append({'Features': set_name, 'Model': model_name,
                          'R2_out_of_sample': round(r2, 3)})

# ---- 4. Supporting test: who won a close election? --------------------------
# Accuracy from 5-fold cross-validation (provinces kept together), within
# 10 and 5 percentage points of the cutoff.
classifiers = {
    'Logit': make_pipeline(StandardScaler(),
                           LogisticRegression(C=np.inf, max_iter=1000)),
    'Forest': RandomForestClassifier(n_estimators=200, min_samples_leaf=5,
                                     random_state=1234),
}

accuracy = []

for window in [0.10, 0.05]:
    close = df[df['iwm94'].abs() < window]
    won = close['i94'].values
    for set_name, features in feature_sets.items():
        for model_name, model in classifiers.items():
            scores = cross_val_score(
                model,  # estimator or pipeline
                close[features].values,  # predictors
                won,  # 1 = Islamic mayor won
                groups=close['pcode'].values,  # provinces kept together
                cv=cv_splitter,  # cross-validation splitter
                scoring='accuracy',  # scoring rule
            )
            accuracy.append({'Window': window, 'Features': set_name,
                             'Model': model_name, 'Towns': len(close),
                             'Accuracy': round(scores.mean(), 3),
                             'SD_across_folds': round(scores.std(), 3),
                             'Baseline': round(max(won.mean(), 1 - won.mean()), 3)})

# ---- 5. Save ----------------------------------------------------------------
benchmark = pd.DataFrame(benchmark)
accuracy = pd.DataFrame(accuracy)

predictions.to_csv('output/extension2/predictions.csv', index=False)
benchmark.to_csv('output/extension2/benchmark.csv', index=False)
accuracy.to_csv('output/extension2/accuracy.csv', index=False)

print(benchmark)
print(accuracy)
print('Done: results saved in output/extension2/')
