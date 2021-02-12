import numpy as np
from sklearn import linear_model

#%% Constituency-level regressions
# Separate models by party
## CDU
cdu_results = const_results.loc[(const_results['party'] == 'cdu') & (const_results['pct_lag'] > 0)]
cdu_X = cdu_results[['pct_lag', 'state_pct', 'state_pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
cdu_y = cdu_results['pct'].to_numpy()
cdu_lm = linear_model.LinearRegression().fit(cdu_X, cdu_y)

## SPD
spd_results = const_results.loc[(const_results['party'] == 'spd') & (const_results['pct_lag'] > 0)]
spd_X = spd_results[['pct_lag', 'state_pct', 'state_pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
spd_y = spd_results['pct'].to_numpy()
spd_lm = linear_model.LinearRegression().fit(spd_X, spd_y)

## AfD: different variables because it's only contested two federal elections
afd_results = const_results.loc[(const_results['party'] == 'afd') & (const_results['pct_lag'] > 0)]
afd_X = afd_results[['pct_lag', 'state_pct', 'state_pct_lag']].to_numpy()
afd_y = afd_results['pct'].to_numpy()
afd_lm = linear_model.LinearRegression().fit(afd_X, afd_y)

## FDP
fdp_results = const_results.loc[(const_results['party'] == 'fdp') & (const_results['pct_lag'] > 0)]
fdp_X = fdp_results[['pct_lag', 'state_pct', 'state_pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
fdp_y = fdp_results['pct'].to_numpy()
fdp_lm = linear_model.LinearRegression().fit(fdp_X, fdp_y)

## Linke
linke_results = const_results.loc[(const_results['party'] == 'linke') & (const_results['pct_lag'] > 0)]
linke_X = linke_results[['pct_lag', 'state_pct', 'state_pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
linke_y = linke_results['pct'].to_numpy()
linke_lm = linear_model.LinearRegression().fit(linke_X, linke_y)

## Gruene
gruene_results = const_results.loc[(const_results['party'] == 'gruene') & (const_results['pct_lag'] > 0)]
gruene_X = gruene_results[['pct_lag', 'state_pct', 'state_pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
gruene_y = gruene_results['pct'].to_numpy()
gruene_lm = linear_model.LinearRegression().fit(gruene_X, gruene_y)

# Model list in alphabetical order by party
const_lm_list = [afd_lm, cdu_lm, fdp_lm, gruene_lm, linke_lm, spd_lm]
const_lm_coef_list = [model.coef_ for model in const_lm_list]
## add zero coefficients to AfD's model
const_lm_coef_list[0] = np.concatenate((const_lm_coef_list[0], np.zeros(shape = (2, ))))

# Extract coefficients for later use
## e.g. coefficients for parties (in alphabetical order) on pct_lag variable
const_coefs_intercept = [model.intercept_ for model in const_lm_list]
const_coefs_pct_lag = [coefs[0] for coefs in const_lm_coef_list]
const_coefs_state_pct = [coefs[1] for coefs in const_lm_coef_list]
const_coefs_state_pct_lag = [coefs[2] for coefs in const_lm_coef_list]
const_coefs_natl_pct = [coefs[3] for coefs in const_lm_coef_list]
const_coefs_natl_pct_lag = [coefs[4] for coefs in const_lm_coef_list]

#%% Constituency-level residual covariance for simulation
# Compute residuals
cdu_results = cdu_results.assign(pred_pct = cdu_lm.predict(cdu_X))
spd_results = spd_results.assign(pred_pct = spd_lm.predict(spd_X))
afd_results = afd_results.assign(pred_pct = afd_lm.predict(afd_X))
fdp_results = fdp_results.assign(pred_pct = fdp_lm.predict(fdp_X))
linke_results = linke_results.assign(pred_pct = linke_lm.predict(linke_X))
gruene_results = gruene_results.assign(pred_pct = gruene_lm.predict(gruene_X))

# Stick them all together
const_residuals = cdu_results\
    .append(spd_results, ignore_index = True)\
    .append(afd_results, ignore_index = True)\
    .append(fdp_results, ignore_index = True)\
    .append(linke_results, ignore_index = True)\
    .append(gruene_results, ignore_index = True)\
    .assign(residual = lambda x: x['pct'] - x['pred_pct'])\
    .loc[:, ['year', 'constituency', 'party', 'residual']]
    
const_residuals_wide = const_residuals\
    .pivot_table(index = ['year', 'constituency'], columns = 'party', values = 'residual')
    
# Calculate covariance ignoring NaNs
const_residual_cov = const_residuals_wide.cov()

#%% State-level regressions
# Again, separate models by party
## CDU
cdu_results = state_results.loc[(state_results['party'] == 'cdu') & (state_results['pct_lag'] > 0)]
cdu_X = cdu_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
cdu_y = cdu_results['pct'].to_numpy()
cdu_state_lm = linear_model.LinearRegression().fit(cdu_X, cdu_y)

## SPD
spd_results = state_results.loc[(state_results['party'] == 'spd') & (state_results['pct_lag'] > 0)]
spd_X = spd_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
spd_y = spd_results['pct'].to_numpy()
spd_state_lm = linear_model.LinearRegression().fit(spd_X, spd_y)

## AfD: use the data from the FDP, Left, and Greens
afd_results = state_results.loc[(state_results['party'].isin(['fdp', 'linke', 'gruene'])) & (state_results['pct_lag'] > 0)]
afd_X = afd_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
afd_y = afd_results['pct'].to_numpy()
afd_state_lm = linear_model.LinearRegression().fit(afd_X, afd_y)

## FDP
fdp_results = state_results.loc[(state_results['party'] == 'fdp') & (state_results['pct_lag'] > 0)]
fdp_X = fdp_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
fdp_y = fdp_results['pct'].to_numpy()
fdp_state_lm = linear_model.LinearRegression().fit(fdp_X, fdp_y)

## Linke
linke_results = state_results.loc[(state_results['party'] == 'linke') & (state_results['pct_lag'] > 0)]
linke_X = linke_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
linke_y = linke_results['pct'].to_numpy()
linke_state_lm = linear_model.LinearRegression().fit(linke_X, linke_y)

## Gruene
gruene_results = state_results.loc[(state_results['party'] == 'gruene') & (state_results['pct_lag'] > 0)]
gruene_X = gruene_results[['pct_lag', 'natl_pct', 'natl_pct_lag']].to_numpy()
gruene_y = gruene_results['pct'].to_numpy()
gruene_state_lm = linear_model.LinearRegression().fit(gruene_X, gruene_y)

# Model list in alphabetical order by party
state_lm_list = [afd_state_lm, cdu_state_lm, fdp_state_lm, gruene_state_lm, 
                 linke_state_lm, spd_state_lm]

# Extract coefficients for later use
## e.g. coefficients for parties (in alphabetical order) on pct_lag variable
state_coefs_intercept = [model.intercept_ for model in state_lm_list]
state_coefs_pct_lag = [model.coef_[0] for model in state_lm_list]
state_coefs_natl_pct = [model.coef_[1] for model in state_lm_list]
state_coefs_natl_pct_lag = [model.coef_[2] for model in state_lm_list]

#%% State-level residual covariance for simulation
# Compute residuals
cdu_results = cdu_results.assign(pred_pct = cdu_state_lm.predict(cdu_X))
spd_results = spd_results.assign(pred_pct = spd_state_lm.predict(spd_X))
fdp_results = fdp_results.assign(pred_pct = fdp_state_lm.predict(fdp_X))
linke_results = linke_results.assign(pred_pct = linke_state_lm.predict(linke_X))
gruene_results = gruene_results.assign(pred_pct = gruene_state_lm.predict(gruene_X))
afd_results = afd_results\
    .assign(pred_pct = afd_state_lm.predict(afd_X))\
    .assign(residual = lambda x: x['pct'] - x['pred_pct'])\
    .groupby(['year', 'state'], as_index = False)\
    .mean()

# Stick them all together
state_residuals = cdu_results\
    .append(spd_results, ignore_index = True)\
    .append(fdp_results, ignore_index = True)\
    .append(linke_results, ignore_index = True)\
    .append(gruene_results, ignore_index = True)\
    .assign(residual = lambda x: x['pct'] - x['pred_pct'])\
    .append(afd_results, ignore_index = True)\
    .loc[:, ['year', 'state', 'party', 'residual']]\
    .fillna({'party': 'afd'})

state_residuals_wide = state_residuals\
    .pivot_table(index = ['year', 'state'], columns = 'party', values = 'residual')

# Calculate covariance ignoring NaNs
state_residual_cov = state_residuals_wide.cov().to_numpy()

# Adjust AfD elements of covariance matrix:
## Set covariance with other parties to zero
state_residual_cov[0, 1:5] = 0
state_residual_cov[1:5, 0] = 0

## Scale up variance due to "too much data"
state_residual_cov[0, 0] = state_residual_cov[0, 0] * np.sqrt(3)