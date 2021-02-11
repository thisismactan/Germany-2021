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
