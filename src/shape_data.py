import pandas as pd
import re

#%% Initial read-in
results_2005 = pd.read_csv('data/results_2005.csv').assign(year = 2005)
results_2009 = pd.read_csv('data/results_2009.csv').assign(year = 2009)
results_2013 = pd.read_csv('data/results_2013.csv').assign(year = 2013)
results_2017 = pd.read_csv('data/results_2017.csv').assign(year = 2017)

# Keep only columns with these strings in them
patterns = ['year', 'id', 'constituency', 'state', 'total_votes', 'valid', 
            'cdu', 'spd', 'linke', 'pds', 'fdp', 'gruene', 'afd']
col_pattern = re.compile('^(?:' + '|'.join(patterns) + ')')

#%% Cleanup and reshape
# Results from 2005 to 2017
results = results_2005.append(results_2009, ignore_index = True)\
    .append(results_2013, ignore_index = True)\
    .append(results_2017, ignore_index = True)\
    .fillna(0)

cols_to_keep = [x for x in list(results.columns) if re.search(col_pattern, x)]

# Smoosh together CDU and CSU votes
results['cdu_1'] = results['cdu_1'] + results['csu_1']
results['cdu_1_p'] = results['cdu_1_p'] + results['csu_1_p']
results['cdu_2'] = results['cdu_2'] + results['csu_2']
results['cdu_2_p'] = results['cdu_2_p'] + results['csu_2_p']
results = results[cols_to_keep]

results_long = results\
    .melt(id_vars = ['id', 'year', 'constituency', 'state', 'total_votes', 
                     'total_votes_p', 'valid_1', 'valid_1_p', 'valid_2', 
                     'valid_2_p'],
          var_name = 'party', value_name = 'votes')\
    .assign(pct_1 = lambda x: x['votes'] / x['valid_1'],
            pct_2 = lambda x: x['votes'] / x['valid_2'],
            pct_1_lag = lambda x: x['votes'] / x['valid_1_p'],
            pct_2_lag = lambda x: x['votes'] / x['valid_2_p'])

# Four types of results: first or second vote, and then current or lagged
## 1st vote for the current election
cols_1 = ['id', 'year', 'constituency', 'state', 'party', 'votes', 'pct_1']
results_1 = results_long\
    .loc[results_long['party'].str.contains('_1$'), cols_1]\
    .rename(columns = {'pct_1': 'pct'})
results_1['party'] = results_1['party'].str.replace('_1$', '')

## 1st vote for the previous election
cols_1_lag = ['id', 'year', 'constituency', 'state', 'party', 'votes', 'pct_1_lag']
results_1_lag = results_long\
    .loc[results_long['party'].str.contains('_1_p$'), cols_1_lag]\
    .rename(columns = {'pct_1_lag': 'pct_lag', 'votes': 'votes_lag'})
results_1_lag['party'] = results_1_lag['party'].str.replace('_1_p$', '')

## 2nd vote for the current election    
cols_2 = ['id', 'year', 'constituency', 'state', 'party', 'votes', 'pct_2']
results_2 = results_long\
    .loc[results_long['party'].str.contains('_2$'), cols_2]\
    .rename(columns = {'pct_2': 'pct'})
results_2['party'] = results_2['party'].str.replace('_2$', '')

## 2nd vote for the previous election
cols_2_lag = ['id' ,'year', 'constituency', 'state', 'party', 'votes', 'pct_2_lag']
results_2_lag = results_long\
    .loc[results_long['party'].str.contains('_2_p$'), cols_2_lag]\
    .rename(columns = {'pct_2_lag': 'pct_lag', 'votes': 'votes_lag'})
results_2_lag['party'] = results_2_lag['party'].str.replace('_2_p$', '')

# Join results with lags
results_1 = results_1\
    .merge(results_1_lag, how = 'left', on = ['id', 'year', 'state', 'constituency', 'party'])

results_2 = results_2\
    .merge(results_2_lag, how = 'right', on = ['id', 'year', 'state', 'constituency', 'party'])
    
#%% Split up national, state, and constituency levels
natl_cols = ['year', 'party', 'votes', 'pct', 'votes_lag', 'pct_lag']
natl_results = results_2\
    .loc[results_2['constituency'] == 'Bundesgebiet', natl_cols]

state_cols = ['year', 'constituency', 'party', 'votes', 'pct', 'votes_lag', 'pct_lag']
state_results = results_2\
    .loc[(results_2['state'] == 0) & (results_2['constituency'] != 'Bundesgebiet'), state_cols]\
    .rename(columns = {'constituency': 'state'})\
    .merge(natl_results[['year', 'party', 'pct', 'pct_lag']]\
               .rename(columns = {'pct': 'natl_pct', 'pct_lag': 'natl_pct_lag'}))

states_key = pd.read_csv('data/states.csv')
const_state_key = results_2017\
    .loc[:, ['id', 'constituency', 'state']]\
    .rename(columns = {'state': 'state_id'})\
    .merge(states_key.loc[:, ['state_id', 'german_name']].rename(columns = {'german_name': 'state'}),
           how = 'inner', on = 'state_id')

const_results = results_1\
    .loc[results_1['id'] < 900, :]\
    .merge(state_results.merge(states_key[['state_id', 'german_name']], how = 'left', 
                               left_on = 'state', right_on = 'german_name')\
               .drop(columns = ['votes', 'votes_lag', 'german_name'])\
               .rename(columns = {'state': 'state_name', 'pct': 'state_pct',
                                  'pct_lag': 'state_pct_lag', 'state_id': 'state'}),
           how = 'inner', on = ['year', 'party', 'state'])

state_results.to_csv('data/state_results.csv', index = False)
const_results.to_csv('data/constituency_results.csv', index = False)

