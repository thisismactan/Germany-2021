import datetime as dt
import numpy as np
import pandas as pd
from string import punctuation

#%% Scrape and clean polls
polls_url = 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election'
poll_tables = pd.read_html(polls_url)

# Bit of cleanup
polls_2021 = poll_tables[0]

# Rename columns
polls_2021.columns = ['pollster', 'dates', 'n', 'abs', 'cdu', 'spd', 'afd',
                      'fdp', 'linke', 'gruene', 'other', 'lead']

# Convert n to numeric
polls_2021['n'] = pd.to_numeric(polls_2021['n'].str.replace('[' + punctuation + ']', ''))

# Fill NA's with minimum sample size in the data
polls_2021['n'].fillna(polls_2021['n'].min(), inplace = True)

# Start and end dates
## End dates first (easier)
end_date_list = [i[1] for i in polls_2021['dates'].str.split('–')]

## End month-years and just years
end_monthyear = [j[1] + ' ' + j[2] for j in [i.split(' ') for i in end_date_list]]
end_year = [j[2] for j in [i.split(' ') for i in end_date_list]]

## Now use those to create start dates
start_date_list = [i[0] for i in polls_2021['dates'].str.split('–')]
for i in range(len(start_date_list)):
    # If all we have is the day, add the same month-year as end date
    if len(start_date_list[i]) <= 2:
        start_date_list[i] = start_date_list[i] + ' ' + end_monthyear[i]
        continue
    # If all we have is the day and month, add the same year as end date
    if len(start_date_list[i]) < 10:
        start_date_list[i] = start_date_list[i] + ' ' + end_year[i]
        continue

#%% Create some relevant variables
# Variables related to field dates
polls_2021['start_date'] = pd.to_datetime(pd.Series(start_date_list), format = '%d %b %Y')
polls_2021['end_date'] = pd.to_datetime(pd.Series(end_date_list), format = '%d %b %Y')
polls_2021['spread'] = (polls_2021['end_date'] - polls_2021['start_date']) + pd.Timedelta('1 day')
polls_2021['median_date'] = (polls_2021['start_date'] + 0.5 * polls_2021['spread']).dt.round(freq = 'D')
polls_2021['spread'] = polls_2021['spread'].dt.days

# Drop columns I don't care about
polls_2021_long = polls_2021[['start_date', 'median_date', 'end_date', 'spread',
                              'pollster', 'n', 'cdu', 'spd', 'afd', 'fdp', 'linke',
                              'gruene', 'other']]\
    .melt(id_vars = ['start_date', 'median_date', 'end_date', 'spread',
                     'pollster', 'n'], var_name = 'party', value_name = 'pct')
    
# Convert to percentage scale
polls_2021_long['pct'] = polls_2021_long['pct'] / 100
polls_2021_long.to_csv('data/polls_2021.csv', index = False)

# Add some more variables after writing to CSV
polls_2021_long['age'] = (dt.datetime.today() - polls_2021_long['median_date']).dt.days

#%% Polling average
# Create weights
polls_2021_long = polls_2021_long.loc[polls_2021_long['age'] <= 60]
polls_2021_long['weight'] = (polls_2021_long['n'] ** 0.25) * np.exp(-(polls_2021_long['age'] + 1) ** 0.5)

poll_average = polls_2021_long\
    .groupby('party')\
    .agg(avg = ('pct', lambda x: np.average(x, weights = polls_2021_long.loc[x.index, 'weight'])))
    
print(poll_average)

#%% Create arrays for polling average and covariance\
# Convert the poll DataFrame to an array
polls_2021_wide = polls_2021_long\
    .pivot_table(index = ['median_date', 'pollster', 'weight'],
                 columns = 'party', values = 'pct')
    
polls_array = polls_2021_wide\
    .reset_index()[['afd', 'cdu', 'fdp', 'gruene', 'linke', 'other', 'spd']]\
    .to_numpy()

weight_array = np.array([i[2] for i in polls_2021_wide.index])
weight_array = weight_array / weight_array.sum()
eff_n = (weight_array.sum() ** 2) / ((weight_array ** 2).sum())

# Poll averages and covariance
poll_average_array = np.dot(polls_array.T, weight_array)
poll_cov = np.cov(polls_array, rowvar = False, aweights = weight_array)

#%% For calculation of covariance for simulation, add WMSE from 2017 polling
polls_2017 = pd.read_csv('data/polls_2017.csv')\
    .assign(weight = lambda x: (x['n'] ** 0.25) / np.exp((x['age'] + 1) ** 0.5))

polls_2017_long = pd.melt(polls_2017, id_vars = ['pollster', 'median_date', 'age', 'n', 'weight'],
                          var_name = 'party', value_name = 'pct')\
    .assign(pct = lambda x: x['pct'] / 100)
    
polls_2017_error = polls_2017_long\
    .merge(natl_results.loc[natl_results['year'] == 2017, ['party', 'pct']],
           how = 'left', on = 'party')\
    .assign(error = lambda x: x['pct_x'] - x['pct_y'])\
    .fillna(0)

polls_2017_error_array = polls_2017_error[['pollster', 'median_date', 'party', 'weight', 'error']]\
    .pivot_table(index = ['pollster', 'median_date', 'weight'], columns = 'party', values = 'error')\
    .to_numpy()

# Weights
weight_array_2017 = polls_2017['weight'].to_numpy()
weight_array_2017 = weight_array_2017 / weight_array_2017.sum()

# Poll error covariance
poll_error_cov = np.cov(polls_2017_error_array, rowvar = False, 
                        aweights = weight_array_2017)

#%% Simulation
# 10,000 simulations; set seed
n_sims = 10000
np.random.seed(2021)

# Simulate national vote shares
natl_vote_sims = np.random.multivariate_normal(poll_average_array, poll_cov + poll_error_cov,
                                               size = n_sims)
natl_vote_sims = np.delete(natl_vote_sims, 5, axis = 1) # get rid of others

# From that, simulate state vote shares
## Creating arrays for the known state-level variables
### natl_pct
natl_vote_contrib = np.dstack(
    (np.matmul(natl_vote_sims, np.diag(state_coefs_natl_pct)), ) * 16
)

### natl_pct_lag
last_natl_vote = natl_results\
    .loc[natl_results['year'] == 2017, ['party', 'pct']]\
    .pivot_table(columns = 'party', values = 'pct')\
    .to_numpy()
    
last_natl_vote_stack = np.concatenate((last_natl_vote, ) * n_sims)
last_natl_vote_contrib = np.dstack(
    (np.matmul(last_natl_vote_stack, np.diag(state_coefs_natl_pct_lag)), ) * 16
)

### pct_lag
last_state_vote = state_results\
    .loc[state_results['year'] == 2017, ['state', 'party', 'pct']]\
    .pivot_table(index = 'state', columns = 'party', values = 'pct')\
    .to_numpy()

last_state_vote_contrib = np.dstack(
    (np.matmul(last_state_vote, np.diag(state_coefs_pct_lag)), ) * n_sims
).T

### Intercept
state_intercept_contrib = np.dstack(
    (np.vstack(
        (np.array(state_coefs_intercept), ) * n_sims
    ), ) * 16
)

## Simulated state-level error
state_sim_error = np.zeros(shape = (n_sims, 6, 16))

for state in range(16):
    state_sim_error[:, :, state] = np.random.multivariate_normal(
        np.zeros(shape = (6, )), state_residual_cov, size = n_sims
    )

## Simulated state vote shares are the sum of all these things
state_vote_sims = state_intercept_contrib + natl_vote_contrib + last_natl_vote_contrib\
    + last_state_vote_contrib + state_sim_error

# From that, simulate constituency vote shares
## Creating arrays for the known constituency-level variables
### natl_pct
natl_vote_const_contrib = np.dstack(
    (np.matmul(natl_vote_sims, np.diag(const_coefs_natl_pct)), ) * 299
)

### natl_pct_lag
last_natl_vote_const_contrib = np.dstack(
    (np.matmul(last_natl_vote_stack, np.diag(const_coefs_natl_pct_lag)), ) * 299
)

#%%
### state_pct
# Initialize a DataFrame
state_vote_sim_unstack = pd.DataFrame()

for state in range(16):
    state_vote_sim_unstack = state_vote_sim_unstack\
        .append(pd.DataFrame(state_vote_sims[:, :, state])\
                    .rename(columns = {0: 'afd', 1: 'cdu', 2: 'fdp', 3: 'gruene',
                                       4: 'linke', 5: 'spd'})\
                .assign(state_name = list(states_key['german_name'].sort_values())[state],
                        sim_id = range(n_sims)))
#%%
### state_pct_lag
last_state_vote_const = const_results\
    .loc[const_results['year'] == 2017, ['state', 'id', 'party', 'state_pct']]\
    .pivot_table(index = ['id', 'state'], columns = 'party', values = 'state_pct')\
    .to_numpy()

last_state_vote_const_contrib = np.dstack(
    (np.matmul(last_state_vote_const, np.diag(const_coefs_state_pct_lag)), ) * n_sims
).T

### pct_lag
last_const_vote = const_results\
    .loc[const_results['year'] == 2017, ['state', 'id', 'party', 'pct']]\
    .pivot_table(index = ['id', 'state'], columns = 'party', values = 'pct')\
    .to_numpy()

last_const_vote_contrib = np.dstack(
    (np.matmul(last_const_vote, np.diag(const_coefs_pct_lag)), ) * n_sims
).T

### Intercept
const_intercept_contrib = np.dstack(
    (np.vstack((np.array(const_coefs_intercept), ) * n_sims), ) * 299
)

## Simulated constituency-level error
const_sim_error = np.zeros(shape = (n_sims, 6, 299))

for const in range(299):
    const_sim_error[:, :, const] = np.random.multivariate_normal(
        np.zeros(shape = (6, )), const_residual_cov, size = n_sims
    )
