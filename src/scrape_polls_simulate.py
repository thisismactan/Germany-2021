import datetime as dt
import numpy as np
import pandas as pd
from os import listdir
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

# Simulate national polling variation
print('Simulating national polling variation....', end = ' ')
natl_poll_sims = np.random.multivariate_normal(poll_average_array, poll_cov + poll_error_cov,
                                               size = n_sims)
natl_poll_sims = np.delete(natl_poll_sims, 5, axis = 1) # get rid of others
natl_poll_sims_eligible = natl_poll_sims.copy()
natl_poll_sims_eligible[natl_poll_sims_eligible < 0.05] = 0

# Simulate state share of total electorate
state_electorate_share = np.random.dirichlet(alpha = 10000 * proj_state_votes['pct_of_electorate'],
                                             size = n_sims)

print('Done!')

# From that, simulate state vote shares
print('Simulating state vote shares....', end = ' ')
## Creating arrays for the known state-level variables
### natl_pct_lag
last_natl_vote = natl_results\
    .loc[natl_results['year'] == 2017, ['party', 'pct']]\
    .pivot_table(columns = 'party', values = 'pct')\
    .to_numpy()
    
last_natl_vote_stack = np.concatenate((last_natl_vote, ) * n_sims)
natl_pct_change = natl_poll_sims - last_natl_vote_stack

natl_pct_change_contrib = np.dstack((natl_pct_change * all_party_state_lm.coef_[1], ) * 16)

### pct_lag
last_state_vote = state_results\
    .loc[state_results['year'] == 2017, ['state', 'party', 'pct']]\
    .pivot_table(index = 'state', columns = 'party', values = 'pct')\
    .to_numpy()
    
# n_sims x 6 x 16 version
last_state_vote_stack = np.dstack((last_state_vote, ) * n_sims).T

last_state_vote_contrib = np.dstack((last_state_vote * all_party_state_lm.coef_[0], ) * n_sims).T

### Intercept
state_intercept_contrib = np.full(shape = (n_sims, 6, 16), 
                                  fill_value = all_party_state_lm.intercept_)

## Simulated state-level error
state_sim_error = np.zeros(shape = (n_sims, 6, 16))

for state in range(16):
    state_sim_error[:, :, state] = np.random.multivariate_normal(
        np.zeros(shape = (6, )), all_party_state_residual_cov, size = n_sims
    )

## Simulated state vote shares are the sum of all these things
state_vote_sims = last_state_vote_stack + state_intercept_contrib\
    + natl_pct_change_contrib + state_sim_error

state_vote_sims[state_vote_sims < 0] = 0
print('Done!')

# National vote share implied by state vote
natl_vote_sims = np.zeros(shape = (n_sims, 6))
for p in range(6):
    natl_vote_sims[:, p] = (state_vote_sims[:, p, :] * state_electorate_share).sum(axis = 1)

#%%
# From that, simulate constituency vote shares
print('Simulating constituency vote shares....', end = ' ')
states_alpha = list(states_key['german_name'].sort_values())

## Creating arrays for the known constituency-level variables
### state_pct
# Initialize a DataFrame
state_vote_sim_unstack = pd.DataFrame()
for s in range(16):
    state_vote_sim_unstack = state_vote_sim_unstack\
        .append(pd.DataFrame(state_vote_sims[:, :, s])\
                    .rename(columns = {0: 'afd', 1: 'cdu', 2: 'fdp', 3: 'gruene',
                                       4: 'linke', 5: 'spd'})\
                .assign(state = states_alpha[s],
                        sim_id = range(n_sims)))

# Copy as many times as needed per state (one copy for each constituency)            
state_pct_unstack = state_vote_sim_unstack.copy()

state_pct_const_unstack_df = const_state_key\
    .drop(columns = 'state_id')\
    .merge(state_pct_unstack, how = 'left', on = 'state')

# Drop into array
state_vote_const_unstack = np.zeros(shape = (n_sims, 6, 299))
for c in range(299):
    state_vote_const_unstack[:, :, c] = state_pct_const_unstack_df\
        .loc[state_pct_const_unstack_df['id'] == c + 1, 
             ['afd', 'cdu', 'fdp', 'gruene', 'linke', 'spd']]

### state_pct_lag
last_state_vote_const = const_results\
    .loc[const_results['year'] == 2017, ['state', 'id', 'party', 'state_pct']]\
    .pivot_table(index = ['id', 'state'], columns = 'party', values = 'state_pct')\
    .to_numpy()

last_state_vote_const_unstack = np.dstack((last_state_vote_const, ) * n_sims).T

state_pct_change_contrib = all_party_lm.coef_[1]\
    * (state_vote_const_unstack - last_state_vote_const_unstack)

### pct_lag
last_const_vote = const_results\
    .loc[const_results['year'] == 2017, ['state', 'id', 'party', 'pct']]\
    .pivot_table(index = ['id', 'state'], columns = 'party', values = 'pct')\
    .to_numpy()

# n_sims x 6 x 299 version
last_const_vote_stack = np.dstack((last_const_vote, ) * n_sims).T

last_const_vote_contrib = np.dstack((last_const_vote * all_party_lm.coef_[0], ) * n_sims).T

### Intercept
const_intercept_contrib = np.full(shape = (n_sims, 6, 299), 
                                  fill_value = all_party_lm.intercept_)

## Simulated constituency-level error
const_sim_error = np.zeros(shape = (n_sims, 6, 299))

for const in range(299):
    const_sim_error[:, :, const] = np.random.multivariate_normal(
        np.zeros(shape = (6, )), all_party_const_residual_cov, size = n_sims
    )

## Simulated constituency vote share is just the sum of these components
const_vote_sims = last_const_vote_stack + const_intercept_contrib\
    + last_const_vote_contrib + state_pct_change_contrib + const_sim_error

const_vote_sims[const_vote_sims < 0] = 0

# Simulated constituency winners
const_sim_winners = pd.DataFrame(np.argmax(const_vote_sims, axis = 1))\
    .assign(sim_id = range(n_sims))\
    .melt(id_vars = 'sim_id', var_name = 'id', value_name = 'winner')
print('Done!')

# Compute constituency seats
print('Tallying constituency seats....', end = ' ')
const_sim_winners['id'] = const_sim_winners['id'] + 1.0
const_sim_winners = const_sim_winners\
    .merge(const_state_key[['id', 'constituency', 'state']], how = 'left', on = 'id')

direct_seats_by_state = const_sim_winners\
    .groupby(['sim_id', 'state', 'winner'], as_index = False)\
    .count()\
    .drop(columns = 'id')\
    .pivot_table(index = ['sim_id', 'state'], columns = 'winner', values = 'constituency',
                 fill_value = 0)\
    .reset_index()

total_direct_seats = const_sim_winners\
    .groupby(['sim_id', 'winner'], as_index = False)\
    .count()\
    .drop(columns = ['id', 'state'])\
    .pivot_table(index = 'sim_id', columns = 'winner', values = 'constituency')\
    .reset_index()

# Make sure all parties have a column
for p in range(6):
    if p not in direct_seats_by_state.columns:
        direct_seats_by_state[str(p)] = 0
    if p not in total_direct_seats.columns:
        total_direct_seats[str(p)] = 0

direct_seats_by_state.columns = [str(x) for x in direct_seats_by_state.columns]
total_direct_seats.columns = [str(x) for x in total_direct_seats.columns]

direct_seats_by_state = pd.melt(direct_seats_by_state, id_vars = ['sim_id', 'state'],
                                var_name = 'winner', value_name = 'direct_seats')\
    .sort_values(['winner', 'sim_id'])

total_direct_seats = pd.melt(total_direct_seats, id_vars = 'sim_id', 
                             var_name = 'winner', value_name = 'direct_seats')\
    .sort_values(['winner', 'sim_id'])
print('Done!')

# Computing party-list seat guarantee based on state vote
print('Computing state-level seat guarantees....', end = ' ')

## First eligibility condition: party wins at least three constituencies
natl_seats_3 = (total_direct_seats\
    .pivot_table(index = 'sim_id', columns = 'winner', values = 'direct_seats',
                 fill_value = 0)\
    .to_numpy()) >= 3
    
## Second eligibility condition: party wins at least 5% of the nationwide vote
natl_pct_5 = natl_vote_sims >= 0.05

## Any party that satisfies *either* is eligible for party-list seats
party_list_eligible = np.dstack((np.logical_or(natl_seats_3, natl_pct_5), ) * 16)

## National vote taking into account either eligibility threshold
natl_vote_sims_eligible = natl_vote_sims.copy()
natl_vote_sims_eligible[~np.logical_or(natl_seats_3, natl_pct_5)] = 0

## State percentages taking eligibility into account
state_pct_eligible = state_vote_sims * party_list_eligible

# Webster/Sainte-Lague seat allocation
## Initialize seat allocation array
state_seat_guarantee = np.zeros(shape = (n_sims, 6, 16))

constituency_counts = states_key.sort_values('german_name').reset_index()['constituencies']

## Loop through states
for s in range(16):
    # Minimum number of seats to be allocated
    total_state_seats = 2 * constituency_counts[s]
    
    # Loop through seats in the state
    for t in range(total_state_seats):
        state_seat_allocation = state_seat_guarantee[:, :, s]
        quotients = state_pct_eligible[:, :, s] / (2 * state_seat_allocation + 1)
        
        # Add one to state_seat_allocation where quotients is equal to its row max
        max_quotient_inds = np.argmax(quotients, axis = 1)
        state_seat_allocation[range(n_sims), max_quotient_inds] += 1
    
    # Put result in corresponding sub-array of seats_allocated
    state_seat_guarantee[:, :, s] = state_seat_allocation

# Calculate party-list seats
direct_seats_by_state_array = np.zeros(shape = (n_sims, 6, 16))

for s in range(16):
    direct_seats_by_state_array[:, :, s] = direct_seats_by_state\
        .loc[direct_seats_by_state['state'] == states_alpha[s], :]\
        .pivot_table(index = ['state', 'sim_id'], columns = 'winner', 
                     values = 'direct_seats')\
        .to_numpy()

# Calculate party-list seats awarded (cannot be negative)
party_list_seats = np.maximum(state_seat_guarantee - direct_seats_by_state_array, 0)
print('Done!')

print('Calculating leveling seats....', end = ' ')
# Total seats before leveling
## By state
total_state_seats_pre_level = party_list_seats + direct_seats_by_state_array

## Nationally
total_natl_seats_pre_level = total_state_seats_pre_level.sum(axis = 2)

total_votes_prev = proj_state_votes['total_votes'].sum()

# Calculate national divisors (if eligible, it's total second votes / (seats - 0.5))
natl_divisors = total_votes_prev * natl_vote_sims_eligible / (total_natl_seats_pre_level - 0.5)

# Set negative or zero divisors to infinity so we can ignore them when taking minima
natl_divisors[natl_divisors <= 0] = np.inf
min_natl_divisors = np.vstack((natl_divisors.min(axis = 1), ) * 6).T

# Calculate total and leveling seats
total_natl_seats = np.round(total_votes_prev * natl_vote_sims_eligible / min_natl_divisors)
leveling_seats = total_natl_seats - total_natl_seats_pre_level
print('Done!')

# Allocate leveling seats by state, again using Webster/Sainte-Lague
print('Distributing leveling seats across states....', end = ' ')
leveling_seats_by_state = np.zeros(shape = (n_sims, 6, 16))
total_state_seats_post_level = total_state_seats_pre_level.copy()

## This time start by looping through parties and distribute by state
for p in range(6):
    leveling_seats_to_allocate = leveling_seats[:, p] - leveling_seats_by_state.sum(axis = 2)[:, p]
    while np.any(leveling_seats_to_allocate != 0):
        # Distinguish between positive and negative leveling seats
        positive_leveling_seats = (leveling_seats_to_allocate > 0)
        negative_leveling_seats = (leveling_seats_to_allocate < 0)
        
        # # Calculate quotients by state
        # state_leveling_quotients = np.matmul(state_vote_sims[:, p, :], 
        #                                      np.diag(proj_state_votes['total_votes']))\
        #     / (2 * total_state_seats_post_level[:, p, :] + 1)
        
        # Calculate quotients by state
        state_leveling_quotients = state_vote_sims[:, p, :] * total_votes_prev\
            * state_electorate_share / (2 * total_state_seats_post_level[:, p, :] + 1)
        
        # For simulations where party gets positive leveling seats: add to state with highest quotient
        largest_quotient_inds = np.argmax(state_leveling_quotients, axis = 1)
        positive_leveling_inds = largest_quotient_inds[positive_leveling_seats]
        total_state_seats_post_level[positive_leveling_seats, p, positive_leveling_inds] += 1
        leveling_seats_by_state[positive_leveling_seats, p, positive_leveling_inds] += 1
        
        # For simulations where party gets negative leveling seats: subtract from state with lowest quotient
        smallest_quotient_inds = np.argmin(state_leveling_quotients, axis = 1)
        negative_leveling_inds = smallest_quotient_inds[negative_leveling_seats]
        total_state_seats_post_level[negative_leveling_seats, p, negative_leveling_inds] += -1
        leveling_seats_by_state[negative_leveling_seats, p, negative_leveling_inds] += -1
        
        # Recalculate leveling seats to allocate
        leveling_seats_to_allocate = leveling_seats[:, p] - leveling_seats_by_state.sum(axis = 2)[:, p]

print('Done!')

# Concatenate results into handy-dandy data frames
print('Writing results....', end = ' ')
party_rename_dict = {0: 'afd', 1: 'cdu', 2: 'fdp', 3: 'gruene', 4: 'linke', 5: 'spd'}

## National-level vote simulations
natl_vote_sims_df = pd.DataFrame(np.round(natl_vote_sims, decimals = 4))\
    .rename(columns = party_rename_dict)\
    .assign(sim_id = range(n_sims))\
    .melt(id_vars = 'sim_id', var_name = 'party', value_name = 'pct')
    
natl_vote_sims_df.to_csv('output/natl_sims.csv', index = False)

## State-level DataFrames to create: total seats, direct seats, party-list seats,
## leveling seats, vote share
total_state_seats_df = pd.DataFrame()
direct_state_seats_df = pd.DataFrame()
party_list_seats_df = pd.DataFrame()
leveling_seats_df = pd.DataFrame()
state_vote_share_df = pd.DataFrame()

for s in range(16):
    # Total seats
    total_state_seats_df = total_state_seats_df\
        .append(pd.DataFrame(total_state_seats_post_level[:, :, s])\
                    .rename(columns = party_rename_dict)\
                    .assign(state = states_alpha[s],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'state'], var_name = 'party',
                          value_name = 'total_seats'))
    
    # Direct seats
    direct_state_seats_df = direct_state_seats_df\
        .append(pd.DataFrame(direct_seats_by_state_array[:, :, s])\
                    .rename(columns = party_rename_dict)\
                    .assign(state = states_alpha[s],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'state'], var_name = 'party',
                          value_name = 'direct_seats'))
            
    # Party-list seats
    party_list_seats_df = party_list_seats_df\
        .append(pd.DataFrame(party_list_seats[:, :, s])\
                    .rename(columns = party_rename_dict)\
                    .assign(state = states_alpha[s],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'state'], var_name = 'party',
                          value_name = 'party_list_seats'))
    
    # Leveling seats
    leveling_seats_df = leveling_seats_df\
        .append(pd.DataFrame(leveling_seats_by_state[:, :, s])\
                    .rename(columns = party_rename_dict)\
                    .assign(state = states_alpha[s],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'state'], var_name = 'party',
                          value_name = 'leveling_seats'))
    
    # Vote share
    state_vote_share_df = state_vote_share_df\
        .append(pd.DataFrame(np.round(state_vote_sims[:, :, s], decimals = 4))\
                    .rename(columns = party_rename_dict)\
                    .assign(state = states_alpha[s],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'state'], var_name = 'party',
                          value_name = 'pct'))

## Join them all together
state_sims = state_vote_share_df\
    .merge(total_state_seats_df, how = 'left', on = ['sim_id', 'state', 'party'])\
    .merge(direct_state_seats_df, how = 'left', on = ['sim_id', 'state', 'party'])\
    .merge(party_list_seats_df, how = 'left', on = ['sim_id', 'state', 'party'])\
    .merge(leveling_seats_df, how = 'left', on = ['sim_id', 'state', 'party'])

## Write it to a CSV
state_sims.to_csv('output/state_sims.csv', index = False)

# Constituency DataFrame: all you need is votes
const_sims = pd.DataFrame()

for c in range(299):
    const_sims = const_sims\
        .append(pd.DataFrame(const_vote_sims[:, :, c])\
                    .rename(columns = party_rename_dict)\
                    .assign(constituency = const_state_key['constituency'][c],
                            sim_id = range(n_sims))\
                    .melt(id_vars = ['sim_id', 'constituency'], var_name = 'party',
                          value_name = 'pct'))

const_sims.to_csv('output/const_sims.csv', index = False)

print('Done!')

#%% Summary statistics
seat_summary_stats = state_sims\
    .loc[:, ['sim_id', 'state', 'party', 'total_seats']]\
    .pivot_table(index = ['sim_id', 'state'], columns = 'party', values = 'total_seats')\
    .assign(total = lambda x: x['afd'] + x['cdu'] + x['fdp'] + x['gruene'] + x['linke'] + x['spd'],
            cdu_fdp = lambda x: x['cdu'] + x['fdp'],
            cdu_spd = lambda x: x['cdu'] + x['spd'],
            spd_gruene = lambda x: x['spd'] + x['gruene'],
            cdu_fdp_gruene = lambda x: x['cdu'] + x['fdp'] + x['gruene'],
            spd_fdp_gruene = lambda x: x['spd'] + x['fdp'] + x['gruene'])\
    .reset_index()\
    .groupby('sim_id', as_index = False)\
    .sum()\
    .melt(id_vars = ['sim_id', 'total'], var_name = 'coalition', value_name = 'seats')\
    .assign(seat_frac = lambda x: x['seats'] / x['total'])\
    .groupby('coalition')\
    .agg(prob_majority = pd.NamedAgg(column = 'seat_frac', aggfunc = lambda x: (x > 0.5).mean()),
         pct_05 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.05))),
         pct_50 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.5))),
         pct_95 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.95))))\
    .reset_index()\
    .assign(date = dt.datetime.today().strftime('%Y-%m-%d'),
            state = 'National')\
    .loc[:, ['date', 'state', 'coalition', 'prob_majority', 'pct_05', 'pct_50', 'pct_95']]

print(seat_summary_stats)

# Add state timeline
state_seat_summary_stats = state_sims\
    .loc[:, ['sim_id', 'state', 'party', 'total_seats']]\
    .pivot_table(index = ['sim_id', 'state'], columns = 'party', values = 'total_seats')\
    .reset_index()\
    .groupby(['sim_id', 'state'], as_index = False)\
    .sum()\
    .melt(id_vars = ['sim_id', 'state'], var_name = 'coalition', value_name = 'seats')\
    .groupby(['state', 'coalition'])\
    .agg(pct_05 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.05))),
         pct_50 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.5))),
         pct_95 = pd.NamedAgg(column = 'seats', aggfunc = lambda x: np.round(np.quantile(x, q = 0.95))))\
    .reset_index()\
    .assign(date = dt.datetime.today().strftime('%Y-%m-%d'))\
    .loc[:, ['date', 'state', 'coalition', 'pct_05', 'pct_50', 'pct_95']]

seat_summary_stats = seat_summary_stats\
    .append(state_seat_summary_stats)

# If summary statistics over time isn't in output folder, write it
if 'summary_stats_timeline.csv' not in listdir('output'):
    seat_summary_stats.to_csv('output/summary_stats_timeline.csv', index = False)

# Read in timeline and append the summary stats to it, keeping the more recent of duplicate rows
summary_stats_timeline = pd.read_csv('output/summary_stats_timeline.csv')\
    .append(seat_summary_stats)\
    .drop_duplicates(subset = ['date', 'coalition', 'state'], keep = 'last', ignore_index = True)

# Write it back out
summary_stats_timeline.to_csv('output/summary_stats_timeline.csv', index = False)

#### To add: timeline by state as well