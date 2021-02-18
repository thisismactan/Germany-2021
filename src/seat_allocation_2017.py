import pandas as pd
import numpy as np

#%% Minimum seat guarantees
state_results_2017 = state_results\
    .loc[state_results['year'] == 2017, ['state', 'party', 'votes']]\
    .merge(states_key[['german_name', 'constituencies']].rename(columns = {'german_name': 'state'}), 
           how = 'left', on = 'state')
    
state_results_2017_array = state_results_2017\
    .drop(columns = 'constituencies')\
    .pivot_table(index = 'state', columns = 'party', values = 'votes')\
    .to_numpy()
    
state_seat_guarantee_2017 = np.zeros(shape = (16, 6))
constituency_counts_2017 = states_key.sort_values('german_name').reset_index()['constituencies']

for s in range(16):
    # Minimum number of seats to be allocated
    total_state_seats = 2 * constituency_counts_2017[s]
    
    # Loop through seats in the state
    for t in range(total_state_seats):
        state_seat_allocation_2017 = state_seat_guarantee_2017[s, :]
        quotients = state_results_2017_array[s, :] / (2 * state_seat_allocation_2017 + 1)
        
        # Add one to state_seat_allocation where quotients is equal to its row max
        max_quotient_ind = np.argmax(quotients)
        state_seat_allocation_2017[max_quotient_ind] += 1
    
    # Put result in corresponding sub-array of seats_allocated
    state_seat_guarantee_2017[s, :] = state_seat_allocation_2017

state_seat_guarantee_2017_df = pd.DataFrame(state_seat_guarantee_2017)\
    .rename(columns = {0: 'afd', 1: 'cdu', 2: 'fdp', 3: 'gruene', 4: 'linke', 5: 'spd'})\
    .assign(state = states_key.sort_values('german_name').reset_index()['german_name'])
    
#%% Direct mandates
const_results_2017 = const_results\
    .loc[const_results['year'] == 2017, ['constituency', 'state_name', 'party', 'votes']]\
    .rename(columns = {'state_name': 'state'})
    
max_const_votes = const_results_2017\
    .groupby(['constituency', 'state'])\
    .apply(max)\
    .drop(columns = ['constituency', 'state', 'party'])\
    .reset_index()

direct_mandates_by_state_2017 = const_results_2017\
    .merge(max_const_votes, how = 'left', on = ['constituency', 'state'])

direct_mandates_by_state_2017 = direct_mandates_by_state_2017\
    .loc[direct_mandates_by_state_2017['votes_x'] == direct_mandates_by_state_2017['votes_y'], :]\
    .groupby(['state', 'party'], as_index = False)\
    .count()\
    .drop(columns = ['votes_x', 'votes_y'])\
    .pivot_table(index = 'state', columns = 'party', values = 'constituency', fill_value = 0)

direct_mandates_by_state_2017['fdp'] = 0
direct_mandates_by_state_2017 = direct_mandates_by_state_2017\
    .loc[:, ['afd', 'cdu', 'fdp', 'gruene', 'linke', 'spd']]\
    .reset_index()

#%%
seat_composition_2017 = direct_mandates_by_state_2017\
    .melt(id_vars = 'state', var_name = 'party', value_name = 'direct_mandates')\
    .merge(state_seat_guarantee_2017_df.melt(id_vars = 'state', var_name = 'party', value_name = 'guarantee'))\
    .assign(party_list_seats = lambda x: np.maximum(x['guarantee'] - x['direct_mandates'], 0))\
    .assign(total_seats = lambda x: x['direct_mandates'] + x['party_list_seats'])