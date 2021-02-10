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