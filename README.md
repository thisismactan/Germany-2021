# Germany-2021
## What is this?
You're looking at a forecast for the German federal election tentatively scheduled for September 26, 2021. (Well, technically you're looking at the README for it. But you get the idea.) As with many previous election forecasts I've done at the [Election StatSheet](https://quora.com/q/elections) space on Quora, I've thrown the source code and necessary datasets up on Github for two primary reasons: 

  1. **To practice good data science housekeeping for myself.** Nothing incentivizes good programming practices like doing it in public.
  2. **To provide transparency.** That's mainly so you, the interested reader/programmer/data scientist, can see that I'm not just making stuff up. But you can also play with my results and see what conclusions you can draw from it should it strike your fancy; it can also inform recommendations you have for improvements on my end.
 
## How does it work?
Like all the forecasts I do, the German forecast relies on simulation to obtain probability distributions for the relevant parties/candidates. Three Python scripts are needed to build the models and run the simulations from scratch:

  1. [`shape_data.py`](https://github.com/thisismactan/Germany-2021/blob/master/src/shape_data.py): for taking historical election data and doing the necessary reshaping and feature engineering for model-building, as well as creating some useful supplementary DataFrames;
  2. [`models.py`](https://github.com/thisismactan/Germany-2021/blob/master/src/models.py): for taking the datasets built in `shape_data.py` and building models to predict state- and constituency-level vote from national results and previous election results, as well as computing the error structure; and
  3. [`scrape_polls_simulate.py`](https://github.com/thisismactan/Germany-2021/blob/master/src/scrape_polls_simulate.py): for scraping polls of the election and using them along with the models to simulate 10,000 elections as well as the seat allocation process.

This last script writes several CSV files to the `output` directory that record the results of the simulations. I've used the `visualizations.R` script to generate some useful plots of the results, including things like vote distributions, seat distributions, and the evolution of the forecast over time.

Those are the essentials, really; the wiki goes into some more detail on a few things. I've gotten to work on this one relatively early, so later on in the year I'll begin writing forecast updates and going in-depth on how the forecast works on the aforementioned Quora space. Be sure to check that out!
