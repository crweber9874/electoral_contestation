# Electoral Contestation Paper

This repository uses the 2020 Western Data. You may need to change directories and install packages, but begin by running

1. run_first.r

**This file loads dependencies, configures drives, and so forth. The user should modify according to their computer settings.**

2. recode.r

**This file accomplishes all the recodes, transformations, etc. It spits out a file called *dat*. It also displays a frequency histogram, for descriptive purposes**

3. Measurement. factor_models.R

**This file runs the measurement models; in particular, EFA and CFA models.**


4. baseline.R

**This file runs the baseline models and produces a images**


5. helper_functions.R 

**This code includes all the functions I wrote and use throughout the analysis -- i.e., I piece together the marginal structural model in lavaan and then boostrap. You can ignore this, unless you're changing estimators. This file includes all the functions to generate graphics. Most functions return a list or a data frame that is post processed with some of the graphics functions.***

6.  winner_loser_estimation.R


**The primary analysis in the paper. This estimates the winner and loser models using the marginal structural model. It calls helper functions regularly.**


7.  simulation_parameters.R

**The file with the details to simulate values, run lavaan models, etc. It's sort of a misc file that includes verbose code that I repeatedely call.**

8. appendix.r

**This file generates the robustness models/figures for the appendix.

