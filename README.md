# Electoral Contestation Paper

This file includes all the replication materials for our electoral contestation


1. Start with run_first.rda. This installs the necessary packages, loads helper function. Change the paths as necessary and run this code. You can run everything from within the VM; if you run on own machine, make sure these packages are installed and the paths are changed.
2. The analysis is broken into several chunks within "analysis" folder
3. Main_effects.rda generates the pre-treatment effects and an associated set of tables. Here, we can also specify the question wording effects.
4. Factor models runs several factor models: An EFA, CFA, etc. 
5. Interaction_effects.rda generates the plots for the two-way interactions. These are the primary findings in the paper.  This is still a work in progress.
7. The original, untouched data are data.dta; the codebook is also in the folder. I recode all the data using. Recode.rda and I save the data as raw_data.rda. This is the data that I use throughout all the analyses. Please modify as you see fit.
