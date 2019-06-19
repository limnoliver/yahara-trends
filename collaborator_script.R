# Get "targets" out of the workflow

# to find a target of interest, look in the .yml files. You can use and play with any
# targets in these files by using the make command (e.g., to get the model eLists) and 
# listing the target and which yaml file the target is located in. E.g., to get the model
# output for TP:

tp_mod <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')

# the underlying raw data that goes into the model can be found in 10_get_data
# some data were processed before going into the model in 12_get_data_subsets
# e.g., we used daily mean tp concentrations as an input into the model

tp_daily_means <- make('tp_means', remake_file = '12_get_data_subsets.yml')

# if a target is a file, you can just read the data into the environment (or navigate to
# that location and open the file outside of R)

precip <- read.csv('10_get_data/out/msn_precip.csv')

# some data in 10_get_data that are supporting data and not necessarily used in model
# e.g., daily GCLAS loads

gclas_loads <- make('tp_loads_gclas', remake_file = '10_get_data.yml')

# if you want to use a function that builds a target, but outside of the 
#.yml files, you can:

# find where the function is and source it, e.g., to build the countour plots
source('40_analysis_figs/src/plot_change_conditions.R')

# you want to build a different contour plot, that just shows a single year for TP.
# you'll need to build any of the targets that are within the function
# for example:
tp_wy_out <- make('tp_wy_out', remake_file = '30_analyze_data_series.yml')
plot_contours(target_name = 'figures/test.png', eList = tp_wy_out, conc_breaks = seq(0,0.8,.1), yearStart = 1995, yearEnd=1996)
