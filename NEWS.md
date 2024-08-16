accrualPlot 1.0.9
------------------
accrual_create_df: warning if by refers to a NULL object
accrual_create_df: fix typo in documentation (last instead of first date)
test-accrual_plots: named vector for target

accrualPlot 1.0.8
------------------
bug fix in gg_accrual_plot_predict if accrual_df contains only one site and no overall

accrualPlot 1.0.7
------------------
prediction of sample size at specific date

accrualPlot 1.0.6
------------------
allow for empty sites in by (if specified as factor)

accrualPlot 1.0.5
------------------
new `as.data.frame` method for accrual_list objects

accrualPlot 1.0.4
------------------

bug fix in accrual_plot_abs (stacking of bars) fix vignette and help files

accrualPlot 1.0.3
------------------
add a demo dataset `accrualdemo`

accrualPlot 1.0.2
------------------
named vector for target, default plot size for vignette

accrualPlot 1.0.1
------------------
Fix par setting for accrual_plots
Reinstate old par at the end of the vignette

accrualPlot 1.0.0
------------------
Clean help files
Add names to header in print method

accrualPlot 0.6.8
------------------
Update vignette
Small changes in accrual_plot_predict (size of legend) and gg_accrual_plot_abs (name of x-axis title)

accrualPlot 0.6.7
------------------
Remove LazyDate from description

accrualPlot 0.6.6
------------------
gg_accrual_plot_abs: site as factor so that the legend is then the same order as the sites in the accrual_df_list

accrualPlot 0.6.5
------------------
accrual_create_df: named vector for start_date and current_date

accrualPlot 0.6.4
------------------
accrual_time_unit: use weeks form start (last Monday) instead of calender weeks

accrualPlot 0.6.3
------------------
accrual_table: correct typo in weeks
summary: no longer prints the table

accrualPlot 0.6.2
------------------
accrual_table: remove options for start and end date
accrual_table: include 0-entries in accrual_df for calculation of accrual time

accrualPlot 0.6.1
------------------
accrual_plot_predict: remove 0 from center strip

accrualPlot 0.6.0
------------------
addition of names_overall attribute to accrual_df objects
new accrual_list class for accrual_dfs with multiple sites
new ggplot methods comparable to the base graphics capabilities
new print methods for accrual_dfs and accrual_lists
vignette updated to include above info (where appropriate)

accrualPlot 0.5.3
------------------
accrual_df class applied to all dataframes when by is used (allows methods to work on subelements of the list too, e.g. plot(list[[1]]))
tests added for accrual_create_df, accrual_plot_cum

accrualPlot 0.5.2
------------------
bug fix for force_start0 in accrual_create_df
test for accrual_predict

accrualPlot 0.5.1
------------------
helper function for plotting center info in accrual_plot_predict

accrualPlot 0.5.0
------------------
modifications of accrual_plot_predict
- no longer allows input of start and end date
- no longer allows input of enrollment dates
- center info is shown if accrual_df is a list
- separate predictions are shown if accrual_df is a list and target is a vector of the same length
accrual_create_df allows to add overall at top or bottom
removal of overall option from accrual_plot_cum

accrualPlot 0.4.1
------------------
accrual_plot_cum no longer allows input of start and end date

accrualPlot 0.4.0
------------------
accrual_plot_abs produces stacked barplots if accrual_df is a list
no longer allows additional input of start an end dates
no longer produces several plots if accrual_df is a list

accrualPlot 0.3.5
------------------
enrollment_date no longer allowed to include NA values

accrualPlot 0.3.4
------------------
adapt options for start and end dates accrual_create_df

accrualPlot 0.3.3
------------------
removal of possibility to pass dates are strings
>>>>>>> 0c51608820193f8d715acf93d90e88fd59caa751

accrualPlot 0.3.2
------------------
Adapt x-axis labeling for accrual_plot_cum and accrual_plot_predict

accrualPlot 0.3.1
------------------
bug fix for accrual_plot_abs

accrualPlot 0.3.0
------------------
bug fix for accrual_create_df with by as factor
plots for list of accrual data frames (accrual_create_df with by option)

accrualPlot 0.2.0
------------------
new summary and plot methods can be used instead of accrual_plot_x

accrualPlot 0.1.1
------------------
accrual_table works without a by argument

accrualPlot 0.1.0
------------------
accrualPlot uploaded to github

