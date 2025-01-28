#starting and ending year for plots
yearstart <- 2015
yearend <- 2060
#regions to visualize: "all" or a vector of regions
regionviz <- All
#plot against a baseline?
vsbaseline <- FALSE
#want to plot the baseline still?
plot_bench <- TRUE
#plot as % of bench or absolute dev.
plot_bench_percent <- TRUE
#facet plot for multi regions
facet <- FALSE
# force y scale for plots
force_y_scale  <- FALSE


## Visualization parameters
# #number of facet cols
ncol_facet <- 3
#want to export the plot
export_plot <- TRUE
# add a vertical line at 2020
add_vline  <- TRUE
# plot name
plot_name  <- "Plot"
# palette number for plots
palette_nb <- 5 #default palette

marge_plot <- 1

if (!exists("load_data")){
  load_data <- TRUE
}