################################################
###########~~Plots parameters~~#################
################################################



##### colors
source("scripts/plot_palette.R")

if(vsbaseline){
  facetcolours <- facetcolours[-1]
  if (plot_bench == FALSE) {
    facetcolours <- facetcolours[-length(facetcolours)]
  }
}

#scales and colors :
list_colors <- list()
scenarioviz_colour <- scen_list

if (vsbaseline) {#vsbaseline cases
scenarioviz_colour <- scenarioviz_colour[-1]
  if (plot_bench == FALSE) {
  scenarioviz_colour <- scenarioviz_colour[-length(scenarioviz_colour)]
  }
}

for (i in seq_along(scenarioviz_colour)){
  list_colors[[i]] <- facetcolours[i]
}
names(list_colors) <- scenarioviz_colour


##### text and font size
title_h_just <-  0.5 # plot title height adjustment


length_x_axis <- 25 #max length of x axis title: wrap if longer
police_plot <- "Helvetica"
coefplot <- ifelse(length(regionviz) == 1, 1, 1)
coefplot_legend <- ifelse(length(regionviz) == 1, 1, 1)
coefplot_legend <- ifelse(facet == TRUE, coefplot_legend * 1.1, coefplot_legend)
gridfontsize <- 25 * coefplot

x_text_size <- 22 * coefplot
y_text_size <- 22 * coefplot

legend_title_size <- 25 * coefplot
legend_text_size <- 27 * coefplot_legend

### esthetics
# line esthetics
typ_vs <- "solid"
typ_novs <- "solid"

siz_vs <- 3
siz_novs <- 3
lintyp <-  ifelse(vsbaseline, typ_vs , typ_novs) #linetype: solid for normal plot, dashed for variation
linesize <-  ifelse(vsbaseline, siz_vs, siz_novs) #size of geom_line

# dot esthetics

dotsize <- 12
# axis
breaksyear <-  20 #break between years
break_y_axis  <- 10

coef_axis_plot <- 1 # adjusting y axis height

yearstart_plot <- round(yearstart, digits = -1) #start the plot on decades
# frame and background
margin_plot <- 0.5  #margin aroung plot in cm

color_fill  <-  "white"
color_contour  <-  "white"

spaceplot <-  3 #space between facet_wrap plots

# Facet options
if (!exists("ncol_facet")) {
  ncol_facet <- 3
}
size_text_facet  <-  25 * coefplot
nrow_facet_legend <- 3


### title and legend
graph_title <- df_plot$Variable[1] #title of the plot

#y title angle
angle_title_y <- 90

#legend
legend_position <- "bottom" #legend position
legend_plot <- TRUE

#vline on graphs?
if (!exists("add_vline")) {
  add_vline <- FALSE
}

#name and units for y axis 

nameviz <- df_plot$Variable[1]
unitviz <- df_plot$Unit[1]

