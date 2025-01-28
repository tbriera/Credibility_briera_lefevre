theme_plot <- theme_bw() +
  theme(plot.title = element_blank(),
        strip.text.x = element_text(size = gridfontsize), #facet grid size
        axis.title.y = element_text(family = police_plot, face = "plain", size = 30, angle = angle_title_y), # horizontal y axis
        axis.text.x = element_text(family = police_plot, face = "plain", size = x_text_size),
        axis.text.y = element_text(family = police_plot, face = "plain", size = y_text_size),
        axis.title.x = element_blank(),
        # axis.ticks.x = element_blank(), #remove ticks
        #axis.ticks.y = element_blank(),
        legend.title = element_text(family = police_plot, face = "plain", size = legend_text_size),
        legend.text = element_text(family = police_plot, face = "plain", size = legend_text_size),
        legend.position = legend_position,
        legend.spacing.x = unit(0.8, "cm"),
        panel.border = element_blank(),
        #add light grey background
        panel.background = element_blank(),
        #remove grid
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()
        )

theme_facet <- theme(
      strip.text.x = element_text(size = size_text_facet, face = "plain", family = police_plot),
      strip.background = element_rect(
        color = color_contour, fill = color_fill, size = 1.5, linetype = "solid"
      ),
      panel.spacing = unit(spaceplot, "lines") # space between panels
    )


theme_duo  <- theme(axis.title.y = element_blank(),
  legend.spacing.y = unit(0.4, "cm")) # y axis

theme_trio  <- theme(legend.spacing.y = unit(0.4, "cm"))
