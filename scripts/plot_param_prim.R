facetcolours <- c("#003300", "#0000FF", "#FF6600", "#FFFF00", "#99CCFF", "#990066", "#003333", "#CCCCCC", "#666666", "#CC6600", "#663300")

if (diff_ccs == FALSE) {
    facetcolours <- facetcolours[-c(8, 10)] #removing CCS technoss
}

facetcolours <- facetcolours[seq_along(var_list_fil)]
