#####################################################
######~~ Colour palette for plots ~~#################
#####################################################

if (!exists("palette_nb")) {
    palette_nb <- 1
}

# 1: defaut colour palette
# 2: energy mix palette
# 3: viridis palette

#### case 1: default palette https://coolors.co/palettes/popular/10%20colors

if (palette_nb == 1) {
    facetcolours <- c("#001219", "#005F73", "#94D2DB", "#E9D8A6", "#EE9B00", "#CA6702", "#BB3E03", "#AE2021", "#9B2226")
}
### case 2: energy mix palette

if (palette_nb == 2) {
    facetcolours <- c("#003300", "006b00","#0000FF", "#FF6600", "#FFFF00", "#99CCFF", "#990066", "#00e7e7", "#CCCCCC", "#666666", "#CC6600", "#663300")

if (diff_ccs == FALSE) {
    facetcolours <- facetcolours[-c(2,8, 10)] #removing CCS technoss
}
#facetcolours <- facetcolours[seq_along(techno_list_cap)]

}

### case 3: viridis palette

if (palette_nb == 3) {
#using viridis palette
nb_colors <- 9
viridis_list <- viridisLite::viridis(nb_colors)

# and remove the yellow "#FDE725FF"
viridis_list  <- viridis_list[which(viridis_list != "#FDE725FF")]

facetcolours <- c(viridis_list) #colour for Mitigation/Baseline lines
# this palette is supposed to be color blind - friendly
if (nb_colors > length(scenarioviz)+2){
  facetcolours <- facetcolours[-c(3,4)]
}
}

###case 4: de-risking - sensibility analysis palette

if (palette_nb == 4) {
    facetcolours  <- c("#E9D8A6", #C&B
    rep("#e9d8a68e", nb_sensib), #sensib
    "#EE9B00") #converge
}


###case 5: WP2 - GDP losses boxplot - Tol Palette https://personal.sron.nl/~pault/

if (palette_nb == 5) {
    facetcolours  <- rev(c("#222255", "#66CCEE",
 "#EE6677"))
}

if (palette_nb == 5.1) { #medium constrat
    facetcolours  <- c( "#44BB99",
    "#EE6677","#222255","#66CCEE", "#DDCC77" ) # mint- dark yellow - cyan  - red dark blue -   (mostly from medium-contrast)
}

# 5 variant
if (palette_nb == 5.2) {
    facetcolours  <- rev(c("#222255", "#66CCEE",
 "#EE6677"))
}