
######################################################################
###########. IMACLIM - loading outputs ..##############################
######################################################################

############################
## Thibault Briera
## March 2024
## briera.cired@gmail
############################

# Filter variable
IMACLIM_data <- IMACLIM_data %>%
  filter(Variable %in% var_IMC)

#world plot
if (length(regionviz) == length(All) && facet == FALSE){
IMACLIM_data <- IMACLIM_data %>%
  filter(Region == "World")
} else {
  IMACLIM_data <- IMACLIM_data %>%
  filter(Region %in% regionviz)
}

# filter years 
IMACLIM_data <- IMACLIM_data %>%
  filter(Year >= yearstart & Year <= yearend)
