##############################################################
##################~~ Init environment ~~######################
##############################################################

############################
## Thibault Briera
## Feb 2024
## briera.cired@gmail
############################

#clear the workspace but keep the loaded packages and functions
rm(list = setdiff(ls(), c(lsf.str(),'force_install_packages','load_data')))

dir <- getwd()
# do you want to export?
export_plot <- TRUE

##########################
###~~ Loading packages ~##
###~~ and parameters ~####
##########################

if(!exists("load_usr")){ # if the user has not loaded the packages yet
  load_usr <- TRUE
}


if(load_usr){
  source("scripts/packages.R")
  source("scripts/IMC_reg.R") #IMACLIM-R regions
  source("scripts/functions_Viz.R")
}


##########################
########~~ init ~#########
##########################

# init empty variable for the list of scenarios, used to prevent unnecessary loading of the same scenario
scen_list_prev <- c()