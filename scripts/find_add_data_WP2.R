
###############################################
##~~ Adding extra info: imperfect foresight  ~##
###############################################


# for each scenario in scenarioviz, open the csv file scenarioviz.csv and extract the information

i <- which(list_IMACLIM_scenarios == IMACLIM_file)
# remove the .xlsx extension
scenarioviz <- gsub(".xlsx", "", list_IMACLIM_scenarios)

IMACLIM_data$Scenario_full <- scenarioviz[i]
# open the csv file
# add a tryCatch sequence
tryCatch({
  df_temp <- as_tibble(read.csv(paste0(dir, "/IMACLIM_outputs/", scenarioviz[i], ".csv"), sep = "|", header = TRUE))
}, error = function(e) {
  print(paste0("Error in reading ", scenarioviz[i], ".csv: the file may not exist"))
  print(e)
})

# extract the information
IMACLIM_data$Truncated_Horizon <- df_temp$Truncated_Horizon
IMACLIM_data$Planning_Horizon <- df_temp$Planning_Horizon
IMACLIM_data$Wait_See <- df_temp$Wait_See
IMACLIM_data$Max_Inj_Rate <- df_temp$Max_Inj_Rate
IMACLIM_data$CB_NZ <- df_temp$CB_NZ
  