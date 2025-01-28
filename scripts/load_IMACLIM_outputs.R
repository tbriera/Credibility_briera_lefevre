
######################################################################
###########. IMACLIM - loading outputs ..##############################
######################################################################

############################
## Thibault Briera
## March 2024
## briera.cired@gmail
############################

# load excel
path_IMACLIM_outputs <- paste0(dir, "/IMACLIM_outputs/")

IMACLIM_data <- read_excel(paste0(path_IMACLIM_outputs, "/", IMACLIM_file), sheet = "data")

# Pivoting in long format for Years to 2015 to 2100, Year in numeric, Scenario as factor
IMACLIM_data <- IMACLIM_data %>%
  pivot_longer(cols = starts_with(c("20","21")), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(str_remove(Year, "X")))  %>% 
  mutate(Scenario = as.factor(Scenario)) %>% 
  mutate(Region = as.factor(Region))

# Values as numeric
IMACLIM_data <- IMACLIM_data %>%
  mutate(Value = as.numeric(Value))

if (var_list_update){
    # do not count the NA only values
  var_list_export <- tibble(Varn_names = IMACLIM_data  %>% filter(!is.na(Value)) %>% select(Variable) %>%  unique())
  #exporting in csv in the IMACLIM_outputs folder
  write.csv(var_list_export$Varn_names, paste0(path_IMACLIM_outputs, "/var_list.csv"))
}
