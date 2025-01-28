######################################################################
###########. main plots - NCC - Credibility - V2..##################
######################################################################

############################
## Thibault Briera
## January 2024
## briera.cired@gmail
############################


#################################
####.. Environment ..############
#################################

#want to force the installation of the packages?
force_install_packages <- FALSE

suppressWarnings(source("scripts/env.R"))
library(ggmagnify)

var_list_update <- FALSE # want to update the list of variables?

############################################
#########~~ Locating xlsx files ~~ #######
############################################

# init
list_IMACLIM_scenarios <- c()

extract_scen <- c("014", "1440","1441", "1442", "1443")
# all files in IMACLIM_outputs
list_IMACLIM_scenarios_raw <- list.files(paste0(dir, "/IMACLIM_outputs"), full.names = TRUE)

# keep only the xlsx files
list_IMACLIM_scenarios_raw <- list_IMACLIM_scenarios_raw[grep(".xlsx", list_IMACLIM_scenarios_raw)]

# keep only the file name
list_IMACLIM_scenarios_raw <- gsub(paste0(dir,"/IMACLIM_outputs/"), "", list_IMACLIM_scenarios_raw)

# getting all the xlsx files that start with extract_scen
for (scen in extract_scen){
  list_IMACLIM_scenarios <- c(list_IMACLIM_scenarios_raw[grep(paste0("^", scen), list_IMACLIM_scenarios_raw)],list_IMACLIM_scenarios)
}

##########################
####~~ Initialization ~###
##########################

source("scripts/settings/CO2_pathways_WP2.R")

##########################
####~~ Data extraction ~###
##########################
#create an emply df_plot to store the data
df_plot <- tibble(Model = character(),
Scenario = character(),
Scenario_full = character(),
Region = character(),
Variable = character(),
Unit = character(),
Year = double(),
Value = double(),
Truncated_Horizon = double(),
Planning_Horizon = double(),
Wait_See = double(),
Max_Inj_Rate = double(),
CB_NZ = double())

total_emi <- "Emissions|CO2|Energy"

sectoral_emi <- c("Emissions|CO2|Energy|Supply","Emissions|CO2|Energy|Demand","Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Other Sector","Emissions|CO2|Energy|Supply|Electricity", "Emissions|CO2|Energy and Industrial Processes","Emissions|CO2|Industrial Processes")

techno_elec <- c("Biomass|w/ CCS","Biomass|w/o CCS","Coal|w/ CCS","Coal|w/o CCS","Gas|w/ CCS","Gas|w/o CCS","Hydro","Nuclear","Oil","Solar","Wind")

primary_ener <- c("Biomass|w/ CCS","Biomass|w/o CCS","Coal|w/ CCS","Coal|w/o CCS","Gas|w/ CCS","Gas|w/o CCS","Hydro","Nuclear","Oil","Solar","Wind")

primary_ener_bio <- c("Primary Energy|Biomass|Electricity|w/ CCS","Primary Energy|Biomass|Electricity|w/o CCS","Primary Energy|Biomass|Liquids")
second_elec <- c("Biomass|w/ CCS","Biomass|w/o CCS","Coal|w/ CCS","Coal|w/o CCS","Gas|w/ CCS","Gas|w/o CCS","Hydro","Nuclear","Oil","Solar","Wind")

second_liquids <- c("Biomass","Coal","Oil")


capacity  <- paste0("Capacity|Electricity|",techno_elec)
capacity_add  <- paste0("Capacity Additions|Electricity|",techno_elec)
energy  <- paste0("Primary Energy|",primary_ener)
liquids <- paste0("Secondary Energy|Liquids|",second_liquids)

elec <- paste0("Secondary Energy|Electricity|",second_elec)

var_IMC  <-  c(total_emi, sectoral_emi,capacity,capacity_add,energy,elec,liquids ,primary_ener_bio,"Emissions|CO2|Energy","GDP|MER", "Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Energy|Demand|Transportation|Passenger|LDV","Final Energy","Primary Energy","Final Energy|Residential and Commercial","Final Energy|Electricity")

#extracting all variables at the time
if (load_data){
  for (IMACLIM_file in list_IMACLIM_scenarios){
    print(paste0("Loading ", IMACLIM_file,". ", which(IMACLIM_file == list_IMACLIM_scenarios), " / ", length(list_IMACLIM_scenarios)))
    source("scripts/load_IMACLIM_outputs.R")
      # new columns: Trun_horizon, Plan_horizon, Wait_see, Max_Inj_Rate, CB
    IMACLIM_data  <- IMACLIM_data  %>% mutate(Truncated_Horizon = c(seq_along(nrow(IMACLIM_data))),
                                  Planning_Horizon = c(seq_along(nrow(IMACLIM_data))),
                                  Wait_See = c(seq_along(nrow(IMACLIM_data))),
                                  Max_Inj_Rate = c(seq_along(nrow(IMACLIM_data))),
                                  CB_NZ = c(seq_along(nrow(IMACLIM_data))))

  # keep the all dataset in memory for bar plot
    source("scripts/find_add_data_WP2.R")
    source("scripts/process_IMACLIM_data.R")


    #merging dataset
    df_plot <- rbind(df_plot, IMACLIM_data)

}
  #reloading data?
  load_data <- FALSE
}

#merging two scenarios: NZD and NZD wait and see
df_plot$Scenario <- recode(df_plot$Scenario, "Impfor-NDC" = "CurPol", "Impfor-NZ" = "Early", "Impfor-NZ_WS" = "Early", "Impfor-NZD" = "Delayed", "Impfor-NZD_WS" = "Delayed")

#reorder the scenario: Early, Delayed, NDC

df_plot$Scenario <- factor(df_plot$Scenario, levels = c("Early", "Delayed", "CurPol"))

#filter only the scenarios of interest: 
#-planning horizon of 20y
plan_limit <- 50
df_plot <- df_plot %>% filter(Planning_Horizon == plan_limit)

#-only the variables of interest
df_plot <- df_plot %>% mutate(Cred = case_when(
Truncated_Horizon == 1 & Wait_See == 0 ~ "Myopic",
Truncated_Horizon == plan_limit & Wait_See == 0 ~ "Full",
Truncated_Horizon == 5 & Wait_See == 1 ~ "Low",
TRUE ~ "other cases",
))

df_plot <- df_plot %>% filter(Cred != "other cases")

#keeping the full dataset in memory for bar plots
df_plot_all <- df_plot




line_plot <- function(df,minVal, maxVal, yearstart, yearend, unit_plot){
  plot <- ggplot(data = df, aes(x = Year, y = Value, color = Scenario)) +
  geom_line(aes(linetype = Cred), alpha = 0.5, size = 2) + 
  geom_point(aes(shape = Cred), size = 6) +
  scale_colour_manual(values = facetcolours, aesthetics = c("color"))  +
  scale_linetype_manual(values=c(1,1,1)) +
  theme_plot +
  theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(axis.line = element_line(color = 'white')) + 
  #ylab(expression(paste("[GTCO2 ", yr^{-1}, "]"))) + 
  theme(plot.margin = margin(margin_plot, margin_plot, margin_plot, margin_plot, "cm"))  

  if (missing(minVal)|missing(maxVal)){} else {
     plot  <- plot + scale_y_continuous(limits = c(minVal * coef_axis_plot, maxVal * coef_axis_plot)) 
  }
  if (missing(yearstart)|missing(yearend)){} else {
     plot  <- plot +  scale_x_continuous(breaks = seq(yearstart, yearend, by = breaksyear), limits = c(yearstart, yearend))  

# title
if (missing(unit_plot)){
  unit_plot <- unique(df$Unit)
}
plot  <- plot + ylab(paste(unit_plot))

}


# renaming the legend: Scenario is 'Policy Scenario' and Cred is 'Credibility Level'
  plot <- plot + labs(color = "Policy Scenario", shape = "Credibility")

  # legend on 3 lines
  plot <- plot + guides(color=guide_legend(nrow=3,byrow=TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 5, alpha = 1))) + guides(shape=guide_legend(nrow=3,byrow=TRUE)) + guides(linetype=FALSE)

}

mixplot <- function(df, plot_dev, minVal, maxVal, yearstart, yearend, breaksyear, yearbreakstart,yearbreakend,unit_plot){

if (plot_dev == FALSE){
  var <- "Value"
} else {
  var <- "Deviation"
  df  <- df %>% filter(Cred != "Full")

}



plot <- ggplot(data = df, aes(x = Year, y = get(var), fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = facetcolours, aesthetics = c("fill")) +
  theme_plot + theme_facet + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.line = element_line(color = 'white')) +
  theme(plot.margin = margin(margin_plot, margin_plot, margin_plot, margin_plot, "cm")) +
  theme(panel.spacing = unit(1.5, "lines")) + #space between panels
  xlab("Year") +
 theme(strip.text.y = element_text(size = size_text_facet)) + #increase the size of the y axis text
  facet_grid(Scenario ~ Cred) + # remove the legend title
  theme(legend.title = element_blank()) # remove the legend title


  if (missing(minVal)|missing(maxVal)){} else {
     plot  <- plot + scale_y_continuous(limits = c(minVal * coef_axis_plot, maxVal * coef_axis_plot)) 
  }
  if (missing(breaksyear)){
    breaksyear <- 20
  }
  if (missing(yearstart)|missing(yearend)){} else {
     plot  <- plot +  scale_x_continuous(breaks = seq(yearbreakstart, yearbreakend, by = breaksyear), limits = c(yearstart, yearend))  

  }

  # title

  if (missing(unit_plot)){
    unit_plot <- unique(df$Unit)
  }
  plot  <- plot + ylab(c(unit_plot))

  return(plot)
}

prepare_df_bar <- function(df) {
  df <-  df  %>% group_by(Scenario, Cred) %>% filter(Year == 2020)  %>% unique()

  return (df)
}

barplot <- function(df){ 
  
  plot <- ggplot(data = df, aes(x = Scenario, y = CB_NZ, color= Scenario)) +
  geom_point(aes(shape = Cred), size = 6)  +
 # scale_fill_manual(values = facetcolours, aesthetics = c("fill")) +
  theme_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.line = element_line(color = 'white')) +
  scale_y_continuous(position = "right") +
  theme(plot.margin = margin(margin_plot, margin_plot, margin_plot, margin_plot, "cm")) +
  theme(axis.text.x = element_blank())+
  theme(legend.position = "none")+
  ylab(c("GtCO2")) +
  xlab("Scenarios")  

  return(plot)
}
################################################################################################################
############################ Plot 1: CO2 pathways
################################################################################################################
df_plot <- df_plot  %>% filter(Variable == total_emi)

#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))


# in GtCO2
df_plot$Value <- df_plot$Value / 10^3


scen_list <- rev(unique(df_plot$Scenario))
source("scripts/plot_param.R")


##### manually overwrite some parameters
length_x_axis <- 25
x_text_size <- x_text_size*1.3
y_text_size <- y_text_size*1.3


source("scripts/theme_plot.R")

minVal <- -5
maxVal <- 40

df_plot %>% filter(Scenario == "CurPol")

p <- line_plot(df_plot, minVal = minVal, maxVal = maxVal, yearstart = 2020, yearend = yearend, unit_plot = "GtCO2 per year") 

######preparing csv outputs: CO2 emi
CO2_emi_2030 <- df_plot %>% filter(Year == 2030)  %>% select(Scenario, Cred, Value) %>% mutate(Value = round(Value,0))
CO2_emi_2035 <- df_plot %>% filter(Year == 2035)  %>% select(Scenario, Cred, Value) %>% mutate(Value = round(Value,0))
CO2_emi_2040  <- df_plot %>% filter(Year == 2040)  %>% select(Scenario, Cred, Value)%>% mutate(Value = round(Value,0))
CO2_emi_2050  <- df_plot %>% filter(Year == 2050)  %>% select(Scenario, Cred, Value)%>% mutate(Value = round(Value,0))
######preparing csv outputs: emission gap with respect to the Perfect Foresight case

CO2_gap_2030 <- df_plot  %>% group_by(Year,Scenario) %>% mutate(Value = round(Value - Value[Cred == "Full"],1)) %>% filter(Year == 2030)  %>% select(Scenario, Cred, Value) 
CO2_gap_2035  <- df_plot  %>% group_by(Year,Scenario) %>% mutate(Value = round(Value - Value[Cred == "Full"],1)) %>% filter(Year == 2035)  %>% select(Scenario, Cred, Value) 
CO2_gap_2040  <- df_plot  %>% group_by(Year,Scenario) %>% mutate(Value = round(Value - Value[Cred == "Full"],1)) %>% filter(Year == 2040)  %>% select(Scenario, Cred, Value) 

################################################################################################################
############################ Plot 2: cumul CO2
################################################################################################################
df_plot_bar <- df_plot %>% prepare_df_bar
# manually overwrite some parameters

source("scripts/plot_param.R")

df_plot_bar <- df_plot_bar %>% filter(Scenario != "CurPol")

# bar plot for CB_NZ, grouped by scenario and credibility

p_bar <- barplot(df_plot_bar)

######preparing csv outputs:total emission gap, in % of perfect foresight case

CO2_gap_total_pct <- df_plot_bar  %>% group_by(Scenario) %>% mutate(Value = round((CB_NZ / CB_NZ[Cred == "Full"] - 1)*100,1))  %>% select(Scenario, Cred, Value) 

df_plot_bar %>% select(Scenario, Cred, CB_NZ) 
################################################################################################################
############################ Plot 3: Difference in energy mix vs full credibility case, over time
################################################################################################################
# facet by scenario and level of credibility

#loading the full dataset
df_plot <- df_plot_all
# primary energy variables
df_plot <- df_plot  %>% filter(Variable %in% energy)


# removing "Primary Energy" at the beginning of the string
df_plot$Variable <- gsub("Primary Energy", "", df_plot$Variable)

#removing the first character
df_plot$Variable <- substr(df_plot$Variable, 2, nchar(df_plot$Variable))

#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

df_plot$Variable<- factor(df_plot$Variable, levels = c("Oil","Gas|w/o CCS", "Gas|w/ CCS", "Coal|w/o CCS", "Coal|w/ CCS", "Biomass|w/o CCS", "Biomass|w/ CCS", "Nuclear", "Hydro", "Solar", "Wind"))

# for each Year, calculate the deviation from the Full Credibility case (Cred = "Full Credibility")
df_plot_dev <- df_plot %>% group_by(Scenario, Year) %>% mutate(Deviation = Value - Value[Cred == "Full"])


palette_nb  <- 2
diff_ccs <- TRUE
source("scripts/plot_param.R")

p_ener <- mixplot(df = df_plot_dev, plot_dev = FALSE,yearstart = 2010, yearend = 2075, yearbreakstart = 2020, yearbreakend = 2070, breaksyear = 20, unit_plot = "EJ per year")

p_dev_ener <- mixplot(df = df_plot_dev  %>% filter(Scenario != "CurPol"), plot_dev = TRUE,yearstart = 2010, yearend = 2055, yearbreakstart = 2020, yearbreakend = 2050, breaksyear = 10, unit_plot = "EJ per year")



######preparing csv outputs: differences in primary energy
# FF wo CCS: sum Coal|w/o CCS, Gas|w/o CCS, Oil
df_plot_dev %>% filter(Scenario == "Delayed", Year == 2040)  %>% select(Variable, Truncated_Horizon, Value, Deviation) 

# sum if the variable is Coal|w/o CCS, Gas|w/o CCS, Oil
techno_dev <- c("Coal|w/o CCS", "Gas|w/o CCS", "Oil")
prim_ener_dev_FF_2035 <- df_plot_dev %>% group_by(Scenario, Cred,Year) %>%  filter(Variable %in% techno_dev & Year == 2035) %>% select(Scenario,Year, Cred,Variable, Deviation)  %>% summarise(Deviation = round(sum(Deviation),0))  

techno_dev <- c("Coal|w/ CCS", "Gas|w/ CCS")
prim_ener_dev_FFCCS_2035 <- df_plot_dev %>% group_by(Scenario, Cred,Year) %>%  filter(Variable %in% techno_dev & Year == 2035) %>% select(Scenario, Year, Cred,Variable, Deviation)  %>% summarise(Deviation = round(sum(Deviation),0))

techno_dev <- c("Biomass|w/ CCS", "Biomass|w/o CCS","Hydro","Nuclear","Solar","Wind")
prim_ener_dev_lowc_2035 <- df_plot_dev %>% group_by(Scenario, Cred,Year) %>%  filter(Variable %in% techno_dev & Year == 2035) %>% select(Scenario, Year, Cred,Variable, Deviation)  %>% summarise(Deviation = round(sum(Deviation),0))
################################################################################################################
############################ Plot 4: Difference in capacity additions vs full credibility case, over time
################################################################################################################
# facet by scenario and level of credibility

#loading the full dataset
df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% capacity_add)

# removing "Capacity Additions|Electricity" at the beginning of the string
df_plot$Variable <- gsub("Capacity Additions|Electricity", "", df_plot$Variable)

#removing the first two characters
df_plot$Variable <- substr(df_plot$Variable, 3, nchar(df_plot$Variable))
#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

# remove CurPol case
df_plot <- df_plot %>% filter(Scenario != "CurPol")

df_plot$Variable<- factor(df_plot$Variable, levels = c("Oil","Gas|w/o CCS", "Gas|w/ CCS", "Coal|w/o CCS", "Coal|w/ CCS", "Biomass|w/o CCS", "Biomass|w/ CCS", "Nuclear", "Hydro", "Solar", "Wind"))

palette_nb  <- 2
diff_ccs <- TRUE
source("scripts/plot_param.R")

df_plot %>% filter(Year==2040 & Variable == "Solar")
  # for each Year, calculate the deviation from the Full Credibility case (Cred = "Full Credibility")
df_plot_dev <- df_plot %>% group_by(Scenario, Year) %>% mutate(Deviation = Value - Value[Cred == "Full"])

p_cap_add <- mixplot(df = df_plot_dev, plot_dev = FALSE,yearstart = 2010, yearend = 2075, yearbreakstart = 2020, yearbreakend = 2070, breaksyear = 20,, unit_plot = "GW per year")

p_dev_cap_add <- mixplot(df = df_plot_dev  %>% filter(Scenario != "CurPol"), plot_dev = TRUE,yearstart = 2010, yearend = 2055, yearbreakstart = 2020, yearbreakend = 2050, breaksyear = 10, unit_plot = "GW per year")


################################################################################################################
############################ Plot 5:  Capacity over time
################################################################################################################
# facet by scenario and level of credibility

#loading the full dataset
df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% capacity)

# removing "Capacity Additions|Electricity" at the beginning of the string
df_plot$Variable <- gsub("Capacity|Electricity", "", df_plot$Variable)

#removing the first two characters
df_plot$Variable <- substr(df_plot$Variable, 3, nchar(df_plot$Variable))
#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

# change the order of the factors: Coal, Gas, Oil, Nuclear, Hydro, Solar, Wind, Biomass, Other
df_plot$Variable<- factor(df_plot$Variable, levels = c("Oil","Gas|w/o CCS", "Gas|w/ CCS", "Coal|w/o CCS", "Coal|w/ CCS", "Biomass|w/o CCS", "Biomass|w/ CCS", "Nuclear", "Hydro", "Solar", "Wind"))
#in TW
df_plot$Value <- df_plot$Value * 10^3
df_plot %>% filter(Year == 2040) %>% filter(Variable == "Gas|w/o CCS")%>% mutate(Value = round(Value,0)) %>% print(n=100) 
p_cap <- mixplot(df = df_plot, plot_dev = FALSE,yearstart = 2010, yearend = 2075, yearbreakstart = 2020, yearbreakend = 2070, breaksyear = 20, unit_plot = "TW")
################################################################################################################
############################ Plot 5:  Secondary elec over time
################################################################################################################
# facet by scenario and level of credibility

#loading the full dataset
df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% elec)

# removing "Capacity Additions|Electricity" at the beginning of the string
df_plot$Variable <- gsub("Secondary Energy|Electricity", "", df_plot$Variable)

#removing the first two characters
df_plot$Variable <- substr(df_plot$Variable, 3, nchar(df_plot$Variable))
#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

# change the order of the factors: Coal, Gas, Oil, Nuclear, Hydro, Solar, Wind, Biomass, Other
df_plot$Variable<- factor(df_plot$Variable, levels = c("Oil","Gas|w/o CCS", "Gas|w/ CCS", "Coal|w/o CCS", "Coal|w/ CCS", "Biomass|w/o CCS", "Biomass|w/ CCS", "Nuclear", "Hydro", "Solar", "Wind"))

# for each Year, calculate the deviation from the Full Credibility case (Cred = "Full Credibility")
df_plot_dev <- df_plot %>% group_by(Scenario, Year) %>% mutate(Deviation = Value - Value[Cred == "Full"])

p_elec <- mixplot(df = df_plot_dev, plot_dev = FALSE,yearstart = 2010, yearend = 2075, yearbreakstart = 2020, yearbreakend = 2070, breaksyear = 20, unit_plot = "EJ per year")

df_plot_dev <- df_plot_dev %>% filter(Cred != "Full")

p_dev_elec <- mixplot(df = df_plot_dev  %>% filter(Scenario != "CurPol"), plot_dev = TRUE, yearstart = 2010, yearend = 2055, yearbreakstart = 2020, yearbreakend = 2050, breaksyear = 10, unit_plot = "EJ per year")




################################################################################################################
############################ Plot 7:  Demand|Transportation|Passenger|LDV vs Emissions|CO2|Energy|Supply|Electricity
################################################################################################################

#loading the full dataset
df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% c("Emissions|CO2|Energy|Supply|Electricity","Emissions|CO2|Energy|Demand|Transportation|Passenger|LDV"))


# removing "Primary Energy|" at the beginning of the string
df_plot$Variable <- gsub("Emissions|CO2|Energy|", "", df_plot$Variable)

#removing the first character
df_plot$Variable <- substr(df_plot$Variable, 4, nchar(df_plot$Variable))

#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

# in GtCO2
df_plot$Value <- df_plot$Value / 10^3

#renave the "Demand|Transportation|Passenger|LDV" into "Trans.|Passenger|LDV"
df_plot$Variable <- ifelse(df_plot$Variable == "Demand|Transportation|Passenger|LDV", "Trans.|Passenger|LDV", df_plot$Variable)

######preparing csv outputs: emission gap with respect to the Perfect Foresight case, in percentage
CO2_emi_sector_2035 <- df_plot %>% group_by(Year, Scenario, Variable)  %>% mutate(Value = round((Value / Value[Cred == "Full"]-1)*100),0)  %>%filter(Year == 2035)  %>% select(Scenario, Cred, Variable, Value)  %>% print(n=100)

# new colours
palette_nb=5
source("scripts/plot_param.R")

# filter only Delayed scenario
df_plot <- df_plot %>% filter(Scenario == "Delayed" | (Scenario == "Early" & Cred == "Myopic"))
# NOTE: CO2 emi from cars decrease too much??
# Use the custom labeller in your plot
p_elecvstrans <- line_plot(df_plot) + facet_wrap(Variable ~. , scales = 'free_y',  labeller = labeller(Variable = label_wrap_gen(width = 2))) +  scale_x_continuous(breaks = seq(2020, yearend, by = breaksyear), limits = c(2020, yearend))  + ylab(c("GtCO2 per year")) + theme_facet + theme(panel.spacing = unit(1.5, "lines"))

# selecting the 2nd color of the palette with scale_color_manual

p_elecvstrans <- p_elecvstrans + scale_color_manual(values = c(facetcolours[1],facetcolours[2]), aesthetics = c("color")) + theme(legend.position = "bottom")


# bar plots
df_plot_bar <- df_plot %>% prepare_df_bar
# manually overwrite some parameters

source("scripts/plot_param.R")

df_plot_bar <- df_plot_bar %>% filter(Scenario != "CurPol")

# bar plot for CB_NZ, grouped by scenario and credibility

p_bar_LDV <- barplot(df_plot_bar %>% filter(Variable == "Trans.|Passenger|LDV"))

################################################################################################################
############################ Plot 8:  Final Energy
################################################################################################################

#loading the full dataset

df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% c("Final Energy"))

# removing "Final Energy|" at the beginning of the string

df_plot$Variable <- gsub("Final Energy", "", df_plot$Variable)

#removing the first character

df_plot$Variable <- substr(df_plot$Variable, 2, nchar(df_plot$Variable))

#remove Nan values

df_plot <- df_plot %>% filter(!is.na(Value))

# change the order of the factors: Coal, Gas, Oil, Nuclear, Hydro, Solar, Wind, Biomass, Other
df_plot$Variable<- factor(df_plot$Variable, levels = c("Oil","Gas|w/o CCS", "Gas|w/ CCS", "Coal|w/o CCS", "Coal|w/ CCS", "Biomass|wo/ CCS", "Biomass|w/ CCS", "Nuclear", "Hydro", "Solar", "Wind"))

# new colours

palette_nb=5
source("scripts/plot_param.R")

# 2 line plot for each variable

p_fin <- line_plot(df_plot, minVal = 0, maxVal = 700, yearstart = yearstart, yearend = yearend)

################################################################################################################
############################ Plot 9:  Primary Energy
################################################################################################################

#loading the full dataset

# df_plot <- df_plot_all

# df_plot <- df_plot  %>% filter(Variable %in% c("Primary Energy"))

# # removing "Final Energy|" at the beginning of the string

# df_plot$Variable <- gsub("Primary Energy", "", df_plot$Variable)

# #removing the first character

# df_plot$Variable <- substr(df_plot$Variable, 2, nchar(df_plot$Variable))

# #remove Nan values

# df_plot <- df_plot %>% filter(!is.na(Value))

# # new colours

# palette_nb=5
# source("scripts/plot_param.R")

# # 2 line plot for each variable

# p_prim <- line_plot(df_plot, minVal = 0, maxVal = 700, yearstart = yearstart, yearend = yearend)


################################################################################################################
############################ Plot 10:  sectoral emi
################################################################################################################

# loading the full dataset
df_plot <- df_plot_all

df_plot <- df_plot  %>% filter(Variable %in% c("Emissions|CO2|Energy|Demand|Residential and Commercial","Emissions|CO2|Energy|Demand|Industry","Emissions|CO2|Energy|Demand|Transportation","Emissions|CO2|Energy|Demand|Other Sector","Emissions|CO2|Industrial Processes"))


# removing "Emissions|CO2"" at the beginning of the string
df_plot$Variable <- gsub("Emissions|CO2", "", df_plot$Variable)

# removing "Energy" at the beginning of the string
df_plot$Variable <- gsub("Energy", "", df_plot$Variable)

#removing the first character
df_plot$Variable <- substr(df_plot$Variable, 3, nchar(df_plot$Variable))

#remove Nan values
df_plot <- df_plot %>% filter(!is.na(Value))

# in GtCO2
df_plot$Value <- df_plot$Value / 10^3

# new colours
palette_nb=5
source("scripts/plot_param.R")
# 2 line plot for each variable

p_sec <- ggplot(data = df_plot, aes(x = Year, y = Value,color = Scenario)) +
  geom_line(aes(linetype = Cred), alpha = 0.7, size = 2) + 
  scale_colour_manual(values = facetcolours, aesthetics = c("color"))  +
  scale_x_continuous(breaks = seq(yearstart_plot, yearend, by = breaksyear), limits = c(yearstart, yearend))  +
  # scale_y_continuous(limits = c(minVal * coef_axis_plot, maxVal * coef_axis_plot)) +
  ylab(expression(paste("[GTCO2 ", yr^{-1}, "]"))) +
  theme_plot +
  theme(panel.background = element_rect(fill = "white", colour = "white"))+
  facet_grid(Variable ~ .)

p_sec  <- p_sec + theme(strip.text.y = element_text(size = size_text_facet))

#put the legend on two lines
p_sec   <- p_sec   + guides(linetype=guide_legend(nrow=2,byrow=TRUE))


p_resid <- line_plot(df_plot %>% filter(Variable == "|Demand|Residential and Commercial"), yearstart = 2020, yearend = yearend, unit_plot = "GtCO2 per year") 

###########################################################################################################
############################Exporting
################################################################################################################

list_export <- c("p","p_ener","p_dev_ener", "p_cap_add", "p_dev_cap_add", "p_cap", "p_elec", "p_elecvstrans","p_dev_elec","p_resid")

# exporting plots
for (i in list_export){
  ggsave(paste0("outputs/", i, ".png"), plot = get(i), width = 30, height = 20, units = "cm")
}

ggsave(paste0("outputs/", "p_bar", ".png"), plot = p_bar, width = 10, height = 10, units = "cm")

ggsave(paste0("outputs/", "p_elecvstrans", ".png"), plot = p_elecvstrans, width = 30, height = 20, units = "cm")

#### exporting csv files for Latex

for (i in c("CO2_emi_2030","CO2_emi_2035","CO2_emi_2040","CO2_emi_2050","CO2_gap_2030","CO2_gap_2035","CO2_gap_2040","CO2_gap_total_pct","prim_ener_dev_FF_2035","prim_ener_dev_FFCCS_2035","prim_ener_dev_lowc_2035","CO2_emi_sector_2035")){
  write.csv(get(i), file = paste0("outputs/", i, ".csv"), row.names = TRUE)
}