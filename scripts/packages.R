##########################
#####~~ Packages ~########
##########################


# loading packages

packages_list <- c("plyr",
"tidyverse",
"ggplot2",
"RColorBrewer",
"Hmisc", # %nin%
"extrafont",
"patchwork", #+ in ggplots
"ggrepel",
"ggpubr",
"roxygen2", #  for documentation
"extrafont",
"grid",
"latex2exp",
"readxl", #read_excel function
"ggbreak" #breaking axis on ggplots
)

for (i in packages_list) {
  if (!require(i, character.only = TRUE)) {
    if (force_install_packages) {
      install.packages(i, dependencies = TRUE)
    }
    library(i, character.only = TRUE)
  }
}

# font
#font_import()
# and suppressing warnings
suppressMessages({
  
  if(!exists("import_font")){
  loadfonts(device = "win")
  import_font=TRUE
  }

})