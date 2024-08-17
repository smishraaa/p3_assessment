#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script checks if the packages required for the analysis are installed. 
#          If not, it installs them.
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

# Listing the packages used in the analysis
required_packages <- c("rmarkdown",
                       "tidyverse", 
                       "knitr", 
                       "lubridate", 
                       "psych", 
                       "ltm", 
                       "patchwork", 
                       "htmlwidgets", 
                       "networkD3", 
                       "RColorBrewer", 
                       "scales", 
                       "kableExtra", 
                       "broom", 
                       "stargazer", 
                       "srvyr",
                       "quarto"
)

# Writing a function to check and install packages 
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Running the function to check and install packages
check_and_install_packages(required_packages)