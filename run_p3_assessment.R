
#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is to complete step 02 for task 01 ie 
#          calculate weighted averages for on-track and off-track countries
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

### !!! BEFORE EXECUTING THIS FILE, PLEASE SET UP USER PROFILE STORED IN THE MAIN REPOSITORY (user_profile.R) ###

# Read the user functions script 
source(file.path(rCodes, "01_user_packages.R"))

# Read the scripts for task 1
source(file.path(codesTask1, "020101_data_prep.R"))
source(file.path(codesTask1, "020102_data_weighting.R"))
source(file.path(codesTask1, "020103_visual.R"))

# Read the scripts for task 2
source(file.path(codesTask2, "020201_data_prep.R"))
source(file.path(codesTask2, "020202_data_analysis.R"))
source(file.path(codesTask2, "020203_reliability_analysis.R"))

# Render the Quarto/Markdown file and save the output in pdf
quarto_render("p3_doc.qmd")