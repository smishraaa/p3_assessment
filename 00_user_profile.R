#-----------------------------------------------------------------------------------------------------------
# Project: P3 Assessment 
# 
# Purpose: This script is used to set the working directories for the project
# 
# Date: 16 August 2024
#-----------------------------------------------------------------------------------------------------------

# set working directories and all directories 

# !!!  PROFILE MUST BE LOADED BEFORE RUNNING ANY OTHER SCRIPT IN THE REPO !!!

# !!! Two options are provided for setting the working directory
#
#!!! Option 1: Manually specifying the directory. For mac users do (USER == "<yoursername>")

# if (USERNAME == "<add your user name here>"){
# projectFolder  <- file.path(Sys.getenv("USERPROFILE"), "<this is a demo adjust as needed> Documents/GitHub/p3_assessment")
#
# !!! Option 2: Automatically setting the directory based on the user name
# 
#
# if (USERNAME == "<your username>") {
#
#projectFolder  <- getwd()
# } 

#version from everyone, the profile works for everyone


USERNAME    <- Sys.getenv("USERNAME")
USER        <- Sys.getenv("USER")

if (USERNAME == "smishra") {
  
  projectFolder  <- getwd()
  
}


# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))


inputData             <-  file.path(projectFolder, "01_rawdata")
rCodes                <-  file.path(projectFolder, "02_codes")
codesTask1            <-  file.path(rCodes, "0201_task01")
codesTask2            <-  file.path(rCodes, "0202_task02")
outputData            <-  file.path(projectFolder, "03_output")


# check if directories exist
stopifnot(dir.exists(inputData))
stopifnot(dir.exists(rCodes))
stopifnot(dir.exists(codesTask1))
stopifnot(dir.exists(codesTask2))
stopifnot(dir.exists(outputData))

