rm(list = ls())

# Load require libraries
library(tidyverse)
library(rjson)

# Source utility functions
source("utils/utils.R")

# Define working directories (NOTE THIS IS MACHINE SPECIFIC)
wds = list(
  root = "C:/Users/marcu/Dropbox/GitKraken/white-rhino/repositories/Kidsights-REDCap-Translation", 
  onedrive = "C:/Users/marcu/OneDrive - University of Nebraska Medical Center/Documents - Kidsights Data/General",
)


setwd(wds$root)

# Construct a temporary directory and make copies of flat/JSON files 
if(!dir.exists("tmp")){dir.create("tmp")}
file.copy(
  from = file.path(wds$onedrive,
                   "Phase 2 2022-2024/Online Survey/Spanish Translation of the Survey/SPANISH TRANSLATION_Nebraska Child Development Study- Phase 2 SURVEY-EN-ES-ES.xlsx"
                   ), 
  to = file.path(wds$root,"tmp")
)
file.copy(
  from = file.path(wds$onedrive,
                   "Phase 3/Survey Platform/Kidsights-RedCap/kidsights-redcap-v0_00/KidsightsData_RedCap_Modules_v0_00.xlsx"
  ), 
  to = file.path(wds$root,"tmp")
)
file.copy(
  from = file.path(wds$onedrive,
                   "Phase 3/Survey Platform/Kidsights-RedCap/kidsights-redcap-v0_00/translations/spanish/REDCapTranslation_es_pid7679_20250306-132047.json"
                  ),
  to = file.path(wds$root,"tmp")
)
