rm(list = ls())

# Load require libraries
library(tidyverse)
library(jsonlite)
library(stringr)
library(readxl)
library(xlsx)
library(raster)
library(text2vec)
library(tm)
library(pbapply)

# Source utility functions
source("utils/utils.R")

# Define working directories (NOTE THIS IS MACHINE SPECIFIC)
wds = list(
  root = "C:/Users/marcu/Dropbox/GitKraken/white-rhino/repositories/Kidsights-REDCap-Translation", 
  onedrive = "C:/Users/marcu/OneDrive - University of Nebraska Medical Center/Documents - Kidsights Data/General"
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

# Read in the REDCap .json MLM file
es_json = jsonlite::read_json(file.path("tmp","REDCapTranslation_es_pid7679_20250306-132047.json"))


# Read in the Modules.xlsx, construting a list of data frames (VERIFY EACH TIME)
mfile = file.path("tmp","KidsightsData_RedCap_Modules_v0_00.xlsx")
modules = list(
  `Module 00` = readxl::read_xlsx(path = mfile, sheet = "Module 00", range = "B4:M9"),
  `Module 0` = NULL,
  `Module 1` = readxl::read_xlsx(path = mfile, sheet = "Module 1", range = "A3:L10"),
  `Module 2` = readxl::read_xlsx(path = mfile, sheet = "Module 2", range = "A3:K58"), 
  `Module 3` = readxl::read_xlsx(path = mfile, sheet = "Module 3", range = "A3:K46"),
  `Module 4` = readxl::read_xlsx(path = mfile, sheet = "Module 4", range = "A3:K30"),
  `Module 5` = readxl::read_xlsx(path = mfile, sheet = "Module 5", range = "A5:H6"),
  `Module 6` = readxl::read_xlsx(path = mfile, sheet = "Module 6", range = "A7:K235"),
  `Module 7` = readxl::read_xlsx(path = mfile, sheet = "Module 7", range = "A3:I49"), 
  `Module 8` = readxl::read_xlsx(path = mfile, sheet = "Module 8", range = "A2:K5"), 
  `Module 9` = readxl::read_xlsx(path = mfile, sheet = "Module 9", range = "A7:I13")
)


# Read in the Spanish version
es_xlsx = readxl::read_xlsx(path = file.path("tmp","SPANISH TRANSLATION_Nebraska Child Development Study- Phase 2 SURVEY-EN-ES-ES.xlsx"))
es_translations = list(
  stems = es_xlsx %>% 
    dplyr::filter(endsWith(PhraseID, "QuestionText")) %>% 
    dplyr::group_by(PhraseID) %>% 
    dplyr::reframe(EN = EN %>% clean_stems(), ES = `ES-ES`) %>% 
    dplyr::ungroup() %>% 
    na.omit() %>% 
    dplyr::group_by(EN) %>% 
    dplyr::summarise(ES = ES[1] %>% clean_stems()),
  resp_opts = es_xlsx %>% dplyr::filter(stringr::str_detect(PhraseID,"_Choice")) %>%
    na.omit() %>% 
    dplyr::group_by(EN) %>% 
    dplyr::summarise(ES = names(which.max(table(`ES-ES`))))
)



### Part 1: Let's subsitute response options
for(m in 1:length(modules)){
  if(!is.null(modules[[m]])){modules[[m]] = translate_options(df = modules[[m]], translations = es_translations, lang = "ES")}
}

## Part 2: Let's translate the stems
for(m in 1:length(modules)){
  if(!is.null(modules[[m]])){modules[[m]] = translate_stems(df = modules[[m]], translations = es_translations, lang = "ES")}
}

### Create a translations tab in the existing spreadsheet
ES_sheet = lapply(1:length(modules), function(m){
  
  if(is.null(modules[[m]])){return(NULL)}
  
  return(modules[[m]] %>% dplyr::mutate(module = names(modules)[m]) %>% dplyr::select(module, lex_ne25, dplyr::starts_with("ES_")) %>% dplyr::relocate(module, lex_ne25, dplyr::ends_with("stem")))
  
}) %>% dplyr::bind_rows()

## let's write out
xlsx::write.xlsx(ES_sheet, mfile, sheetName = "ES Translation", append = T)


