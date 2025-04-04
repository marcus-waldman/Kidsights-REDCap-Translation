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

# Define working directories (NOTE THIS IS MACHINE SPECIFIC)
  #wds = list(
  #  root = "C:/Users/marcu/Dropbox/GitKraken/white-rhino/repositories/Kidsights-REDCap-Translation", 
  #  onedrive = "C:/Users/marcu/OneDrive - University of Nebraska Medical Center/Documents - Kidsights Data/General"
  #)
  wds = list(
    root = "C:/Users/waldmanm/git-repositories/Kidsights-REDCap-Translation", 
    onedrive = "C:/Users/waldmanm/University of Nebraska Medical Center/Kidsights Data - Documents/General"
  )



setwd(wds$root)

# Source utility functions
source("utils/utils.R")

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
                   "Phase 3/Survey Platform/Kidsights-RedCap/SPANISH KidsightsData_RedCap_Modules_v2.April 4.xlsx"
  ), 
  to = file.path(wds$root,"tmp")
)
file.copy(
  from = file.path(wds$onedrive,
                   "Phase 3/Survey Platform/Kidsights-RedCap/kidsights-redcap-v0_00/translations/spanish/REDCapTranslation_es_pid7679_20250306-132047.json"
                  ),
  to = file.path(wds$root,"tmp")
)

# # Read in the Modules.xlsx, construting a list of data frames (VERIFY EACH TIME)
# mfile = file.path("tmp","KidsightsData_RedCap_Modules_v0_00.xlsx")
# modules = list(
#   `Module 00` = readxl::read_xlsx(path = mfile, sheet = "Module 00", range = "B4:M9"),
#   `Module 0` = NULL,
#   `Module 1` = readxl::read_xlsx(path = mfile, sheet = "Module 1", range = "A3:L10"),
#   `Module 2` = readxl::read_xlsx(path = mfile, sheet = "Module 2", range = "A3:K58"), 
#   `Module 3` = readxl::read_xlsx(path = mfile, sheet = "Module 3", range = "A3:K46"),
#   `Module 4` = readxl::read_xlsx(path = mfile, sheet = "Module 4", range = "A3:K30"),
#   `Module 5` = readxl::read_xlsx(path = mfile, sheet = "Module 5", range = "A5:H6"),
#   `Module 6` = readxl::read_xlsx(path = mfile, sheet = "Module 6", range = "A7:K235"),
#   `Module 7` = readxl::read_xlsx(path = mfile, sheet = "Module 7", range = "A3:I49"), 
#   `Module 8` = readxl::read_xlsx(path = mfile, sheet = "Module 8", range = "A2:K5"), 
#   `Module 9` = readxl::read_xlsx(path = mfile, sheet = "Module 9", range = "A7:I13")
# )
# 
# 
# # Read in the Spanish version
# es_xlsx = readxl::read_xlsx(path = file.path("tmp","SPANISH TRANSLATION_Nebraska Child Development Study- Phase 2 SURVEY-EN-ES-ES.xlsx"))
# es_translations = list(
#   stems = es_xlsx %>% 
#     dplyr::filter(endsWith(PhraseID, "QuestionText")) %>% 
#     dplyr::group_by(PhraseID) %>% 
#     dplyr::reframe(EN = EN %>% clean_stems(), ES = `ES-ES`) %>% 
#     dplyr::ungroup() %>% 
#     na.omit() %>% 
#     dplyr::group_by(EN) %>% 
#     dplyr::summarise(ES = ES[1] %>% clean_stems()),
#   resp_opts = es_xlsx %>% dplyr::filter(stringr::str_detect(PhraseID,"_Choice")) %>%
#     na.omit() %>% 
#     dplyr::group_by(EN) %>% 
#     dplyr::summarise(ES = names(which.max(table(`ES-ES`))))
# )
# 
# 
# 
# ### Part 1: Let's subsitute response options
# for(m in 1:length(modules)){
#   if(!is.null(modules[[m]])){modules[[m]] = translate_options(df = modules[[m]], translations = es_translations, lang = "ES")}
# }
# 
# ## Part 2: Let's translate the stems
# for(m in 1:length(modules)){
#   if(!is.null(modules[[m]])){modules[[m]] = translate_stems(df = modules[[m]], translations = es_translations, lang = "ES")}
# }
# 
# ### Part 3; Create a translations tab in the existing spreadsheet
# ES_sheet = lapply(1:length(modules), function(m){
#   
#   if(is.null(modules[[m]])){return(NULL)}
#   
#   return(modules[[m]] %>% dplyr::mutate(module = names(modules)[m]) %>% dplyr::select(module, lex_ne25, dplyr::starts_with("ES_")) %>% dplyr::relocate(module, lex_ne25, dplyr::ends_with("stem")))
#   
# }) %>% dplyr::bind_rows()
# 
# ## let's write out
# xlsx::write.xlsx(ES_sheet, mfile, sheetName = "ES Translation", append = T)

### Part 4: Append and export the JSON-file ###

jsons = list.files(path = "tmp", pattern = ".json")


x = 1
# Read in the REDCap .json MLM file
es_json = jsonlite::read_json(file.path("tmp",jsons[x]))


ES_sheet = readxl::read_xlsx(path = "tmp/SPANISH KidsightsData_RedCap_Modules_v2.April 4.xlsx", sheet = "ES Translation") %>% 
  dplyr::filter(!is.na(lex_ne25)) %>% 
  dplyr::mutate(lex_ne25 = tolower(lex_ne25))

fieldT = es_json$fieldTranslations
translations = pbapply::pblapply(1:length(fieldT), function(i){
  
      enum_code = NULL
      enum_exists = "enum" %in% names(fieldT[[i]])
      if(enum_exists){
        for(j in 1:length(fieldT[[i]]$enum) ){
          enum_code = c(enum_code, paste0(fieldT[[i]]$enum[[j]]$id, " = ", fieldT[[i]]$enum[[j]]$default))
        }
        enum_code = paste(enum_code, collapse = "; ")
      } else {
        enum_code = ""
      }
      label_exists = ifelse("label" %in% names(fieldT[[i]]), ifelse("default" %in% names(fieldT[[i]]$label), T, F), F)
      if(label_exists){
        label = fieldT[[i]]$label$default
      } else {
        label = ""
      }
      return(data.frame(module = fieldT[[i]]$form, lex_ne25 = fieldT[[i]]$id, stem = label, enum_exists = enum_exists, enum_code = enum_code))
      
  }
) %>% dplyr::bind_rows()

translations = translations %>% 
  dplyr::left_join(ES_sheet %>% dplyr::select(lex_ne25,ES_stem,ES_num_code), by = "lex_ne25") %>% 
  dplyr::rename(ES_enum_code = ES_num_code)


# If there is not an enum_code in english, make blank in spanish
translations$ES_enum_code[!translations$enum_exists] = NA

# Clean up 9 = "Don't Know" 
for(i in 1:nrow(translations)){
  if(stringr::str_detect(translations$enum_code[i], "9") & stringr::str_detect(tolower(translations$enum_code[i]), "know")){
    print(i)
    translations$ES_enum_code[i] = stringr::str_replace_all(translations$ES_enum_code[i], "-9", "9")
  }
}
# Clean up 99 = 
for(i in 1:nrow(translations)){
  if(stringr::str_detect(translations$enum_code[i], "99 =")){
    print(i)
    translations$ES_enum_code[i] = stringr::str_replace_all(translations$ES_enum_code[i], "-99", "99")
  }
}

# Transport the html
i = 214
if(startsWith(translations$stem[i], "<div class=\"rich-text-field-label\"><p>")){
  begin_i = stringr::str_locate(translations$stem[i],"<div class=\"rich-text-field-label\"><p>")[2]+1
  end_i = stringr::str_locate(translations$stem[i],"</p>")[1]-1
  stem_i = substr(translations$stem[i], begin_i, end_i)
  ES_stem_i = paste0(
    substr(translations$stem[i], 1, begin_i-1), 
    translations$ES_stem[i], 
    substr(translations$stem[i], end_i+1, stringr::str_length(translations$stem[i]))
  )
  translations$ES_stem[i] = ES_stem_i
}



# Make all missing and text
translations[is.na(translations)] = ""


# Transfer all text with complete
translations$ES_enum_code[endsWith(translations$lex_ne25,"_complete")] = translations$enum_code[endsWith(translations$lex_ne25,"_complete")]

write.xlsx(translations %>% dplyr::select(module,lex_ne25, stem,ES_stem,enum_code,ES_enum_code), file = "tmp/porting-translation.xlsx")



# Let's add in the translated labels/stems
for(i in which(!is.na(translations$ES_stem))){
  fieldT[[i]]$label$translation = translations$ES_stem[i]
}

# Let's add in response options
for(i in which(!is.na(translations$ES_stem) & translations$enum_exists & !is.na(translations$ES_num_code))){
  resp_opts_i = translations$ES_num_code[i] %>%   stringr::str_split_1(pattern = ";") %>% stringr::str_squish()
  for(j in 1:length(fieldT[[i]]$enum)){
    u_ij = fieldT[[i]]$enum[[j]]$id
    jj = which(startsWith(resp_opts_i,as.character(u_ij)))
    if(length(jj)==1){
      fieldT[[i]]$enum[[jj]]$translation = stringr::str_remove_all(resp_opts_i[jj], paste0(u_ij," = "))
    }
  }
}

es_json$fieldTranslations = fieldT
jsonlite::write_json(es_json, path = "tmp/REDCapTranslation_es_pid7679_20250306-132047-ES.json", pretty = T)


# Note that "don't know" was set to 9 and not -9
# "id": ["c122"],
# "form": ["module_6_549_731"],
# "label": {
#   "hash": ["9ebe818dda"],
#   "translation": ["¿Su niño/niña puede nombrar al menos un color (p. ej., rojo, azul o amarillo)?"]
# },
# "note": {
#   "hash": ["92b794adef"],
#   "translation": [""]
# },
# "enum": [
#   {
#     "id": [1],
#     "hash": ["5397e0583f"],
#     "translation": ["Sí"]
#   },
#   {
#     "id": [0],
#     "hash": ["816c52fd2b"],
#     "translation": ["No"]
#   },
#   {
#     "id": [9],
#     "hash": ["6953dc95d5"],
#     "translation": [""]
#   }
# ]
