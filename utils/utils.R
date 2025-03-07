translate_options<-function(df,translations, lang="ES"){
  
  df = df %>% dplyr::mutate(t_resp_opts = "", t_num_code = "")
  
  for(i in which(!is.na(df$resp_opts))){
    resp_opts_i = df$resp_opts[i] %>% stringr::str_split_1(pattern = "; ") %>% clean_resp_opts() 
      
    
    t_resp_opts_i = plyr::mapvalues(toupper(resp_opts_i), from = toupper(translations$resp_opts$EN) , to = translations$resp_opts$ES, warn_missing = F)
    
    t_resp_opts_i[is.na(t_resp_opts_i)] = toupper(resp_opts_i)[is.na(t_resp_opts_i)]
        
    num_code_i = df$num_code[i] %>% 
      stringr::str_replace_all("’", "'")
    
    t_num_code_i = num_code_i
    for(j in 1:length(resp_opts_i)){
      if( !is.na(t_resp_opts_i[j]) ){
        t_num_code_i = stringr::str_replace_all(t_num_code_i, pattern = resp_opts_i[j], replacement = t_resp_opts_i[j])
      }
    }
    
    df$t_resp_opts[i] = paste(t_resp_opts_i, collapse =  "; ")
    df$t_num_code[i] = t_num_code_i
  }
  
  names(df)[names(df)=="t_resp_opts"] = paste0("ES_resp_opts")
  names(df)[names(df)=="t_num_code"] = paste0("ES_num_code")
  
  return(df)
  
}

clean_resp_opts<-function(txt){
  
  txt = txt %>% stringr::str_replace_all("’", "'")
  
  return(txt)
  
}
