translate_stems<-function(df,translations,lang="ES"){
  df = df %>% dplyr::mutate(t_stem = NA, t_similarity_stem = NA)
  n_j = nrow(translations$stems)
  
  out_list<-pbapply::pblapply(1:nrow(df), function(i){
    cstem_i = df$stem[i] %>% clean_stems() %>% toupper()
    idx = which(toupper(translations$stems$EN) == cstem_i)
    if(length(idx)==1){
      df$t_stem[i] = translations$stems$ES[idx]
      df$t_similarity_stem[i] = 1
      return(df[i,])
    } 
    jaccards = rep(NA,n_j)
    token_i = unique(unlist(strsplit(cstem_i, " ")))
    for(j in 1:n_j){
      EN_j = toupper(translations$stem$EN)[j]
      token_j = unique(unlist(strsplit(EN_j, " ")))
      jaccards[j] = length(intersect(token_i, token_j)) / length(union(token_i, token_j))
    }
    idx = which.max(jaccards)
    df$t_stem[i] = translations$stems$ES[idx]
    df$t_similarity_stem[i] = jaccards[idx]
    return(df[i,])
  })  
  
  out_df =  out_list %>% dplyr::bind_rows()
  names(out_df)[names(out_df)=="t_stem"] = paste(lang,"stem",sep="_")
  names(out_df)[names(out_df)=="t_similarity_stem"] = paste(lang,"similarity_stem",sep="_")
  
  return(out_df)
}

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
  
  for(i in 1:nrow(replacement_chars())){
    txt = txt %>% stringr::str_replace_all(replacement_chars()$from[i], replacement_chars()$to[i])
  }
  return(txt)
  
}

html_remove<-function(txt){
  
  located = stringr::str_locate_all(txt,c("<",">"))
  located = data.frame(start = located[[1]][,1], end = located[[2]][,1])
  
  # Return if no HTML text detected
  if(nrow(located)==0){return(txt)} 
  
  ids_discard = c()
  for(i in 1:nrow(located)){
    ids_discard = c(ids_discard, seq(located$start[i],located$end[i]))
  }
  ids_keep = setdiff(seq(1,stringr::str_length(txt)), ids_discard)
  
  strvec = c()
  for(i in 1:length(ids_keep)){
    strvec = c(strvec,stringr::str_sub(txt, start = ids_keep[i], end = ids_keep[i]))
  }
  
 return(paste0(strvec, collapse=""))
  
  
  
}

clean_stems<-function(txt){
  
  # Remove html
  txt = txt %>% html_remove()
  
  # Remove all nuisance chars
  for(i in 1:length(nuisance_chars())){
    txt = txt %>% stringr::str_remove_all(nuisance_chars()[i])
  }
  
  # Trim white space on both left and right side
  txt = txt %>% stringr::str_trim(side = "both")
  
  # Replace special characters
  for(i in 1:nrow(replacement_chars())){
    txt = txt %>% stringr::str_replace_all(replacement_chars()$from[i], replacement_chars()$to[i])
  }
  
  return(txt)
  
}

nuisance_chars<-function(){
  
  nuisances = 
  c(
    "&nbsp;", 
    "➘"
  )
  
  return(nuisances)
  
}

replacement_chars<-function(){
  
  from_vec = c("’")
  
  to_vec = c("'")
  
  return(data.frame(from = from_vec, to = to_vec))
  
}