# required packages
#library(tidyverse)
#library(jsonlite)








###########################################
###                                     ###
###  DOI Registration Agency Checker    ###
###    by jens.aasheim@sikt.no          ###
###                                     ###
###########################################




#  Description:
#    Function to check DOI registration agency (ra). 
#    Also solves the validation issue: DOI's that are not found in Crossref could belong to other ra's


#  Function call example:
# df %>% 
#   ra_doiCheck(doi_column_name)



### Function: doiRAcheck()

ra_doiCheck = function(df, doi, ...){
  
  
  #/// internal API query function
  getRA = function(doi_in){
    
    get_res = httr::RETRY("GET", paste0("https://doi.org/doiRA/", doi_in), times = 5)
    
    get_out = get_res$content %>% 
      rawToChar() %>% 
      jsonlite::fromJSON() %>% 
      dplyr::rename_with(tolower) %>% 
      dplyr::mutate(
        ra = if("ra" %in% colnames(.)) 
          ra 
        else NA
      ) %>% 
      select(ra, doi)
    
    return(get_out)
  }
  #/// end: internal function
  
  
  # safe internal function
  safe_getRA = purrr::safely(getRA)
  
  
  # quotation
  doi_q = rlang::enquo(doi)
  
  
  # doi.org API does not accept to many doi's at a time
  limit = 250
  crows = df %>% 
    dplyr::distinct({{ doi }}) %>% 
    nrow()
  nsplit = rep(1:ceiling(crows/limit), each = limit)[1:crows]
  
  
  # convert the doi column to character vector chunks
  dchar = df %>% 
    dplyr::distinct({{ doi }}) %>% 
    dplyr::pull({{ doi }}) %>% 
    split(nsplit) %>% 
    map_chr(~ str_c(.x, collapse = ", "))
  
  
  # API query
  ra_out = dchar %>% 
    map_df(~{
      safe_getRA(.) %>% 
        purrr::pluck("result") %>% 
        purrr::compact()
    }) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename({{ doi }} := "doi") %>% 
    dplyr::right_join(df, by = rlang::quo_name(doi_q))
  
  
  return(ra_out)
  
}

### End Function: doiRAcheck()

