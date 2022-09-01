# required packages
#library(tidyverse)



#####################################
###                               ###
###  The Journal Join             ###
###    by jens.aasheim@sikt.no    ###
###                               ###
#####################################




#  Description:
#    Cross-join two journal lists by all combinations of print and electronic issn.
#    And de-duplicate the result.




j_join = function(df_a, col_a1, col_a2, df_b, col_b1, col_b2){
  
  #! Info:
  #! df_a       - Main dataframe
  #! col_a_id   - A unique id per row in df_a
  #! col_a1     - First column to join by from df_a
  #! col_a2     - Second column to join by from df_a
  #! df_b       - The other dataframe
  #! col_b1     - First column to join by from df_b
  #! col_b2     - Second column to join by from df_b
  
  
  # quotation
  col_a1_q = dplyr::enquo(col_a1)
  col_a2_q = dplyr::enquo(col_a2)
  col_b1_q = dplyr::enquo(col_b1)
  col_b2_q = dplyr::enquo(col_b2)
  
  
  
  # lowercase
  df_a = df_a %>% 
    dplyr::mutate(
      {{ col_a1 }} := toupper({{ col_a1 }}),
      {{ col_a2 }} := toupper({{ col_a2 }})
    )
  
  df_b = df_b %>% 
    dplyr::mutate(
      {{ col_b1 }} := toupper({{ col_b1 }}),
      {{ col_b2 }} := toupper({{ col_b2 }})
    )
  
  
  
  
  # internal row de-duplication id
  df_a = df_a %>% 
    dplyr::mutate(
      col_a_id = dplyr::row_number()
    )
  
  
  
  
  # nesting
  df_a_condensed = df_a %>% 
    dplyr::distinct(col_a_id, .keep_all = TRUE) %>% 
    dplyr::select(col_a_id, !!col_a1_q, !!col_a2_q)
  
  
  
  
  # join-by vectors
  a1b1 = rlang::set_names(quo_name(col_b1_q), quo_name(col_a1_q)) 
  a1b2 = rlang::set_names(quo_name(col_b2_q), quo_name(col_a1_q)) 
  a2b1 = rlang::set_names(quo_name(col_b1_q), quo_name(col_a2_q)) 
  a2b2 = rlang::set_names(quo_name(col_b2_q), quo_name(col_a2_q)) 
  
  
  
  
  # df_b id column
  df_b_int = df_b

  

    
  # a1/b1 
  df_res1 = df_a_condensed %>% 
    dplyr::inner_join(
      df_b_int, 
      by = a1b1,
      na_matches = "never"
    )
  
  df_tmp = df_a_condensed %>% 
    dplyr::anti_join(
      df_res1 %>% select(col_a_id),
      by = "col_a_id",
      na_matches = "never"
    )
  
  
  
  
  # a1/b2
  df_res2 = df_tmp %>% 
    dplyr::inner_join(
      df_b_int,
      by = a1b2, 
      na_matches = "never"
    )
  
  df_out = dplyr::bind_rows(
    df_res1,
    df_res2
  )
  
  df_tmp = df_a_condensed %>% 
    dplyr::anti_join(
      df_out %>% select(col_a_id),
      by = "col_a_id",
      na_matches = "never"
    )
  
  
  
  
  # a2/b1
  df_res3 = df_tmp %>% 
    dplyr::inner_join(
      df_b_int,
      by = a2b1, 
      na_matches = "never"
    )
  
  df_out = dplyr::bind_rows(
    df_out,
    df_res3
  )
  
  df_tmp = df_a_condensed %>% 
    dplyr::anti_join(
      df_out %>% select(col_a_id),
      by = "col_a_id",
      na_matches = "never"
    )
  
  
  
  
  # a2/b2
  df_res4 = df_tmp %>% 
    dplyr::inner_join(
      df_b_int,
      by = a2b2, 
      na_matches = "never"
    )
  
  
  
  
  # output
  df_out = dplyr::bind_rows(
    df_out,
    df_res4
  ) %>% 
    # remove duplicate issn columns
    dplyr::select(
      -!!col_a1_q,
      -!!col_a2_q,
      -dplyr::starts_with(paste0(rlang::as_label(col_a1_q), ".")),
      -dplyr::starts_with(paste0(rlang::as_label(col_a2_q), "."))
      ) 
  
  
  
  
  df_out = df_a %>% 
    dplyr::left_join(
      df_out,
      by = "col_a_id",
      na_matches = "never"
      ) %>% 
    dplyr::select(
      -col_a_id,
      )
  
  
  
  
  return(df_out)
  
}

# end: j_join()







