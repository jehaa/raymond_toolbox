#library("tidyverse")



#######################################
#                                     #
#  /raymond                           #
#                                     #
#  ISSN - Cleand, fix and standardize #
#                                     #
#                                     #
#######################################




# journal df function
issn_fix = function(df) {

  # Description
  # Cleans, wrangles and standardizes issn data

  # Example01
  # issn_fix(df)

  # Example02
  # df %>%
  #   issn_fix()




  # the magic starts here
  df %>%

    # set column names to lower case (as they should be)
    rename_with(tolower) %>%

    mutate(
      across(contains("issn"), toupper),

      # remove whitespaces
      across(contains("issn"), ~ str_trim(.x)),

      # standardize string length and hyphen character (replace EN_DASH (char 2013) or other)
      # for when leading 0's are missing (e.g. removed by excel)
      across(contains("issn"), ~ str_replace_all(.x, "\\p{Pd}", "")),
      across(contains("issn"), ~ case_when(
        str_length(.x) == 7 ~ paste0("0", .x),
        str_length(.x) == 6 ~ paste0("00", .x),
        str_length(.x) == 5 ~ paste0("000", .x),
        TRUE ~ .x
        )
      ),

      # extract a string of 8 characters, where the first 7 must be numerical and the last can be either numerical or "X" (anything else becomes NA)
      across(contains("issn"), ~ str_extract(.x, "(\\d{7})([X0-9])")),

      # insert correct hyphen
      across(contains("issn"), ~ str_replace(.x, "^(\\d{4})", "\\1-")),

      # set to NA if blank or 0
      across(contains("issn"), ~ na_if(.x, "0")),
      across(contains("issn"), ~ na_if(.x, "")),
    )


}
