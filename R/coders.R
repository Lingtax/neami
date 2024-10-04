#' Codes Persons into CALD status
#' 
#' Defininc CALD communities is difficult. Per Pham et al. (2021), 
#' for routine data projects, Neami presumes consumers come from a 
#' Culturally or Linguistically Diverse Community if their Main 
#' language is not English and their country of Birth is not an 
#' English majority-speaking country. This will underestimate 
#' cultural diversity. 
#'
#' @param country_of_birth a vector of countries of birth
#' @param main_language a vector of main language spoken at home
#'
#' @return a vector of cald statuses
#' @export
#'
#' @examples
#'  code_cald(country_of_birth =c("England", "England", "Narnia", "Spain", NA_character_), 
#'  main_language = c("English", "Spanish", "French", "English", NA_character_))
code_cald <- function(country_of_birth, main_language) {
  if(length(country_of_birth) != length(main_language)) errorCondition("Length of the two vectors is not equal. It is recommended to use this function within a mutate.")
  
  country_of_birth <- tidyr::replace_na(country_of_birth, "Unknown")
  main_language <- tidyr::replace_na(main_language, "Unknown")
  
  dplyr::case_when(main_language != "English"  & !(main_language %in% c("Unknown",
                                                                        "Not stated/Inadequately described",
                                                                        "UNKNOWN",
                                                                        "[Not Stated]")) ~ "CALD community",
                   !country_of_birth %in% c("Australia",
                      "Australian External Territories, nec",
                      "England",
                      "New Zealand",
                      "Ireland",
                      "Canada",
                      "South Africa",
                      "Scotland",
                      "United States of America",
                      "Wales",
                      "British Antarctic Territory",
                      "Northern Ireland",
                      "Australian Antarctic Territory",
                      "Jersey",
                      "Pitcairn Islands",
                      "Norfolk Island",
                      "Northern Mariana Islands",
                      "Virgin Islands, British",
                      "United Kingdom",
                      "Bermuda",
                      "Falkland Islands",
                      "Cook Islands",
                      "Ross Dependency (New Zealand)",
                      "Guam",
                      "Isle of Man",
                      "Virgin Islands, United States", 
                      "Unknown",
                      "Unknown (for use in economic statistics)",
                      "[Not Stated]",
                      "NOT KNOWN",
                      "Unidentified (for use in economic statistics)",
                      "Not Stated, Inadequately Described",
                      "N/A",
                      "Other, Not Elsewhere Classified",
                      "Not Stated",
                      "Not Elsewhere Classified") ~  "CALD community",
                   main_language %in% c("Unknown",
                                        "Not stated/Inadequately described",
                                        "UNKNOWN",
                                        "[Not Stated]")  &
                     country_of_birth %in% c("Unknown",
                                             "Unknown (for use in economic statistics)",
                                             "[Not Stated]",
                                             "NOT KNOWN",
                                             "Unidentified (for use in economic statistics)",
                                             "Not Stated, Inadequately Described",
                                             "N/A",
                                             "Other, Not Elsewhere Classified",
                                             "Not Stated",
                                             "Not Elsewhere Classified") ~ "Unknown",
                   TRUE ~ "Non-CALD community")
}
