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


#' Code PMHC service contact types against reference codes
#'
#' @param guid a vector of service contact contact IDs
#'
#' @return a vector of contact types
#' @export
#'
#' @examples
#' code_contacts(c("33A9B8BA-341C-4D25-B5C0-2A7DAE8C8AB6", "73F89E5F-AF10-47DD-B3DC-37018D1DA6A7"))
code_contacts <-  function(guid) {
  
  idx = c("33A9B8BA-341C-4D25-B5C0-2A7DAE8C8AB6" = "Assessment",
          "73F89E5F-AF10-47DD-B3DC-37018D1DA6A7" = "Suicide prevention specific assistance NEC",
          "60E0FF71-1E64-494D-8A84-654C7A9187B1" = "Clinical care coordination/liaison",
          "F674C5F7-CF22-47CF-BEE2-75E7118BB6F0" = "Other psychological intervention",
          "26EDB0F2-73B5-423B-A5B1-7E1DFBAFE783" = "Cultural specific assistance NEC",
          "62EB1066-0F3F-44BD-AFBF-B85D5BFA968C" = "Clinical nursing services",
          "F1229BD0-9D24-4A08-B242-CDC83D9ACDCC" = "Child or youth specific assistance NEC",
          "664A133A-C615-46BB-847C-D4B2A46FBE08" = "Psychosocial support",
          "052DA0EF-90F2-4FC7-85D3-D792B55A0C9E" = "No Contact",
          "85CB0A9C-1874-4F35-ADE1-E464A5E6183E" = "Administration",
          "0FFE4137-074C-4D81-A52F-E6A422F86F18" = "Structured psychological intervention")
  
  idx[guid]
  
}
