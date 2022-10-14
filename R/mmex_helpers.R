#' Recodes country of birth from PMHC codes to names
#'
#' @param x a vector of integers indexing countries of birth
#'
#' @return a vector of names
#' @export
#'
mmex_birth_country_code <- function(x) {
  neami::cob[as.character(x)]
}


#' Recodes main language at home from PMHC codes to names
#'
#' @param x a vector of integers indexing main language spoken at home
#'
#' @return a vector of names
#' @export
#'
mmex_home_lang <- function(x) {
  neami::home_lang[as.character(x)]
}

#' Recodes English proficiency
#'
#' @param x a vector of integers indexing proficiency in spoken english
#'
#' @return a vector of values
#' @export
#'
mmex_eng_prof <- function(x) {
  neami::eng_prof[as.character(x)]
}

#' eng_prof
#'
#' A named list of English proficiency levels for MMEx
#'
#' @name eng_prof
#' @docType data
NULL

#' cob
#'
#' A named list of countries of birth for MMEx
#'
#' @name cob
#' @docType data
NULL

#' home_lang
#'
#' A named list of Languages spoken at home for MMEx
#'
#' @name home_lang
#' @docType data
NULL


