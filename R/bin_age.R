#' Bins ages against ABS age standard
#'
#' @param age a vector of integer ages
#'
#' @return a factor vector of 10 year age brackets
#' @export
#'
#' @examples
#' bin_age_10(sample(2:90, 20, replace = TRUE))
bin_age_10 <- function(age){

    age <-  as.character(
    cut(age, breaks = c(0, seq(4, 104, 10), Inf))
    )

    factor(dplyr::recode(age,
            `(0,4]` = "0-4",
            `(4,14]` =  "5-14",
            `(14,24]` =  "15-24",
            `(24,34]` =  "25-34",
            `(34,44]` =  "35-44",
            `(44,54]` =  "45-54",
            `(54,64]` =  "55-64",
            `(64,74]` =  "65-74",
            `(74,84]` =  "75-84",
            `(84,94]` =  "85-94",
            `(94,104]` = "95-104",
            `(104,Inf]` = "105 and over"
    ), levels = c("0-4",
         "5-14",
         "15-24",
         "25-34",
         "35-44",
         "45-54",
         "55-64",
         "65-74",
         "75-84",
         "85-94",
         "95-104",
         "105 and over"))
}
