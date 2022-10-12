#' Implements a paired t-test from a long dataframe
#'
#' @param df a long dataframe or tibble
#' @param outcome the continuous numerical outcome variable for comparison
#' @param timeid the variable indexing the timepoint
#' @param id_cols the variable indexing the subject
#'
#' @return
#' @export
#'
#' @examples
#' test <- data.frame(id= sort(rep(1:10, 2)),obs = rep(1:2, 10), value = rnorm(20))
#' paired_t_test(test, value, obs, id_cols = id)
paired_t_test <-  function(df, outcome, timeid, id_cols = NULL) {
  df <- dplyr::ungroup(df)
  lvls <- dplyr::distinct(df, {{ timeid }}) %>% dplyr::pull(1)
  df <- tidyr::pivot_wider(df, names_from = {{ timeid }},
                           values_from = {{ outcome }},
                           id_cols = {{ id_cols }})

  x <- purrr::pluck(df, lvls[1])
  y <- purrr::pluck(df, lvls[2])
  t.test(x, y, paired = TRUE)
}



#' Wraps the output of a t-test into a pretty format
#'
#' @param ttest An S3 t-test model object
#'
#' @return A character string of the model summary
#' @export
#'
#' @examples
caption_ttest <- function(ttest) {

  paste0('The difference between groups was ',
         ifelse(ttest$p.value > .05, 'not ', ''),
         'statistically significant (',
         t_apa(ttest, print = FALSE),
         ')')

}
