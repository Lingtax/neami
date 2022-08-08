#' Implements a paired t-test from a long dataframe
#'
#' @param df a long dataframe or tibble
#' @param outcome the continuous numerical outcome variable for comparison
#' @param timeid the variable indexing the timepoint
#'
#' @return
#' @export
#'
#' @examples
paired_t_test <-  function(df, outcome, timeid) {
  lvls <- dplyr::distinct(df, {{ timeid }}) %>% dplyr::pull(1)
  df <- tidyr::pivot_wider(df, names_from = {{ timeid }}, values_from = {{ outcome }})

  x <- purrr::pluck(df, lvls[1])
  y <- purrr::pluck(df, lvls[2])
  t.test(x, y, paired = TRUE)
  }
