#' Calculates reliable change for k10 scores
#'
#' Simple differences in pre- and post- scores risks overinterpreting variation.
#' Variation in outcome measures are to be expected due to uncontrolled factors
#' unrelated to a consumers' actual outcomes. Reliable change indices
#' /url(https://ebmh.bmj.com/content/1/3/70) seek to place benchmarks on changes
#' that are attributable to random noise and those that are more systematic.
#' This calculation relies on an estimate of the standard deviation (AMHOCN
#' estimate 9.2, n =281; Neami intake estimate 8.84, n = 2215) and of scale
#' reliability (alpha = .91: https://rdcu.be/cW31L, neami estimate alpha = .92)
#'
#' @param data a dataframe in wide format
#' @param pre the column containing the pre-measure
#' @param post the column containing a post measure
#' @param pop_sd a revised estimate for the population standard deviation
#' @param rel a revised estimate from the measure reliability
#'
#' @return the input dataframe with a raw change and a reliable change indicator appended
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 30, 9.2), post = rnorm(30, 22, 9.2))
#' add_k10_change(data, pre, post)
add_k10_change <- function(data, pre, post, pop_sd = 9.2, rel = .92) {


  crit = 1.96*pop_sd*sqrt(2)*sqrt(1-rel)

  dplyr::mutate(data, k10_change = {{post}} - {{pre}},
           rel_k10_change = dplyr::case_when(k10_change < -crit ~ "Decreased",
                                      k10_change > crit ~ "Increased",
                                      TRUE ~ "Unchanged"


           ))

}

#' Calculates reliable change for k5 scores
#'
#' Simple differences in pre- and post- scores risks overinterpreting variation.
#' Variation in outcome measures are to be expected due to uncontrolled factors
#' unrelated to a consumers' actual outcomes. Reliable change indices
#' /url(https://ebmh.bmj.com/content/1/3/70) seek to place benchmarks on changes
#' that are attributable to random noise and those that are more systematic.
#' The critical value in this function was provided by the department of health 2024-25
#'
#' @param data a dataframe in wide format
#' @param pre the column containing the pre-measure
#' @param post the column containing a post measure
#' 
#' @return the input dataframe with a raw change and a reliable change indicator appended
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 15, 7), post = rnorm(30, 11, 7))
#' add_k5_change(data, pre, post)
add_k5_change <- function(data, pre, post) {
  
  
  crit = 4.960261645 # From DoH PMHC - 2025
  
  dplyr::mutate(data, k5_change = {{post}} - {{pre}},
                rel_k5_change = dplyr::case_when(k5_change < -crit ~ "Decreased",
                                                  k5_change > crit ~ "Increased",
                                                  TRUE ~ "Unchanged"
                                                  
                                                  
                ))
  
}

#' Calculates reliable change for ras-ds scores
#'
#' Simple differences in pre- and post- scores risks overinterpreting variation.
#' Variation in outcome measures are to be expected due to uncontrolled factors
#' unrelated to a consumers' actual outcomes. Reliable change indices
#' /url(https://ebmh.bmj.com/content/1/3/70) seek to place benchmarks on changes
#' that are attributable to random noise and those that are more systematic.
#' This calculation relies on an estimate of the standard deviation (Neami 
#' intake estimate 20, n ~= 900) and of scale  reliability (neami estimate alpha = .94)
#'
#' @param data a dataframe in wide format
#' @param pre the column containing the pre-measure
#' @param post the column containing a post measure
#' @param pop_sd a revised estimate for the population standard deviation
#' @param rel a revised estimate from the measure reliability
#'
#' @return the input dataframe with a raw change and a reliable change indicator appended
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 85, 20), post = rnorm(30, 115, 20))
#' add_rasds_change(data, pre, post)
add_rasds_change <- function(data, pre, post, pop_sd = 20, rel = .94) {
  
  
  crit = 1.96*pop_sd*sqrt(2)*sqrt(1-rel)
  
  dplyr::mutate(data, rasds_change = {{post}} - {{pre}},
                rel_rasds_change = dplyr::case_when(rasds_change < -crit ~ "Decreased",
                                                  rasds_change > crit ~ "Increased",
                                                  TRUE ~ "Unchanged"
                                                  
                                                  
                ))
  
}

#' Flags reliable changes to k10s in a long data frame
#'
#' @param data a long data frame containing 2 observations per consumer
#' @param name the unquoted column name containing timepoint names
#' @param value the unquoted column name containing timepoint values
#' @param levels a vector of timepoint labels ordered ascending
#'
#' @return the input dataframe with a raw change and a reliable change indicator appended
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 30, 9.2), post = rnorm(30, 22, 9.2))
#' dl <- tidyr::pivot_longer(data,  pre:post)
#' flag_k10_changes(dl, name, value, levels = c("pre", "post"))
flag_k10_changes <-  function(data, name = name, value = value, levels = c("pre", "post")) {


  data %>%
    tidyr::pivot_wider(names_from = {{name}}, values_from = {{value}}) %>%
    add_k10_change(pre = .[[levels[[1]]]], post = .[[levels[[2]]]]) %>%
    tidyr::pivot_longer(cols = levels)
}
