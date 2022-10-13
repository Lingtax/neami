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
#' @param measure the name of a supported measure ("ras", or "k10")
#'
#' @return
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 30, 9.2), post = rnorm(30, 22, 9.2))
#' add_rel_change(data, pre, post, measure = "k10")
add_rel_change <- function(data, pre, post, measure = NULL) {


  if (measure == "k10") {
    # From Neami stats on 2022/10/7 n > 700 n = 2215
    pop_sd <-  8.84
    rel <-  .92
  }

  if (measure == "ras") {
    # From Neami stats on 2022/10/13 n > 700
    pop_sd <-  17.95
    rel <-  .94

  }


  crit <-  1.96 * pop_sd * sqrt(2) * sqrt(1 - rel)

  data %>%
    mutate(change = {{post}} - {{pre}},
           rel_change = case_when(change < -crit ~ "Improved",
                                      change > crit ~ "Worsened",
                                      TRUE ~ "Unchanged"


           ))

}



#' Flags reliable changes to measures in a long data frame
#'
#' @param data a long data frame containing 2 observations per consumer
#' @param name the unquoted column name containing timepoint names
#' @param value the unquoted column name containing timepoint values
#' @param levels a vector of timepoint labels ordered ascending
#'
#' @return
#' @export
#'
#' @examples
#' data <-  data.frame(person = 1:30, pre = rnorm(30, 30, 9.2), post = rnorm(30, 22, 9.2))
#' dl <- tidyr::pivot_longer(data,  pre:post)
#' flag_rel_changes(dl, name, value, measure = "k10", levels = c("pre", "post"))
flag_rel_changes <-  function(data, name = name, value = value, measure = NULL,  levels = c("pre", "post")) {


  data %>%
    tidyr::pivot_wider(names_from = {{name}}, values_from = {{value}}) %>%
  add_rel_change(pre = .[levels[[1]]], post = .[levels[[2]]], measure = measure) %>%
    tidyr::pivot_longer(cols = all_of(levels)) %>%
    tidyr::unnest(all_of("change")) %>%
    dplyr::rename(change = matches(all_of(levels[[2]])))
}
