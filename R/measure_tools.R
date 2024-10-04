#' Filters and pairs outcome measures
#'
#' @param df a dataframe
#' @param grouping_id a column name indicating the episode key / person id
#' @param measure_total a column name indicating the measure
#' @param collection_occasions a column name indicating the collection occasion 
#' @param collection_date a column name indicating the collation date
#' @param min_interval a integer of days between measures for valid pairing
#'
#' @return a data frame of paired measures more than an arbirtary number of days apart
#' @export
#'
#' @examples
pair_outcome_measures <-  function(df, grouping_id, measure_total, collection_occasions, collection_date, min_interval = 30) {
  
 df |> 
    dplyr::group_by({{ grouping_id }}) |>  
    dplyr::filter(!is.na({{ measure_total }}),
             ({{collection_occasions}} %in% c("Entry", "Episode start") &
                {{ collection_date }} == min({{ collection_date }})) |
             ({{collection_occasions}} %in% c("Review", "Ongoing", "Periodic", "Periodic/Ongoing", "Review (not at Entry or Exit)") &
                {{ collection_date }} == max({{ collection_date }})) |
             ({{collection_occasions}} %in% c("Exit", "Episode end") &
                {{ collection_date }} == max({{ collection_date }})),
           lubridate::time_length(lubridate::interval(min({{ collection_date }}), max({{ collection_date }})), "days") > {{ min_interval }},
           ) |>
    dplyr::filter(dplyr::n() > 1) |> 
    dplyr::arrange({{ collection_date }}) |>
    dplyr::slice(1, dplyr::n()) |>
    dplyr::mutate(timepoint = factor(c("Earliest Measure", "Latest Measure"))) |>
               dplyr::ungroup()
}


  