#' List date units between a start and end date 
#'
#' @param from the start date (will be floored to the `by` unit)
#' @param to the end date
#' @param by the units the span should be counted by (e.g. "month", "quarter")
#'
#' @return A vector of dates
#' @export
#'
#' @examples
#' list_periods(ymd("2024-06-01"), Sys.Date())
list_periods <-  function(from, to, by = "month") {
  if(from == to) { lubridate::floor_date(from, by)
    } else {
    seq(lubridate::floor_date(from, by), to, by = by)
  }
}

#' Extend a dataframe over arbitrary time units
#'
#' @param data a dataframe containing date time spans
#' @param from column containing the start date (will be floored to the `by` unit) 
#' @param to column containing the end date
#' @param by the units the span should be counted by (e.g. "month", "quarter")
#'
#' @return
#' @export
#'
#' @examples
#' test <-  tibble(x= ymd("2023-01-15"), y = ymd("2023-05-06"))
#' spread_episodes(test, x, y, by = "month")
#' spread_episodes(test, x, y, by = "quarter")
spread_episodes <-  function(data, from, to, by = "month") {
  
  data %>% 
    dplyr::mutate("{by}"  := purrr::pmap(list({{from}}, {{to}}, by), ~list_periods(..1, ..2, ..3))) %>% 
    tidyr::unnest({{ by }})
}

