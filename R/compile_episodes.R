#' Compile grouped fundings into one episode of care
#' 
#' Some services' data is represented in multiple overlapping funding streams.
#' This function collapses these into continuous periods of service or episodes of care.
#'
#' @param data A dataframe of fundings within one service
#' @param start_date The name of the column containing the start dates of the fundings
#' @param end_date The name of the column containing the end dates of the fundings
#' @param personid The name of the column containing the person identifier
#' @param time_gap_threshold The tolerance threshold for gaps between fundings in days (default 1)
#'
#' @return an augmented dataframe containing episodes and a global date range
#' @export
#'
#' @examples
#' type_convert(tribble(
#'~personid, ~streamid, ~datetimestart, ~datetimeend, ~targetgroup,
#'1,         1,   "2023-01-01", "2023-01-05",            1,
#'1,         2,   "2023-01-07", "2023-01-30",            2,
#'
#'2,         2,   "2023-12-01", NA_character_,           1, 
#'2,         1,   "2024-01-12", "2024-01-30",            1,
#'2,         3,   "2024-02-10", "2024-02-28",            1,
#'2,         1,   "2024-02-25", NA_character_,           1,
#'
#'3,         3,   "2023-12-01", "2024-01-14",            1, 
#'3,         2,   "2024-01-12", "2024-01-30",            1,
#'3,         1,   "2024-01-10", "2024-02-01",            1,
#'
#'4,         3,   "2023-12-01", "2024-01-14",            1, 
#'4,         2,   "2024-01-12", "2024-01-20",            1,
#'4,         1,   "2024-01-21", NA_character_,           1
#'
#')) %>% 
#'  compile_episodes(datetimestart, datetimeend, personid, 1L) 
compile_episodes <- function(data, start_date, end_date, personid, time_gap_threshold = 1L) {
  
data %>% 
  mutate(temp_end_date = if_else(is.na({{ end_date }}), as.Date("2099-12-31"), {{ end_date }})) %>% 
  # replace_na( as.list(tibble({{ end_date }} := as.Date("2099-12-31")))) %>%  # filling with large value
  mutate(across(where(is.POSIXct), as.Date), 
         datetimeend_num = as.numeric(temp_end_date)) %>%  #{{ end_date}})) # because cummax can't treat Date
  arrange({{ personid }}, {{ start_date }}) %>%
  group_by({{ personid }}) %>%
  mutate(cummax_end = as.Date(cummax(datetimeend_num), origin = "1970-01-01"),
         new_episode_group = {{ start_date }} - lag(cummax_end, default = cummax_end[1]) > days(time_gap_threshold),
         episode = paste0({{ personid }}, "_", cumsum(new_episode_group) + 1),  
         temp_end_date = if_else({{ end_date }} == "2099-12-31", NA_Date_, {{ end_date }})) %>%
  group_by(episode) %>%
  mutate(episode_start = min({{ start_date }}),
         episode_end   = max(temp_end_date)) %>%
  ungroup() %>%
  select(-c(datetimeend_num, cummax_end, new_episode_group, temp_end_date))

  }

