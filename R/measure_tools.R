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




#' Codes K10/k5 items
#'
#' @param item a vector of k10/k5 response character strings in Sentence case   
#'
#' @return a vector of integer values
#' @export
k10_coder <- function(item) {
  case_when(
    item == "None of the time" ~ 1L, 
    item == "A little of the time" ~ 2L, 
    item == "Some of the time" ~ 3L, 
    item == "Most of the time" ~ 4L, 
    item == "All of the time"  ~ 5L, 
    item == "" ~ NA_integer_, 
    
  )
}

#' Codes sdq items
#'
#' @param question a vector of SDQ response character strings 
#' @param type a scalar indicating the type of question "tf" for true/false, "impact" for impact
#' @param reverse a Logical indicates the item should be reverse coded
#'
#' @return a numeric vector of coded responses 
#' @export
sdq_coder <- function(question, type, reverse = FALSE) {
  
  if (type == "tf") {
    idx = c("Not True" = 1, 
            "Somewhat True" = 2, 
            "Certainly True" = 3)
    
    if(reverse == TRUE) {
      return(4 - idx[question]  - 1)
    } else {
      return(idx[question] - 1)
    }
  }
  
  if (type == "impact") {
    
    idx <- c("Not at all"  = 0,
             "A little"  = 1,
             "A medium amount" = 2,
             "A great deal" = 3)
    
    return(idx[question])
    
  }
  
}

#' Standardises measure occasion representations
#'
#' @param item a character vector indicating the parameter for standardisation
#' @param type a string indicating the type of variable for coding
#'
#' @return a standardised vector of strings.
#' @export
#'
#' @examples
standardise_measures <- function(item, type = "occasion") {
  
  if(type == "occasion") {
    idx <- c("Episode start" = "Episode start",
             "Episode end" = "Episode end",
             "Review" = "Review",
             "Entry" = "Episode start",
             "Exit" = "Episode end")
    
    
    return(idx[item])
  }
  
  
}

#' Prepares raw long measures from query into usable wide data frames
#'
#' @param measures a long dataframe containing one measure
#' @param fundings a data frame containing fundings
#' @param type a character indicating the measure type
#'
#' @return a wide data frame of forms linked to fundings 
#' @export
#'
#' @examples
prep_measures <-  function(measures, fundings, type){

  if(!(type %in% c("k10", "k5", "sdq", "pmhc"))) warningCondition("Type is not one of 'k10', 'k5', 'sdq', or 'pmhc'. Minimal prep applied.")
     
  k10_prep <- function(k10_data) {
  k10_data |>
  dplyr::filter(!(questiontext %in% c("Episode number", "Over the past...", "(k10) Reference Number", "(k10) Statewide UR number")),
                stringr::str_detect(questiontext, "The score displayed will not be accurate if any questions are not completed.", negate = TRUE)) |>
  dplyr::mutate(questiontext = case_when(
    questiontext %in%  c("Overall Score", "Total Score", "(k10)Total K10 score") ~ 'k10_total',
    questiontext %in%  c('About how often did you feel tired out for no good reason?', '1. About how often did you feel tired out for no good reason?') ~ 'k10_q1',
questiontext %in%  c('About how often did you feel tired out for no good reason?', '1. About how often did you feel tired out for no good reason?') ~ 'k10_q1',
questiontext %in%  c('About how often did you feel nervous?', '2. About how often did you feel nervous?') ~ 'k10_q2',
questiontext %in%  c('About how often did you feel so nervous that nothing could calm you down?', '3. About how often did you feel so nervous that nothing could calm you down?') ~ 'k10_q3',
questiontext %in%  c('How often did you feel hopeless?', '4. About how often did you feel hopeless?') ~ 'k10_q4',
questiontext %in%  c('How often did you feel restless or fidgety?', '5. About how often did you feel restless or fidgety?') ~ 'k10_q5',
questiontext %in%  c('How often did you feel so restless you could not sit still?', '6. About how often did you feel so restless you could not sit still?') ~ 'k10_q6',
questiontext %in%  c('How often did you feel depressed?', '7. About how often did you feel depressed?') ~ 'k10_q7',
questiontext %in%  c('How often did you feel that everything was an effort?', '8. About how often did you feel that everything was an effort?') ~ 'k10_q8',
questiontext %in%  c('How often did you feel so sad that nothing could cheer you up?', '9. About how often did you feel so sad that nothing could cheer you up?') ~ 'k10_q9',
questiontext %in%  c('How often did you feel worthless?', '10. About how often did you feel worthless?') ~ 'k10_q10',
questiontext %in%  c('How many days were you totally unable to work, study or manage your day to day activities because of these feelings?', '11. How many days were you totally unable to work, study or manage your day to day activities because of these feelings?') ~ 'days_unable_to_work',
questiontext %in%  c('Aside from those days, how many days were you able to work or study or manage your day to day activities, but had to cut down ', '12. Aside from those days, how many days were you able to work or study or manage your day to day activities, but had to cut down ') ~ 'days_work_reduced_cap',
questiontext %in%  c('How many times have you seen a doctor or any other health professional about these feelings?', '13. How many times have you seen a doctor or any other health professional about these feelings?') ~ 'times_seen_doctor',
questiontext %in%  c('How often have physical health problems been the main cause of these feelings?', '14. How often have physical health problems been the main cause of these feelings?') ~ 'freq_phys_health_cause',
questiontext %in%  c('(K10) If the form was not completed please select the reason from the list:', '(k10) If the form was not completed please select the reason from the list:') ~ 'decline_reason',
questiontext %in%  c('Collection Occasion - Reason', '(k10) Are the following answers taken at Entry or Exit?') ~ 'collection_reason',
questiontext %in%  c('Date Completed', '(k10) Date of completion') ~ 'date_complete',
TRUE ~ questiontext))
}

k5_prep <- function(k5_data) {
  k5_data |>
    dplyr::filter(stringr::str_detect(questiontext, "Episode number|If the respondent does not answer any one question, the entire score might be invalidated|score displayed will not be accurate if any questions are not completed", negate = TRUE)) |>
    dplyr::mutate(questiontext = dplyr::case_when(
    questiontext == 'Date Completed' ~ "date_complete",
    questiontext == 'Collection Occasion - Reason' ~ "collection_reason",
    questiontext == 'Overall Score' ~ "k5_total",
    str_detect(questiontext, "If the form was not completed") ~ "decline_reason",
    questiontext %in% c('1. In the last 4 weeks, about how often did you feel nervous?', 'In the last 4 weeks, about how often did you feel nervous?') ~ "k5_q1",
    questiontext %in% c('2. In the last 4 weeks, about how often did you feel without hope?', 'In the last 4 weeks, about how often did you feel without hope?') ~ "k5_q2",
    questiontext %in% c('3. In the last 4 weeks, about how often did you feel restless or jumpy?', 'In the last 4 weeks, about how often did you feel restless or jumpy?') ~ "k5_q3",
    questiontext %in% c('4. In the last 4 weeks, about how often did you feel everything was an effort?', 'In the last 4 weeks, about how often did you feel everything was an effort?') ~ "k5_q4",
    questiontext %in% c('5. In the last 4 weeks, about how often did you feel so sad that nothing could cheer you up?', 'In the last 4 weeks, about how often did you feel so sad that nothing could cheer you up?') ~ "k5_q5",
    TRUE ~ questiontext) )
}

sdq_prep <- function(sdq_data) {
  sdq_data |>
    dplyr::filter(stringr::str_detect(questiontext, "Thank you very much for your help.", negate = TRUE)) |>
    dplyr::mutate(questiontext = stringr::str_replace(questiontext, "^(?=\\d)", "sdq_q"),
           questiontext = dplyr::case_when(stringr::str_detect(questiontext, "^sdq_q\\d+") ~ stringr::str_extract(questiontext, "^sdq_q\\d+"),
                                     questiontext == 'Date Completed' ~ "date_complete",
                                     questiontext == 'Measure Completion' ~ "decline_reason",
                                     questiontext == 'Collection Occasion' ~ "collection_reason",
                                     questiontext == 'Total Score - ' ~ 'sdq_total',
                                     TRUE ~ questiontext),
           answer = dplyr::case_when(questiontext %in%
                              paste0("sdq_q", c(1:6, 8:10, 12:13, 15:20, 22:24)) ~
                              as.character(sdq_coder(answer, type = "tf", reverse = FALSE)),
                            questiontext %in%
                              paste0("sdq_q", c(7, 11, 14, 21, 25)) ~
                              as.character(sdq_coder(answer, type = "tf",  reverse = TRUE)),
                            answer == "Measure Completed" ~ "Measure Complete",
                            TRUE ~ answer)
         )
}

pmhc_prep <- function(pmhc_form) {
  pmhc_form |>
  # filter() |>
    dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date completed' ~ "date_complete",
                                    TRUE ~ questiontext)
         )
}





  step_a <-  function(measures, fundings) {
    measures |>
    dplyr::inner_join(fundings, by = c("PersonId" = "fldpersonid")) |>
    dplyr::group_by(AcpFilledFormId, questiontext) |>
    dplyr::filter(answer_modified_date == max(answer_modified_date) | is.na(answer_modified_date)) |>
    dplyr::ungroup()
  }

  step_c <- function(measures) {
    measures |>
      dplyr::mutate(answer = case_when(answer == "" ~ NA_character_,
                              TRUE ~ answer)) |>
      dplyr::group_by(AcpFilledFormId, questiontext) |> 
      dplyr::slice(1) |> 
      dplyr::ungroup() |> 
      tidyr::pivot_wider(id_cols = c(AcpFilledFormId, PersonId, fldservicesrequiredid,
                                     funding_start, funding_end, fldservicename, version_name, DateCreated),
                         names_from = questiontext,
                         values_from = answer,
                         values_fill = NA) |>
      readr::type_convert() |>
      janitor::clean_names()  |>
      bind_rows(tibble(date_complete = character())) |> 
      dplyr::mutate(date_complete = lubridate::dmy(date_complete),
                    across(where(is.character), ~ na_if(.,""))) |>
      dplyr::filter(date_complete >= funding_start,
                    date_complete <= funding_end | is.na(funding_end))  

  }

  if (type == "k10") {
    k10_items <-  paste0("k10_q", 1:10)
    
  out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      k10_prep() |>
      step_c() |>
    dplyr::mutate(#custom scoring
                  across(starts_with("k10_q"), k10_coder),
                  completion_status = case_when(!if_any(starts_with("k10_q"), is.na) ~ "Measure Complete",
                                       !is.na(decline_reason) ~ as.character(decline_reason))) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(missing_items = sum(is.na(c(k10_q1, k10_q2, k10_q3, k10_q4, k10_q5, 
                                       k10_q6, k10_q7, k10_q8, k10_q9, k10_q10))),
           k10_total = case_when(version_name == "K10" | version_name == "K10 - WA SUSD Only" ~ k10_total, 
                                 !(version_name == "K10" | version_name == "K10 - WA SUSD Only") &
                                   missing_items > 1 ~ NA_real_, 
                                 !(version_name == "K10" | version_name == "K10 - WA SUSD Only") &
                                   missing_items == 1 ~ 
                                   round(sum(c(k10_q1, k10_q2, k10_q3, k10_q4, k10_q5, 
                                               k10_q6, k10_q7, k10_q8, k10_q9, k10_q10), na.rm = TRUE) / 9 * 10, 0),
                                 TRUE ~ k10_q1 + k10_q2 + k10_q3 + k10_q4 + k10_q5 + 
                                   k10_q6 + k10_q7 + k10_q8 + k10_q9 + k10_q10)
    ) |>
  dplyr::ungroup() |> 
  dplyr::filter(!if_all(c(completion_status, starts_with("k10_q")), is.na)) |> 
  dplyr::select(-decline_reason, -missing_items)

  return(out)

  }

  if (type == "k5") {
  out <-  measures |>
    step_a(fundings = fundings) |>
    # Custom filters and recodes
      k5_prep() |>
    step_c() |>
    dplyr::mutate(#custom scoring
                  across(starts_with("k5_q"), k10_coder),
                  completion_status = case_when(!if_any(starts_with("k5_q"), is.na) ~ "Measure Complete",
                                                !is.na(decline_reason) ~ as.character(decline_reason)),
                  k5_total = k5_q1 + k5_q2 + k5_q3 + k5_q4 + k5_q5
                  ) |>
                  filter(!if_all(c(completion_status, starts_with("k5_q")), is.na))|> 
    select(-decline_reason)


   return(out)
  }

  if (type == "sdq") {

  out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      sdq_prep() |>
      step_c() |>
    dplyr::mutate(collection_reason = standardise_measures(collection_reason, "occasion"),
                  completion_status = case_when(!if_any(starts_with("sdq_q"), is.na) ~ "Measure Complete",
                                       !is.na(decline_reason) ~ decline_reason)) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(emotional_symptoms_summary_score = case_when(sum((c(sdq_q3, sdq_q8,  sdq_q13,  sdq_q16, sdq_q24) %in% 0:2), na.rm = TRUE) < 3 ~ NA_real_,
                                                               sum((c(sdq_q3, sdq_q8,  sdq_q13,  sdq_q16, sdq_q24) %in% 0:2), na.rm = TRUE) == 3 ~ round(sum(c(sdq_q3, sdq_q8,  sdq_q13,  sdq_q16, sdq_q24)/3 *5), 0),
                                                               sum((c(sdq_q3, sdq_q8,  sdq_q13,  sdq_q16, sdq_q24) %in% 0:2), na.rm = TRUE) == 4 ~ round(sum(c(sdq_q3, sdq_q8,  sdq_q13,  sdq_q16, sdq_q24)/4 *5), 0),
                                                               TRUE ~ sdq_q3 + sdq_q8 + sdq_q13 + sdq_q16 + sdq_q24),

                  conduct_problem_summary_score =  case_when(sum((c(sdq_q5, sdq_q7, sdq_q12, sdq_q18, sdq_q22) %in% 0:2), na.rm = TRUE) < 3 ~ NA_real_,
                                                             sum((c(sdq_q5, sdq_q7, sdq_q12, sdq_q18, sdq_q22) %in% 0:2), na.rm = TRUE) == 3 ~ round(sum(c(sdq_q5, sdq_q7, sdq_q12, sdq_q18, sdq_q22)/3 *5), 0),
                                                             sum((c(sdq_q5, sdq_q7, sdq_q12, sdq_q18, sdq_q22) %in% 0:2), na.rm = TRUE) == 4 ~ round(sum(c(sdq_q5, sdq_q7, sdq_q12, sdq_q18, sdq_q22)/4 *5), 0),
                                                             TRUE ~ sdq_q5 + sdq_q7 + sdq_q12 + sdq_q18 + sdq_q22),


                  hyperactivity_summary_score = case_when(sum((c(sdq_q2, sdq_q10, sdq_q15, sdq_q21, sdq_q25) %in% 0:2), na.rm = TRUE) < 3 ~ NA_real_,
                                                          sum((c(sdq_q2, sdq_q10, sdq_q15, sdq_q21, sdq_q25) %in% 0:2), na.rm = TRUE) == 3 ~ round(sum(c(sdq_q2, sdq_q10, sdq_q15, sdq_q21, sdq_q25)/3 *5), 0),
                                                          sum((c(sdq_q2, sdq_q10, sdq_q15, sdq_q21, sdq_q25) %in% 0:2), na.rm = TRUE) == 4 ~ round(sum(c(sdq_q2, sdq_q10, sdq_q15, sdq_q21, sdq_q25)/4 *5), 0),
                                                          TRUE ~ sdq_q2 + sdq_q10 + sdq_q15 + sdq_q21 + sdq_q25),

                  peer_problem_summary_score =  case_when(sum((c(sdq_q6, sdq_q11, sdq_q14, sdq_q19, sdq_q23) %in% 0:2), na.rm = TRUE) < 3 ~ NA_real_,
                                                          sum((c(sdq_q6, sdq_q11, sdq_q14, sdq_q19, sdq_q23) %in% 0:2), na.rm = TRUE) == 3 ~ round(sum(c(sdq_q6, sdq_q11, sdq_q14, sdq_q19, sdq_q23)/3 *5), 0),
                                                          sum((c(sdq_q6, sdq_q11, sdq_q14, sdq_q19, sdq_q23) %in% 0:2), na.rm = TRUE) == 4 ~ round(sum(c(sdq_q6, sdq_q11, sdq_q14, sdq_q19, sdq_q23)/4 *5), 0),
                                                          TRUE ~ sdq_q6 + sdq_q11 + sdq_q14 + sdq_q19 + sdq_q23),

                  prosocial_summary_score =  case_when(sum((c(sdq_q1, sdq_q4, sdq_q9, sdq_q17, sdq_q20) %in% 0:2), na.rm = TRUE) < 3 ~ NA_real_,
                                                       sum((c(sdq_q1, sdq_q4, sdq_q9, sdq_q17, sdq_q20) %in% 0:2), na.rm = TRUE) == 3 ~ round(sum(c(sdq_q1, sdq_q4, sdq_q9, sdq_q17, sdq_q20) / 3 *5), 0),
                                                       sum((c(sdq_q1, sdq_q4, sdq_q9, sdq_q17, sdq_q20) %in% 0:2), na.rm = TRUE) == 4 ~ round(sum(c(sdq_q1, sdq_q4, sdq_q9, sdq_q17, sdq_q20) / 4 *5), 0),
                                                       TRUE ~ sdq_q1 + sdq_q4 + sdq_q9 + sdq_q17 + sdq_q20),
                  sdq_total = round(case_when(sum(is.na(c(emotional_symptoms_summary_score, conduct_problem_summary_score,  peer_problem_summary_score, hyperactivity_summary_score))) == 1 ~ sum(c(emotional_symptoms_summary_score, conduct_problem_summary_score,  peer_problem_summary_score, hyperactivity_summary_score), na.rm = TRUE) / 3 * 4,
                                        TRUE ~ emotional_symptoms_summary_score + conduct_problem_summary_score +  peer_problem_summary_score + hyperactivity_summary_score), 0)
                  ) |>
    dplyr::ungroup() |> 
    dplyr::filter(!if_all(c(completion_status, starts_with("sdq_q")), is.na)) |> 
    dplyr::select(-decline_reason)

    return(out)

  }

  if (type == "pmhc") {
  out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      pmhc_prep() |>
      step_c()

  return(out)

  }
 
   else {
  out <-  measures |>
      step_a(fundings = fundings) |>
      step_c()

  return(out)

  }

  }

  