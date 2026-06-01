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
  # case_when(
  #   item == "None of the time" ~ 1L, 
  #   item == "A little of the time" ~ 2L, 
  #   item == "Some of the time" ~ 3L, 
  #   item == "Most of the time" ~ 4L, 
  #   item == "All of the time"  ~ 5L, 
  #   item == "" ~ NA_integer_, 
  #   
  # )
  idx <- c( "None of the time" = 1L, 
            "A little of the time" = 2L, 
            "Some of the time" = 3L, 
            "Most of the time" = 4L, 
            "All of the time"  = 5L)
  
  
  return(idx[item])
  
}
#' Codes RAS-DS items
#'
#' @param item a vector of k10/k5 response character strings in Sentence case   
#'
#' @return a vector of integer values
#' @export
ras_coder <- function(item) {
  # case_when(
  #   item == "Untrue" ~ 1L, 
  #   item == "A bit True" ~ 2L, 
  #   item == "Mostly True" ~ 3L, 
  #   item == "Completely True  " ~ 4L, 
  #   item == "" ~ 0L, 
  #   item == " " ~ 0L, 
  #   is.na(item) ~ 0L,
  #   )
  
  idx <- c("Untrue" = 1L, 
           "A bit True" = 2L, 
           "Mostly True" = 3L, 
           "Completely True" = 4L, 
           # "" = 0L, 
           " " = 0L)
  
  
  return(idx[item])
  
  
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
  
  if(!(type %in% c("k10", "k5", "sdq", "pmhc", "stsh", "iar", "amhc_gp", "ras", "lcq", "sn", "isp", "intreg"))) {
    warningCondition("Type is not one of 'k10', 'k5', 'sdq', 'pmhc', 'iar', 'amhc_gp', 'lcq', 'sn', 'isp', 'intreg', or 'stsh'. Minimal prep applied.")
    }
  
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
        TRUE ~ questiontext))
    
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
  
  ras_prep <- function(ras_data) {
    ras_data |>
      dplyr::filter(
        stringr::str_detect(questiontext, "DOING THINGS I VALUE", negate = TRUE),
        stringr::str_detect(questiontext, "MASTERING MY ILLNESS", negate = TRUE),
        stringr::str_detect(questiontext, "LOOKING FORWARD", negate = TRUE),
        stringr::str_detect(questiontext, "Finally, is there anything else that is important to you and your recovery that has not been covered?", negate = TRUE),
        stringr::str_detect(questiontext, "Comments", negate = TRUE),
      ) |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date completed' ~ "date_complete",
                                                    questiontext == 'If the form was not completed please select the reason from the list:' ~ "decline_reason",
                                                    questiontext == 'Collection Occasion' ~ "collection_reason",
                                                    TRUE ~ questiontext),
                    answer = dplyr::case_when(str_detect(questiontext, "\\d+\\.") ~ as.character(ras_coder(answer)),
                                              TRUE ~ as.character(answer))
      )
  }
  lcq_prep <- function(lcq_data) {
    lcq_data |>
      dplyr::filter(
        questiontext == "Completion Date" | 
          stringr::str_detect(questiontext, 
                     "^Amount of time|Living situation rating|Physical health_1|^Get support|^Hopefulness|^Part of group/community|^Wellbeing|^Achieve important things|^Happiness") 
        ) |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Completion Date' ~ "date_complete",
                                                    TRUE ~ questiontext),
                    answer = dplyr::case_when(stringr::str_detect(questiontext, 
                                                                  "^Amount of time|Living situation rating|Physical health_1|^Get support|^Hopefulness|^Part of group/community|^Wellbeing|^Achieve important things|^Happiness")  ~ 
                                                str_extract(answer, "\\d"),
                                              TRUE ~ as.character(answer))
      )
  }
  
  pmhc_prep <- function(pmhc_form) {
    pmhc_form |>
      # filter() |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date completed' ~ "date_complete",
                                                    TRUE ~ questiontext)
      )
  }
  stsh_prep <- function(stsh_form) {
    stsh_form |>
      # filter() |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date completed:' ~ "date_complete",
                                                    questiontext == 'Date this form was completed:' ~ "date_complete",
                                                    questiontext == 'Date this form was completed ' ~ "date_complete",
                                                    TRUE ~ questiontext)
      )
  }
  
  iar_prep <- function(iar_form) {
    iar_form |>
      # filter() |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date Completed' ~ "date_complete",
                                                    TRUE ~ questiontext)
      )
  }
  
  amhcgp_prep <- function(amhcgp_form) {
    amhcgp_form |>
      # filter() |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date Completed' ~ "date_complete",
                                                    TRUE ~ questiontext) 
      ) |> 
      dplyr::mutate(questiontext = case_when(row_number() == 1 ~ "dr_title", 
                                             row_number() == 2 ~ "dr_forename", 
                                             row_number() == 3 ~ "dr_surname", 
                                             TRUE ~ questiontext),
                    .by = c(AcpFilledFormId, fldservicesrequiredid))
  }
  
  isp_prep <- function(isp_form) {
    isp_form |>
      filter(SectionName == "Goal Setting" | questiontext == 'Date Completed', 
             !questiontext %in% c("Planned Supports (Will/Way Forward)", 
                                  "Barriers and Strengths (Reality)", 
                                  "Strategies (Options)", 
                                  "Goal")) |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date Completed' ~ "date_complete",
                                                    questiontext == 'Area' ~ "goal_area",
                                                    
                                                    TRUE ~ stringr::str_to_snake(stringr::str_remove(stringr::str_remove_all(questiontext, "Progress "), " \\(1-10\\)"))
      ), 
      questiontext = stringr::str_replace(questiontext,  "final", "end") |> 
        stringr::str_replace("initial", "start") |>  
        stringr::str_replace("exit", "end"))
  }
  
  sn_prep <- function(sn_form) {
    
    form_complete <- sn_form |>
      # filter() |>
      dplyr::filter(questiontext %in% c("What aspects of the consumer’s narrative did they express as (or did you perceive as) being important in understanding the past and/or present factors that may impact safety?",
                                        "Will outreach support be provided to the consumer?",
                                        "What can be done and who will do what to create, maintain, or strengthen opportunities for safety?",
                                        "What would the consumer expect staff to know if things were not going well for them, and what would they want staff to do?",
                                        "What are the strengths, priorities, and opportunities the consumer has identified?",
                                        "What are the safety considerations the consumer and worker identified as important to talk about and how does the consumer make sense of them?")) |> 
      dplyr::summarise(complete = sum(is.na(answer)) == 0, 
                       .by = c(AcpFilledFormId, PersonId, DateCreated, OriginalName, 
                               version_name, answer_modified_date, fldservicesrequiredid, 
                               funding_start, funding_end, fldservicename)) |> 
      tidyr::pivot_longer(complete, names_to = "questiontext", values_to = "answer") |> 
      dplyr::mutate(answer = as.character(answer))
    
    sn_form |> 
      dplyr::filter(questiontext  == 'Date Completed') |> 
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date Completed' ~ "date_complete",
                                                    TRUE ~ questiontext) 
      ) |> 
      dplyr::bind_rows(form_complete) 
    
  }
  
  intreg_prep <- function(intreg_form) {
    intreg_form |>
      dplyr::mutate(questiontext = dplyr::case_when(questiontext == 'Date Completed' ~ "date_complete",
                                                    TRUE ~ questiontext)
                    
      )
  }
  
  
  step_a <-  function(measures, fundings) {
    a <-  measures |>
      dplyr::inner_join(fundings, by = c("PersonId" = "fldpersonid")) 
    
    aos <-  a |>
      dplyr::filter(questiontext == "Area of Support Focus") |> 
      dplyr::group_by(AcpFilledFormId) |>
      dplyr::mutate(questiontext = case_when(!is.na(answer) ~ paste0("aos_", stringr::str_to_snake(answer)), 
                                      TRUE ~ questiontext),
             answer = case_when(!is.na(answer) ~ "TRUE", 
                                TRUE ~ answer)
      ) |> 
      dplyr::ungroup() 
    
    goals <- a |> 
      dplyr::filter(SectionName  == "Goal Setting") |> 
      dplyr::mutate(AcpFilledFormId = paste0(AcpFilledFormId, SubSectionRowCounter))
    
    a |> dplyr::filter(
           questiontext != "Area of Support Focus", 
           SectionName  != "Goal Setting") |> 
      dplyr::bind_rows(aos,
                       goals) |>
      dplyr::group_by(AcpFilledFormId, questiontext) |>
      dplyr::filter(answer_modified_date == max(answer_modified_date) | is.na(answer_modified_date)) |>
      dplyr::ungroup()
  }
  
  step_c <- function(measures) {
    measures |>
      dplyr::mutate(answer = case_when(answer == "" ~ NA_character_,
                                       TRUE ~ answer)) |>
      dplyr::group_by(AcpFilledFormId,fldservicesrequiredid, questiontext) |> 
      dplyr::slice(1) |> 
      dplyr::ungroup() |> 
      tidyr::pivot_wider(id_cols = c(AcpFilledFormId, PersonId, fldservicesrequiredid,
                                     funding_start, funding_end, fldservicename, version_name, DateCreated),
                         names_from = questiontext,
                         values_from = answer,
                         values_fill = NA) |>
      readr::type_convert() |>
      janitor::clean_names()  |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.logical), as.character)) |> 
      bind_rows(tibble(date_complete = character(),
                       collection_reason = character(),
                       completion_status = character(), 
                       decline_reason = character())) |> 
      dplyr::mutate(date_complete = lubridate::dmy(date_complete),
                    date_complete = dplyr::case_when(is.na(date_complete) ~ date_created, 
                                                     TRUE ~ date_complete),
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
                                      !is.na(decline_reason) ~ as.character(decline_reason), 
                                      TRUE ~ "Measure incomplete")) |> 
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
      dplyr::bind_rows(tibble::tibble(
        k5_q1 = character(), 
        k5_q2 = character(), 
        k5_q3 = character(), 
        k5_q4 = character(), 
        k5_q5 = character())) |>
      dplyr::mutate(#custom scoring
        across(starts_with("k5_q"), k10_coder),
        completion_status = case_when(!if_any(starts_with("k5_q"), is.na) ~ "Measure Complete",
                                      !is.na(decline_reason) ~ as.character(decline_reason), 
                                      TRUE ~ "Measure incomplete"),
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
                                                  !is.na(decline_reason) ~ decline_reason, 
                                                  TRUE ~ "Measure incomplete")) |> 
      bind_rows(tibble(
        sdq_q1 = integer(), sdq_q2 = integer(), sdq_q3 = integer(), 
        sdq_q4 = integer(), sdq_q5 = integer(), sdq_q6 = integer(), 
        sdq_q7 = integer(), sdq_q8 = integer(), sdq_q9 = integer(), 
        sdq_q10 = integer(), sdq_q11 = integer(), sdq_q12 = integer(), 
        sdq_q13 = integer(), sdq_q14 = integer(), sdq_q15 = integer(), 
        sdq_q16 = integer(), sdq_q17 = integer(), sdq_q18 = integer(), 
        sdq_q19 = integer(), sdq_q20 = integer(), sdq_q21 = integer(), 
        sdq_q22 = integer(), sdq_q23 = integer(), sdq_q24 = integer(), 
        sdq_q25 = integer())) |> 
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
  if (type == "stsh") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      stsh_prep() |>
      step_c()
    
    return(out)
    
  }
  if (type == "iar") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      iar_prep() |>
      step_c()
    
    return(out)
    
  }
  if (type == "amhc_gp") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      amhcgp_prep() |>
      step_c() |> 
      dplyr::bind_rows(tibble::tibble(
        dr_title = character(), 
        dr_forename = character(), 
        dr_surname = character())) |>
      unite(gp, c(dr_title, dr_forename, dr_surname), sep = " ") 
    
    return(out)
    
  }
  if (type == "lcq") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      lcq_prep() |>
      step_c() |> 
      mutate(across(.cols = matches("^amount_of_time|living_situation_rating|physical_health_1"), ~as.integer(.x)), 
             total_overall = get_support + hopefulness + part_of_group_community + wellbeing + achieve_important_things + happiness)
     
    return(out)
    
  }
  if (type == "sn") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      sn_prep() |>
      step_c() 
     
    return(out)
    
  }
  if (type == "intreg") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      intreg_prep() |>
      step_c() 
     
    return(out)
    
  }
  if (type == "isp") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      isp_prep() |>
      step_c() |> 
      dplyr::bind_rows(tibble::tibble(
        goal_area = character(), 
        start_date = character(), 
        start_rating = character(), 
        review_date = character(), 
        review_rating = character(), 
        end_date = character(), 
        end_rating = character(), 
        end_goal_status = character())
        ) |> 
      dplyr::relocate(goal_area, start_date, start_rating, 
                      review_date, review_rating, 
                      end_date, end_rating, end_goal_status,  
                      .after = everything()) |> 
      tidyr::drop_na(goal_area, start_date)
     
    return(out)
    
  }
  if (type == "ras") {
    out <-  measures |>
      step_a(fundings = fundings) |>
      # Custom filters and recodes
      ras_prep() |>
      step_c() |> 
      mutate(across(matches("^x\\d"), ~if_else(is.na(.x), 0L, .x)), 
             d1_sum = rowSums(across(matches(paste0("^x", 1:6, "_")))),
             d1_count = rowSums(across(matches(paste0("^x", 1:6, "_")), ~.x!=0L)),
             d2_sum = rowSums(across(matches(paste0("^x", 7:24, "_")))),
             d2_count = rowSums(across(matches(paste0("^x", 7:24, "_")), ~.x!=0L)),
             d3_sum = rowSums(across(matches(paste0("^x", 25:31, "_")))),
             d3_count = rowSums(across(matches(paste0("^x", 25:31, "_")), ~.x!=0L)),
             d4_sum = rowSums(across(matches(paste0("^x", 32:38, "_")))),
             d4_count = rowSums(across(matches(paste0("^x", 32:38, "_")), ~.x!=0L)),
             d1_perc = d1_sum / d1_count / 4 * 100,
             d2_perc = d2_sum / d2_count / 4 * 100,
             d3_perc = d3_sum / d3_count / 4 * 100,
             d4_perc = d4_sum / d4_count / 4 * 100, 
             
             total_score = (d1_sum + d2_sum + d3_sum + d4_sum),
             # total score i is calculated per the method for total score(just adjusting for completeness), averaging all complete items 
             total_score_i = (d1_sum + d2_sum + d3_sum + d4_sum) / (d1_count + d2_count + d3_count + d4_count) / 4 * 100,
             # total score d is calculated by averaging all the domains, this puts less weight on the looking forward domain
             total_score_d = (d1_perc + d2_perc + d3_perc + d4_perc) / 4
      ) 
    
    return(out)
    
  }
  
  else {
    out <-  measures |>
      step_a(fundings = fundings) |>
      step_c()
    
    return(out)
    
  }
  
}

