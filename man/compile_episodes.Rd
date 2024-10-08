% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compile_episodes.R
\name{compile_episodes}
\alias{compile_episodes}
\title{Compile grouped fundings into one episode of care}
\usage{
compile_episodes(data, start_date, end_date, personid, time_gap_threshold = 1L)
}
\arguments{
\item{data}{A dataframe of fundings within one service}

\item{start_date}{The name of the column containing the start dates of the fundings}

\item{end_date}{The name of the column containing the end dates of the fundings}

\item{personid}{The name of the column containing the person identifier}

\item{time_gap_threshold}{The tolerance threshold for gaps between fundings in days (default 1)}
}
\value{
an augmented dataframe containing episodes and a global date range
}
\description{
Some services' data is represented in multiple overlapping funding streams.
This function collapses these into continuous periods of service or episodes of care.
}
\examples{
type_convert(tribble(
~personid, ~streamid, ~datetimestart, ~datetimeend, ~targetgroup,
1,         1,   "2023-01-01", "2023-01-05",            1,
1,         2,   "2023-01-07", "2023-01-30",            2,

2,         2,   "2023-12-01", NA_character_,           1, 
2,         1,   "2024-01-12", "2024-01-30",            1,
2,         3,   "2024-02-10", "2024-02-28",            1,
2,         1,   "2024-02-25", NA_character_,           1,

3,         3,   "2023-12-01", "2024-01-14",            1, 
3,         2,   "2024-01-12", "2024-01-30",            1,
3,         1,   "2024-01-10", "2024-02-01",            1,

4,         3,   "2023-12-01", "2024-01-14",            1, 
4,         2,   "2024-01-12", "2024-01-20",            1,
4,         1,   "2024-01-21", NA_character_,           1

)) \%>\% 
  compile_episodes(datetimestart, datetimeend, personid, 1L) 
}
