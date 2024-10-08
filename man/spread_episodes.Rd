% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spread_episodes.R
\name{spread_episodes}
\alias{spread_episodes}
\title{Extend a dataframe over arbitrary time units}
\usage{
spread_episodes(data, from, to, by = "month")
}
\arguments{
\item{data}{a dataframe containing date time spans}

\item{from}{column containing the start date (will be floored to the `by` unit)}

\item{to}{column containing the end date}

\item{by}{the units the span should be counted by (e.g. "month", "quarter")}
}
\description{
Extend a dataframe over arbitrary time units
}
\examples{
test <-  tibble(x= ymd("2023-01-15"), y = ymd("2023-05-06"))
spread_episodes(test, x, y, by = "month")
spread_episodes(test, x, y, by = "quarter")
}
