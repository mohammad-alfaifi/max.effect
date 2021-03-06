% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_max.R
\name{calculate_max_rank}
\alias{calculate_max_rank}
\title{calculate max and classify firms accordingly}
\usage{
calculate_max_rank(dt, n = 1, num_cuts = 3)
}
\arguments{
\item{dt}{data table with daily prices}

\item{n}{is the number of maxmium returns taken for each stock,
if above 1, the average of n period maxmium returns is taken}
}
\value{
\code{dt} with monthly return, firms, max rank and max value
}
\description{
It takes a long-formated data table with daily prices, call for return
calculation and then calculates the maxmium return for each firm in each month and
then classify firms accordingly. The maxmimum return if n=1 is the highest return
for a firm in the month, while if n=2, it is the average of the highest and second highest
returns
}
