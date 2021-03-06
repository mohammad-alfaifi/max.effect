% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_max.R
\name{calculate_max_factor_for_e_and_v_returns}
\alias{calculate_max_factor_for_e_and_v_returns}
\title{calculate returns for max for equally and value weighted portfolio}
\usage{
calculate_max_factor_for_e_and_v_returns(dt, portfolio = F, is_ew = T,
  n = 1, num_cuts)
}
\arguments{
\item{e_rt}{data table with daily  equilly weighted returns}

\item{v_rt}{data table with daily value weighted returns
returns}
}
\value{
\code{dt} with max_factor for both equally and value weighted returns
}
\description{
It takes a long-formated data table with daily return and calculates
max factors for for equally and value weighted portfolio
}
