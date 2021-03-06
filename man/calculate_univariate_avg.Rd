% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_fama_macbeth.R
\name{calculate_univariate_avg}
\alias{calculate_univariate_avg}
\title{calculate_univariate_avg}
\usage{
calculate_univariate_avg(dt, factor_name, name_displayed)
}
\arguments{
\item{dt}{data table with stock returns and ten factorS (e.g momentum,size,IV,max)}
}
\value{
\code{dt} data table with the average beta for each  factors and
its p-value
}
\description{
It takes a data table with ten factors and returns
and run monthly regression for each  factors of the ten factors as
the indpedent variable and the#' return is the dependent and take
the beta average for each factor #' and display them with the p-value
}
