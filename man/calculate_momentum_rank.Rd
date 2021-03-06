% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_momentum.R
\name{calculate_momentum_rank}
\alias{calculate_momentum_rank}
\title{calculate rate of change and classify firms accordingly}
\usage{
calculate_momentum_rank(dt, portfolio_only = F, num_cuts)
}
\arguments{
\item{dt}{data table with daily prices and size columns}

\item{portfolio_only}{{logical} if True, returns only each firm with its momentum rank}
}
\value{
\code{dt} with monthly return, firms, max rank and max value
}
\description{
calculate rate of change based on 11 months and then lag it one  month, and then
classify #' firms accordingly
}
