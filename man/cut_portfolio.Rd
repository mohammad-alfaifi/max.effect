% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_portfolio.R
\name{cut_portfolio}
\alias{cut_portfolio}
\title{devide or cut portfolio}
\usage{
cut_portfolio(dt, factor_name, rank_name, num_cuts)
}
\arguments{
\item{dt}{data table with daily prices}

\item{factor_name}{name of the factor(e.g. firms_max)}

\item{rank_name}{name of the rank(e.g. max_rank)}

\item{num_cuts}{number of cuts(e.g. 3,5,or 10)}
}
\value{
\code{dt} with portfolio cuts
}
\description{
it cut portfolio to the desired number
}
