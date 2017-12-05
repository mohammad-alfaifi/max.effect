#' calculate iskewness and coskewness and classify firms accordingly
#' @description It calculates iskewness and coskewness and then classify firms accordingly.
#' @param dt data table with prices, market value and book value
#'@return \code{dt} t data table with coskewness and iskewness values and rank
#' @import data.table
#' @importFrom zoo yearmon
#' @importFrom moments skewness
#'@export
#'

calculate_skewness_rank <- function(dt,num_cuts,double_sorted=F){


  #calculate fama-french three factors model
  ff3 <- calculate_ff3(dt)
  ff3<-ff3[,-"RF",with=F]
  #calculate indvidual stock excess return
  returns <- calculate_daily_returns(dt)
  returns$excess_returns <- returns$daily_returns - returns$RF

  #make ready for regression
  daily_ff3 <- merge(ff3,returns,by=("dates"))


  #calculate monthly resudials for each firm
  skewness=na.omit(daily_ff3[,.(iskew=skewness(lm(excess_returns ~ mkt_prem)$residuals),
                              coskew=(lm(excess_returns ~ I(mkt_prem^2))$coefficients[2])),
                           by=.(yearmon,firms)])



  #to get different ranks
  skewness <-cut_portfolio(skewness,"iskew","ISKEW_rank",num_cuts)
  skewness <-cut_portfolio(skewness,"coskew","SSKEW_rank",num_cuts)



  #change dates to next month date in order to merge with the returns
  #and also to represent IV correctly since it is for the previous month
  #by definition

  if(double_sorted==F){
    skewness$yearmon <- skewness$yearmon + (1/12)

  }


  return(skewness)
}

