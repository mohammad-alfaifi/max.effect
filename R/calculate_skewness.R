#' calculate iskewness and coskewness and classify firms accordingly
#' @description It calculates iskewness and coskewness and then classify firms accordingly. 
#' @param dt data table with prices, market value and book value
#'@return \code{dt} t data table with coskewness and iskewness values and rank 
#' @import data.table  
#' @importFrom zoo yearmon
#' @importFrom moments skewness
#'@export
#'

calculate_skewness_rank <- function(dt){
  
  daily_ff3 <- get_ew_daily_returns_and_ff3(dt)
  
  #calculate monthly resudials for each firm
  skewness=na.omit(daily_ff3[,.(iskew=skewness(lm(daily_returns ~ mkt_prem)$residuals),
                              coskew=(lm(daily_returns ~ mkt_prem^2)$coefficients[2])),
                           by=.(yearmon,firms)])
  
  skewness[,iskew_rank:=ifelse(iskew < quantile(iskew,0.333),"iS-",
                      ifelse(iskew < quantile(iskew,0.666),"iS0","iS+")),by=.(yearmon)]
  skewness[,coskew_rank:=ifelse(coskew < quantile(coskew,0.333),"coS-",
                            ifelse(coskew < quantile(coskew,0.666),"coS0","coS+")),by=.(yearmon)]
  #change dates to next month date in order to merge with the returns 
  #and also to represent IV correctly since it is for the previous month
  #by definition
  
  skewness$yearmon <- skewness$yearmon + (1/12)
  
  
  return(skewness)
}

