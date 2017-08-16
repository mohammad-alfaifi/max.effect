
#' rank firms based on the returns of the past month
#' @description It take a data table with daily prices. It calls calculate monthly returns
#' function, then lag the return by one and then classify firms accordingly   
#' @param dt data table with daily prices
#'@return \code{dt} odata table with monthly reversal value(past month return) and rank
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_reversal <- function(dt){
  
  dt_monthly <- calculate_monthly_returns(dt)
  
  dt_monthly <- dt_monthly[,rev:=shift(monthly_returns,1L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,.(yearmon,firms,rev)])
  dt_monthly <- dt_monthly[,rev_rank:=ifelse(rev < quantile(rev,0.333),"q1",
                                                   ifelse(rev < quantile(rev,0.666),"q2","q3"))
                           ,by=.(yearmon)]
  return(dt_monthly)
}