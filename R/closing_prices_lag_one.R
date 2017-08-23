
#' rank firms based on closing price of the past month
#' @description It take a data table with daily prices. It keeps the prices of the last
#' day of each month for each firm, lag them by one, and then classify firms according

#' @param dt data table with daily prices
#'@return \code{dt} odata table with monthly prices rank  
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_cl_prices_rank <- function(dt){
  
  dt <- dt[,.(dates,yearmon,firms,prices)][order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)][order(+dates)]
  dt_monthly <- dt_monthly[,lag_prices:=shift(prices,1L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,-c("dates","prices"),with=F])
  
  dt_monthly <- dt_monthly[,CP_rank:=ifelse(lag_prices < quantile(lag_prices,0.333),"q1",
                                              ifelse(lag_prices < quantile(lag_prices,0.666),"q2","q3"))
                           ,by=.(yearmon)]
  return(dt_monthly)
}