
#' rank firms based on closing price of the past month
#' @description It take a data table with daily prices. It keeps the prices of the last
#' day of each month for each firm, lag them by one, and then classify firms according

#' @param dt data table with daily prices
#'@return \code{dt} odata table with monthly prices rank
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_cl_prices_rank <- function(dt,num_cuts,double_sorted=T){

  dt <- dt[,.(dates,yearmon,firms,prices)][order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)][order(+dates)]
  dt_monthly <- dt_monthly[,lag_prices:=shift(prices,1L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,-c("dates","prices"),with=F])

  #to get different ranks
  dt_monthly <-cut_portfolio(dt_monthly,"lag_prices","CP_rank",num_cuts)

  dt_monthly$lag_prices<-log(dt_monthly$lag_prices)
  if(double_sorted){
    dt_monthly$yearmon<-dt_monthly$yearmon - (1/12)
  }
  return(dt_monthly)
}
