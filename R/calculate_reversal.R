
#' rank firms based on the returns of the past month
#' @description It take a data table with daily prices. It calls calculate monthly returns
#' function, then lag the return by one and then classify firms accordingly
#' @param dt data table with daily prices
#'@return \code{dt} odata table with monthly reversal value(past month return) and rank
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_reversal <- function(dt,num_cuts,double_sorted=F){

  dt_monthly <- calculate_monthly_returns(dt)

  dt_monthly <- dt_monthly[,rev:=shift(monthly_returns,1L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,.(yearmon,firms,rev)])

  #to get different ranks
  dt_monthly <-cut_portfolio(dt_monthly,"rev","Rev_rank",num_cuts)
  if(double_sorted){
    dt_monthly$yearmon<-dt_monthly$yearmon - (1/12)
  }

  return(dt_monthly)
}
