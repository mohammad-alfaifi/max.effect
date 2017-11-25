
#' rank firms based on size
#' @description It take a data table with daily market cap. It keeps the market value of the last
#' day of each month for each firm, takes the natural log, lag them by 1 and then classify them according

#' @param dt data table with daily market cap
#'@return \code{dt} odata table with monthly market cap rank
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_size_rank <- function(dt,num_cuts,double_sorted=F){

  dt <- dt[,.(dates,yearmon,firms,log_mv=log(MV))][order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)][order(+dates)]
  dt_monthly <- dt_monthly[,lag_mv:=shift(log_mv,1L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,-c("dates","log_mv"),with=F])

  dt_monthly <- dt_monthly[,Size_rank:=ifelse(lag_mv < quantile(lag_mv,0.333),"q1",
                                              ifelse(lag_mv < quantile(lag_mv,0.666),"q2","q3"))
                           ,by=.(yearmon)]

  #to get size with different ranks
  dt_monthly <-cut_portfolio(dt_monthly,"lag_mv","Size_rank",num_cuts)
  dt_monthly<-dt_monthly[,.(yearmon,firms,lag_mv,Size_rank)]

  if(double_sorted){
    dt_monthly$yearmon<-dt_monthly$yearmon - (1/12)
    dt_monthly<-dt_monthly[,-"lag_mv",with=F]
  }


  return(dt_monthly)
}
