
#' rank firms based on book-to-market (BM)
#' @description It take a data table with daily BM. It keeps the BM of the last
#' day of each month for each firm and then lag them by six and then classify firms according
#' to their BM

#' @param dt data table with daily BM
#'@return \code{dt} data table with monthly BM rank
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_bm_rank <- function(dt,num_cuts,double_sorted=F){

  dt <- dt[,.(dates,yearmon,firms,BM)][order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)][order(+dates)]
  dt_monthly <- dt_monthly[,lag_bm:=shift(BM,6L, type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly[,-c("dates","BM"),with=F])

  dt_monthly <-cut_portfolio(dt_monthly,"lag_bm","BM_rank",num_cuts)

  if(double_sorted){
    dt_monthly$yearmon<-dt_monthly$yearmon - (1/12)
  }

  return(dt_monthly)
}
