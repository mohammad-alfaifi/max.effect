#' calculate illiquidty
#' @description It takes a long-formated data table with daily prices. It calculates its return
#' and illiquidty and classify firms accordingly.
#'@param dt  data table with daily prices
#'@return \code{dt} with monthly illiquidty  value and rank
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_monthly_illiq_rank <- function(dt,num_cuts,double_sorted=F){

  dt <- calculate_daily_returns(dt)

  #trading days for each firm
  dt[,trading_days:=.N,by=.(yearmon,firms)]

  #absulte value of daily returns
  dt[,abs_returns:=abs(daily_returns)]
  #volume in dollars
  dt[,vol_in_dollar:=prices*Vol]
  #monthly sum of absolute returns/volumn in dollars
  dt[,rt_over_vol:=sum(abs_returns/vol_in_dollar),by=.(yearmon,firms)][order(+dates)]
  #last day of the month calues
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]

  #illiquidty ratio
  dt_monthly[,illiq_ratio:=(rt_over_vol/trading_days)*1000000]


  dt_monthly$yearmon <- dt_monthly$yearmon + (1/12)

  #to get iliq with different ranks
  dt_monthly <-cut_portfolio(dt_monthly,"illiq_ratio","ILLIQ_rank",num_cuts)

  dt_monthly <- dt_monthly[,.(yearmon,firms,illiq_ratio,ILLIQ_rank)]

  if(double_sorted){
    dt_monthly$yearmon<-dt_monthly$yearmon - (1/12)
  }

  return(dt_monthly)
}
