

#' calculate rate of change and classify firms accordingly
#' @description calculate rate of change based on 11 months and then lag it one  month, and then 
#' classify #' firms accordingly
#'@param dt  data table with daily prices and size columns
#'@param portfolio_only {logical} if True, returns only each firm with its momentum rank
#'@return \code{dt} with monthly return, firms, max rank and max value
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'

calculate_momentum_rank <-function(dt,portfolio_only=F){
  
  dt <- dt[,.(dates,yearmon,firms,prices)][order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)][order(+dates)]
  
  dt_monthly[,roc:=log(prices)-shift(log(prices), 11L, type="lag"),by=.(firms)]
  dt_monthly[,roc:=shift(roc,1L,type="lag"),by=.(firms)]
  dt_monthly <- na.omit(dt_monthly)
  
  if(portfolio_only){
    
    dt_monthly[,mom_rank:=ifelse(roc < quantile(roc,.333),"WML-L",
                                    ifelse(roc < quantile(roc,.666),"WML-N","WML-W")),
       by=.(yearmon)]
    
    dt_monthly <- dt_monthly[,.(yearmon,firms,roc,mom_rank)]
    
  }else{
    
    dt_monthly[size=="big",mom_rank:=ifelse(roc < quantile(roc,.333),"BL",
                                              ifelse(roc < quantile(roc,.666),"BN","BW")),
                    by=.(yearmon)]
    dt_monthly[size=="small",mom_rank:=ifelse(roc < quantile(roc,.333),"SL",
                                    ifelse(roc < quantile(roc,.666),"SN","SW")),
       by=.(yearmon)]
}
  
  return(dt_monthly)

}


#' calculate returns for momentum portfolios
#' @description calculate equally weighted-return for momentum portfolio
#'@param dt  data table with monthly prices and momentum rank
#'@return \code{dt} origianl data table with portfolio return (momentum portfolios)
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'
#'

calculate_momentum_e_rt <- function(mom){
  

  mom_rt <- mom[,.(sum_prices = sum(prices)),by=.(mom_rank,yearmon)][order(+yearmon)]
  
  mom_rt <- mom_rt[,portfolio_return:= log(sum_prices)-shift(log(sum_prices),1L, type="lag"),
                   by=.(mom_rank)]
  mom_rt <- mom_rt[,-("sum_prices"),with=F]
  
  return(mom_rt)
}

#' calculate momentum factor
#' @description calculate  momentum factor is the average difference between winners and
#' losers portfolios. 
#'@param dt  data table with monthly prices and size columns
#'@return \code{dt} with monthly return, firms, max rank and max value
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'
#'

calculate_momentum_factor <- function(dt){
  
  mom <- calculate_momentum_rank(dt)
  mom_rt <- calculate_momentum_e_rt(mom)
  
  mom_rt_wide<- spread(mom_rt,mom_rank,portfolio_return)
  mom_rt_wide$mom_factor <- .5 * (mom_rt_wide$SW + mom_rt_wide$BW) -
                            .5 *(mom_rt_wide$SL + mom_rt_wide$BL) 
  mom_rt_wide <- na.omit(mom_rt_wide[,.(yearmon,mom_factor)])
  
  return(mom_rt_wide)
  
}


