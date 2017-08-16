
#' calculate max and classify firms accordingly
#' @description It takes a long-formated data table with daily prices, call for return
#' calculation and then calculates the maxmium return for each firm in each month and 
#' then classify firms accordingly. 
#' 
#'@param dt  data table with daily prices
#'@return \code{dt} with monthly return, firms, max rank and max value
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'


calculate_max_rank <- function(dt){
  
  dt <- calculate_daily_returns(dt)
  
  max <- dt[,.(firms_max=max(daily_returns)),by=.(yearmon,firms)]
  
  max[,max_rank:=ifelse(firms_max < quantile(firms_max,0.333),"q1",ifelse(firms_max < quantile(firms_max,0.666),"q2","q3")),by=.(yearmon)]
  max$yearmon <- max$yearmon + (1/12)
  
  dt <- dt[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]
  dt_monthly <- merge(max,dt_monthly)
  
  return(dt_monthly)
  
}


#' calculate equally-weighted return for stock sorted on max
#' @description It takes a long-formated data table with daily returns  and calculates
#'  maxmim return for a firm in a given month and classifying firms accordingly by calling
#'  calculate_max function. Next, it calculates the  return for each 
#'  max portfolio: low,med and high using ln(Pt1) - ln(Pt0)
#'@param max  data table with max rank and firms max
#'@return \code{dt} with volum,prices,market value and book value
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'


calculate_max_e_returns<- function(max){
  
  max_rt <- max[,.(sum_prices = sum(prices)),by=.(max_rank,yearmon)][order(+yearmon)]
  max_rt <- max_rt[,portfolio_return:= log(sum_prices)-shift(log(sum_prices),1L, type="lag"),
                 by=.(max_rank)]
  max_rt <- na.omit(max_rt[,-("sum_prices"),with=F])
  
  return(max_rt)
  
}

#' calculate  value-weighted return for stock sorted on max
#' 
#' @description It takes a long-formated data table with daily returns  and calculates
#'  maxmim return for a firm in a given month and classifying firms accordingly by calling
#'  calculate_max function. Next, it calculates the value-weighted return for each 
#'  max portfolio: low,med and high. The weight for each portfolio is the total market
#'  cap for all of its stock divided by the market cap for the total market cap of all the
#'  three portfolios. The return is then normalised using natural log (1+return)
#' 
#'@param max  data table with max rank and firms max
#'@return \code{dt} with volum,prices,market value and book value
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'

calculate_max_v_returns <- function(max){
  
  max_rt <- max[,.(sum_mv = sum(MV)),by=.(max_rank,yearmon)][order(+yearmon)]
  
  max_rt <- max_rt[,portfolio_return:= log(sum_mv)-shift(log(sum_mv),1L, type="lag"),
                   by=.(max_rank)]
  max_rt <- na.omit(max_rt[,-("sum_mv"),with=F])
  
  return(max_rt)
}

#' calculate max factor for given portfolio
#' @description It takes a long-formated data table with max rank and value and call one of two
#' functions (equally-weighted or value-weighted) to calculate reurn. Next, it calculates the
#' difference between high max and low max as the max factor.
#' @param max  data table with max rank and max value
#' @param is_equally_weighted logical indicating wether the factor for equally or value weighted
#' returns  
#'@return \code{dt} the orginal data table with two more columns:  IV and iv_rank   
#' @import data.table  
#' @importFrom zoo yearmon
#' @importFrom tidyr spread
#'@export
#'

calculate_max_factor <- function(max,is_equally_weighted,portfolio=F){
  
  
  if(is_equally_weighted){
    max_rt <- calculate_max_e_returns(max)
  }else{
    max_rt <- calculate_max_v_returns(max)
  }
  
  if(portfolio){return(max_rt)}
  
  max_rt <- max_rt[max_rank != "q2"]
  
  max_rt_wide<- spread(max_rt,max_rank,portfolio_return)
  max_rt_wide$max_factor <- max_rt_wide$q3 - max_rt_wide$q1
  max_rt_wide <- na.omit(max_rt_wide[,.(yearmon,max_factor)])
  
  
 
  return(max_rt_wide)
  
}



#' calculate returns for max for equally and value weighted portfolio
#' @description It takes a long-formated data table with daily return and calculates 
#' max factors for for equally and value weighted portfolio  
#'@param e_rt  data table with daily  equilly weighted returns
#'@param v_rt  data table with daily value weighted returns

#' returns  
#'@return \code{dt} with max_factor for both equally and value weighted returns
#' @import data.table  
#' @importFrom zoo yearmon
#'@export
#'

calculate_max_factor_for_e_and_v_returns <- function(dt,portfolio=F,weighted=F){
  

  max <- calculate_max_rank(dt)

  e_max <- calculate_max_factor(max,is_equally_weighted = T, portfolio)
  v_max <- calculate_max_factor(max,is_equally_weighted = F, portfolio)

  if(portfolio ){
    
    if(weighted){ return(v_max)}
    return(e_max)
  }
  colnames(e_max)[which(colnames(e_max)=="max_factor")] <- "e_max_factor"
  colnames(v_max)[which(colnames(v_max)=="max_factor")] <- "v_max_factor"
  
  max_factors <- merge(e_max, v_max)
  
  return(max_factors)
  
}