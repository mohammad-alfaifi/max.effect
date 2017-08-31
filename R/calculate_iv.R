#' calculate IV and classify firms accordingly
#' @description It takes a long-formated data table with daily return, market premium, smb and hml
#' factors and calculates idiosyncratic volatility  and classify firms accordingly.
#'@param dt  data table with daily prices,market value and book to market values
#'@param portfolio_only {logical} if True returns only iv for each stock
#'@return \code{dt} the orginal data table with  IV value and iv_rank, monthly prices
#'and market vlaue
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_iv_rank <- function(dt,portfolio_only=F){

  returns_with_ff3 <- get_ew_daily_returns_and_ff3(dt)

  #calculate monthly resudials for each firm
  iv=na.omit(returns_with_ff3[,.(residuals=lm( daily_returns ~ hml + smb + mkt_prem)$residuals),
            by=.(yearmon,firms)][,.(residuals_std=sd(residuals),observations_count=.N),
                                 by=.(yearmon,firms)] [,.(IV=(residuals_std*sqrt(observations_count))),
                                                       by=.(yearmon,firms)])

  #change dates to next month date in order to merge with the returns
  #and also to represent IV correctly since it is for the previous month
  #by definition
  iv$yearmon <- iv$yearmon + (1/12)

  iv[,IV_rank:=ifelse(IV < quantile(IV,0.333),"q1",
                      ifelse(IV < quantile(IV,0.666),"q2","q3")),by=.(yearmon)]

  #get the value for the end of the month for each firm to merge
  returns_with_ff3 <- returns_with_ff3[order(+dates)]
  monthly_rt_with_ff3 <- returns_with_ff3[,.SD[.N],by=.(yearmon,firms)]
  monthly_rt_with_ff3 <- monthly_rt_with_ff3[,.(yearmon,firms,prices,MV)]

  dt_monthly <- merge(monthly_rt_with_ff3,iv)

  return(dt_monthly)
}



#' calculate equally weighted returns for each iv rank
#' @description It take a data table with IV of the past month and monthly return and
#' calculates iv return for each iv rank/class
#' @param iv data table with iv_rank and stock prices
#'@return \code{dt} data table with portfolio return, rank and dates in yearmon format
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'


calculate_iv_e_returns <- function(iv){

  iv_rt <- iv[order(+yearmon)]
  iv_rt <- iv_rt[,.(portfolio_return=mean(daily_returns)),
                   by=.(IV_rank,yearmon)]

  return(iv_rt)
}


#' calculate value weighted returns for each iv rank
#' @description It take a data table with IV of the past month and monthly return and
#' calculates iv return for each iv rank/class
#' @param iv data table with iv_rank and stock prices
#'@return \code{dt} data table with portfolio return, rank and dates in yearmon format
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_iv_v_returns <- function(iv){


  iv_rt <- iv[,.(sum_prices = sum(prices)),by=.(IV_rank,yearmon)][order(+yearmon)]
  iv_rt <- iv_rt[,portfolio_return:= log(sum_prices)-shift(log(sum_prices),1L, type="lag"),
                 by=.(IV_rank)]
  iv_rt <- na.omit(iv_rt[,-("sum_prices"),with=F])


  return(iv_rt)
}



#' calculate returns for iv portfolio
#' @description It takes a long-formated data table with daily return, market premium, smb and hml
#' factors and calculates idiosyncratic volatility  and classify firms accordingly.
#' @param iv data table with iv rank and iv value
#' @param is_equally_weighted logical indicating wether the factor for equally or value weighted
#' returns
#'@return \code{dt} the orginal data table with two more columns:  IV and iv_rank
#' @import data.table
#' @importFrom zoo yearmon
#' @importFrom tidyr spread
#'@export
#'


calculate_iv_factor <- function(iv,is_equally_weighted,portfolio=F){


   if(is_equally_weighted){
    iv_rt <- calculate_iv_e_returns(iv)
  }else{
    iv_rt <- calculate_iv_v_returns(iv)
  }
  if(portfolio){ return(iv_rt) }

  iv_rt <- iv_rt[IV_rank != "q2"]

  iv_rt_wide<- spread(iv_rt,IV_rank,portfolio_return)
  iv_rt_wide$iv_factor <- iv_rt_wide$q3 - iv_rt_wide$q1
  iv_rt_wide <- na.omit(iv_rt_wide[,.(yearmon,iv_factor)])

  return(iv_rt_wide)
}

#' calculate IV factor
#' @description It takes a long-formated data table with daily prices, market premium, smb and hml
#' factors and call other function to calculates idiosyncratic volatility  and classify firms and
#' then it calculates the IV factor
#'@param dt data table of the daily prices, market value and book-to-market

#' returns
#'@return \code{dt} with iv_factor for both equally and value weighted returns
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_iv_factor_for_e_and_v_returns <- function(dt,portfolio=F,weighted=F){

  iv <- calculate_iv_rank(dt)

  e_iv <- calculate_iv_factor(iv,is_equally_weighted = T, portfolio)
  v_iv <- calculate_iv_factor(iv,is_equally_weighted = F, portfolio)


  if(portfolio ){
    if(weighted){ return(v_iv)}
    return(e_iv)
  }

  colnames(e_iv)[which(colnames(e_iv)=="iv_factor")] <- "e_iv_factor"
  colnames(v_iv)[which(colnames(v_iv)=="iv_factor")] <- "v_iv_factor"

  iv_factors <- merge(e_iv, v_iv)

  return(iv_factors)

}
