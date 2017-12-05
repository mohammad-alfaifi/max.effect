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

calculate_iv_rank <- function(dt,num_cuts,rank_only=F,double_sorted=F){

  #calculate 3 factors model to be used for regression model
  ff3 <- calculate_ff3(dt)
  ff3<-ff3[,-"RF",with=F]
  #calculate indvidual stock excess return
  returns <- calculate_daily_returns(dt)
  returns$excess_returns <- returns$daily_returns - returns$RF

  #make ready for regression
  returns_with_ff3 <- merge(ff3,returns,by=("dates"))


  #calculate monthly resudials for each firm
  iv=na.omit(returns_with_ff3[,.(residuals=lm( excess_returns ~ hml + smb + mkt_prem)$residuals),
            by=.(yearmon,firms)][,.(residuals_std=sd(residuals),observations_count=.N),
                                 by=.(yearmon,firms)] [,.(IV=(residuals_std*sqrt(observations_count))),
                                                       by=.(yearmon,firms)])




  #to get different ranks
   iv <-cut_portfolio(iv,"IV","IV_rank",num_cuts)


   if(rank_only==T){
     return(iv)
   }
    if(double_sorted){
      dt <- dt[order(+dates)]
      dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]
      dt_monthly <-na.omit(merge(iv,dt_monthly,by=c("yearmon","firms")))
      dt_monthly$yearmon<-dt_monthly$yearmon+ (1/12)
      return(dt_monthly)
    }
  #change dates to next month date in order to merge with the returns
  #and also to represent IV correctly since it is for the previous month
  #by definition
  iv$yearmon <- iv$yearmon + (1/12)


  dt <- dt[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]
  dt_monthly <-na.omit(merge(iv,dt_monthly))


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
  iv_rt <- iv_rt[,monthly_returns:=log(prices)-shift(log(prices), 1L, type="lag"),by=.(firms)]
  iv_rt <- na.omit(iv_rt)
  iv_rt <- iv_rt[,.(portfolio_return=mean(monthly_returns)),
                   by=.(IV_rank,yearmon,RF_m)]
  iv_rt$portfolio_return <- iv_rt$portfolio_return - iv_rt$RF_m

  iv_rt<-iv_rt[,-('RF_m'),with=F]



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


  iv_rt <- iv[,.(sum_mvs = sum(MV)),by=.(RF_m,IV_rank,yearmon)][order(+yearmon)]
  iv_rt <- iv_rt[,portfolio_return:= log(sum_mvs)-shift(log(sum_mvs),1L, type="lag"),
                   by=.(IV_rank)]

  iv_rt$portfolio_return <- iv_rt$portfolio_return-iv_rt$RF_m
  iv_rt <- na.omit(iv_rt[,-c("sum_mvs","RF_m"),with=F])


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


calculate_iv_factor <- function(iv,is_equally_weighted,portfolio=F,num_cuts){

  highest_rank <- paste("q",num_cuts,sep="")

   if(is_equally_weighted){
    iv_rt <- calculate_iv_e_returns(iv)
  }else{
    iv_rt <- calculate_iv_v_returns(iv)
  }
  if(portfolio){ return(iv_rt) }

  iv_rt <- iv_rt[IV_rank == "q1" | IV_rank == highest_rank]


  iv_rt_wide<- spread(iv_rt,IV_rank,portfolio_return)
  iv_rt_wide$iv_factor <- iv_rt_wide[[highest_rank]] - iv_rt_wide$q1
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

calculate_iv_factor_for_e_and_v_returns <- function(dt,portfolio=F,is_ew=T,num_cuts){

  iv <- calculate_iv_rank(dt,num_cuts)

  e_iv <- calculate_iv_factor(iv,is_equally_weighted = T, portfolio,num_cuts)
  v_iv <- calculate_iv_factor(iv,is_equally_weighted = F, portfolio,num_cuts)


  if(portfolio ){
    if(is_ew){ return(e_iv)}
    return(v_iv)
  }

  colnames(e_iv)[which(colnames(e_iv)=="iv_factor")] <- "e_iv_factor"
  colnames(v_iv)[which(colnames(v_iv)=="iv_factor")] <- "v_iv_factor"

  iv_factors <- merge(e_iv, v_iv)

  return(iv_factors)

}



#' group iv alpha and raw returns for EW and VW portfolios
#' @description It takes a long-formated data table with daily return and calls
#' calculate_iv_factor_for_e_and_v_returns function to get the equally-weighted and
#' value-weighted returns for iv portfolio. Then, it calss calculate_iv_portfolio_alpha_and_raw_returns
#' function to get the alpha and raw returns for portfolio either equally-weighted or value-weighted. Finally,
#' it groups the equally-weighted and value-weighted alpha and raw returns in one data table
#'@param dt  data table with daily  prices

#' returns
#'@return \code{dt} with alphas and raw returns for equally-weighted and value-weighted portfolios
#' @import data.table
#'@export
#'
get_iv_ew_and_vw_portfolio_returns <- function(dt,num_cuts){
  e_iv_ps<-calculate_iv_factor_for_e_and_v_returns(dt,portfolio=T,is_ew=T,num_cuts)
  ew_rt<-calculate_iv_portfolio_alpha_and_raw_returns(e_iv_ps,is_ew=T,dt,num_cuts)

  v_iv_ps<-calculate_iv_factor_for_e_and_v_returns(dt,portfolio=T,is_ew=F,num_cuts)
  vw_rt<-calculate_iv_portfolio_alpha_and_raw_returns(v_iv_ps,is_ew=F,dt,num_cuts)
  iv_avg_returns<-cbind(ew_rt,vw_rt)


  return(iv_avg_returns)
}

#' calculate alpha and raw return for portfolio
#' @description It takes a data table with the monthly return of each iv rank,
#' then it calculates the difference between the HMAX and LIV and merge them
#' with the dt of three-factor model. Next, it calls calculate_iv_portfolio_alpha_or_raw_returns function
#' twice to get the raw and alpha returns. It calls round_and_format function to format the alpha and raw
#' returns and then combine them in one data table
#'@param iv_ps  data table with monthly return of each iv rank
#'@param is_ew logical, indicating wither the monthly return of each iv rank is equally-weighted
#'or value weighted.
#'@param dt  data table with daily prices, market value and book to market. used for FF-3 calculation
#'@return \code{dt} with alpha and raw returns for either equally-weighted or value-weighted iv portfolio
#' @import data.table
#'@export
#'
calculate_iv_portfolio_alpha_and_raw_returns<-function(iv_ps,is_ew,dt,num_cuts){

  highest_rank <- paste("q",num_cuts,sep="")
  highest_portfolio <- paste("q",(num_cuts+1),sep = "")
  #HMAX-LIV iv return on ff3
  iv_ps<-spread(iv_ps,IV_rank,portfolio_return)
  #create High-Low Max
  iv_ps[[highest_portfolio]] <- iv_ps[[highest_rank]] - iv_ps$q1

  ff3_m<-calculate_ff3(dt,monthly = T)
  #ff3_m<-calculate_US_ff3()

  iv_ps<-merge(iv_ps,ff3_m,by="yearmon")


  #regression results

  iv_alphas<-calculate_iv_portfolio_alpha_or_raw_returns(iv_ps,alphas=T,num_cuts)
  iv_raw_rt<-calculate_iv_portfolio_alpha_or_raw_returns(iv_ps,alphas=F,num_cuts)


  iv_alphas<-round_and_format_iv(iv_alphas,alphas = T,num_cuts)
  iv_raw_rt<-round_and_format_iv(iv_raw_rt,alphas = F,num_cuts)

  iv_rt<-rbind(iv_raw_rt,iv_alphas)

  colnames(iv_rt)[which(colnames(iv_rt)=="factor")] <- ifelse(is_ew==TRUE,"EW","VW")

  return(iv_rt)
}

#' calculates alpha or raw return for a iv portfolio
#' @description It takes a data table with monthly return of each iv rank and H-IV-LIV rank
#' and calculates either the alpha or raw raturn for each rank. It uses only the intercept and P-value
#'@param alphas  logical, if True, it calculates the alpha return

#' returns
#'@return \code{dt} with alpha or raw return for each iv rank
#' @import data.table
#'@export
#'
calculate_iv_portfolio_alpha_or_raw_returns<-function(iv_ps,alphas=T,num_cuts){

  rt_avg<-data.table()


  for(i in 1:(num_cuts+1)){
    y=as.name(paste("q",i,sep=""))
    if(alphas){
      q<-lm(eval(y)~smb+hml+mkt_prem,data=iv_ps,na.action = na.exclude)
    }else{
      q<-lm(eval(y)~1,data=iv_ps,na.action = na.exclude)
    }
    #get roubst standard error of the model
    r_q<-roubst_se(q)

    if(i==1){
      rt_avg<-r_q

    }else{
      rt_avg<-rbind(rt_avg,r_q)

    }
  }

  return(rt_avg)
}


#' format the raw or alpha return results
#' @description It takes data table with the result of raw return or
#' alpha return for each iv rank and round the return and p-value and format
#' the results.
#' iv factors for for equally and value weighted portfolio
#'@param avg_rt  data table with alpha or raw return
#'@param alphas  logical, if TRUE, it highlights that the return is for
#' the alpha of the three-factor model

#'@return \code{dt} with iv_factor for both equally and value weighted returns
#' @import data.table
#'@export
#'
round_and_format_iv<-function(avg_rt,alphas=T,num_cuts){

  avg_rt[,c(1,2)]<-round(avg_rt[,c(1,2)],3)
  avg_rt$estimate<-ifelse(avg_rt$p.value<=.01,paste( avg_rt$estimate,"***"),
                          ifelse(avg_rt$p.value<=.05,paste(avg_rt$estimate,"**"),
                                 ifelse(avg_rt$p.value<=.1,paste(avg_rt$estimate,"*"),
                                        avg_rt$estimate)) )
  avg_rt$p.value <- paste("(",avg_rt$p.value,")")

  avg_rt<-data.table(t(avg_rt))
  if(num_cuts==3){
    colnames(avg_rt)<-c("Low-IV","Med-IV","High-IV","High-Low-IV")
  }else{
    middle_iv<-as.character(2:(num_cuts-1))
    colnames(avg_rt)[1]<-"Low-IV"
    colnames(avg_rt)[2:(num_cuts-1)]<-middle_iv
    colnames(avg_rt)[num_cuts]<-"High-IV"
    colnames(avg_rt)[(num_cuts+1)]<-"High-Low-IV"
  }
  #colnames(avg_rt)<-as.character(avg_rt[3,])
  #avg_rt<-avg_rt[-3,]
  avg_rt$factor <- ifelse(alphas==TRUE,"FF-3","Raw")
  avg_rt$factor[duplicated(avg_rt$factor)] <- ""
  avg_rt<-avg_rt[,.(factor,`Low-IV`,`Med-IV`,`High-IV`,`High-Low-IV`)]


  return(avg_rt)
}
