
#' calculate max and classify firms accordingly
#' @description It takes a long-formated data table with daily prices, call for return
#' calculation and then calculates the maxmium return for each firm in each month and
#' then classify firms accordingly. The maxmimum return if n=1 is the highest return
#' for a firm in the month, while if n=2, it is the average of the highest and second highest
#' returns
#'
#'@param dt  data table with daily prices
#'@param n is the number of maxmium returns taken for each stock,
#'if above 1, the average of n period maxmium returns is taken
#'@return \code{dt} with monthly return, firms, max rank and max value
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'


calculate_max_rank <- function(dt,n=1,num_cuts=3,rank_only=F,max_type="max"){

print("range_rt")

  dt <- calculate_daily_returns(dt)[order(-yearmon,+daily_returns)]



  #using rank to get different portfolios
  #  rank <- dt[,.(firm_rt_rank=rank(daily_returns),daily_returns,dates),
  #            by=.(yearmon,firms)][order(-firm_rt_rank,-yearmon)]
  #
  # # calculate max 1 or max 2 according to the value of n
  # max<-rank[,.SD[1],by=.(yearmon,firms,firm_rt_rank)]  [,.(firms_max=round(mean(daily_returns),2)),
  #                                          by=.(yearmon,firms)]
  # max<-na.omit(max)

  if(max_type=="max"){
    print("max max")

    max<-dt[,.(firms_max=round(max(daily_returns),2)),by=.(yearmon,firms)]
  }
  else if(max_type=="min"){
    print("min max")
    max<-dt[,.(firms_max=round(min(daily_returns),2)),by=.(yearmon,firms)]
  }else{
    print("diff max")
    max<-dt[,.(firms_max=round(abs(max(daily_returns))-abs(min(daily_returns)),2)),
               by=.(yearmon,firms)]

  }


  #cut max into decile or quntile, etc
  max<-cut_portfolio(max,"firms_max","max_rank",num_cuts)


  if(rank_only==T){
    return(max)
  }

  max$yearmon <- max$yearmon + (1/12)

  dt <- dt[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]

  dt_monthly <-na.omit(merge(max,dt_monthly))

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

  max_rt <- max[order(+yearmon)]
  max_rt <- max_rt[,monthly_returns:=log(prices)-shift(log(prices), 1L, type="lag"),by=.(firms)]
  max_rt <- na.omit(max_rt)
  max_rt <- max_rt[,.(portfolio_return=mean(monthly_returns)),
                   by=.(max_rank,yearmon,RF_m)]
  max_rt$portfolio_return <- max_rt$portfolio_return - max_rt$RF_m

  max_rt<-max_rt[,-('RF_m'),with=F]
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

  max_rt <- max[,.(sum_mvs = sum(MV)),by=.(RF_m,max_rank,yearmon)][order(+yearmon)]

  max_rt <- max_rt[,portfolio_return:= log(sum_mvs)-shift(log(sum_mvs),1L, type="lag"),
                   by=.(max_rank)]

  max_rt$portfolio_return <- max_rt$portfolio_return-max_rt$RF_m

  max_rt <- na.omit(max_rt[,-c("sum_mvs","RF_m"),with=F])

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

calculate_max_factor <- function(max,is_equally_weighted,portfolio=F,num_cuts){

  #mark upper portfolio

  highest_rank <- paste("q",num_cuts,sep="")

  if(is_equally_weighted==TRUE){
    max_rt <- calculate_max_e_returns(max)
  }else{
    max_rt <- calculate_max_v_returns(max)
  }

  if(portfolio){ return(max_rt) }

 #keep highest rank with the lowest to subtract and consutrcut max factor
  max_rt <- max_rt[max_rank == "q1" | max_rank == highest_rank]

  max_rt_wide<- spread(max_rt,max_rank,portfolio_return)
  max_rt_wide$max_factor <- max_rt_wide[[highest_rank]] - max_rt_wide$q1
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

calculate_max_factor_for_e_and_v_returns <- function(dt,portfolio=F,is_ew=T,n=1,num_cuts,max_type="max"){


  max <- calculate_max_rank(dt,n,num_cuts,rank_only=F,max_type=max_type)

  e_max <- calculate_max_factor(max,is_equally_weighted = T, portfolio,num_cuts)
  v_max <- calculate_max_factor(max,is_equally_weighted = F, portfolio,num_cuts)

  if(portfolio ){

    if(is_ew==T){ return(e_max)}
    return(v_max)
  }
  colnames(e_max)[which(colnames(e_max)=="max_factor")] <- "e_max_factor"
  colnames(v_max)[which(colnames(v_max)=="max_factor")] <- "v_max_factor"

  max_factors <- merge(e_max, v_max)

  return(max_factors)

}

#' group max alpha and raw returns for EW and VW portfolios
#' @description It takes a long-formated data table with daily return and calls
#' calculate_max_factor_for_e_and_v_returns function to get the equally-weighted and
#' value-weighted returns for max portfolio. Then, it calss calculate_max_portfolio_alpha_and_raw_returns
#' function to get the alpha and raw returns for portfolio either equally-weighted or value-weighted. Finally,
#' it groups the equally-weighted and value-weighted alpha and raw returns in one data table
#'@param dt  data table with daily  prices

#' returns
#'@return \code{dt} with alphas and raw returns for equally-weighted and value-weighted portfolios
#' @import data.table
#'@export
#'
get_max_ew_and_vw_portfolio_returns <- function(dt,n=1,num_cuts,max_type="max"){
  e_max_ps<-calculate_max_factor_for_e_and_v_returns(dt,portfolio=T,is_ew=T,n,num_cuts,max_type)
  ew_rt<-calculate_max_portfolio_alpha_and_raw_returns(e_max_ps,is_ew=T,dt,num_cuts)

  v_max_ps<-calculate_max_factor_for_e_and_v_returns(dt,portfolio=T,is_ew=F,n,num_cuts,max_type)
  vw_rt<-calculate_max_portfolio_alpha_and_raw_returns(v_max_ps,is_ew=F,dt,num_cuts)
  max_avg_returns<-cbind(ew_rt,vw_rt)


  return(max_avg_returns)
}

#' calculate alpha and raw return for portfolio
#' @description It takes a data table with the monthly return of each max rank,
#' then it calculates the difference between the HMAX and LMAX and merge them
#' with the dt of three-factor model. Next, it calls calculate_max_portfolio_alpha_or_raw_returns function
#' twice to get the raw and alpha returns. It calls round_and_format function to format the alpha and raw
#' returns and then combine them in one data table
#'@param max_ps  data table with monthly return of each max rank
#'@param is_ew logical, indicating wither the monthly return of each max rank is equally-weighted
#'or value weighted.
#'@param dt  data table with daily prices, market value and book to market. used for FF-3 calculation
#'@return \code{dt} with alpha and raw returns for either equally-weighted or value-weighted max portfolio
#' @import data.table
#'@export
#'
calculate_max_portfolio_alpha_and_raw_returns<-function(max_ps,is_ew,dt,num_cuts){

  highest_rank <- paste("q",num_cuts,sep="")
  highest_portfolio <- paste("q",(num_cuts+1),sep = "")
  #HMAX-LMAX max return on ff3
  max_ps<-spread(max_ps,max_rank,portfolio_return)
  #create High-Low Max
  max_ps[[highest_portfolio]] <- max_ps[[highest_rank]] - max_ps$q1

  ff3_m<-calculate_ff3(dt,monthly = T)
  #ff3_m<-calculate_US_ff3()

  max_ps<-merge(max_ps,ff3_m,by="yearmon")


  #regression results

  max_alphas<-calculate_max_portfolio_alpha_or_raw_returns(max_ps,alphas=T,num_cuts)
  max_raw_rt<-calculate_max_portfolio_alpha_or_raw_returns(max_ps,alphas=F,num_cuts)


  max_alphas<-round_and_format(max_alphas,alphas = T,num_cuts)
  max_raw_rt<-round_and_format(max_raw_rt,alphas = F,num_cuts)

  max_rt<-rbind(max_raw_rt,max_alphas)

  colnames(max_rt)[which(colnames(max_rt)=="factor")] <- ifelse(is_ew==TRUE,"EW","VW")

  return(max_rt)
}

#' calculates alpha or raw return for a max portfolio
#' @description It takes a data table with monthly return of each max rank and H-MAX-LMAX rank
#' and calculates either the alpha or raw raturn for each rank. It uses only the intercept and P-value
#'@param alphas  logical, if True, it calculates the alpha return

#' returns
#'@return \code{dt} with alpha or raw return for each max rank
#' @import data.table
#'@export
#'
calculate_max_portfolio_alpha_or_raw_returns<-function(max_ps,alphas=T,num_cuts){

   rt_avg<-data.table()


  for(i in 1:(num_cuts+1)){
    y=as.name(paste("q",i,sep=""))
    if(alphas){
      q<-lm(eval(y)~smb+hml+mkt_prem,data=max_ps,na.action = na.exclude)
    }else{
      q<-lm(eval(y)~1,data=max_ps,na.action = na.exclude)
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
#' alpha return for each max rank and round the return and p-value and format
#' the results.
#' max factors for for equally and value weighted portfolio
#'@param avg_rt  data table with alpha or raw return
#'@param alphas  logical, if TRUE, it highlights that the return is for
#' the alpha of the three-factor model

#'@return \code{dt} with max_factor for both equally and value weighted returns
#' @import data.table
#'@export
#'
round_and_format<-function(avg_rt,alphas=T,num_cuts){

  avg_rt[,c(1,2)]<-round(avg_rt[,c(1,2)],3)
  avg_rt$estimate<-ifelse(avg_rt$p.value<=.01,paste( avg_rt$estimate,"***"),
                          ifelse(avg_rt$p.value<=.05,paste(avg_rt$estimate,"**"),
                                 ifelse(avg_rt$p.value<=.1,paste(avg_rt$estimate,"*"),
                                        avg_rt$estimate)) )
  avg_rt$p.value <- paste("(",avg_rt$p.value,")")

  avg_rt<-data.table(t(avg_rt))
  if(num_cuts==3){
    colnames(avg_rt)<-c("Low-Max","Med-Max","High-Max","High-Low-Max")
  }else{
    middle_max<-as.character(2:(num_cuts-1))
    colnames(avg_rt)[1]<-"Low-Max"
    colnames(avg_rt)[2:(num_cuts-1)]<-middle_max
    colnames(avg_rt)[num_cuts]<-"High-Max"
    colnames(avg_rt)[(num_cuts+1)]<-"High-Low-Max"
    }
  #colnames(avg_rt)<-as.character(avg_rt[3,])
  #avg_rt<-avg_rt[-3,]
  avg_rt$factor <- ifelse(alphas==TRUE,"FF-3","Raw")
  avg_rt$factor[duplicated(avg_rt$factor)] <- ""
  if(num_cuts==3){
    avg_rt<-avg_rt[,.(factor,`Low-Max`,`Med-Max`,`High-Max`,`High-Low-Max`)]

  }



  return(avg_rt)
}
