#' ccalculate daily fama-french
#' @description It takes a long formated data frame with prices, market value, book balue and volumn
#' and calculate the daily SMB and HML.
#' It first calls for calculate_six_portfolios_returns to calculate the daily six portfolios
#' then make the SMB and HML calculation.
#'
#'@param dt  data table with prices column
#'@param country_code the country in which the data belongs to.
#'@param save {optioanl} save data
#'@return \code{dt} the orginal data table with additional three columns for value, size
#'and ff3_por returns
#' @import data.table
#' @importFrom zoo as.yearmon
#' @importFrom lubridate year

#'@export
#'

calculate_ff3 <- function(dt, monthly=F,country_code=NULL, save=FALSE){

  mkt <- calculate_market_excess_returns(dt,monthly)

  p6 <- calculate_six_portfolios_returns(dt,monthly)

  p6$smb <- 1/3 * (p6$SV + p6$SN + p6$SG) - 1/3 *(p6$BV + p6$BN + p6$BG)
  p6$hml <- 1/2 * (p6$SV + p6$BV) - 1/2 *(p6$SG + p6$BG )

  if(monthly){
    ff3 <- p6[,.(yearmon,smb,hml)]
    ff3 <- na.omit(merge(ff3,mkt,by="yearmon"))


  }else{
    ff3 <- p6[,.(dates,smb,hml)]
    ff3 <- na.omit(merge(ff3,mkt,by="dates"))

  }


  if(save){
  dataset_file_name <- paste("data/daily_stock_returns",country_code,".rda",sep = "_")
  save(dt,file=dataset_file_name)
  }
  return(ff3)
}





#' ccalculate daily market return
#' @description It takes a long formated data frame with prices, market value, book balue and volumn
#' and calculate the daily weighted returns for all the stocks in the market
#'@param dt  data table with weighted_return column
#'@param country_code the country in which the data belongs to.
#'@param save {optional} save data on working directory
#'@return \code{dt} dates with market returns
#' @import data.table
#' @import zoo
#'@export
#'
#'
calculate_market_excess_returns <- function(dt,monthly=F, country_code=NULL, save=FALSE){


  if(monthly){
    dt <- dt[order(+dates)]
    dt <- dt[,.SD[.N],by=.(yearmon,firms)]
    dt <- dt[,.(sum_mvs=sum(MV)),by= .(yearmon,RF_m)][order(+yearmon)]
    dt <- dt[ ,mkt_prem:=((log(sum_mvs)- shift(log(sum_mvs), 1L, type="lag"))-RF_m)]

  }else{
    dt <- dt[,.(sum_mvs = sum(MV)),by= .(dates,RF)][order(+dates)]
    dt <- dt[ ,mkt_prem:=((log(sum_mvs)- shift(log(sum_mvs), 1L, type="lag"))-RF)]
  }

  dt <- na.omit(dt[,-"sum_mvs"])



  if(save){
    dataset_file_name <- paste("data/market_returns",country_code,".rda",sep = "_")
    save(dt,file=dataset_file_name)
  }
  return(dt)
}





#' classfiys firms based on market cap and book-to-market
#' @description It classfiys firms according to their market cap. Firms Above the median are big
#' while firms below the median are small. Next, it classfys each size (big and small) to value,
#' neutral and growth. Firms with high book-to-market ratio are value while those with low
#'  book-to-marketa ratio are growth.
#'@param dt  data table with daily returns, market cap, book-to-market
#'@param mv_breakpoint {optional} market value break point to classify firms as big or small
#'@return \code{dt} with the size and value columns
#' @import data.table
#' @import zoo
#'@export
#'
#'
#'
classify_firms_according_to_mv_and_bm <- function(dt){

print(mv_breakpoint)
  #to use one day information on the month of June and month of December
  dt <- dt[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]

  #get MV every June to classify big and small
  mv <- dt_monthly[yearmon %like% "Jun" ]
  mv[,size:=ifelse(MV > quantile(MV,mv_breakpoint),"big","small"),by=.(yearmon)]
  mv <- mv[,.(years,firms,size)]

  #get book-to-market every last Dec to classify value,growth and neutral
  bm <- dt_monthly[yearmon %like% "Dec" ]

  #to make the classfication for firms which their information avaiable in June and December
  accounting_info <- merge(bm,mv)

  accounting_info[size=="big",value:=ifelse(BM < quantile(BM,.3),"BG",
                                            ifelse(BM < quantile(BM,.7),"BN","BV")),
                  by=.(yearmon)]
  accounting_info[size=="small",value:=ifelse(BM < quantile(BM,.3),"SG",
                                              ifelse(BM < quantile(BM,.7),"SN","SV")),
                  by=.(yearmon)]


  accounting_info <- accounting_info[,.(years,firms,size,value)]

  #to assign the firms classfication BG, BN, BV and so on to firms daily information
  dt <- merge(dt,accounting_info,by=c("years","firms"))[order(+dates)]

  return(dt)


}


#' calculate fama-french six portfolios
#' @description It starts with calling classify_firms_according_to_mv_and_bm function to make
#' the necessary classfication of firms. Next, it calculates the return for each one of the six
#'  portfolios: growth, neutral and value for big and small firms.
#'@param dt  data table with daily returns, market cap, book-to-market
#'@param country_code {optional} the country in which the data belongs to.
#'@param save {optioanl} save data
#'@return \code{dt} with the sum and return of fama-french six portflios
#' @import data.table
#'@export
#'
#'
#'
calculate_six_portfolios_returns <- function(dt,monthly=F){

  dt <- classify_firms_according_to_mv_and_bm(dt)
  if(monthly){
    dt_monthly <- dt[,.SD[.N],by=.(yearmon,firms)]
    p6 <- dt_monthly[,.(sum_mvs=sum(MV)),by=.(value,yearmon)]
  }else{
    p6 <- dt[,.(sum_mvs=sum(MV)),by=.(value,dates)]
  }
  p6 <- p6[,portfolio_return:= log(sum_mvs)-shift(log(sum_mvs),1L, type="lag"),
           by=.(value)]
  p6 <- p6[,-("sum_mvs"),with=F]
  p6 <- spread(p6,value,portfolio_return)

  return(p6)

}


#' calculate fama-french US values
#' @description reterive, orginze the data from French website of the FF-3
#'@return \code{dt} with three factor values with risk free rate
#' @import data.table
#'@export
#'
#'
#'
calculate_US_ff3 <- function(){
  ff3_file<-'/home/moh/Downloads/ff3.csv'

  ff3<-as.data.table(read.csv(ff3_file))

  ff3$year <- substr(ff3$dates,1,4)
  ff3$month <- substr(ff3$dates,5,6)
  ff3$day <- "1"
  ff3$dates<-paste(ff3$year,ff3$month,ff3$day,sep = "-")
  ff3$yearmon<-as.yearmon(ff3$dates)

  ff3<-ff3[,.(yearmon,smb=smb/100,hml=hml/100,mkt_prem=mkt_prem/100)]
  return(ff3)
}

