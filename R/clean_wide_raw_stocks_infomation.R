
#' clean raw stocks information
#' @description It takes a wide formated data frame with prices, market value, book balue and volumn
#'  and changes it to long format. It changes the class of the date column to date, and it adds
#' a colum for months dates. It excludes dates after july of the last year and before June of the
#' first year. It creates a years column which marks the year start in Jun and its end in July to
#' ease fama french three factors calculation later. It changes prices which their volume shows
#'  NA or zero to zero. It keepsfirms only which their book, market, prices and volume
#'  information are avaiable.
#'
#'@param raw_stocks_info_file  linke to datastream file in wide format
#'@param rf_info_file links to risk free rate file with 'dates' and 'RF' columns
#'@param country_code the country in which the data belongs to.
#'@return \code{dt} with volum,prices,market value and book value
#' @import data.table
#' @importFrom  dplyr as.tbl
#' @importFrom  dplyr select
#' @importFrom zoo as.yearmon
#' @importFrom tidyr gather
#' @importFrom lubridate year

#'@export
#'

clean_wide_raw_stocks_infomation=function(raw_stocks_info_file,rf_daily_file,
                                          rf_monthly_file,country_code=NULL,save=FALSE)
{


 # raw_stocks_info_file <- "data/US_stocks.csv"
  #rf_daily_file <-"data/US_rf_d.csv"
   #rf_monthly_file <- "data/US_rf_m.csv"
  #
  #

  #use tbl to exclude error colums
  dt <- as.tbl(read.csv(raw_stocks_info_file))
  names(dt)[1]<-"dates"
  dt <- select(dt,-starts_with("X.ERROR"))
  #change to long format to easily work with data
  dt$dates <- as.Date(dt$dates,format="%d/%m/%Y")
  dt<-dt[dt$dates < "2007-01-01" & dt$dates > "2000-01-01"  ,]
  #dt<-dt[dt$dates > "2000-01-01" ,]

  print("new2")

  #to remove the 's' entry in the venz data
  # tbl <- tbl[tbl$dates > "1990-01-05",]
  dt[,2:ncol(dt)]<-lapply(dt[,2:ncol(dt)],as.numeric)

  dt <- gather(dt,firms,values,-dates)
  dt <- as.data.table(dt)

  dt<-clean_stocks_infomation(dt,rf_daily_file,rf_monthly_file)
  #bottom_half<-round(nrow(dt)/2,0)
  #upper_half <- bottom_half+1

  #bottom_dt <- dt[1:bottom_half,]
  #upper_dt<-dt[upper_half:nrow(dt)]
  #rm(dt)
  #bottom_dt<-clean_stocks_infomation(bottom_dt,rf_daily_file,rf_monthly_file)
  #upper_dt<-clean_stocks_infomation(upper_dt,rf_daily_file,rf_monthly_file)

 # dt<-rbind(bottom_dt,upper_dt)

  return(dt)

}
#' clean stocks information
#' @description It takes a wide formated data frame with prices, market value, book balue and volumn
#'  and changes it to long format. It changes the class of the date column to date, and it adds
#' a colum for months dates. It excludes dates after july of the last year and before June of the
#' first year. It creates a years column which marks the year start in Jun and its end in July to
#' ease fama french three factors calculation later. It changes prices which their volume shows
#'  NA or zero to zero. It keepsfirms only which their book, market, prices and volume
#'  information are avaiable.
#'
#'@param raw_stocks_info_file  linke to datastream file in wide format
#'@param rf_info_file links to risk free rate file with 'dates' and 'RF' columns
#'@param country_code the country in which the data belongs to.
 #'@return \code{dt} with volum,prices,market value and book value
#' @import data.table
#' @importFrom  dplyr as.tbl
#' @importFrom  dplyr select
#' @importFrom zoo as.yearmon
#' @importFrom tidyr gather
#' @importFrom lubridate year

#'@export
#'

clean_stocks_infomation=function(dt,rf_daily_file,
                                          rf_monthly_file,country_code=NULL,save=FALSE)
{




  ############################################################

  #creating prices, volumn, bm and mv columns from firms column

  ###########################################################


  #extract volumn info
  StockVolume <- dt[firms %like% "...TURNOVER.BY.VOLUME",]
  StockVolume$firms <- gsub("...TURNOVER.BY.VOLUME", "", StockVolume$firms)
  names(StockVolume)[3]<- "Vol"

    #to extract the prices info
  StockPrices <- dt[firms %like% "...TOT.RETURN.IND",]
  StockPrices$firms <- gsub("...TOT.RETURN.IND", "", StockPrices$firms)
  names(StockPrices)[3]<- "prices"


  #to exttract bookvalues
  BookValue <- dt[firms %like% "BOOK",]
  BookValue$firms <- gsub("...MRKT.VALUE.TO.BOOK", "", BookValue$firms)
  names(BookValue)[3] <- "BM"
  BookValue <- BookValue[,BM:=ifelse(BM == 0,0.0,1/BM)]


  #to extract the market cap info
  MarketValue <- dt[firms %like% "MARKET.VALUE",]
  MarketValue$firms <- gsub("...MARKET.VALUE", "", MarketValue$firms)
  names(MarketValue)[3] <- "MV"
  #MarketValue<-MarketValue[MV>10000]

  rm(dt)
  ############################################################################

  #merging vol, prices, bm and mv in one data frame

  ##########################################################################

  #m34ging intwo two stages
  StockPricesANDVol <- merge(StockPrices,StockVolume)
  rm(StockPrices,StockVolume)
  BookAndMV <- merge(MarketValue,BookValue)
  rm(MarketValue,BookValue)
  StockInfo <- merge(BookAndMV,StockPricesANDVol)
  rm(BookAndMV,StockPricesANDVol)


  #to exclude duplicated prices and easily work with monthly data
  StockInfo[Vol < 1 | is.na(Vol),prices:=0,]
  StockInfo <- StockInfo[prices > 0,]

  #to have all info
  stocks_info_cleaned <- na.omit(StockInfo)[order(-dates)]
  rm(StockInfo)
  ############################################################################
  #starting data from first june of the first year and ending it
  #at the last july of the last year and creating years variable to show
  #the start and finish for every year - July to Jun
  ##########################################################################

  stocks_info_cleaned$yearmon <- as.yearmon(stocks_info_cleaned$dates)

  #exclude data after July of the last year to have the start of the data June and the end July
  first_jun <- tail(stocks_info_cleaned[yearmon %like% "Jun"]$yearmon)[1]
  last_jul <- stocks_info_cleaned[yearmon %like% "Jul"]$yearmon[1]

  stocks_info_cleaned <- stocks_info_cleaned[yearmon <= as.yearmon(last_jul) &
                                               yearmon >= as.yearmon(first_jun)  ]

  #creating years column to show that each year start in June and end in July
  stocks_info_cleaned$year <-year(stocks_info_cleaned$dates)
  stocks_info_cleaned[,years := ifelse(yearmon %like% "Jan" | yearmon %like% "Feb" |
                             yearmon %like% "Mar" | yearmon %like% "Apr" |
                             yearmon %like% "May" | yearmon %like% "Jun",paste(year-1,year,sep = "-"), paste(year,year+1,sep = "-"))]

  #removing the year column for cleaningup
  stocks_info_cleaned <- stocks_info_cleaned[,-("year"),with=F]

  ############################################################################
  #adding daily risk free rate and monthly risk free rate
  ##########################################################################

  #add the daily risk free rate column

  rf_d<-as.data.table(read.csv(rf_daily_file))
  names(rf_d)<-c("dates", "RF")
  rf_d$dates <- as.Date(rf_d$dates,format="%d/%m/%Y")
  #get the average number of trading days per year to divide daily RF by it
  rf_d$year<-year(rf_d$dates)
  trading_days_count <- rf_d[,.(days_in_year=.N),by=.(year)][,round(mean(days_in_year),0)]
  rf_d <- rf_d[,.(dates,RF=(RF/100)/trading_days_count)]
  #rf_d$RF <- rf_d$RF/100

  stocks_info_cleaned <- merge(stocks_info_cleaned,rf_d,by="dates")

  #add monthly risk free rate column
  rf_m<-as.data.table(read.csv(rf_monthly_file))
  names(rf_m)<-c("dates", "RF_m")
  rf_m$dates<- as.Date(rf_m$dates,format="%d/%m/%Y")
  rf_m$yearmon<-as.yearmon(rf_m$dates)
  rf_m<-rf_m[,-'dates',with=F]
  rf_m$RF_m<-(rf_m$RF_m/100)/12
  rf_m <- rf_m[,.(yearmon,RF_m)]

  #adding monthly risk free rate column
  stocks_info_cleaned <- merge(stocks_info_cleaned,rf_m,by="yearmon")
  rm(rf_m)
  rm(rf_d)

  #have data saved for every country to easily load it later
  if(save){
    dataset_file_name <- paste("data/stock_info_cleaned",country_code,".rda",sep = "_")
    save(stocks_info_cleaned,file=dataset_file_name)
  }

  return(stocks_info_cleaned)
}


