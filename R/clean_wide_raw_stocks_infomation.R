
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

clean_wide_raw_stocks_infomation=function(raw_stocks_info_file,country_code=NULL,save=FALSE)
{
  #use tbl to exclude error colums
  tbl <- as.tbl(read.csv(raw_stocks_info_file))
  tbl <- select(tbl,-starts_with("X.ERROR"))
  df <- as.data.frame(tbl)

  #change to long format to easily work with data
  df$dates <- as.Date(df$dates,format="%d/%m/%y")
  df <- gather(df,firms,values,-dates)
  dt <- as.data.table(df)


  ############################################################

  #creating prices, volumn, bm and mv columns from firms column

  ###########################################################


  #extract volumn info
  StockVolume <- dt[firms %like% "...TURNOVER.BY.VOLUME",]
  StockVolume$firms <- gsub("...TURNOVER.BY.VOLUME", "", StockVolume$firms)
  names(StockVolume)[3]<- "Vol"

    #to extract the prices info
  StockPrices <- dt[!(firms %like% "MARKET.VALUE") & !(firms %like% "MRKT.VALUE.TO.BOOK") & !(firms %like% "TURNOVER.BY.VOLUME"),]
  names(StockPrices)[3] <- "prices"

  #to exttract bookvalues
  BookValue <- dt[firms %like% "BOOK",]
  BookValue$firms <- gsub("...MRKT.VALUE.TO.BOOK", "", BookValue$firms)
  names(BookValue)[3] <- "BM"
  BookValue$BM=round(1/BookValue$BM,digits = 2)
  names(BookValue)[3]<- "BM"


  #to extract the market cap info
  MarketValue <- dt[firms %like% "MARKET.VALUE",]
  MarketValue$firms <- gsub("...MARKET.VALUE", "", MarketValue$firms)
  names(MarketValue)[3] <- "MV"


  ############################################################################

  #merging vol, prices, bm and mv in one data frame

  ##########################################################################

  #m34ging intwo two stages
  StockPricesANDVol <- merge(StockPrices,StockVolume)
  BookAndMV <- merge(MarketValue,BookValue)
  StockInfo <- merge(BookAndMV,StockPricesANDVol)


  #to exclude duplicated prices and easily work with monthly data
  StockInfo[Vol < 1 | is.na(Vol),prices:=0,]
  StockInfo <- StockInfo[prices > 0,]

  #to have all info
  stocks_info_cleaned <- na.omit(StockInfo)[order(-dates)]

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

  #have data saved for every country to easily load it later
  if(save){
    dataset_file_name <- paste("data/stock_info_cleaned",country_code,".rda",sep = "_")
    save(stocks_info_cleaned,file=dataset_file_name)
  }

  return(stocks_info_cleaned)
}


