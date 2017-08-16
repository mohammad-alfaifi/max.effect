#' ccalculate daily return
#' @description It takes a long formated data frame with prices, market value, book balue and volumn
#' and calculate the daily returns using log(price_today/price_yesterday)
#'@param dt  data table with prices column
#'@param country_code the country in which the data belongs to.
#'@param save {optional} save data on working directory
#'@return \code{dt} the orginal data table with additional column for daily returns
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#' @note the risk free rate still needs to be subracted from daily return

#'

calculate_daily_returns <- function(dt,country_code=NULL, save=FALSE,clean_NA=TRUE){

  dt <- dt[order(+dates)]
  dt[,daily_returns:=log(prices)-shift(log(prices), 1L, type="lag"),by=.(firms)]

  
  if(clean_NA){ dt <- na.omit(dt)}
 
   if(save){
    dataset_file_name <- paste("data/daily_stock_returns",country_code,".rda",sep = "_")
    save(dt,file=dataset_file_name)
  }
  return(dt)
}


#' ccalculate monthly return
#' @description It takes a long formated data frame with prices, market value, book balue and volumn
#' and use the first day of the month per firm to divide it in the past month value minus 1
#' to get the simple monthly return. Next, it adds 1 to the return and take its log  
#'@param dt  data table with prices column
#'@param country_code the country in which the data belongs to.
#'@param save {optional} save data on working directory

#'@return \code{dt} the orginal data table with additional column for monthly returns
#' @import data.table
#' @importFrom  zoo yearmon

#'@export
#'
#'
calculate_monthly_returns <- function(dt, country_code=NULL, save=FALSE){

    #  get the first day prices of a month to calculate their simple returns
    dt <- dt[order(+dates)]
    dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][order(+dates)]
    dt_monthly <- dt_monthly[,monthly_returns:=log(prices)-shift(log(prices), 1L, type="lag"),by=.(firms)]
    dt_monthly <- na.omit(dt_monthly)
    
    if(save){
       
      dataset_file_name <- paste("data/monthly_stock_returns",country_code,".rda",sep = "_")
      save(dt_monthly,file=dataset_file_name)
    }

  return(dt_monthly)
}
