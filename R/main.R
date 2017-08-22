
main <- function(){

   raw_stocks_info_file <- '/home/moh/Documents/MAX Effect/Data/Raw/tasi-2017.csv'
  dt <- group_vars(raw_stocks_info_file)

  alphas <- calculate_double_sorted_alpha(dt)

}


#' group the ranking of various factors
#' @description It calls functions to calulate the nine factors (e.g. max,iv),then
#' merge them all in one data table
#'@param raw_stocks_info_file  link of the stock information
#'@return \code{dt} data table with stocks nine factors and their ranking
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'


group_vars <- function(raw_stocks_info_file,country_code=FALSE,save=FALSE){

  dt <- clean_wide_raw_stocks_infomation(raw_stocks_info_file)[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][,-"dates"]
  ff3_monthly <- calculate_ff3(dt,monthly = T)

  size <- calculate_monthly_size_rank(dt)
  bm <- calculate_monthly_bm_rank(dt)
  cl <- calculate_monthly_cl_prices_rank(dt)
  rev <- calculate_monthly_reversal(dt)
  mom <- calculate_momentum_rank(dt,portfolio_only = T)
  illiq <- calculate_monthly_illiq_rank(dt)
  skewness <- calculate_skewness_rank(dt)
  max <- calculate_max_rank(dt)[,.(yearmon,firms,firms_max,max_rank)]
  iv <- calculate_iv_rank(dt,portfolio_only = T)[,-c("prices","MV")]


  data <-as.data.table(Reduce(function(...) merge(...,by=c("yearmon","firms")), list(size, bm, cl,rev,mom,max,skewness,iv,illiq,dt_monthly)))
  data <- merge(data,ff3_monthly, by="yearmon")

  factor_cols <- c("size_rank","bm_rank","cl_prices_rank","rev_rank","mom_rank",
                   "coskew_rank","iskew_rank","illiq_rank","iv_rank")
  df <- as.data.frame(data)
  df[factor_cols] <- lapply(df[factor_cols], factor)
  data <- as.data.table(df)

  data[,returns:=log(prices)-shift(log(prices),1L,type="lag"),by=.(firms)]
  data <- na.omit(data)

  if(save){
    dataset_file_name <- paste("data/all_vars",country_code,".rda",sep = "_")
    save(dt,file=dataset_file_name)
  }

  return(data)

}

