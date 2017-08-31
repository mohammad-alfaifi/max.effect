#' exporting the analysis results
#' @description It exports the analysis results for a country to csv file
#' so that they can be used in writing
#'@param country_code  a string character indicating the country code
#'@return export analysis results to excel
#' @import data.table
#'@export
#'


export_results <- function(country_code){


  if(country_code=="SA"){
    dt <- clean_wide_raw_stocks_infomation("data/saudi_stocks.csv",
                                           "data/saudi_rf_d.csv","data/saudi_rf_m.csv")[order(+dates)]
  }else{
    dt <- clean_wide_raw_stocks_infomation("data/venz_stocks.csv",
                                           "data/saudi_rf_d.csv","data/saudi_rf_m.csv")[order(+dates)]
  }
  dt_cleaned <- group_vars(dt)

  max<-get_max_ew_and_vw_portfolio_returns(dt,n=1)
  max2<-get_max_ew_and_vw_portfolio_returns(dt,n=2)
  max3<-get_max_ew_and_vw_portfolio_returns(dt,n=3)
  max4<-get_max_ew_and_vw_portfolio_returns(dt,n=4)
  max5<-get_max_ew_and_vw_portfolio_returns(dt,n=5)

  alphas <- calculate_double_sorted_alpha(dt_cleaned)
  fama_macbeth_bi<-get_fama_macbeth_bivariate_values(dt_cleaned)
  fama_macbeth_uni<-get_fama_macbeth_univariate_values(dt_cleaned)

  tabels<-list(max=max,max2=max2,max3=max3,max4=max4,max5=max5,alphas=alphas,
               fama_macbeth_bi=fama_macbeth_bi,fama_macbeth_uni=fama_macbeth_uni)

  for (i in seq_along(tabels)){

    write.csv(tabels[i],paste("data/",country,"/",names(tabels[i]),".csv",sep=""))
    }

  # ew_p_scater<-graph_double_sorted_portfolios_count(dt)
  # ew_p_distrb<-graph_double_sorted_portfolios_count(dt,distrb = T)
  #
  # vw_p_scater<-graph_double_sorted_portfolios_count(dt,is_ew=F)
  # vw_p_distrb<-graph_double_sorted_portfolios_count(dt,is_ew=F, distrb = T)




}


#' group the ranking of various factors
#' @description It calls functions to calulate the nine factors (e.g. max,iv),then
#' merge them all in one data table
#'@param dt  data table with prices,mv,bm and volume
#'@return \code{dt} data table with stocks nine factors and their ranking
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'


group_vars <- function(dt,country_code=FALSE,save=FALSE){


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

  factor_cols <- c("Size_rank","BM_rank","CP_rank","Rev_rank","MOM_rank",
                   "SSKEW_rank","ISKEW_rank","ILLIQ_rank","IV_rank")
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

