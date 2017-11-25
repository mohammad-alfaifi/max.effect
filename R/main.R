

pkgs<-list("data.table", "zoo", "tidyr", "dplyr", "lubridate", "moments", "broom",
          "lmtest","sandwich","purrr")
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)

#' exporting the analysis results
#' @description It exports the analysis results for a country to csv file
#' so that they can be used in writing
#'@param country_code  a string character indicating the country code
#'@return export analysis results to excel
#' @import data.table
#'@export
#'



export_results <- function(country_code="SA"){


  if(country_code=="SA"){
    dt <- clean_wide_raw_stocks_infomation("data/US_stocks.csv",
                                           "data/US_rf_d.csv","data/US_rf_m.csv")[order(+dates)]

    }else{
    dt <- clean_wide_raw_stocks_infomation("data/venz_stocks.csv",
                                           "data/saudi_rf_d.csv","data/saudi_rf_m.csv")[order(+dates)]
  }
  dt_cleaned <- group_vars(dt,n=1,num_cuts)

  max1<-get_max_ew_and_vw_portfolio_returns(dt,n=1,num_cuts)

  max2<-get_max_ew_and_vw_portfolio_returns(dt,n=2,num_cuts)
  max3<-get_max_ew_and_vw_portfolio_returns(dt,n=3,num_cuts)
  max4<-get_max_ew_and_vw_portfolio_returns(dt,n=4,num_cuts)
  max5<-get_max_ew_and_vw_portfolio_returns(dt,n=5,num_cuts)

  alphas <- calculate_double_sorted_alpha(dt)
  fama_macbeth_uni<-get_fama_macbeth_univariate_values(dt_cleaned)
  fama_macbeth_bi<-get_fama_macbeth_bivariate_values(dt_cleaned)

  tabels<-list(dt=dt,dt_cleaned=dt_cleaned,max=max,max2=max2,max3=max3,max4=max4,max5=max5,alphas=alphas,
               fama_macbeth_bi=fama_macbeth_bi,fama_macbeth_uni=fama_macbeth_uni)





   mod<-calculate_max_portfolio_alpha_or_raw_returns(l,alphas = T,num_cuts)
  for (i in seq_along(tabels)){

    saveRDS(tabels[i],paste("data/",country_code,"/",names(tabels[i]),".rds",sep=""))
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


group_vars <- function(dt,n=1,num_cuts=3,country_code=FALSE,save=FALSE){


  dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][,-c("dates","RF_m"),with=F]
  ff3_monthly <- calculate_ff3(dt,monthly = T)


  size <- calculate_monthly_size_rank(dt,num_cuts,double_sorted = F)
  bm <- calculate_monthly_bm_rank(dt,num_cuts)
  cl <- calculate_monthly_cl_prices_rank(dt,num_cuts)
  rev <- calculate_monthly_reversal(dt,num_cuts)
  mom <- calculate_momentum_rank(dt,portfolio_only = T,num_cuts)
  illiq <- calculate_monthly_illiq_rank(dt,num_cuts)
  skewness <- calculate_skewness_rank(dt,num_cuts)
  max <- calculate_max_rank(dt,n=1,num_cuts)[,.(yearmon,firms,firms_max,max_rank)]
  iv <- calculate_iv_rank(dt,portfolio_only = T,num_cuts)[,-c("prices","MV","RF_m")]


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

# require(foreign)
# require(plm)
# require(lmtest)
# test <- read.dta("http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.dta")
# fpm <- plm(y ~ x, test, model='pooling', index=c('firmid', 'year'))
# fpmg <- pmg(y~x, test, index=c("year","firmid")) ##Fama-MacBeth
# fpmg <- pmg(returns~firms_max + lag_mv + lag_bm +
#               roc + rev + lag_prices + coskew + iskew +
#               illiq_ratio + IV, data=dt_cleaned,index=c("yearmon","firms")) ##Fama-MacBeth
#
# data("Gasoline", package = "plm")
# Gas <- pdata.frame(dt_cleaned, index = c("firms", "yearmon"), drop.index = TRUE)
