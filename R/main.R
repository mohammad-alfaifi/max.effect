

pkgs<-list("data.table", "zoo", "tidyr", "dplyr", "lubridate", "moments", "broom",
          "lmtest","sandwich","purrr","IDPmisc")
lapply(pkgs, require, character.only = TRUE)


rm(pkgs)
#' make ready for analysis
#' @description clean and make basic calculation to make ready for analaysis
#'@return list with two data table
#' @import data.table
#'@export
#'
make_data_ready_for_modeling <- function(country_name="saudi",n=1,num_cuts=3){

  stocks<-paste("data/",country_name,"_stocks.csv",sep="")
  RF_d<-paste("data/",country_name,"_rf_d.csv",sep="")
  RF_m<-paste("data/",country_name,"_rf_m.csv",sep="")
  mv_breakpoint<<-.1 #anything above this is considered big stock

  dt <- clean_wide_raw_stocks_infomation(stocks,RF_d,RF_m)[order(+dates)]
  dt_cleaned<-group_vars(dt,n=n,num_cuts = num_cuts)

  data_ready<-list(dt,dt_cleaned)

  return(data_ready)

}





#' exporting the analysis results
#' @description It exports the analysis results for a country to csv file
#' so that they can be used in writing
#'@param country_code  a string character indicating the country code
#'@return export analysis results to excel
#' @import data.table
#'@export
#'



export_results <- function(){


  single_sort_countries<-list("saudi","egypet","turkey","pakistan","brazil","korea")
  max_double_sort_countries<-list("turkey","pakistan")
  min_double_sort_countries<-list("saudi","turkey","pakistan")
  countries_2<-list("germany","japan")

  countries_max<-get_max_or_min_or_iv_by_country(single_sort_countries,num_cuts=3,factor_type="max")
  countries_min<-get_max_or_min_or_iv_by_country(single_sort_countries,num_cuts=3,factor_type="min")
  countries_iv<-get_max_or_min_or_iv_by_country(single_sort_countries,num_cuts=3,factor_type="iv")

  countries_alphas_on_max<-get_double_sorted_alpha_by_country(max_double_sort_countries,num_cuts=3,factor_type="max")
  countries_alphas_on_min<-get_double_sorted_alpha_by_country(min_double_sort_countries,num_cuts=3,factor_type="min")
  #countries_alphas_on_iv<-get_double_sorted_alpha_by_country(countries,num_cuts=3,factor_type="iv")

  countries_uni_macbeth<-get_fama_macbeth_uni_by_country(countries)
  countries_bi_macbeth<-get_fama_macbeth_bi_by_country(countries)

  korea<-list("korea")

  korea_max<-get_max_or_min_or_iv_by_country(korea,num_cuts=3,factor_type="max")
  korea_min<-get_max_or_min_or_iv_by_country(korea,num_cuts=3,factor_type="min")
  korea_iv<-get_max_or_min_or_iv_by_country(korea,num_cuts=3,factor_type="iv")

  korea_alphas_on_max<-get_double_sorted_alpha_by_country(korea,num_cuts=3,factor_type="max")
  #korea_alphas_on_min<-get_double_sorted_alpha_by_country(korea,num_cuts=3,factor_type="min")
  #countries_alphas_on_iv<-get_double_sorted_alpha_by_country(countries,num_cuts=3,factor_type="iv")

  korea_uni_macbeth<-get_fama_macbeth_uni_by_country(korea)
  korea_bi_macbeth<-get_fama_macbeth_bi_by_country(korea)


  brazil<-list("brazil")

  brazil_max<-get_max_or_min_or_iv_by_country(brazil,num_cuts=3,factor_type="max")
  brazil_min<-get_max_or_min_or_iv_by_country(brazil,num_cuts=3,factor_type="min")
  #brazil_iv<-get_max_or_min_or_iv_by_country(brazil,num_cuts=3,factor_type="iv")

  #brazil_alphas_on_max<-get_double_sorted_alpha_by_country(brazil,num_cuts=3,factor_type="max")
  brazil_alphas_on_min<-get_double_sorted_alpha_by_country(brazil,num_cuts=3,factor_type="min")
  #countries_alphas_on_iv<-get_double_sorted_alpha_by_country(countries,num_cuts=3,factor_type="iv")
  #
  # brazil_uni_macbeth<-get_fama_macbeth_uni_by_country(brazil)
  # brazil_bi_macbeth<-get_fama_macbeth_bi_by_country(brazil)



  dt<-NaRV.omit(dt)
  dt_cleaned<-NaRV.omit(dt_cleaned)





  tabels<-list(dt=dt,dt_cleaned=dt_cleaned,max=max,max2=max2,max3=max3,max4=max4,max5=max5,alphas=alphas,
               fama_macbeth_bi=fama_macbeth_bi,fama_macbeth_uni=fama_macbeth_uni)






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


  dt<-dt[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][,-c("dates","RF_m"),with=F]
  ff3_monthly <- calculate_ff3(dt,monthly = T)


  max <- calculate_max_rank(dt,n=1,num_cuts,max_type = "max")[,.(yearmon,firms,firms_max,max_rank)]
  min <- calculate_max_rank(dt,n=1,num_cuts,max_type = "min")[,.(yearmon,firms,firms_min=firms_max,min_rank=max_rank)]
  iv <- calculate_iv_rank(dt,num_cuts)[,.(yearmon,firms,IV,IV_rank)]

  size <- calculate_monthly_size_rank(dt,num_cuts,double_sorted = F)
  bm <- calculate_monthly_bm_rank(dt,num_cuts)
  cl <- calculate_monthly_cl_prices_rank(dt,num_cuts)
  rev <- calculate_monthly_reversal(dt,num_cuts)
  mom <- calculate_momentum_rank(dt,portfolio_only = T,num_cuts)
  illiq <- calculate_monthly_illiq_rank(dt,num_cuts)
  skewness <- calculate_skewness_rank(dt,num_cuts)
  beta<-calculate_stock_beta(dt)

  data <-as.data.table(Reduce(function(...) merge(...,by=c("yearmon","firms")), list(size, bm, cl,rev,mom,max,min,skewness,iv,illiq,beta,dt_monthly)))
  data <- merge(data,ff3_monthly, by="yearmon")

  factor_cols <- c("Size_rank","BM_rank","CP_rank","Rev_rank","MOM_rank",
                   "SSKEW_rank","ISKEW_rank","ILLIQ_rank","IV_rank","min_rank")
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








# test <- read.dta("http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.dta")
# # fpm <- plm(y ~ x, test, model='pooling', index=c('firmid', 'year'))
# #  fpmg <- pmg(y~x, test, index=c("year","firmid")) ##Fama-MacBeth
   # max_only <- pmg(returns~IV + lag_mv
   #            , data=dt_cleaned,index=c("yearmon","firms")) ##Fama-MacBeth
# # min_only <- pmg(returns~firms_min + lag_mv + lag_bm +roc + rev +
#                   lag_prices + coskew + iskew +illiq_ratio + IV
#                 , data=dt_cleaned,index=c("yearmon","firms"))
# #
# max_and_min_only2 <- pmg(returns~firms_min
#                 , data=dt_cleaned,index=c("yearmon","firms"))
# #
#  max_min<-pmg(returns~firms_max + lag_mv + lag_bm +roc + rev +
#                            lag_prices + coskew + iskew +illiq_ratio + IV+firms_min
#                           , data=dt_cleaned,index=c("yearmon","firms"))
#
# #
#  # fpmg <- pmg(returns~ lag_prices + firms_max ,
#  #             data=dt_cleaned,index=c("yearmon","firms")) ##Fama-MacBeth
#  #
#  #
# # data("Gasoline", package = "plm")
# # Gas <- pdata.frame(dt_cleaned, index = c("firms", "yearmon"), drop.index = TRUE)
# #
#   if(is_max_p){
#     reg_vars_names <- c("yearmon","returns","MAX","Size",
#                         "BM","Momentum","Rev","CP","SSKEW",
#                         "ISKEW","IV","MIN")
#
#   }else{
#     reg_vars_names <- c("yearmon","returns","IV","Size",
#                         "BM","Momentum","Rev","CP","SSKEW",
#                         "ISKEW","ILLIQ","MAX","Max:Min")
#   }
#
#
#   #the loop is just to get the beta coefficents of each factor and takes
#   #its average and p-value to ease the grouping into one data frame
#    for (i in 1:10){
#
#      if(is_max_p){
#        slope <- dt_c[,.(betas=lm(returns~firms_max+ lag_mv + lag_bm +
#                                  roc + rev + lag_prices + coskew + iskew +
#                                  IV+firms_min)$coefficients[i+1]),
#                    by=.(yearmon)][,.(factor= as.numeric(roubst_se(lm(betas~1))))]
#      }else{
#        slope <- dt_c[,.(betas=lm(returns~IV + lag_mv + lag_bm +
#                                  roc + rev + lag_prices + coskew + iskew +
#                                  illiq_ratio + firms_max+firms_min)$coefficients[i+1]),
#                    by=.(yearmon)][,.(factor= as.numeric(roubst_se(lm(betas~1))))]
#      }
#   #
#      slope_avg <- data.frame(round(slope$factor,5))
#      colnames(slope_avg)<- reg_vars_names[i+2]
#
#      slope_avg[1,1]<-ifelse(slope_avg[2,1]<=.01,paste(slope_avg[1,1],"***"),
#                             ifelse(slope_avg[2,1]<=.05,paste(slope_avg[1,1],"**"),
#                                    ifelse(slope_avg[2,1]<=.1,paste(slope_avg[1,1],"*"),
#                                           slope_avg[1,1])) )
#   #
#      slope_avg[2,]<-paste("(",slope_avg[2,],")")
#   #
#      if(i==1){
#        slope_avgs <- data.frame(slope_avg)
#      }else{
#        slope_avgs <- cbind(slope_avgs,slope_avg)
#      }
#    }
#    rownames(slope_avgs) <- c("slope","p-value")
#   #
#    slope_avgs <- as.data.table(slope_avgs)
#
#
#   max_only <- pmg(returns~firms_max , data=dt_cleaned,index=c("yearmon","firms")) ##F
