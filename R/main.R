
group_vars <- function(raw_stocks_info_file,country_code=FALSE,save=FALSE){
  
  dt <- clean_wide_raw_stocks_infomation(raw_stocks_info_file)[order(+dates)]
  dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][,-"dates"]

  size <- calculate_monthly_size_rank(dt)
  bm <- calculate_monthly_bm_rank(dt)
  cl <- calculate_monthly_cl_prices_rank(dt)
  rev <- calculate_monthly_reversal(dt)
  mom <- calculate_momentum_rank(dt,portfolio_only = T)
  illiq <- calculate_monthly_illiq_rank(dt)
  skewness <- calculate_skewness_rank(dt)
  max <- calculate_max_rank(dt)[,.(yearmon,firms,firms_max,max_rank)]
  iv <- calculate_iv_rank(dt,portfolio_only = T)[,-c("prices","MV")]
  
  data<-as.data.table(Reduce(function(...) merge(...,by=c("yearmon","firms")), list(size, bm, cl,rev,mom,max,skewness,iv,illiq,dt_monthly)))

  if(save){
    dataset_file_name <- paste("data/all_vars",country_code,".rda",sep = "_")
    save(dt,file=dataset_file_name)
  }
  
  return(data)
  
}

main <- function(){
  raw_stocks_info_file <- '/home/moh/Documents/MAX Effect/Data/Raw/tasi-2017.csv'
  
  dt <- group_vars(raw_stocks_info_file)
}