
main <- function(){
  raw_stocks_info_file <- '/home/moh/Documents/MAX Effect/Data/Raw/tasi-2017.csv'
  dt <- group_vars(raw_stocks_info_file)
  dt[,returns:=log(prices)-shift(log(prices),1L,type="lag"),by=.(firms)]
  dt <- na.omit(dt)
  alphas <- calculate_double_sorted_alpha(dt)

}


fama_macbeth_max_with_all_factors <- function(dt){


  max <- get_slope_avg(dt,"firms_max")
  size <- get_slope_avg(dt,"lag_mv")
    size <- dt[,.(alphas=lm(returns~lag_mv)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  bm <- dt[,.(alphas=lm(returns~lag_bm)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  momentum <- dt[,.(alphas=lm(returns~roc)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  rev <- dt[,.(alphas=lm(returns~rev)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  cp <- dt[,.(alphas=lm(returns~lag_prices)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  sskew <- dt[,.(alphas=lm(returns~coskew)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  iskew <- dt[,.(alphas=lm(returns~iskew)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  illiq <- dt[,.(alphas=lm(returns~illiq_ratio)$coefficients[1]),by=.(yearmon)][,mean(alphas)]
  iv <- dt[,.(alphas=lm(returns~IV)$coefficients[1]),by=.(yearmon)][,mean(alphas)]



}

get_slope_avg <- function(dt,factor_name){

  factor_name <- as.name(factor_name)
  slope <- dt[,.(betas=lm(returns~eval(factor_name))$coefficients[2]),
            by=.(yearmon)][,.(factor=list(coeftest(lm(betas~1),
                                                vcov. = NeweyWest)))]
}
