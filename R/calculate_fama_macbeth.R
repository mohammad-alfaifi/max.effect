
fama_macbeth_max_with_all_factors <- function(dt){


  max <- get_slope_avg(dt,"firms_max")
  size <- get_slope_avg(dt,"lag_mv")
  bm <- get_slope_avg(dt,"lag_bm")
  momentum <- get_slope_avg(dt,"roc")
  rev <- get_slope_avg(dt,"rev")
  cp <- get_slope_avg(dt,"lag_prices")
  sskew <- get_slope_avg(dt,"coskew")
  iskew <- get_slope_avg(dt,"iskew")
  illiq <- get_slope_avg(dt,"illiq_ratio")
  iv <- get_slope_avg(dt,"IV")

}

get_slope_avg <- function(dt,factor_name){

  factor_name <- as.name(factor_name)
  slope <- dt[,.(betas=lm(returns~eval(factor_name))$coefficients[2]),
              by=.(yearmon)][,.(factor=list(coeftest(lm(betas~1),
                                                     vcov. = NeweyWest(lm(betas~1),
                                                                       prewhite = FALSE))))]
}
