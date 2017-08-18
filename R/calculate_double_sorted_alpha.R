#' ccalculate double sorted alphas
#' @description It takes a link of the stock information, then it calls group_vars function
#' to get the data with all stocks various ranks. Next it calls get_factor_controlled_portfolio_stats
#' function to calculates the alpha for a certain factor (e.g. size) of equally and double sorted
#' portfolios
#'@param raw_stocks_info_file  link of the stock information
#'@return \code{dt} data table with double sorted alpha statstics
#' @import data.table
#'@export
#'

calculate_double_sorted_alpha <- function(dt){



  size <- get_factor_controlled_portfolio_stats(dt,"size_rank",levels(dt$size_rank))
  bm <- get_factor_controlled_portfolio_stats(dt,"bm_rank",levels(dt$bm_rank))
  cl <- get_factor_controlled_portfolio_stats(dt,"cl_prices_rank",levels(dt$cl_prices_rank))
  rev <- get_factor_controlled_portfolio_stats(dt,"rev_rank",levels(dt$rev_rank))
  mom <- get_factor_controlled_portfolio_stats(dt,"mom_rank",levels(dt$mom_rank))
  coskew <- get_factor_controlled_portfolio_stats(dt,"coskew_rank",levels(dt$coskew_rank))
  iskew <- get_factor_controlled_portfolio_stats(dt,"iskew_rank",levels(dt$iskew_rank))
  illiq <- get_factor_controlled_portfolio_stats(dt,"illiq_rank",levels(dt$illiq_rank))
  iv <- get_factor_controlled_portfolio_stats(dt,"iv_rank",levels(dt$iv_rank))




  e_p<-rbind(size,bm,cl,rev,mom,coskew,iskew,illiq,iv)


  size_v <- get_factor_controlled_portfolio_stats(dt,"size_rank",levels(dt$size_rank),is_e_weighted = F)
  bm_v <- get_factor_controlled_portfolio_stats(dt,"bm_rank",levels(dt$bm_rank),is_e_weighted = F)
  cl_v <- get_factor_controlled_portfolio_stats(dt,"cl_prices_rank",levels(dt$cl_prices_rank),is_e_weighted = F)
  rev_v <- get_factor_controlled_portfolio_stats(dt,"rev_rank",levels(dt$rev_rank),is_e_weighted = F)
  mom_v <- get_factor_controlled_portfolio_stats(dt,"mom_rank",levels(dt$mom_rank),is_e_weighted = F)
  coskew_v <- get_factor_controlled_portfolio_stats(dt,"coskew_rank",levels(dt$coskew_rank),is_e_weighted = F)
  iskew_v <- get_factor_controlled_portfolio_stats(dt,"iskew_rank",levels(dt$iskew_rank),is_e_weighted = F)
  illiq_v <- get_factor_controlled_portfolio_stats(dt,"illiq_rank",levels(dt$illiq_rank),is_e_weighted = F)
  iv_v <- get_factor_controlled_portfolio_stats(dt,"iv_rank",levels(dt$iv_rank),is_e_weighted = F)




  v_p<-rbind(size_v,bm_v,cl_v,rev_v,mom_v,coskew_v,iskew_v,illiq_v,iv_v)

  alphas_double_sorted <-cbind(e_p,v_p)

  return(alphas_double_sorted)
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

  if(save){
    dataset_file_name <- paste("data/all_vars",country_code,".rda",sep = "_")
    save(dt,file=dataset_file_name)
  }

  return(data)

}


#' get the alpha and p values for double sorted portfolios on max and another factor
#' @description It calls functions to calulate the nine factors (e.g. max,iv),then
#' merge them all in one data table
#'@param dt  data table with double sorted stocks
#'@param factor_rank_name  the name of the factor that we want to double sort with max
#'@param factor_ranking  the ranks of the factor (levels)
#'@param is_e_weighted  logical, if False calculates the return of value weighted portfolios

#'@return \code{dt} data table with alpha and p value of nine double sorted portfolios
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'


get_factor_controlled_portfolio_stats <- function(dt,factor_rank_name,factor_ranking,portfolio=F,is_e_weighted=T){

  factor_rank_name = as.name(factor_rank_name)

  max_ranking <- c("q1","q2","q3")
  fc_portfolios_stats <- data.frame()

  for(i in 1:3){

    for(j in 1:3){

      if(is_e_weighted){
        fc_portfolio <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) & max_rank == eval(max_ranking[j]),
                           .(sum_prices=sum(prices)),by=.(yearmon,smb,hml,mkt_prem)][,
                                                                                     portfolio_returns:=log(sum_prices)-
                                                                                       shift(log(sum_prices),1L,type="lag")]
      }else{
        fc_portfolio <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) & max_rank == eval(max_ranking[j]),
                           .(sum_mv=sum(MV)),by=.(yearmon,smb,hml,mkt_prem)][,
                                                                             portfolio_returns:=log(sum_mv)-
                                                                               shift(log(sum_mv),1L,type="lag")]
      }


      fc_stats <- tidy(coeftest(lm(portfolio_returns~smb+hml+mkt_prem,data=fc_portfolio),
                                vcov. = NeweyWest))[1,c(2,5)]

      fc_stats$factor=paste(max_ranking[j],"max",factor_ranking[i],factor_rank_name,sep = "_")

      if(i==1 & j ==1){
        fc_portfolios_stats <- data.frame(fc_stats)
      }else{
        fc_portfolios_stats <- rbind(fc_portfolios_stats,fc_stats)
      }

      if(j==3){

        # hmax_lmax <- get_hmax_minus_lmax_for_factor_rank(dt,factor_rank_name,factor_ranking,
        #                                                  max_ranking,i,j)
        # fc_portfolios_stats <- rbind(fc_portfolios_stats,hmax_lmax)

      }
    }
  }


  return(fc_portfolios_stats)


}



######################Unsure about the code below yet#############################

##########################3#######################################################

#
# get_hmax_minus_lmax_for_factor_rank <- function(dt,factor_rank_name,factor_ranking,max_ranking,i,j){
#
#   print(max_ranking)
#   fc_portfolio_h<- dt[eval(factor_rank_name)== eval(factor_ranking[i]) & max_rank == eval(max_ranking[j]),
#                       .(sum_prices=sum(prices)),by=.(yearmon,smb,hml,mkt_prem)][,
#                                                                                 portfolio_returns_h:=log(sum_prices)-
#                                                                                   shift(log(sum_prices),1L,type="lag")]
#
#   fc_portfolio_l <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
#                          max_rank == eval(max_ranking[j-2]),
#                        .(sum_prices=sum(prices)),by=.(yearmon)][,
#                                                                 portfolio_returns_l:=log(sum_prices)-
#                                                                   shift(log(sum_prices),1L,type="lag")]
#
#   fc_portfolio_m <- merge(fc_portfolio_h,fc_portfolio_l,by="yearmon")
#
#   fc_portfolio_m$hmax_lmax <- fc_portfolio_m$portfolio_returns_h-fc_portfolio_m$portfolio_returns_l
#   fc_stats <- tidy(coeftest(lm(hmax_lmax~smb+hml+mkt_prem,data=fc_portfolio_m),
#                             vcov. = NeweyWest))[1,c(2,5)]
#   fc_stats$factor=paste("hmax_lmax",factor_ranking[i],factor_rank_name,sep = "_")
#
#   return(fc_stats)
#
# }
#
#
# # m1, m2: the sample means
# # s1, s2: the sample standard deviations
# # n1, n2: the same sizes
# # m0: the null value for the difference in means to be tested for. Default is 0.
# # equal.variance: whether or not to assume equal variance. Default is FALSE.
# t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
# {
#   if( equal.variance==FALSE )
#   {
#     se <- sqrt( (s1^2/n1) + (s2^2/n2) )
#     # welch-satterthwaite df
#     df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
#   } else
#   {
#     # pooled standard deviation, scaled by the sample sizes
#     se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
#     df <- n1+n2-2
#   }
#   t <- (m1-m2-m0)/se
#   dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))
#   names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
#   return(dat)
# }
#
# # t.test2( mean(fc_portfolio_l$portfolio_returns_l), mean(fc_portfolio_h$portfolio_returns_h),
# #          sd(fc_portfolio_l$portfolio_returns_l), sd(fc_portfolio_h$portfolio_returns_h), 144, 136)
