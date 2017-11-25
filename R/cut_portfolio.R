#' devide or cut portfolio
#' @description it cut portfolio to the desired number
#'
#'@param dt  data table with daily prices
#'@param factor_name name of the factor(e.g. firms_max)
#'@param rank_name name of the rank(e.g. max_rank)
#'@param num_cuts number of cuts(e.g. 3,5,or 10)
#'@return \code{dt} with portfolio cuts
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'
cut_portfolio <- function(dt,factor_name,rank_name,num_cuts){


  #to be evaluted by data table
  factor_name <- as.name(factor_name)
  rank <- as.name(rank_name)

  #have q1 q2 cut done dynimically
  q<-rep("q",num_cuts)
  num<-1:num_cuts
  portfolio_names <- paste(q,num,sep="")
  #get quntile decile etc .2, .4 done dynmically
  breaking_points<-round(num/num_cuts,2)

  # cut the portfolio. treat first quntile(decile) using to different ifs
  for (i in 1:num_cuts){

    if ( i == 1){
      dt[eval(factor_name) <= quantile(eval(factor_name),probs=breaking_points[i]),
                                      eval(rank_name):= portfolio_names[i],
         by=.(yearmon)]

    }else {

      dt[eval(factor_name) <= quantile(eval(factor_name),probs=breaking_points[i]) &
           eval(factor_name) > quantile(eval(factor_name),breaking_points[i-1]),
         eval(rank_name):= portfolio_names[i],
         by=.(yearmon)]

     }
  }

  return(dt)
}



