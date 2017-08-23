#' calculate double sorted alphas
#' @description It takes a data table with return and ten other factors.
#' It calls get_factor_controlled_portfolio_stats function to calculates
#'  the alpha for a certain factor (e.g. size) of equally and double sorted
#' portfolios
#'@param raw_stocks_info_file  link of the stock information
#'@return \code{dt} data table with double sorted alpha statstics for both
#'equally and value weighted portfolios
#' @import data.table
#'@export
#'

calculate_double_sorted_alpha <- function(dt){



  size <- get_factor_controlled_portfolio_stats(dt,"Size_rank",levels(dt$Size_rank))
  bm <- get_factor_controlled_portfolio_stats(dt,"BM_rank",levels(dt$BM_rank))
  cl <- get_factor_controlled_portfolio_stats(dt,"CP_rank",levels(dt$CP_rank))
  rev <- get_factor_controlled_portfolio_stats(dt,"Rev_rank",levels(dt$Rev_rank))
  mom <- get_factor_controlled_portfolio_stats(dt,"MOM_rank",levels(dt$MOM_rank))
  coskew <- get_factor_controlled_portfolio_stats(dt,"SSKEW_rank",levels(dt$SSKEW_rank))
  iskew <- get_factor_controlled_portfolio_stats(dt,"ISKEW_rank",levels(dt$ISKEW_rank))
  illiq <- get_factor_controlled_portfolio_stats(dt,"ILLIQ_rank",levels(dt$ILLIQ_rank))
  iv <- get_factor_controlled_portfolio_stats(dt,"IV_rank",levels(dt$IV_rank))

  e_p<-rbind(size,bm,cl,rev,mom,coskew,iskew,illiq,iv)
  e_p[1]<-round(e_p[1],3)
  e_p[2]<-round(e_p[2],2)

  e_p$estimate<-ifelse(e_p$p.value<=.01,paste( e_p$estimate,"***"),
                          ifelse(e_p$p.value<=.05,paste(e_p$estimate,"**"),
                                 ifelse(e_p$p.value<=.1,paste(e_p$estimate,"*"),
                                        e_p$estimate)) )

   e_p <- format_table_output(e_p)


  size_v <- get_factor_controlled_portfolio_stats(dt,"Size_rank",levels(dt$Size_rank),is_e_weighted = F)
  bm_v <- get_factor_controlled_portfolio_stats(dt,"BM_rank",levels(dt$BM_rank),is_e_weighted = F)
  cl_v <- get_factor_controlled_portfolio_stats(dt,"CP_rank",levels(dt$CP_rank),is_e_weighted = F)
  rev_v <- get_factor_controlled_portfolio_stats(dt,"Rev_rank",levels(dt$Rev_rank),is_e_weighted = F)
  mom_v <- get_factor_controlled_portfolio_stats(dt,"MOM_rank",levels(dt$MOM_rank),is_e_weighted = F)
  coskew_v <- get_factor_controlled_portfolio_stats(dt,"SSKEW_rank",levels(dt$SSKEW_rank),is_e_weighted = F)
  iskew_v <- get_factor_controlled_portfolio_stats(dt,"ISKEW_rank",levels(dt$ISKEW_rank),is_e_weighted = F)
  illiq_v <- get_factor_controlled_portfolio_stats(dt,"ILLIQ_rank",levels(dt$ILLIQ_rank),is_e_weighted = F)
  iv_v <- get_factor_controlled_portfolio_stats(dt,"IV_rank",levels(dt$IV_rank),is_e_weighted = F)




  v_p<-rbind(size_v,bm_v,cl_v,rev_v,mom_v,coskew_v,iskew_v,illiq_v,iv_v)
  v_p[1]<-round(v_p[1],3)
  v_p[2]<-round(v_p[2],2)
  
  #v_p[2]<-lapply(v_p[,2],round,2)
  

  v_p$estimate<-ifelse(v_p$p.value<=.01,paste( v_p$estimate,"***"),
                       ifelse(v_p$p.value<=.05,paste(v_p$estimate,"**"),
                              ifelse(v_p$p.value<=.1,paste(v_p$estimate,"*"),
                                     v_p$estimate)) )

  v_p <- format_table_output(v_p)
  
  alphas_double_sorted <-as.data.table(cbind(e_p,v_p))

  return(alphas_double_sorted)
}


#' get the alpha and p-values for double sorted portfolios
#' @description It extracts the portfolio which corresponds to the factor_rank_name
#' from the received data table, then it calculates the portfolio's return and
#' regresses it against fama-french three factors
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


#' format alphas table
#' @description It take an alpha table with 81 columns
#' and change their format to long format to end up with three columns only
#'@param alphas_table  data table with alphas and p-vakues

#'@return \code{dt} data table with alpha and p value  in three columns 
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

format_table_output<-function(alphas_table){
  
  alphas_table$factor <- gsub("_", "-", alphas_table$factor)
  alphas_table$factor <- gsub("q1", "L", alphas_table$factor)
  
  alphas_table$factor <- gsub("q2", "M", alphas_table$factor)
  alphas_table$factor <- gsub("q3", "H", alphas_table$factor)
  alphas_table$factor <- gsub("-rank", "", alphas_table$factor)
  alphas_table$factor <- gsub("max","MAX",alphas_table$factor)
  alphas_table$p.value <- paste("(",alphas_table$p.value,")")
  
  alphas_table<-separate(alphas_table,factor,c("max","factor2"),sep="-MAX-")
  alphas_table<-data.table(t(alphas_table))
  
  i<-seq(1,79,3)
  
  for (j in i){
    
    three_col_values <- alphas_table[,j:(j+2)]
    colnames(three_col_values)<-as.character(as.matrix(three_col_values[3,]))
    three_col_values<-three_col_values[-3,]
    three_col_values$factor<-as.character(as.matrix(three_col_values[3,]))[1]
    three_col_values<-three_col_values[-3,]
    
    if(j==1){
      formated_table<-data.frame(three_col_values)
    }else{
      formated_table<-rbind(formated_table,three_col_values)
    }
  }
  
  print(colnames(formated_table))
  formated_table <- formated_table[,.(factor,L,M,H)]
  colnames(formated_table)<-c("factor", "L-MAX","M-MAX","H-MAX")
  
  return(formated_table)
  
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
