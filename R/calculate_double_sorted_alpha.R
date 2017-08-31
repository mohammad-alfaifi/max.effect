
#' graph count of portfolios for double sorted alpha portfolios
#' @description It calls the calculate_portfolios_count_for_double_sorted_alpha function
#' to calculate the number of portfolios then group them in one data table and graph

#'@param dt  data table with return and ten other factors(e.g. size,BM,max)
#'@param is_ew  logical, if FALSE graph the value weighted portfolio
#'@param distrb  logical, if FALSE return the scater plot of portfolios count

#'@return \code{dt} ggplot object
 #' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

graph_double_sorted_portfolios_count<-function(dt,is_ew=T,distrb=F){


  if(is_ew){
    size <- calculate_portfolios_count_for_double_sorted_alpha(dt,"Size_rank")
    bm <- calculate_portfolios_count_for_double_sorted_alpha(dt,"BM_rank")
    cl <- calculate_portfolios_count_for_double_sorted_alpha(dt,"CP_rank")
    rev <- calculate_portfolios_count_for_double_sorted_alpha(dt,"Rev_rank")
    mom <- calculate_portfolios_count_for_double_sorted_alpha(dt,"MOM_rank")
    coskew <- calculate_portfolios_count_for_double_sorted_alpha(dt,"SSKEW_rank")
    iskew <- calculate_portfolios_count_for_double_sorted_alpha(dt,"ISKEW_rank")
    illiq <- calculate_portfolios_count_for_double_sorted_alpha(dt,"ILLIQ_rank")
    iv <- calculate_portfolios_count_for_double_sorted_alpha(dt,"IV_rank")
  }else{
    size <- calculate_portfolios_count_for_double_sorted_alpha(dt,"Size_rank",is_ew = F)
    bm <- calculate_portfolios_count_for_double_sorted_alpha(dt,"BM_rank",is_ew = F)
    cl <- calculate_portfolios_count_for_double_sorted_alpha(dt,"CP_rank",is_ew = F)
    rev <- calculate_portfolios_count_for_double_sorted_alpha(dt,"Rev_rank",is_ew = F)
    mom <- calculate_portfolios_count_for_double_sorted_alpha(dt,"MOM_rank",is_ew = F)
    coskew <- calculate_portfolios_count_for_double_sorted_alpha(dt,"SSKEW_rank",is_ew = F)
    iskew <- calculate_portfolios_count_for_double_sorted_alpha(dt,"ISKEW_rank",is_ew = F)
    illiq <- calculate_portfolios_count_for_double_sorted_alpha(dt,"ILLIQ_rank",is_ew = F)
    iv <- calculate_portfolios_count_for_double_sorted_alpha(dt,"IV_rank",is_ew = F)
  }
  factors_list<- list(size,bm,cl,rev,mom,coskew,iskew,illiq,iv)
  data <-as.data.table(Reduce(function(...) merge(...,by=c("yearmon")),factors_list))
  data<-as.data.table(gather(data,"factor","count",-yearmon))

  distrb_g <- ggplot(data,aes(x=factor,y=count,col=factor))+geom_violin()
  scater_g <- ggplot(data,aes(x=as.Date(yearmon),y=as.factor(count),col=factor=="IV_rank"))+geom_jitter()

  if(distrb){
    return(distrb_g)
  }
return(scater_g)
}

#' calculate number of portfolios for double sorted alpha
#' @description It calculate number of portfolios for a double sorted portfolio. For example,
#' a portfolio sorted on size and max.

#'@param dt  data table with return and ten other factors
#'@return \code{dt} data table with two coluls:one for date(year or yearmon) and the other
#'for the number of portfolios crospoding to the date
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_portfolios_count_for_double_sorted_alpha<- function(dt,factor_rank_name,is_ew=T){

  factor_rank_name = as.name(factor_rank_name)
  factor_ranking<- c("q1","q2","q3")
  max_ranking <- c("q1","q2","q3")

  data<-data.frame()
  counting_list<-list()


  for (i in 1:3){

    for(j in 1:3){
      portfolio_returns=paste("q",j,"max",factor_rank_name,factor_ranking[i],sep = "_")
      portfolio_returns_name<-as.name(portfolio_returns)


      if(is_ew){
        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  max_rank == eval(max_ranking[j]),.(portfolio_returns=mean(returns)),
                by=.(yearmon,smb,hml,mkt_prem)]
        colnames(q)[which(colnames(q)=="portfolio_returns")]<-eval(portfolio_returns)
      }else{
        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  max_rank == eval(max_ranking[j]),.(sum_prices=sum(prices)),
                by=.(yearmon,smb,hml,mkt_prem)][,eval(portfolio_returns):=log(sum_prices)-
                                                  shift(log(sum_prices),1L,type="lag")]
        q<-q[,-"sum_prices"]
      }


      q<-na.omit(q)
      q$year <- year(q$yearmon)
      q_count<-q[,.N,by=.(yearmon)]
      #q_count$factor <- portfolio_returns


      if(i == 1 & j== 1){
        data<-q_count
      }else{
        data<-rbind(data,q_count)
      }
    }
  }
  data<-data[,.(.N),by=.(yearmon)]
  colnames(data)[2]<-paste(factor_rank_name)


  return(data)
}



#' calculate double sorted alphas
#' @description It takes a data table with return and ten other factors.
#' It calls get_factor_controlled_portfolio_stats function to calculates
#'  the alpha for a certain factor (e.g. size) of equally and double sorted
#' portfolios
#'@param dt  data table with return and ten other factors
#'@return \code{dt} data table with double sorted alpha statstics for both
#'equally and value weighted portfolios
#' @import data.table
#'@export
#'


calculate_double_sorted_alpha <- function(dt){



  size <- get_factor_controlled_portfolio_stats(dt,"Size_rank")
  bm <- get_factor_controlled_portfolio_stats(dt,"BM_rank")
  cl <- get_factor_controlled_portfolio_stats(dt,"CP_rank")
  rev <- get_factor_controlled_portfolio_stats(dt,"Rev_rank")
  mom <- get_factor_controlled_portfolio_stats(dt,"MOM_rank")
  coskew <- get_factor_controlled_portfolio_stats(dt,"SSKEW_rank")
  iskew <- get_factor_controlled_portfolio_stats(dt,"ISKEW_rank")
  illiq <- get_factor_controlled_portfolio_stats(dt,"ILLIQ_rank")
  iv <- get_factor_controlled_portfolio_stats(dt,"IV_rank")

  e_p<-rbind(size,bm,cl,rev,mom,coskew,iskew,illiq,iv)

   e_p <- format_table_output(e_p)


  size_v <- get_factor_controlled_portfolio_stats(dt,"Size_rank",is_e_weighted = F)
  bm_v <- get_factor_controlled_portfolio_stats(dt,"BM_rank",is_e_weighted = F)
  cl_v <- get_factor_controlled_portfolio_stats(dt,"CP_rank",is_e_weighted = F)
  rev_v <- get_factor_controlled_portfolio_stats(dt,"Rev_rank",is_e_weighted = F)
  mom_v <- get_factor_controlled_portfolio_stats(dt,"MOM_rank",is_e_weighted = F)
  coskew_v <- get_factor_controlled_portfolio_stats(dt,"SSKEW_rank",is_e_weighted = F)
  iskew_v <- get_factor_controlled_portfolio_stats(dt,"ISKEW_rank",is_e_weighted = F)
  illiq_v <- get_factor_controlled_portfolio_stats(dt,"ILLIQ_rank",is_e_weighted = F)
  iv_v <- get_factor_controlled_portfolio_stats(dt,"IV_rank",is_e_weighted = F)


  v_p<-rbind(size_v,bm_v,cl_v,rev_v,mom_v,coskew_v,iskew_v,illiq_v,iv_v)

  v_p <- format_table_output(v_p,is_ew = F)

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


get_factor_controlled_portfolio_stats <- function(dt,factor_rank_name,portfolio=F,is_e_weighted=T){

  factor_ranking<- c("q1","q2","q3")
  max_ranking <- c("q1","q2","q3")

  factor_rank_name = as.name(factor_rank_name)
  q_list_initiated<-F

  alphas <- data.table()
  q_list <- list()
  diff_max <-list()

  for(i in 1:3){

    for(j in 1:3){

      portfolio_returns=paste("q",j,"max",factor_rank_name,factor_ranking[i],sep = "_")
      portfolio_returns_name<-as.name(portfolio_returns)

       if(is_e_weighted){

        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  max_rank == eval(max_ranking[j]),.(portfolio_returns=mean(returns)),
                by=.(yearmon,smb,hml,mkt_prem)]
        colnames(q)[which(colnames(q)=="portfolio_returns")]<-eval(portfolio_returns)


      }else{

        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  max_rank == eval(max_ranking[j]),.(sum_prices=sum(prices)),
                by=.(yearmon,smb,hml,mkt_prem)][,eval(portfolio_returns):=log(sum_prices)-
                                                  shift(log(sum_prices),1L,type="lag")]
        q<-q[,-"sum_prices"]

        }



      #to keep SMB HML and MKT Prem in the first list only
      if(q_list_initiated){
        q<-q[,.(yearmon,eval(portfolio_returns_name))]
        colnames(q)[2]<-portfolio_returns
      }
      if(i == 1 & j== 1){
        q_list_initiated <- T
        }

      q_list[[portfolio_returns]]<-na.omit(q)


      if(j==3){
        q_list<-get_diff_max_protfolios(q_list,i,factor_rank_name)
      }

      }

    }



  data <-as.data.table(Reduce(function(...) merge(...,by=c("yearmon")),q_list))
  data<-get_avg_max_protfolios(data,factor_rank_name)
  data$avg__diff_factor <- (data[,19]- data[,17])

  colnames(data)[which(colnames(data)=="avg__diff_factor")]<-paste("q3","q1","max",factor_rank_name,"avg","diff",
                                                                   sep="_")

  data$year <- year(data$yearmon)
  data<-calculate_alphas(data)

  return(data)


}

#' calculate HMAX - LMAX returns
#' @description It calculates the difference in returns between HMAX and LMAX portfolios.
#'@param q_list  a list with the return of q1, q2, q3 portfolios for
#'a given rank of the controlloed portfolio (e.g. Big or MED)
#'@param factor_rank_name  factor name (e.g. Size_rank)
#'@param data  data table with the return of a double sorted portfolio

#'@return \code{dt} the orginal list bue with an extra column for the difference between HMAX and LMAX
#' @import data.table
#' @export


get_diff_max_protfolios<-function(q_list,i,factor_rank_name){

  factor_ranking<- c("q1","q2","q3")

  p1<-paste("q",1,"max",factor_rank_name,factor_ranking[i],sep = "_")
  p2<-paste("q",2,"max",factor_rank_name,factor_ranking[i],sep = "_")
  p3<-paste("q",3,"max",factor_rank_name,factor_ranking[i],sep = "_")
  avg_max<-paste(factor_rank_name,"q",factor_ranking[i],"avg","max",sep = "_")

  diff_p<-paste("q3","q1","max",factor_rank_name,factor_ranking[i],sep = "_")

  q1<-q_list[[p1]]
  if(i==1){
    q1<-q1[,c(1,ncol(q1)),with=F]
  }
  q2<-q_list[[p2]]
  q3<-q_list[[p3]]

  diff_max <- merge(q1,q2,by="yearmon")
  diff_max <- merge(diff_max,q3,by="yearmon")

  diff_max$diff<-diff_max[,4]- diff_max[,2]


  diff_max<-diff_max[,c(1,5),with=F]
  colnames(diff_max)[2]<-c(diff_p)
  q_list[[diff_p]]<-diff_max
  return(q_list)
}

#' calculate the average returns for a factor
#' @description It calculates the average returns for a factor controlling on MAX (e.g. average return
#' for three portfolios of BIG,MED and SMA size for LMAX). In order to get the average return, it
#' controlls for max but the factor values varis as illiustrated in the previous example.
#'@param data  data table with the return of a double sorted portfolio
#'@param factor_rank_name  factor name (e.g. Size_rank)
#'@param data  data table with the return of a double sorted portfolio

#'@return \code{dt} data table with the alphas for each double sorted portfolio
#' @import data.table
#' @export


get_avg_max_protfolios<-function(data,factor_rank_name){

  factor_ranking <- c("q1","q2","q3")

  for(i in 1:3){

    p1<-as.name(paste("q",i,"max",factor_rank_name,factor_ranking[1],sep = "_"))
    p2<-as.name(paste("q",i,"max",factor_rank_name,factor_ranking[2],sep = "_"))
    p3<-as.name(paste("q",i,"max",factor_rank_name,factor_ranking[3],sep = "_"))

    max_q<-data.frame(yearmon=data[,1],p1=data[,eval(p1)],p2=data[,eval(p2)],p3=data[,eval(p3)])
    max_q$avg<-(max_q$p1 + max_q$p2 + max_q$p3)/3
    max_q<-max_q[,c(1,5)]
    colnames(max_q)[2]<-paste("q",i,"max",factor_rank_name,"avg",sep="_")
   data<-merge(data,max_q,by="yearmon")

  }

  return(data)
}

#' calculate alphas for double sorted portfolios
#' @description It calculates the alpha for the return of double sorted portfolio for each rank. The
#' rank are max ranks and factor rank(e.g small size for LMAX, MMAX and HMAX),  differcne between the highest and lowest rank (HMAX-LMAX) and average return
#'   for each factor controlling for max(e.g. average return for three portfolios of BIG,MED and SMA size for LMAX).

#'@param dt  data table with the return of a double sorted portfolio
#'@return \code{dt} data table with the alphas for each double sorted portfolio
#' @import data.table
#' @importFrom broom tidy
#' @importFrom lmtest coeftest
#' @importFrom sandwich NeweyWest
#' @export
calculate_alphas<-function(dt){

  alpha_init<-F
  for (n in 5:20){
    y=as.name(colnames(dt)[n])
    mod <- lm(eval(y)~hml+smb+mkt_prem,data=dt)
   alpha <-  tidy(coeftest(mod,vcov. = NeweyWest(mod,prewhite = F,adjust = T)))[1,c(2,5)]
    alpha$factor<-colnames(dt)[n]

    rownames(alpha)<-1:nrow(alpha)

    if(alpha_init){
      alphas<-rbind(alphas,alpha)
    }else{
      alphas <- data.frame(alpha)
      alpha_init<-T
    }
  }
  return(alphas)
}



#' format alphas table
#' @description It take an alpha table with 81 columns
#' and change their format to long format to end up with three columns only
#'@param alphas_table  data table with alphas and p-vakues
#'@param is_ew logical, if TRUE, it changes the name of the factor column to EW(equially-weighted)
#'@return \code{dt} data table with alpha and p value  in three columns
#' @import data.table
#'@export
#'

format_table_output<-function(alphas_table,is_ew=T){


  alphas_table[1]<-round(alphas_table[1],3)
  alphas_table[2]<-round(alphas_table[2],2)

  alphas_table$estimate<-ifelse(alphas_table$p.value<=.01,paste( alphas_table$estimate,"***"),
                       ifelse(alphas_table$p.value<=.05,paste(alphas_table$estimate,"**"),
                              ifelse(alphas_table$p.value<=.1,paste(alphas_table$estimate,"*"),
                                     alphas_table$estimate)) )

  alphas_table$factor <- gsub("_", "-", alphas_table$factor)
  alphas_table$factor <- gsub("avg", "AVE", alphas_table$factor)

  alphas_table$factor <- gsub("q1", "Low", alphas_table$factor)
  alphas_table$factor <- gsub("q-1", "Low", alphas_table$factor)

  alphas_table$factor <- gsub("q2", "Med", alphas_table$factor)
  alphas_table$factor <- gsub("q-2", "Med", alphas_table$factor)

  alphas_table$factor <- gsub("q3", "High", alphas_table$factor)
  alphas_table$factor <- gsub("q-3", "High", alphas_table$factor)

  alphas_table$factor <- gsub("-rank", "", alphas_table$factor)
  alphas_table$factor <- gsub("max","MAX",alphas_table$factor)
  alphas_table$p.value <- paste("(",alphas_table$p.value,")")

  alphas_table<-separate(alphas_table,factor,c("max","factor2"),sep="-MAX-")
  alphas_table<-data.table(t(alphas_table))

  i<-seq(1,144,4)

  for (j in i){

    substed_columns <- alphas_table[,j:(j+3)]
    factor <- as.character(substed_columns[4,1])


    colnames(substed_columns)<-as.character(as.matrix(substed_columns[3,]))
    substed_columns<-substed_columns[-4,]
    substed_columns$factor<-as.character(as.matrix(substed_columns[3,]))[1]
    substed_columns<-substed_columns[-3,]

    substed_columns$factor<-factor

    if(j==1){
      formated_table<-data.table(substed_columns)
    }else{
      formated_table<-rbind(formated_table,substed_columns)
    }
  }
  formated_table$factor[duplicated(formated_table$factor)] <- ""


  colnames(formated_table)[1:4]<-c( "L-MAX","M-MAX","H-MAX",'HMAX-LMAX')
  if(is_ew){
    colnames(formated_table)[5] <- "EW"
  }else{
    colnames(formated_table)[5] <- "VW"

  }
  formated_table<-formated_table[,c(5,1:4)]
  return(formated_table)

}
