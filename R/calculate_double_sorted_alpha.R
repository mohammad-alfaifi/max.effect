
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
    size <- calculate_portfolios_count_for_double_sorted_alpha(dt=dt,factor_rank_name = "Size_rank",num_cuts=num_cuts)
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
#'@return \code{dt} data table with two columnls:one for date(year or yearmon) and the other
#'for the number of portfolios crospoding to the date
#' @import data.table
#' @importFrom zoo yearmon
#'@export
#'

calculate_portfolios_count_for_double_sorted_alpha<- function(dt,factor_rank_name,is_ew=T,tertile=T){




  #have q1 q2 cut done dynimically
  q<-rep("q",num_cuts)
  num<-1:num_cuts
  factor_ranking <- paste(q,num,sep="")
  max_or_iv_ranking <- paste(q,num,sep="")


  factor_rank_name = as.name(factor_rank_name)

  if(is_max_p){
    var_name<-"max"
    var_rank<-as.name("max_rank")
  }else{
    var_name<-"IV"
    var_rank<-as.name("IV_rank")

  }

  data<-data.frame()
  counting_list<-list()


  for (i in 1:num_cuts){

    for(j in 1:num_cuts){
      portfolio_returns=paste("q",j,var_name,factor_rank_name,factor_ranking[i],sep = "_")
      portfolio_returns_name<-as.name(portfolio_returns)


      if(is_ew){
        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  eval(var_rank) == eval(max_or_iv_ranking[j]),.(portfolio_returns=mean(returns)),
                by=.(yearmon,smb,hml,mkt_prem)]
        colnames(q)[which(colnames(q)=="portfolio_returns")]<-eval(portfolio_returns)
      }else{
        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  eval(var_rank) == eval(max_or_iv_ranking[j]),.(sum_mvs=sum(MV)),
                by=.(yearmon,smb,hml,mkt_prem)][,eval(portfolio_returns):=log(sum_mvs)-
                                                  shift(log(sum_mvs),1L,type="lag")]
        q<-q[,-"sum_mvs"]
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




calculate_double_sorted_alpha <- function(dt,num_cuts,n=1,max_type="max"){

  # a file that has monthly return with ff-3 model
  temp_file <- "data/temp_dt_monthly.rds"
  #remove to avoid any possible duplication

  if (file.exists(temp_file)) file.remove(temp_file)

  if(max_type=="max"|max_type=="min"){
    is_max_p<-T

  }else{
    is_max_p<-F

  }

  #######################################################
  ######## #########calculate controlling variables ######################
  #######################################################

  size <- calculate_monthly_size_rank(dt,num_cuts,double_sorted  = T)
  bm <- calculate_monthly_bm_rank(dt,num_cuts,double_sorted = T)
  cl <- calculate_monthly_cl_prices_rank(dt,num_cuts)
  rev <- calculate_monthly_reversal(dt,num_cuts,double_sorted=T)
  mom <- calculate_momentum_rank(dt,portfolio_only = T,num_cuts)
  illiq <- calculate_monthly_illiq_rank(dt,num_cuts,double_sorted=T)
  skewness <- calculate_skewness_rank(dt,num_cuts,double_sorted = T)
  coskew<-skewness[,.(yearmon,firms,coskew,SSKEW_rank)]
  iskew<-skewness[,.(yearmon,firms,iskew,ISKEW_rank)]
  iv <- calculate_iv_rank(dt,num_cuts,rank_only=T)
  if(max_type=="max" | max_type=="min"){
    max<-calculate_max_rank(dt,n=1,num_cuts,rank_only=T,max_type = max_type)

  }else{
    max<-calculate_max_rank(dt,n=1,num_cuts,rank_only=T,max_type = max_type)

  }


  #######################################################
  #######################################################


  size_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=size,num_cuts,factor_rank_name="Size_rank",is_e_weighted = T,max_type)
  bm_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=bm,num_cuts,factor_rank_name="BM_rank",is_e_weighted = T,max_type)
  cl_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=cl,num_cuts,factor_rank_name="CP_rank",is_e_weighted = T,max_type)
  rev_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=rev,num_cuts,factor_rank_name="Rev_rank",is_e_weighted = T,max_type)
  mom_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=mom,num_cuts,factor_rank_name="MOM_rank",is_e_weighted = T,max_type)
  illiq_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=illiq,num_cuts,factor_rank_name="ILLIQ_rank",is_e_weighted = T,max_type)
  coskew_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=coskew,num_cuts,factor_rank_name="SSKEW_rank",is_e_weighted = T,max_type)
  iskew_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=iskew,num_cuts,factor_rank_name="ISKEW_rank",is_e_weighted = T,max_type)
  if(max_type=="max" | max_type=="min"){
    iv_or_max_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=iv,num_cuts,factor_rank_name="IV_rank",is_e_weighted = T,max_type)

  }else{
    iv_or_max_e<- construct_double_sorted_portfolios(dt=dt,factor_dt=max,num_cuts,factor_rank_name="max_rank",is_e_weighted = T,max_type)

  }


  e_p<-rbind(size_e,bm_e,cl_e,rev_e,mom_e,coskew_e,iskew_e,illiq_e,iv_or_max_e)

   e_p <- format_table_output(e_p,is_ew = T,num_cuts = num_cuts,is_max_p)


   #######################################################
   #######calculate value wieghted portfolio #############
   #######################################################

   size_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=size,num_cuts,factor_rank_name="Size_rank",is_e_weighted = F,max_type)
   bm_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=bm,num_cuts,factor_rank_name="BM_rank",is_e_weighted = F,max_type)
   cl_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=cl,num_cuts,factor_rank_name="CP_rank",is_e_weighted = F,max_type)
   rev_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=rev,num_cuts,factor_rank_name="Rev_rank",is_e_weighted = F,max_type)
   mom_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=mom,num_cuts,factor_rank_name="MOM_rank",is_e_weighted = F,max_type)
   illiq_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=illiq,num_cuts,factor_rank_name="ILLIQ_rank",is_e_weighted = F,max_type)
   coskew_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=coskew,num_cuts,factor_rank_name="SSKEW_rank",is_e_weighted = F,max_type)
   iskew_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=iskew,num_cuts,factor_rank_name="ISKEW_rank",is_e_weighted = F,max_type)
   if(max_type=="max" | max_type=="min"){
     iv_or_max_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=iv,num_cuts,factor_rank_name="IV_rank",is_e_weighted = F,max_type)

   }else{
     iv_or_max_v<- construct_double_sorted_portfolios(dt=dt,factor_dt=max,num_cuts,factor_rank_name="max_rank",is_e_weighted = F,max_type)

   }




  v_p<-rbind(size_v,bm_v,cl_v,rev_v,mom_v,coskew_v,iskew_v,illiq_v,iv_or_max_v)

  v_p <- format_table_output(v_p,num_cuts=num_cuts,is_ew = F,is_max_p)

  alphas_double_sorted <-as.data.table(cbind(e_p,v_p))

  return(alphas_double_sorted)
}



#' prepare_double_sorted_portfolios
#' @description It calculates the monthly return and construct the ff-3 model
#' to be ready to merge with controled variable(e.g size).
#'@param dt  data table with daily prices,volume,mv and bm
#'@return \code{dt} data table with monthly return and three factors model
#' @import data.table
#'@export
#'

prepare_double_sorted_portfolios <-function(dt){

  dt_monthly <- dt[,.SD[.N],by=.(firms,yearmon)][,-c("dates","RF_m","BM","Vol","RF"),with=F]
  dt_monthly[,returns:=log(prices)-shift(log(prices),1L,type="lag"),by=.(firms)]
  dt_monthly<-na.omit(dt_monthly)
  ff3_monthly <- calculate_ff3(dt,monthly = T)
  dt_monthly<-merge(dt_monthly,ff3_monthly,by="yearmon")

  saveRDS(dt_monthly,"data/temp_dt_monthly.rds")

  return(dt_monthly)


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

construct_double_sorted_portfolios <-function(dt,factor_dt,num_cuts,factor_rank_name,is_e_weighted=T,max_type="max"){

  if(max_type=="max" | max_type=="min"){
    is_max_p = T
  }else{
    is_max_p = F
  }
  # to be used in calculating the max or iv of each factor rank
  q<-rep("q",num_cuts)
  num<-1:num_cuts
  factor_ranking <- paste(q,num,sep="")

  # a file for ff3 and monthly return for efficency
  temp_file <- "data/temp_dt_monthly.rds"

  if(file.exists(temp_file)){
    dt_monthly<-as.data.table(readRDS(temp_file))
  }else{
    dt_monthly<-prepare_double_sorted_portfolios(dt)
  }


  #get the factor rank with ff3 and monthly returns
  factor_dt<-merge(factor_dt,dt,by=c("yearmon","firms"))

  #construct the double sort e.g(q1 of size with q1...qn of max)
  for (i in 1:num_cuts){

    single_sort<-factor_dt[eval(as.name(factor_rank_name))==factor_ranking[i]]
    #to double sort
    if(max_type=="max"){
      max_or_iv<-calculate_max_rank(single_sort,n=1,num_cuts=num_cuts,max_type=max_type)

    }else if(max_type=="min"){
      max_or_iv<-calculate_max_rank(single_sort,n=1,num_cuts=num_cuts,max_type=max_type)
    }else{
      max_or_iv<-calculate_iv_rank(single_sort,num_cuts,double_sorted = T)
    }

    if (i == 1){
      double_sorted_p<-as.data.table(max_or_iv)
    }else{
      double_sorted_p<-rbind(double_sorted_p,max_or_iv)
    }
  }

  #note double sorted has the values of max or iv and factor rank updates as lag by adding a month to yearmon
  if(is_max_p){
    double_sorted_p<-double_sorted_p[,-c("BM","Vol","dates","RF","RF_m","daily_returns","years","prices","MV"),with=F]

  }else{
    double_sorted_p<-double_sorted_p[,-c("BM","Vol","dates","RF","RF_m","years","prices","MV"),with=F]

  }

  # merge monthly values with double sorted portfolio for regreesion
  factor_dt<-merge(double_sorted_p,dt_monthly,by=c("yearmon","firms"))

  factor_dt <- get_factor_controlled_portfolio_stats(dt=factor_dt,factor_rank_name = factor_rank_name,num_cuts=num_cuts,is_e_weighted,is_max_p)

  return(factor_dt)

}


#' get the alpha and p-values for double sorted portfolios
#' @description It extracts the portfolio which corresponds to the factor_rank_name
#' from the received data table, then it calculates the portfolio's return and
#' regresses it against fama-french three factors
#'@param dt  data table with double sorted stocks
#'@param factor_rank_name  the name of the factor that we want to double sort with max or iv
#'@param factor_ranking  the ranks of the factor (levels)
#'@param is_e_weighted  logical, if False calculates the return of value weighted portfolios

#'@return \code{dt} data table with alpha and p value of nine double sorted portfolios
#' @import data.table
#' @importFrom zoo yearmon
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join


#'
#'@export
#'


get_factor_controlled_portfolio_stats <- function(dt,factor_rank_name,is_e_weighted=T,num_cuts,is_max_p=T){


  print(factor_rank_name)
  #have q1 q2 cut done dynimically
  q<-rep("q",num_cuts)
  num<-1:num_cuts
  factor_ranking <- paste(q,num,sep="")
  max_or_iv_ranking <- paste(q,num,sep="")

  if(is_max_p){
    var_name<-"max"
    var_rank<-as.name("max_rank")
  }else{
    var_name<-"IV"
    var_rank<-as.name("IV_rank")
  }

  factor_rank_name = as.name(factor_rank_name)
  q_list_initiated<-F

  alphas <- data.table()
  q_list <- list()
  diff_max_or_iv <-list()


  for(i in 1:num_cuts){

    for(j in 1:num_cuts){

      portfolio_returns=paste(paste("q",j,sep=""),var_name,factor_rank_name,factor_ranking[i],sep = "_")
      portfolio_returns_name<-as.name(portfolio_returns)

       if(is_e_weighted){

        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  eval(var_rank) == eval(max_or_iv_ranking[j]),.(portfolio_returns=mean(returns)),
                by=.(yearmon,smb,hml,mkt_prem,RF_m)]
        q$portfolio_returns <- q$portfolio_returns - q$RF_m
        colnames(q)[which(colnames(q)=="portfolio_returns")]<-eval(portfolio_returns)


      }else{

        q <- dt[eval(factor_rank_name)== eval(factor_ranking[i]) &
                  eval(var_rank) == eval(max_or_iv_ranking[j]),.(sum_mvs=sum(MV)),
                by=.(yearmon,smb,hml,mkt_prem,RF_m)][,eval(portfolio_returns):=(log(sum_mvs)-
                                                  shift(log(sum_mvs),1L,type="lag"))-RF_m]
        q<-q[,-"sum_mvs"]

        }



        q<-q[,.(yearmon,eval(portfolio_returns_name))]
      colnames(q)[2]<-portfolio_returns

      q_list[[portfolio_returns]]<-na.omit(q)



      if(j==num_cuts){
        q_list<-get_diff_max_or_iv_protfolios(q_list,factor_rank_name,factor_ranking[i],num_cuts,is_max_p)
      }

      }

    }

  data <-Reduce(function(...) merge(..., by="yearmon", all=T), q_list)

  ff3_monthly<-dt[,.(yearmon,smb,hml,mkt_prem)][!duplicated(yearmon)]

  data<-merge(ff3_monthly,data,by="yearmon")

  data<-get_avg_max_or_iv_protfolios(data,factor_rank_name,num_cuts,is_max_p)

  data<-calculate_alphas(data,num_cuts)



  return(data)


}

#' calculate HMAX - LMAX returns
#' @description It calculates the difference in returns between HMAX and LMAX portfolios.
#'@param q_list  a list with the return of q1, q2, q3 portfolios for
#'a given rank of the controlloed portfolio (e.g. Big or MED)
#'@param factor_rank_name  factor name (e.g. Size_rank)
#'@param factor_rank  factor rank (e.g. Size_rank=q1)

#'@param data  data table with the return of a double sorted portfolio

#'@return \code{dt} the orginal list bue with an extra column for the difference between HMAX and LMAX
#' @import data.table
#' @export


get_diff_max_or_iv_protfolios<-function(q_list,factor_rank_name,factor_rank,num_cuts,is_max_p=T){


  if(is_max_p){
    var_name<-"max"
  }else{
    var_name<-"IV"
  }
  low<-paste("q1",var_name,factor_rank_name,factor_rank,sep = "_")
  high<-paste(paste("q",num_cuts,sep=""),var_name,factor_rank_name,factor_rank,sep = "_")

  #to name the difference portfolio at the end
  diff_p<-paste(paste("q",num_cuts,sep=""),"q1",var_name,
                factor_rank_name,factor_rank,sep = "_")

  #reterive high and low max or iv portfolios
  high_max_or_iv<-q_list[[high]]
  low_max_or_iv<-q_list[[low]]


  # calculate the different between highmax(highiv) and low max(lowiv)
  diff_max_or_iv <- merge(low_max_or_iv,high_max_or_iv,by="yearmon")
  diff_max_or_iv$diff<-diff_max_or_iv[[high]]- diff_max_or_iv[[low]]

  #format result
  diff_max_or_iv<-diff_max_or_iv[,.(yearmon,diff)]
  colnames(diff_max_or_iv)[2]<-c(diff_p)
  #add to the list of portfolios
  q_list[[diff_p]]<-diff_max_or_iv

  return(q_list)
}

#' calculate the average returns for a factor
#' @description It calculates the average returns for a factor controlling on MAX (e.g. average return
#' for three portfolios of BIG,MED and SMA size for LMAX). In order to get the average return, it
#' controlls for max or iv but the factor values varis as illiustrated in the previous example.
#'@param data  data table with the return of a double sorted portfolio
#'@param factor_rank_name  factor name (e.g. Size_rank)
#'@param data  data table with the return of a double sorted portfolio

#'@return \code{dt} data table with the alphas for each double sorted portfolio
#' @import data.table
#' @importFrom zoo as.yearmon

#' @export


get_avg_max_or_iv_protfolios<-function(data,factor_rank_name,num_cuts,is_max_p=T){

  # to iterrate  max or iv portfolios
  k <- 1:num_cuts
  # iterate factor portfolios
  j <- rep(k,num_cuts)
  q<-rep("q",num_cuts)
  num<-1:num_cuts
  factor_ranking <- paste(q,num,sep="")
  max_or_iv_ranking <- paste(q,num,sep="")

  if(is_max_p){
    var_name<-"max"
  }else{
    var_name<-"IV"
  }

  data<-as.data.frame(data)
  for (i in 1:num_cuts){

    #to get name of max or iv portfolio (e,g q1)
    #with different factor rank(e.g q1,q2,q3)

    portfolio_name<-paste(paste("q",k[i],sep=""),var_name,factor_rank_name,
                                  factor_ranking[1:num_cuts],sep = "_")
    portfolio_name<-c(portfolio_name,'yearmon')

    #obtain portfolio with the names above to calculate their mean
    max_or_iv_q<-as.data.table(data[portfolio_name])
    max_or_iv_q<-max_or_iv_q[, .(Mean = rowMeans(.SD,na.rm = T)), by = yearmon]
    #format name to be added to received data frame
    colnames(max_or_iv_q)[2]<-paste(paste("q",k[i],sep=""),var_name,factor_rank_name,"avg",sep="_")
    data<-merge(data,max_or_iv_q,by="yearmon")

  }

  low<-paste("q1",var_name,factor_rank_name,"avg",sep="_")
  high<-paste(paste("q",num_cuts,sep=""),var_name,factor_rank_name,"avg",sep="_")


  data$avg_diff_factor <- data[[high]]-data[[low]]
  colnames(data)[which(colnames(data)=="avg_diff_factor")]<-paste(paste("q",num_cuts,sep="")
                                                                  ,"q1",var_name,factor_rank_name,"avg","diff",
                                                                   sep="_")
  data<-as.data.table(data)

  return(data)
}

#' calculate alphas for double sorted portfolios
#' @description It calculates the alpha for the return of double sorted portfolio for each rank. The
#' rank are max or iv ranks and factor rank(e.g small size for LMAX, MMAX and HMAX),  differcne between the highest and lowest rank (HMAX-LMAX) and average return
#'   for each factor controlling for max or iv(e.g. average return for three portfolios of BIG,MED and SMA size for LMAX).

#'@param dt  data table with the return of a double sorted portfolio
#'@return \code{dt} data table with the alphas for each double sorted portfolio
#' @import data.table
#' @export
calculate_alphas<-function(dt,num_cuts){

  alpha_init<-F
  for (n in 5:length(dt)){
    y=as.name(colnames(dt)[n])
    mod <- lm(eval(y)~hml+smb+mkt_prem,data=dt,na.action = na.omit)
   alpha <- roubst_se(mod)
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

format_table_output<-function(alphas_table,is_ew=T,num_cuts,is_max_p=T){

  if(is_max_p){
    var_name<-"max"
    replacing_name<-"MAX"
  }else{
    var_name<-"IV"
    replacing_name<-"IV"

  }

  alphas_table[1]<-round(alphas_table[1],3)
  alphas_table[2]<-round(alphas_table[2],2)

  alphas_table$estimate<-ifelse(alphas_table$p.value<=.01,paste( alphas_table$estimate,"***"),
                       ifelse(alphas_table$p.value<=.05,paste(alphas_table$estimate,"**"),
                              ifelse(alphas_table$p.value<=.1,paste(alphas_table$estimate,"*"),
                                     alphas_table$estimate)) )

  alphas_table$factor <- gsub("_", "-", alphas_table$factor)
  alphas_table$factor <- gsub("avg", "AVE", alphas_table$factor)

  if(num_cuts==3){
    alphas_table$factor <- gsub("q1", "Low", alphas_table$factor)
    alphas_table$factor <- gsub("q-1", "Low", alphas_table$factor)

    alphas_table$factor <- gsub("q2", "Med", alphas_table$factor)
    alphas_table$factor <- gsub("q-2", "Med", alphas_table$factor)

    alphas_table$factor <- gsub("q3", "High", alphas_table$factor)
    alphas_table$factor <- gsub("q-3", "High", alphas_table$factor)
  }else{
    alphas_table$factor <- gsub("q", "", alphas_table$factor)

  }


  alphas_table$factor <- gsub("-rank", "", alphas_table$factor)
  alphas_table$factor <- gsub(var_name,replacing_name,alphas_table$factor)
  alphas_table$p.value <- paste("(",alphas_table$p.value,")")

  if(is_max_p){
    alphas_table<-separate(alphas_table,factor,c(var_name,"factor2"),sep="-MAX-")

  }else{
    alphas_table<-separate(alphas_table,factor,c(var_name,"factor2"),sep="-IV-")

  }
  alphas_table<-data.table(t(alphas_table))

  #to be used in flipping factor column to the right at the end
  cuts_plus_1<-num_cuts+1
  cuts_plus_2<-num_cuts+2

  # to subset each num_cuts +1 number of portfolio(cuts +1 because of high minus low)
  i<-seq(from=1,to=ncol(alphas_table),by=cuts_plus_1)

  for (j in i){

    substed_columns <- alphas_table[,j:(j+num_cuts)]
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


  if(num_cuts==3){
    if(is_max_p){
      colnames(formated_table)[1:cuts_plus_1]<-c( "L-MAX","M-MAX","H-MAX",'HMAX-LMAX')

    }else{
      colnames(formated_table)[1:cuts_plus_1]<-c( "L-IV","M-IV","H-IV",'HIV-LIV')

    }

  }
  if(is_ew){
    colnames(formated_table)[cuts_plus_2] <- "EW"
  }else{
    colnames(formated_table)[cuts_plus_2] <- "VW"

  }
  #flipping factor column to the right
  formated_table<-formated_table[,c(cuts_plus_2,1:cuts_plus_1),with=F]
  return(formated_table)

}
