#' get_fama_macbeth_univariate_values
#' @description It takes a data table with ten factors and returns
#' and call calculate univeriate avg function to get the beta average
#' of fama macbeth regression for each factor then group all
#' factors in one data table
#'@param dt  data table with stock returns and ten factorS (e.g momentum,size,IV,max)
#'@return \code{dt} data table with the average beta for each factor and p-value
#' @import data.table
#'@export
#'
get_fama_macbeth_univariate_values <- function(dt){

  max <- calculate_univariate_avg(dt,"firms_max","MAX")
  size <- calculate_univariate_avg(dt,"lag_mv","Size")
  bm <- calculate_univariate_avg(dt,"lag_bm","BM")
  momentum <- calculate_univariate_avg(dt,"roc","Momentum")
  rev <- calculate_univariate_avg(dt,"rev","Rev")
  cp <- calculate_univariate_avg(dt,"lag_prices","CP")
  sskew <- calculate_univariate_avg(dt,"coskew","SSKEW")
  iskew <- calculate_univariate_avg(dt,"iskew","ISKEW")
  illiq <- calculate_univariate_avg(dt,"illiq_ratio","ILLIQ")
  iv <- calculate_univariate_avg(dt,"IV","IV")


  univariate_famamcbeth=cbind(max,size,bm,momentum,rev,cp,sskew,iskew,illiq,iv)

  return(univariate_famamcbeth)

}


#' get_fama_macbeth_bivariate_values
#' @description It takes a data table with ten factors and returns
#' and call calculate biveriate avg function to get the beta average
#' of fama macbeth regression for each two factors and then group all
#' factors in one data table
#'@param dt  data table with stock returns and ten factorS (e.g momentum,size,IV,max)
#'@return \code{dt} data table with the average beta for each pair factors and
#'their p-value
#' @import data.table
#'@export
#'
get_fama_macbeth_bivariate_values <- function(dt){

  size <- calculate_bivariate_avg(dt,c("firms_max","lag_mv"),c("MAX","Size"))
  bm <- calculate_bivariate_avg(dt,c("firms_max","lag_bm"),c("MAX","BM"))
  momentum <- calculate_bivariate_avg(dt,c("firms_max","roc"),c("MAX","Momentum"))
  rev <- calculate_bivariate_avg(dt,c("firms_max","rev"),c("MAX","Rev"))
  cp <- calculate_bivariate_avg(dt,c("firms_max","lag_prices"),c("MAX","CP"))
  sskew <- calculate_bivariate_avg(dt,c("firms_max","coskew"),c("MAX","SSKEW"))
  iskew <- calculate_bivariate_avg(dt,c("firms_max","iskew"),c("MAX","ISKEW"))
  illiq <- calculate_bivariate_avg(dt,c("firms_max","illiq_ratio"),c("MAX","ILLIQ"))
  iv <- calculate_bivariate_avg(dt,c("firms_max","IV"),c("MAX","IV"))

  bivariate_famamcbeth=cbind(size,bm,momentum,rev,cp,sskew,iskew,illiq,iv)

  values_table <- data.frame(matrix(nrow =18,ncol = 18 ))
  values_table[,]<-""
  for (i in 1:17){

    if(i%%2!=0){

        print(i)
        values_table[i,1]<-bivariate_famamcbeth[1,i]
        values_table[i+1,1]<-bivariate_famamcbeth[2,i]
        if(i==1){
          #to name MAX
          colnames(values_table)[i] <- colnames(bivariate_famamcbeth)[i]
          #to Name second colume
          colnames(values_table)[i+1] <- colnames(bivariate_famamcbeth)[i+1]

          values_table[i,2]<-bivariate_famamcbeth[1,i+1]
          values_table[i+1,2]<-bivariate_famamcbeth[2,i+1]
        }else {
          values_table[i,i]<-bivariate_famamcbeth[1,i+1]
          values_table[i+1,i]<-bivariate_famamcbeth[2,i+1]
          colnames(values_table)[i] <- colnames(bivariate_famamcbeth)[i+1]
        }

    }

  }

  even_col <- seq(4,18,by=2)
  values_table<-subset(values_table,select = -c(even_col))

  bivariate_all_vars <- calculate_bivariate_avg_for_all_vars(dt)

  values_table <- rbind(values_table,bivariate_all_vars)
  return(values_table)

}


#' calculate_bivariate_avg_for_all_vars
#' @description It takes a data table with ten factors and returns
#' and run monthly regression for each ten factors as indpedent and th
#' return is the dependent and take the beta average for each factor
#' and display them with the p-value

#'@param dt  data table with stock returns and ten factorS (e.g momentum,size,IV,max)
#'@return \code{dt} data table with the average beta for all factors and
#'their p-value
#' @import data.table
#'@export
#'

calculate_bivariate_avg_for_all_vars<- function(dt,iv=FALSE){

  #to be used for naming factors during the grouping process
  reg_vars_names <- c("yearmon","returns","MAX","Size",
                "BM","Momentum","Rev","CP","SSKEW",
                "ISKEW","ILLIQ","IV")

  #the loop is just to get the beta coefficents of each factor and takes
  #its average and p-value to ease the grouping into one data frame
  for (i in 1:10){


    slope <- dt[,.(betas=lm(returns~firms_max + lag_mv + lag_bm +
                              roc + rev + lag_prices + coskew + iskew +
                              illiq_ratio + IV)$coefficients[i+1]),
                by=.(yearmon)][,.(factor= as.numeric(roubst_se(lm(betas~1))))]
    slope_avg <- data.frame(round(slope$factor,5))
    colnames(slope_avg)<- reg_vars_names[i+2]

    slope_avg[1,1]<-ifelse(slope_avg[2,1]<=.01,paste(slope_avg[1,1],"***"),
                           ifelse(slope_avg[2,1]<=.05,paste(slope_avg[1,1],"**"),
                                  ifelse(slope_avg[2,1]<=.1,paste(slope_avg[1,1],"*"),
                                         slope_avg[1,1])) )

    slope_avg[2,]<-paste("(",slope_avg[2,],")")

    if(i==1){
      slope_avgs <- data.frame(slope_avg)
    }else{
      slope_avgs <- cbind(slope_avgs,slope_avg)
    }
  }
  rownames(slope_avgs) <- c("slope","p-value")

  slope_avgs <- as.data.table(slope_avgs)
  return(slope_avgs)

}


#' calculate_bivariate_avg
#' @description It takes a data table with ten factors and returns
#' and run monthly regression for pair of factors on the
#' return  and take the beta average for each factor
#' and display it with the p-value

#'@param dt  data table with stock returns and ten factorS (e.g momentum,size,IV,max)
#'@return \code{dt} data table with the average beta for each pair factors and
#'their p-value
#' @import data.table
#'@export
#'

calculate_bivariate_avg <- function(dt,factors_names,names_displayed){

  factor_1 <- as.name(factors_names[1])
  factor_2 <- as.name(factors_names[2])


  for (i in 1:2){

     slope <- dt[,.(betas=lm(returns~eval(factor_1)+
                               eval(factor_2))$coefficients[i+1]),
                  by=.(yearmon)][,.(factor=as.numeric(roubst_se(lm(betas~1))))]

     slope_avg <- data.frame(round(slope$factor,5))

     colnames(slope_avg)<- names_displayed[i]


     slope_avg[1,1]<-ifelse(slope_avg[2,1]<=.01,paste(slope_avg[1,1],"***"),
                       ifelse(slope_avg[2,1]<=.05,paste(slope_avg[1,1],"**"),
                       ifelse(slope_avg[2,1]<=.1,paste(slope_avg[1,1],"*"),
                              slope_avg[1,1])) )
     slope_avg[2,1]<-paste("(",slope_avg[2,1],")")

     if(i==1){
       slope_avgs <- data.frame(slope_avg)
     }else{
       slope_avgs <- cbind(slope_avgs,slope_avg)
     }

  }
  rownames(slope_avgs) <- c("slope","p-value")
  colnames(slope_avgs)<- names_displayed

  return(slope_avgs)

}


#' calculate_univariate_avg
#' @description It takes a data table with ten factors and returns
#' and run monthly regression for each  factors of the ten factors as
#' the indpedent variable and the#' return is the dependent and take
#' the beta average for each factor #' and display them with the p-value

#'@param dt  data table with stock returns and ten factorS (e.g momentum,size,IV,max)
#'@return \code{dt} data table with the average beta for each  factors and
#'its p-value
#' @import data.table
#'@export
#'


calculate_univariate_avg <- function(dt,factor_name,name_displayed){

  factor_name <- as.name(factor_name)

  slope <- dt[,.(betas=lm(returns~eval(factor_name))$coefficients[2]),
              by=.(yearmon)][,.(factor=as.numeric(roubst_se(lm(betas~1))))]
  slope_avg <- data.frame(round(slope$factor,5))
  colnames(slope_avg)<- name_displayed

  slope_avg[1,1]<-ifelse(slope_avg[2,1]<=.01,paste(slope_avg[1,1],"***"),
                         ifelse(slope_avg[2,1]<=.05,paste(slope_avg[1,1],"**"),
                                ifelse(slope_avg[2,1]<=.1,paste(slope_avg[1,1],"*"),
                                       slope_avg[1,1])) )
  rownames(slope_avg) <- c("slope","p-value")
  slope_avg[2,1]<-paste("(",slope_avg[2,1],")")
  return(slope_avg)

}

