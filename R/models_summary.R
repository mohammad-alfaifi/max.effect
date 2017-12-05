

#' get single sort of max , min or IV by country
#' @description results of single sort analysisIt of MAX , Min or IV.
#'@param country  list of countries
#'@param num_cuts numer of portfolios in each category
#'@param factory_type max, min or iv
 #'@return \code{dt} with single sort analysis result
#' @import data.table
#'@export
#'
get_max_or_min_or_iv_by_country<-function(countries,num_cuts=3,factor_type="max"){

  countries_factor<-do.call(rbind,lapply(countries,function(x){

    dt<-readRDS(paste("data/dt_",x[1],".rds",sep = ""))
    if(factor_type=="max"){
      factor<-get_max_ew_and_vw_portfolio_returns(dt,n=1,num_cuts,max_type = "max")

    }else if(factor_type=="min"){
      factor<-get_max_ew_and_vw_portfolio_returns(dt,n=1,num_cuts,max_type = "min")
      names(factor) <- gsub(x = names(factor), pattern = "Max", replacement = "Min")

    }else{
      factor<-get_iv_ew_and_vw_portfolio_returns(dt,num_cuts)

    }
    factor$country<-""
    factor$country[1]<-x
    factor$country[3]<-x


    Country<-factor$country
    factor<-factor[,-"country"]
    factor<-cbind(Country,factor)

    factor

  }))

  return(countries_factor)
}





#' get double sort of max m min or IV by country
#' @description results of double sort analysisIt of MAX , Min or IV.
#'@param country  list of countries
#'@param num_cuts numer of portfolios in each category
#'#'@param factory_type max, min or iv
#'@return \code{dt} with double sort analysis result
#' @import data.table
#'@export
#'

get_double_sorted_alpha_by_country<-function(countries,num_cuts=3,factor_type="max"){

  print("factor type=")
  print(factor_type)
  countries_factor<-do.call(rbind,lapply(countries,function(x){

    print(num_cuts)
    dt<-readRDS(paste("data/dt_",x[1],".rds",sep = ""))


    alphas <- calculate_double_sorted_alpha(dt,num_cuts=num_cuts,n=1,max_type = factor_type)
    alphas<-cbind(Country=x,alphas)

    alphas<-alphas[EW %like% "-AVE"]
    alphas<-alphas[duplicated(Country),Country:=""]
    alphas
  }))

  return(countries_factor)
}

#' get fama macbeth single factor by country
#' @description results of fama macbeth  analysisIt o
#'@param country  list of countries
#'@return \code{dt} with univariate fama macbeth analysis result
#' @import data.table
#'@export
#'

get_fama_macbeth_uni_by_country<-function(countries){

  countries_uni_fama_macbeth<-do.call(rbind,lapply(countries,function(x){

    dt_cleaned<-readRDS(paste("data/dtcleaned_",x[1],".rds",sep = ""))
    fama_macbeth_uni<-get_fama_macbeth_univariate_values(dt_cleaned)


    fama_macbeth_uni$Country<-x


    setcolorder(fama_macbeth_uni, c(13,1:12))

  }))

  return(countries_uni_fama_macbeth)
}
#' get fama macbeth bivariate factor by country
#' @description results of fama macbeth  analysisIt o
#'@param country  list of countries
#'@return \code{dt} with bivariate fama macbeth analysis result
#' @import data.table
#'@export
#'
get_fama_macbeth_bi_by_country<-function(countries){

  countries_bi_fama_macbeth<-do.call(rbind,lapply(countries,function(x){

    dt_cleaned<-readRDS(paste("data/dtcleaned_",x[1],".rds",sep = ""))
    fama_macbeth_bi<-get_fama_macbeth_bivariate_values(dt_cleaned)


    fama_macbeth_bi$Country<-x
    fama_macbeth_bi<-fama_macbeth_bi[duplicated(Country),Country:=""]

    setcolorder(fama_macbeth_bi, c(13,1:12))

  }))

  return(countries_bi_fama_macbeth)
}
