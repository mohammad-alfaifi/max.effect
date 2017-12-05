#' calculates roubst standard error
#' @description calculates roubst standard error and return coeficcents and P value

#'@return \code{broom} with coefficent and p-value of the model
#' @importFrom broom tidy
#' @importFrom lmtest coeftest
#' @importFrom sandwich NeweyWest
#'@export
#'
roubst_se<-function(mod){

  roubst_mod<-tidy(coeftest(mod,vcov. = NeweyWest(mod,lag=0,prewhite = F)))[1,c(2,5)]

  return(roubst_mod)

}
