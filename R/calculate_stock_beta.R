
calculate_stock_beta<-function(dt){

  mkt_prem <- calculate_ff3(dt,monthly = F)[,.(dates,mkt_prem)]
  mkt_prem<- mkt_prem[,mkt_prem_lag:=shift(mkt_prem,1L, type="lag")]
  mkt_prem<- mkt_prem[,mkt_prem_lead:=shift(mkt_prem,1L, type="lead")]

  mkt_prem<-na.omit(mkt_prem)

  st_returns<-calculate_daily_returns(dt)[,.(dates,yearmon,firms,excess_returns=daily_returns-RF)]

  st_returns<-merge(mkt_prem,st_returns,by="dates")

  Beta<-st_returns[,.(Beta=sum(unlist(list(lm(excess_returns~mkt_prem+mkt_prem_lag+mkt_prem_lead)$coefficients[2:4])))),by=.(yearmon,firms)]
  Beta$yearmon <- Beta$yearmon+(1/12)

  return(Beta)
}
