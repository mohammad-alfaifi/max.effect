

get_sama_data <- function(){
  
  sama_tbill_urls <- c("http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?&&p_SAMABillYear=8_2016&p_SAMABillPeriod=20160510%2021%3a00%3a00&&PageFirstRow=1&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2016&p_SAMABillPeriod=20160531%2021%3a00%3a00&p_ID=815&PageFirstRow=16&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2015&p_SAMABillPeriod=20150301%2021%3a00%3a00&p_ID=777&PageFirstRow=31&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2013&p_SAMABillPeriod=20131201%2021%3a00%3a00&p_ID=759&PageFirstRow=46&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2012&p_SAMABillPeriod=20120902%2021%3a00%3a00&p_ID=742&PageFirstRow=61&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2011&p_SAMABillPeriod=20110626%2021%3a00%3a00&p_ID=734&PageFirstRow=76&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_2010&p_SAMABillPeriod=20100328%2021%3a00%3a00&p_ID=711&PageFirstRow=91&View=4ee023c6-55dc-4522-bbc9-07e0458448c5",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/SAMABills.aspx?Paged=TRUE&p_SAMABillYear=8_1994&p_SAMABillPeriod=19940330%2021%3a00%3a00&p_ID=783&PageFirstRow=106&View=4ee023c6-55dc-4522-bbc9-07e0458448c5")
  
  sama_bond_urls <- c("http://www.sama.gov.sa/en-US/GovtSecurity/Pages/GovernmentDevelopmentBonds.aspx?&&p_SAMAYear=8_2004&&PageFirstRow=1&View=6d4a0e97-57d3-4a53-8edb-b8e58bbe6e67",
  "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/GovernmentDevelopmentBonds.aspx?Paged=TRUE&PagedPrev=TRUE&p_SAMAYear=8_2001&p_ID=175&PageFirstRow=16&View=6d4a0e97-57d3-4a53-8edb-b8e58bbe6e67")
  
  for (i in seq_along(sama_tbill_urls)){
   
    html <- read_html(sama_tbill_urls[i])
    cast <-  html_table(html_nodes(html, "table"),fill=T)[[2]]
    cast_tbill <- cast[,c("Period","1st Week","4th Week","13th Week","26th Week",
                              "52nd Week","Year","Month")]
    rownames(cast) <- 1:nrow(cast)
    if ( i !=1){
      casts_tbill <- rbind(casts_tbill,cast_tbill)
    }else{
      casts_tbill <- cast_tbill
    }
  }
  casts_tbill <- casts_tbill[!grepl('Year',casts_tbill$Period),]
  casts_tbill <- na.omit(casts_tbill)
  
  
  
  sama_bond_urls <- c("http://www.sama.gov.sa/en-US/GovtSecurity/Pages/GovernmentDevelopmentBonds.aspx?&&p_SAMAYear=8_2004&&PageFirstRow=1&View=6d4a0e97-57d3-4a53-8edb-b8e58bbe6e67",
                      "http://www.sama.gov.sa/en-US/GovtSecurity/Pages/GovernmentDevelopmentBonds.aspx?Paged=TRUE&PagedPrev=TRUE&p_SAMAYear=8_2001&p_ID=175&PageFirstRow=16&View=6d4a0e97-57d3-4a53-8edb-b8e58bbe6e67")
  
  for (i in seq_along(sama_bond_urls)){
    
    html <- read_html(sama_bond_urls[i])
    cast_bond <-  html_table(html_nodes(html, "table"),fill=T)[[2]]
  
    if ( i !=1){
      casts_bond <- rbind(casts_bond,cast_bond)
    }else{
      casts_bond <- cast_bond
    }
  }
  casts_bond <- casts_bond[!grepl('Year : ',casts_bond$Date),]
  casts_bond <- casts_bond[,c("Time Duration", "Quarter One Rate",
                              "Quarter Two Rate", "Quarter Three Rate", "Quarter Four Rate" ,
                              "Year", "Date")]
  casts_bond <- na.omit(casts_bond)
  
}
