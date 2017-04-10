conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
startdate <- as.Date("2015-12-08")
enddate <- as.Date("2016-11-12")
date_ts<- makeDate(startdate, enddate)
sql_buy.ts<-c()
for ( i in 1:length(date_ts)){
  sql_1<- paste("select channeluserid,ravenitemcd,payamount, now_2, datedt from gholdem.TB_ParsingGameLog_",date_ts[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 2 and logdetailid=1", sep=" ")
  sql_buy.ts<- c(sql_buy.ts, sql_2)
}
buy.data.ts<-data.frame()
for ( i in 1:length(sql_buy.ts)){
  buy1 <- assign(paste("buy.ts_",date_ts[i],sep = ""),sqlQuery(conn_mssql, sql_buy.ts[i]))
  buy.data.ts<-rbind(buy.data.ts, buy1)
}
buy.data.ts$channeluserid<- as.character(buy.data.ts$channeluserid)
buy.data.ts$ravenitemcd <- as.character(buy.data.ts$ravenitemcd)
buy.data.ts$datedt<- as.character(buy.data.ts$datedt)
buy.data.ts<-group_by(buy.data.ts, datedt)
buy.ts.summary <- summarise(buy.data.ts, revenue=sum(payamount)/100)
##visualization
qplot(datedt, revenue, data=buy.ts.summary, geom = c("line","point"))