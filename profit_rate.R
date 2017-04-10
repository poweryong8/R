conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
startdate <- as.Date("2016-10-01")
enddate <- as.Date("2016-10-07")
date_request<- makeDate(startdate, enddate)
sql_reg<-c()
for ( i in 1:length(date_request)){
  sql_1<- paste("select channeluserid,os_2, now_2, datedt from gholdem.TB_ParsingGameLog_",date_request[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 1 and logdetailid=1", sep=" ")
  sql_reg<- c(sql_reg, sql_2)
}
reg.data <-data.frame()
for ( i in 1:length(sql_reg)){
  reg1 <- assign(paste("reg_",date_request[i],sep = ""),sqlQuery(conn_mssql, sql_reg[i]))
  reg.data<-rbind(reg.data, reg1)
}
sql_play<-c()
for ( i in 1:length(date_request)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,getpoint,bettingpoint,playtime, blindstate,dealerpoint,result,win,lose,fold, allintype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_request[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play<- c(sql_play, sql_2)
}
play.data<-data.frame()
for ( i in 1:length(sql_play)){
  play1 <- assign(paste("playsep_",date_request[i],sep = ""),sqlQuery(conn_mssql, sql_play[i]))
  play.data<-rbind(play.data, play1)
}
play.data<-tbl_df(play.data)
reg.data<-tbl_df(reg.data)
