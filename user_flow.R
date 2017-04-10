startdate <- as.Date("2016-10-01")
enddate <- as.Date("2016-10-31")
date_request <-makeDate(startdate, enddate)
conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")

sql_data_1120 <- c("SELECT channeluserid,logid,logdetailid,os_2,platformtype, now_2,datedt from gholdem.TB_ParsingGameLog_20161120")
data_1120 <- sqlQuery(conn_mssql, sql_data_1120)
data_1120 <- tbl_df(data_1120)
data_1120<- arrange(data_1120, channeluserid, now_2)
data_1120$id<- paste(data_1120$logid, data_1120$logdetailid, sep = "-")
data_1120 <- data_1120[data_1120$logid!=101,]
data_1120$now_2<- as.POSIXct(data_1120$now_2)

data_1120$num[1]<-1
for ( i in 2: nrow(data_1120))
{
  if(data_1120$channeluserid[i-1]!=data_1120$channeluserid[i]) {
    data_1120$num[i]<-1} else {
      data_1120$num[i]<-data_1120$num[i-1]+1
    }
}


data_1120$diff[1]<-0
for ( i in 2: nrow(data_1120))
{
  if(data_1120$channeluserid[i-1]!=data_1120$channeluserid[i]) {
    data_1120$diff[i]<-0} else {
      data_1120$diff[i]<-data_1120$now_2[i]-data_1120$now_2[i-1]
    }
}
