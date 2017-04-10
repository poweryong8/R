access.march
reg.feb <- filter(registration, yearmon =='2 2016')%>%group_by(channeluserid)
reg.march <- filter(registration, yearmon =='3 2016')%>%group_by(channeluserid)
reg.april <- filter(registration, yearmon =='4 2016')%>%group_by(channeluserid)
access.table<-tbl_df(access.table)

access.table.1$num <- 0
for (i in 1:nrow(access.table.1))
{
  for( j in 2:5)
  {
    if(access.table.1[i,j]!=0){
      access.table.1$num[i]<- access.table.1$num[i]+1
    }
  }
}
reg.feb <- left_join(reg.feb, access.table, by ="channeluserid")%>%group_by(channeluserid)
reg.march <- left_join(reg.march, access.table, by ="channeluserid")%>%group_by(channeluserid)
reg.april2 <- left_join(reg.april2, access.table, by ="channeluserid")%>%group_by(channeluserid)

library(RODBC)
#connting server using odbc function
conn_mssql2 <- odbcConnect("gholdem2", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
sql_reg.new<- " SELECT channeluserid, joindate, locale
  FROM [dbo].[BI_Members]"

new.reg <- sqlQuery(conn_mssql2, sql_reg.new)

reg.april3.1 <- filter(reg.april3, `2 2016`==0)
reg.april3.2 <- filter(reg.april3.1, `3 2016`==0)
-------------------------------------------------------------------------------------------------------------------
#LTV
buy <-tbl_df(buy)%>%group_by(channeluserid)
buy$date <- as.Date(buy$now_2)
buy$yearmon <- as.yearmon(buy$date)
buy.summary<- summarise(buy, total.paymount=sum(payamount))

reg.us.access.36 <- filter(reg.us.access, date.x =='2016-03-06')
reg.36<-dcast(reg.us.access.36, channeluserid~date)
reg.us.dcast<- dcast(reg.us.access, channeluserid~date)

retent.all<-c()
for( i in 1:(ncol(reg.us.dcast)-1)){
retent.all[i] <- length(which(reg.us.dcast[,i+1] !=0))
}
buy.during.merketing <- filter(buy, date>'2016-02-14')%>%filter(date<'2016-05-31')
access.during.marketing <- filter(access, date>'2016-02-14')%>%filter(date<'2016-05-31')


