startdate <- as.Date("2016-10-01")
enddate <- as.Date("2016-10-31")
date_request <-makeDate(startdate, enddate)
conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
#--------------------------------
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
#---------------------------------
sql_play.oct<-c()
for ( i in 1:length(date_request)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,bettingpoint,playtime, blindstate,dealerpoint,result,win,lose,fold, allintype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_request[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.oct<- c(sql_play.oct, sql_2)
}
play.data.oct<-data.frame()
for ( i in 1:length(sql_play.oct)){
  play1 <- assign(paste("play.oct_",date_request[i],sep = ""),sqlQuery(conn_mssql, sql_play.oct[i]))
  play.data.oct<-rbind(play.data.oct, play1)
}
play.data.oct<-tbl_df(play.data.oct)
reg.data$datedt<-as.character(reg.data$datedt)

channel.summary <-dcast(play.data.oct, channeluserid+datedt~blindstate)
channel.summary<-tbl_df(channel.summary)
channel.summary.v1 <- na.omit(channel.summary)
channel.summary.v1<- channel.summary.v1[,-16]

channel.summary.v1$blind<-c(0)
for(i in 1:nrow(channel.summary.v1)){
  channel.summary.v1$blind[i] <-as.numeric(names(channel.summary.v1)[which.max(channel.summary.v1[i,])])
}
##let them having sequential index
reg.sep.summary.v1$num[1]<-1
for ( i in 2: nrow(reg.sep.summary.v1))
{
  if(reg.sep.summary.v1$channeluserid[i-1]!=reg.sep.summary.v1$channeluserid[i]) {
    reg.sep.summary.v1$num[i]<-1} else {
      reg.sep.summary.v1$num[i]<-reg.sep.summary.v1$num[i-1]+1
    }
}
play.sep.date <- select(reg.sep.summary.v1,channeluserid, datedt.y)
play.reg.summary <- left_join(reg.sep, play.sep.date)
play.reg.summary$datedt.y<- as.Date(play.reg.summary$datedt.y)
play.reg.summary$datedt<- as.Date(play.reg.summary$datedt)
play.reg.summary$diff <- (play.reg.summary$datedt.y-play.reg.summary$datedt)+1
play.reg.summary.v1 <-select(play.reg.summary, channeluserid, datedt.y, diff)
play.reg.summary.v1$datedt.y<-as.character(play.reg.summary.v1$datedt.y)
reg.sep.summary.v3 <- left_join(reg.sep.summary.v1, play.reg.summary.v1, by=c("channeluserid", "datedt.y"))
reg.total.v1 <- dcast(reg.sep.summary.v3, diff~blind, margins = TRUE)
reg.total.v2 <- dcast(reg.sep.summary.v3, diff~blind)
write.csv(reg.total.v1, "reg_total.csv")

