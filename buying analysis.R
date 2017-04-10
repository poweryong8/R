buy.data.1208$yearmon <-as.yearmon(buy.data.1208$datedt)
july.bu <- filter(buy.data.1208, yearmon=='7 2016')
july.bu$now_2<- as.character(july.bu$now_2)
july.bu.melt <-melt(july.bu, id.vars = c("channeluserid", "now_2", "ravenitemcd"),measure.vars = "payamount")
july.bu.melt <-arrange(july.bu.melt, channeluserid, now_2)
july.bu.melt <- tbl_df(july.bu.melt)
july.bu.melt <-group_by(july.bu.melt, channeluserid)
july.id <- unique(july.bu.melt$channeluserid)
july.first <- data.frame()
#searching first buying point each day per id
for( i in 1: length(unique(july.bu.melt$channeluserid))){
  temp = july.bu.melt[july.bu.melt$channeluserid==july.id[i],]
  temp <- group_by(temp, date)
  temp1 <- summarise(temp, channeluserid=max(channeluserid), now_2 = min(now_2))
  july.first <- rbind(july.first, temp1)
}
#data about playing game
library(RODBC)
conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
startdate <- as.Date("2016-08-01")
enddate <- as.Date("2016-08-31")
date_801<- makeDate(startdate, enddate)
sql_play.801<-c()
for ( i in 1:length(date_801)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,prevtotalmypoint, bettingpoint, blindstate,dealerpoint,result,allintype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_801[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.801<- c(sql_play.801, sql_2)
}
play.data.801<-data.frame()
for ( i in 1:length(sql_play.801)){
  play1 <- assign(paste("play801_",date_801[i],sep = ""),sqlQuery(conn_mssql, sql_play.801[i]))
  play.data.801<-rbind(play.data.801, play1)
}
play.data.801<-tbl_df(play.data.801)
play.data.801$channeluserid <- as.character(play.data.801$channeluserid)
play.data.801$result<- as.character(play.data.801$result)
play.data.801$now_2<- as.character(play.data.801$now_2)

july.play.1 <- filter(play.data.702, datedt=='2016-07-01')

#pre-process
july.play.2 <- filter(play.data.702, datedt=='2016-07-02')
july.first.2<- filter(july.first.v1, date =='2016-07-02')

buy.702.before <- data.frame()
for( i in 1:nrow(july.first.1))
{
  buy.702.before <- rbind(buy.702.before, filter(july.play.1, channeluserid==july.first.1$channeluserid[i])%>%filter(now_2<july.first.1$now_2[i]))
}
buy.702.before<-group_by(buy.702.before, channeluserid)
#analysis on pre-point 
buy.702.before$num[1]<-1
for ( i in 2: nrow(buy.702.before))
{
  if(buy.702.before$channeluserid[i-1]!=buy.702.before$channeluserid[i]) {
    buy.702.before$num[i]<-1} else {
      buy.702.before$num[i]<-buy.702.before$num[i-1]+1
    }
}
buy.702.just.before<-filter(buy.702.before, num==1)
#calculating RSI
buy.702.id<-unique(buy.702.before$channeluserid)
rsi <- data.frame(stringsAsFactors = FALSE)
for( i in 1:length(buy.702.id)){
sample1 <- diff(buy.702.before[buy.702.before$channeluserid==buy.702.id[i],11]$real[5:1])
au <- sum(sample1[sample1>0])
ad <- abs(sum(sample1[sample1<0]))
rsi_result <- au/ad
rsi_sample <- cbind(buy.702.id[i],rsi_result, au, ad)
rsi <- rbind(rsi, rsi_sample)
}
rsi<- tbl_df(rsi)
rsi$V1<-as.character(rsi$V1)
rsi$rsi_result<-as.numeric(as.character(rsi$rsi_result))
rsi$au<-as.numeric(as.character(rsi$au))
rsi$ad<-as.numeric(as.character(rsi$ad))
july.buy.702 <- filter(july.first.v2, date=='2016-07-01')
names(rsi)[1]<-c("channeluserid")
july.702.rsi <-left_join(july.buy.702, rsi)
