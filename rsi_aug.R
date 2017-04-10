#buy.data.1208$yearmon <-as.yearmon(buy.data.1208$datedt)
#aug.bu <- filter(buy.data.1208, yearmon=='8 2016')
#library(RODBC)
#conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
#date.702 <-read.csv("D:/4ones/task/date_702.csv", header=T)

#aug.first.total.v1 <-data.frame()
#pre-process

aug.play.830 <- filter(play.data.801, datedt=='2016-08-30')
aug.first.830<- filter(aug.bu, datedt =='2016-08-30')
aug.first.830.p1 <- group_by(aug.first.830, channeluserid)
aug.first.830.p1 <- arrange(aug.first.830.p1, channeluserid, now_2)
aug.first.830.p1$num[1]<-1
for ( i in 2: nrow(aug.first.830.p1))
{
  if(aug.first.830.p1$channeluserid[i-1]!=aug.first.830.p1$channeluserid[i]) {
    aug.first.830.p1$num[i]<-1} else {
      aug.first.830.p1$num[i]<-aug.first.830.p1$num[i-1]+1
    }
}
aug.first.830.p2<- filter(aug.first.830.p1, num==1)
buy.830.before.v1 <- data.frame()
for( i in 1:nrow(aug.first.830.p2))
{
  buy.830.before.v1 <- rbind(buy.830.before.v1, filter(aug.play.830, channeluserid==aug.first.830.p2$channeluserid[i])%>%filter(now_2<aug.first.830.p2$now_2[i]))
}
buy.830.before.v1<-group_by(buy.830.before.v1, channeluserid)
buy.830.before.v1<-arrange(buy.830.before.v1, channeluserid,desc(now_2))
buy.830.before.v1$datedt<-as.character(buy.830.before.v1$datedt)
#buy.830.before.v1$real <- buy.830.before.v1$totalmypoint/buy.830.before.v1$blindstate
#analysis on pre-point 
buy.830.before.v1$num[1]<-1
for ( i in 2: nrow(buy.830.before.v1))
{
  if(buy.830.before.v1$channeluserid[i-1]!=buy.830.before.v1$channeluserid[i]) {
    buy.830.before.v1$num[i]<-1} else {
      buy.830.before.v1$num[i]<-buy.830.before.v1$num[i-1]+1
    }
}
#buy.830.just.before<-filter(buy.830.before.v1, num==1)
#calculating RSI
buy.830.id.v1<-unique(buy.830.before.v1$channeluserid)
rsi_830.v1 <- data.frame()
for( i in 1:length(buy.830.id.v1)){
  sample1 <- diff(buy.830.before.v1[buy.830.before.v1$channeluserid==buy.830.id.v1[i],3]$totalmypoint[5:1])
  au <- sum(sample1[sample1>0])
  ad <- abs(sum(sample1[sample1<0]))
  rsi_result <- au/ad
  rsi_sample <- cbind(buy.830.id.v1[i],rsi_result, au, ad)
  rsi_830.v1 <- rbind(rsi_830.v1, rsi_sample)
}
rsi_830.v1<- tbl_df(rsi_830.v1)
rsi_830.v1$V1<-as.character(rsi_830.v1$V1)
rsi_830.v1$rsi_result<-as.numeric(as.character(rsi_830.v1$rsi_result))
rsi_830.v1$au<-as.numeric(as.character(rsi_830.v1$au))
rsi_830.v1$ad<-as.numeric(as.character(rsi_830.v1$ad))
names(rsi_830.v1)[1]<-c("channeluserid")
aug.first.830.v1 <- left_join(rsi_830.v1, aug.first.830.p2)
aug.first.830.v1 <- na.omit(aug.first.830.v1)
aug.first.830.v1$rsi_result[is.infinite(aug.first.830.v1$rsi_result)]<-0
aug.first.830.v1$ravenitemcd <- as.character(aug.first.830.v1$ravenitemcd)
aug.first.total.v1<- rbind(aug.first.total.v1, aug.first.830.v1)
#chisq.test(aug.first.830.v1$rsi_result,aug.first.830.v1$payamount)
#decision tree
#library(rpart)
#install.packages("rpart.plot")
#library(rpart.plot)
#fit <- rpart(ravenitemcd~rsi_result+au+ad, data=aug.first.830.v1, control = rpart.control(maxsplit = 4))
#plot(fit)

