#july.first.total.v1 <-data.frame()
#pre-process
july.play.730 <- filter(play.data.701, datedt=='2016-07-30')
july.first.730.p1<- filter(july.first.v2, date =='2016-07-30')

buy.730.before.v1 <- data.frame()
for( i in 1:nrow(july.first.730.p1))
{
  buy.730.before.v1 <- rbind(buy.730.before.v1, filter(july.play.730, channeluserid==july.first.730.p1$channeluserid[i])%>%filter(now_2<july.first.730.p1$now_2[i]))
}
buy.730.before.v1<-group_by(buy.730.before.v1, channeluserid)
buy.730.before.v1<-arrange(buy.730.before.v1, channeluserid,desc(now_2))
#buy.730.before.v1$real <- buy.730.before.v1$totalmypoint/buy.730.before.v1$blindstate
#analysis on pre-point 
buy.730.before.v1$num[1]<-1
for ( i in 2: nrow(buy.730.before.v1))
{
  if(buy.730.before.v1$channeluserid[i-1]!=buy.730.before.v1$channeluserid[i]) {
    buy.730.before.v1$num[i]<-1} else {
      buy.730.before.v1$num[i]<-buy.730.before.v1$num[i-1]+1
    }
}
buy.730.just.before<-filter(buy.730.before.v1, num==1)
#calculating RSI
buy.730.id.v1<-unique(buy.730.before.v1$channeluserid)
rsi_730.v1 <- data.frame()
for( i in 1:length(buy.730.id.v1)){
  sample1 <- diff(buy.730.before.v1[buy.730.before.v1$channeluserid==buy.730.id.v1[i],3]$totalmypoint[5:1])
  au <- sum(sample1[sample1>0])
  ad <- abs(sum(sample1[sample1<0]))
  rsi_result <- au/ad
  rsi_sample <- cbind(buy.730.id.v1[i],rsi_result, au, ad)
  rsi_730.v1 <- rbind(rsi_730.v1, rsi_sample)
}
rsi_730.v1<- tbl_df(rsi_730.v1)
rsi_730.v1$V1<-as.character(rsi_730.v1$V1)
rsi_730.v1$rsi_result<-as.numeric(as.character(rsi_730.v1$rsi_result))
rsi_730.v1$au<-as.numeric(as.character(rsi_730.v1$au))
rsi_730.v1$ad<-as.numeric(as.character(rsi_730.v1$ad))
names(rsi_730.v1)[1]<-c("channeluserid")
july.first.730.v1 <- left_join(rsi_730.v1, july.first.730.p1)
july.first.730.v1 <- na.omit(july.first.730.v1)
july.first.730.v1$rsi_result[is.infinite(july.first.730.v1$rsi_result)]<-0
july.first.730.v1$ravenitemcd <- as.character(july.first.730.v1$ravenitemcd)
july.first.total.v1<- rbind(july.first.total.v1, july.first.730.v1)
#chisq.test(july.first.730.v1$rsi_result,july.first.730.v1$payamount)
#decision tree
#library(rpart)
#install.packages("rpart.plot")
#library(rpart.plot)
#fit <- rpart(ravenitemcd~rsi_result+au+ad, data=july.first.730.v1, control = rpart.control(maxsplit = 4))
#plot(fit)

