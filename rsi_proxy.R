#pre-process
july.play.730 <- filter(play.data.701, datedt=='2016-07-30')
july.first.730<- filter(july.first.v2, date =='2016-07-30')

buy.730.before <- data.frame()
for( i in 1:nrow(july.first.))
{
  buy.730.before <- rbind(buy.730.before, filter(july.play.730, channeluserid==july.first.730$channeluserid[i])%>%filter(now_2<july.first.730$now_2[i]))
}
buy.730.before<-group_by(buy.730.before, channeluserid)
buy.730.before<-arrange(buy.730.before, channeluserid,desc(now_2))
#buy.730.before$real <- buy.730.before$totalmypoint/buy.730.before$blindstate
#analysis on pre-point 
buy.730.before$num[1]<-1
for ( i in 2: nrow(buy.730.before))
{
  if(buy.730.before$channeluserid[i-1]!=buy.730.before$channeluserid[i]) {
    buy.730.before$num[i]<-1} else {
      buy.730.before$num[i]<-buy.730.before$num[i-1]+1
    }
}
buy.730.just.before<-filter(buy.730.before, num==1)
#calculating RSI
buy.730.id<-unique(buy.730.before$channeluserid)
rsi_730 <- data.frame()
for( i in 1:length(buy.730.id)){
  sample1 <- diff(buy.730.before[buy.730.before$channeluserid==buy.730.id[i],10]$real[5:1])
  au <- sum(sample1[sample1>0])
  ad <- abs(sum(sample1[sample1<0]))
  rsi_result <- au/ad
  rsi_sample <- cbind(buy.730.id[i],rsi_result, au, ad)
  rsi_730 <- rbind(rsi_730, rsi_sample)
}
rsi_730<- tbl_df(rsi_730)
rsi_730$V1<-as.character(rsi_730$V1)
rsi_730$rsi_result<-as.numeric(as.character(rsi_730$rsi_result))
rsi_730$au<-as.numeric(as.character(rsi_730$au))
rsi_730$ad<-as.numeric(as.character(rsi_730$ad))
names(rsi_730)[1]<-c("channeluserid")
july.first.730 <- left_join(rsi_730, july.first.730)
july.first.730.v1 <- na.omit(july.first.730)
july.first.730.v1$rsi_result[is.infinite(july.first.730.v1$rsi_result)]<-0
july.first.730.v1$ravenitemcd <- as.character(july.first.730.v1$ravenitemcd)
july.first.total<- rbind(july.first.total, july.first.730.v1)
#chisq.test(july.first.730.v1$rsi_result,july.first.730.v1$payamount)
#decision tree
#library(rpart)
#install.packages("rpart.plot")
#library(rpart.plot)
#fit <- rpart(ravenitemcd~rsi_result+au+ad, data=july.first.730.v1, control = rpart.control(maxsplit = 4))
#plot(fit)

