#-----------------------------------------------------------------------------------------------------------
#data pre-handling
#-----------------------------------------------------------------------------------------------------------
buy.m5.uid<- unique(select(buy.monthly.5, channeluserid))
buy<-tbl_df(buy)
buy$lyearmon <-as.yearmon(buy$date)
item$date <-as.Date(item$now_2)
item$yearmon <- as.yearmon(item$date)
character$date<-as.Date(character$now_2)
character$yearmon <- as.yearmon(character$date)
#split up item/character data into month
for ( i in 1:length(buy.v1.yearmon))
{
  assign(paste("character.buy.m",i,sep=""), filter(character, yearmon==buy.v1.yearmon[i]))
}
buy.p.m8 <- left_join(buy.m8.uid, play.monthly.8, by = "channeluserid")%>%group_by(channeluserid)
#adding total number of access 
buy.p.m8<-na.omit(buy.p.m8)
buy.m8.dcast<-dcast(buy.p.m8, channeluserid~blindstate)
buy.m8.dcast$total<-0
for( i in 1:nrow(buy.m8.dcast))
{
  buy.m8.dcast$total[i]<-sum(buy.m8.dcast[i,2:11])
}
buy.m8.summary <-summarise(buy.p.m8, playtime = sum(playtime), dealerpoint=sum(dealerpoint), betting=sum(bettingpoint), getpoint=sum(getpoint))
#item
buy.m8.item <- left_join(buy.m8.uid, item.buy.m2, by = "channeluserid")%>%group_by(channeluserid)
buy.m8.item.summary <- summarise(buy.m8.item, usepoint=sum(usepoint))
#win rate and fold rate
buy.m8.wlf<- dcast(buy.p.m8, channeluserid~result)
buy.m8.wlf$total<-0
for( i in 1:nrow(buy.m8.wlf))
{
  buy.m8.wlf$total[i]<-sum(buy.m8.wlf[i,2:4])
}
buy.m8.wlf$winrate<-buy.m8.wlf$w/buy.m8.wlf$total
buy.m8.wlf$foldrate<-buy.m8.wlf$f/buy.m8.wlf$total

#joining monthly buy data
buy.m8.total<-left_join(buy.m8.summary, buy.m8.dcast, by = "channeluserid")%>%group_by(channeluserid)
buy.m8.total<-left_join(buy.m8.total, buy.m8.item.summary, by = "channeluserid")%>%group_by(channeluserid)
buy.m8.total<-left_join(buy.m8.total, buy.m8.wlf, by = "channeluserid")%>%group_by(channeluserid)
buy.m8.total<-left_join(buy.m8.total, buy.m8.payment, by = "channeluserid")%>%group_by(channeluserid)
buy.m8.payment <- summarise(buy.monthly.8, payment=sum(payment))


#play data analysis
play.m8.summary<- group_by(play.monthly.8, channeluserid)
play.m8.summary<-summarise(play.m8.summary, playtime = sum(playtime), dealerpoint=sum(dealerpoint), betting=sum(bettingpoint), getpoint=sum(getpoint))
play.m4.dcast<- dcast(play.monthly.4, channeluserid~blindstate)
play.m3.dcast<-tbl_df(play.m3.dcast)
play.m3.dcast$num.play <- 0
for (i in 1:nrow(play.m3.dcast))
{
  for( j in 2:10)
  {
    if(play.m3.dcast[i,j]!=0){
      play.m3.dcast$num.play[i]<- play.m3.dcast$num.play[i]+1
    }
  }
}
play.m3.dcast$total<-0
for( i in 1:nrow(play.m3.dcast))
{
  play.m3.dcast$total[i]<-sum(play.m3.dcast[i,2:10])
}

buy.m1.total.v1 <- select(buy.m1.total, playtime, dealerpoint, betting, getpoint, total.x, usepoint, payment)

#--------------------------------------------------------------------------------------------
#buying data analysis
#--------------------------------------------------------------------------------------------
buy.trend <- dcast(buy.v1, channeluserid~yearmon,value.var = "payment",sum)
buy.trend<-tbl_df(buy.trend)
buy.access$num.access <- 0
for (i in 1:nrow(buy.access))
{
  for( j in 2:9)
  {
    if(buy.access[i,j]!=0){
      buy.access$num.access[i]<- buy.access$num[i]+1
    }
  }
}
buy.uid <-unique(select(buy, channeluserid))
buynplay <- left_join(buy.uid, play, parallel=T)
buynplay<-tbl_df(buynplay)
buy.trend<-left_join(buy.trend, buy.access, by = "channeluserid")
buy.trend.v1 <- select(buy.trend, channeluserid,num,num.access)
buy.trend.matrix <- dcast(buy.trend.v1, channeluserid~num.access, value.var = "num")
buy.trend.matrix[is.na(buy.trend.matrix)]<-0
buy.trend.matrix<-tbl_df(buy.trend.matrix)

buy.trend.num.buying <-0
for (i in 1:ncol(buy.trend.matrix)) {
  buy.trend.num.buying[i] <- sum(buy.trend.matrix[,i+1])
}
buy.melt.1<-melt(buy, id.vars = c("channeluserid","date"), measure.vars = "payment")
buy.repurchase <- dcast(buy, channeluserid~lyearmon)
buy.repurchase<- tbl_df(buy.repurchase)
buy.repurchase$num.buy <- 0
for (i in 1:nrow(buy.repurchase))
{
  for( j in 2:9)
  {
    if(buy.repurchase[i,j]!=0){
      buy.repurchase$num.buy[i]<- buy.repurchase$num.buy[i]+1
    }
  }
}
buy.repurchase$total.buy<-0
for( i in 1:nrow(buy.repurchase))
{
  buy.repurchase$total.buy[i]<-sum(buy.repurchase[i,2:9])
}
#-------------------------------------------------------------------------------------
#behavior before first buying
#-------------------------------------------------------------------------------------
for ( i in 1:length(buy.v1.yearmon))
{
  assign(paste("buy.m",i,sep=""), filter(buy, lyearmon==buy.v1.yearmon[i]))
}
buy.m8<- group_by(buy.m8, channeluserid)
buy.m1 <-arrange(buy.m1, channeluserid,now_2)
buy.m1$num[1]<-1
for ( i in 2: nrow(buy.m1))
{
  if(buy.m2$channeluserid[i-1]!=buy.m2$channeluserid[i]) {
    buy.m2$num[i]<-1} else {
      buy.m2$num[i]<-buy.m2$num[i-1]+1
    }
}
buy.m2
buy.m1.fb<-filter(buy.m1, num==1)
buy<-arrange(buy, channeluserid, now_2)
buy$num[1]<-1
for ( i in 2: nrow(buy))
{
  if(buy$channeluserid[i-1]!=buy$channeluserid[i]) {
    buy$num[i]<-1} else {
      buy$num[i]<-buy$num[i-1]+1
    }
}
buy.m1.fb$now_2<-as.character(buy.m1.fb$now_2)
buy.p.m1$now_2 <- as.character(buy.p.m1$now_2)
buy.m1.before <- data.frame()
for( i in 1:nrow(buy.m1.fb))
{
  buy.m1.before <- rbind(buy.m1.sample, filter(buy.p.m1, channeluserid==buy.m1.fb$channeluserid[i])%>%filter(now_2<buy.m1.fb$now_2[i]))
}


#research number of accessed date
buy.m1.date <- dcast(buy.p.m1, channeluserid~date)
buy.m1.date$total.play<-0
for( i in 1:nrow(buy.m1.date))
{
  buy.m1.date$total.play[i]<-sum(buy.m1.date[i,2:25])
}
buy.m1.date$num.access <- 0
for (i in 1:nrow(buy.m1.date))
{
  for( j in 2:25)
  {
    if(buy.m1.date[i,j]!=0){
      buy.m1.date$num.access[i]<- buy.m1.date$num.access[i]+1
    }
  }
}
#test
samp7 <- buy.m1.before[buy.m1.before$channeluserid==buy.m1.uid$channeluserid[7],]
samp7<-arrange(samp7, now_2)
plot(samp5$totalmypoint, type="l")
plot(non.samp3$totalmypoint, type="l")
non.samp1<-play.m1[play.m1$channeluserid=='a64d0ba3660b4d4dbf8b81cccf127203',]
non.samp3<-play.m1[play.m1$channeluserid==play.m1.uid$channeluserid[3],]
ineq(non.samp1$getpoint, type="Gini")
ineq(samp6$totalmypoint, type="Gini")
play.m1.uid <- unique(select(play.m1, channeluserid))
#average purchasing per day
dau.buy <- 0
for (i in 2:ncol(buy.date)) {
  dau.buy[i-1] <- length(which(buy.date[,i]!=0))
}
dau.buy.total <- 0
for (i in 2:ncol(buy.date)) {
  dau.buy.total[i-1] <- sum(buy.date[,i],na.rm = T)
}

#------------------------------------------------------------------------------------
#buy, money and blind
#------------------------------------------------------------------------------------
play.blind <- dcast(play, date~blindstate, value.var = "channeluserid")
play.blind<-tbl_df(play.blind)
buy <- group_by(buy, date)
buy.date <-summarise(buy, payment=sum(payment))
buy.play <- left_join(buy.date, play.blind, by="date")
buy.play.v1 <- buy.play[-c(1:85),]
buy.play.v1 <- buy.play.v1[,-c(3:11)]
buy.play.v1
play.m4.real.lm <- lm(payment~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`+`1000000`, data=play.m4.real.v1)
buy.play.v1.lm <- lm(payment~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`+`1000000`, data=buy.play.v1)
summary(buy.play.v1.lm)
equation <- payment~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`+`1000000`
ist.variables <- ~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`+`1000000`
attach(buy.play.v1)
gmm.buy <- gmm(g=equation, x= ist.variables)
summary(gmm.buy)
play.dealer <-dcast(play, date~blindstate, value.var = "dealerpoint", sum)
buy.play.dealer <- left_join(buy.date, play.dealer, by="date")
buy.play.dealer.v1 <- buy.play.dealer[-c(1:85),]
buy.play.dealer.v1 <- buy.play.dealer.v1[,-c(3:11)]
buy.play.dealer.v1
item.simple <- select(item, channeluserid, level, usepoint, usecash, date)
item.buy <- dcast(item.simple, channeluserid~level, value.var = "usepoint", sum)

equation <- payment~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`
ist.variables <- ~`1000`+`5000`+`10000`+`50000`+`100000`+`500000`
attach(play.m4.real.v1)
gmm.buy.v1 <- gmm(g=equation, x= ist.variables)
summary(gmm.buy.v1)
detach(play.m4.real.buyer)
detach(buy.play.v1)

logout.611 <- filter(logout, date == '2016-06-11')
logout.611<- group_by(logout.611, channeluserid)
logout.611$now_2 <- as.character(logout.611$now_2)
last.logout.611 <-summarise(logout.611, now_2 = max(now_2))
last.logout.611 <-left_join(last.logout.611, logout.611)
sum(last.logout.611$mypoint)

