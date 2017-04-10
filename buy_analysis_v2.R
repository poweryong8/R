#----------------------------------------------------------------------------------
#compare group that bought item only 1 with bought several items
#---------------------------------------------------------------------------------
buynplay<-group_by(buynplay, channeluserid)
buynplay.dcast<-dcast(buynplay, channeluserid~yearmon)
buy.only1 <- filter(buy.repurchase, total.buy==1)
buy.over1<-filter(buy.repurchase, total.buy>1)
buynplay.dcast$num.access <- 0
for (i in 1:nrow(buynplay.dcast))
{
  for( j in 2:9)
  {
    if(buynplay.dcast[i,j]!=0){
      buynplay.dcast$num.access[i]<- buynplay.dcast$num.access[i]+1
    }
  }
}
buynplay.dcast$total.play<-0
for( i in 1:nrow(buynplay.dcast))
{
  buynplay.dcast$total.play[i]<-sum(buynplay.dcast[i,2:9])
}
#compare before with after
buy.m4.before$real <- buy.m4.before$totalmypoint/buy.m4.before$blindstate
buy.m4.after$real <- buy.m4.after$totalmypoint/buy.m4.after$blindstate
buy.m4.before.summary <- summarise(buy.m4.before, dealerpoint=sum(dealerpoint), min.blind=min(blindstate),max.blind=max(blindstate), betting=sum(bettingpoint), real=mean(real))
buy.m4.after.summary <- summarise(buy.m4.after, af.dealerpoint=sum(dealerpoint), af.min.blind=min(blindstate),af.max.blind=max(blindstate), af.betting=sum(bettingpoint), af.real=mean(real))
buy.m4.be.af<- left_join(buy.m4.before.summary, buy.m4.after.summary, by = "channeluserid")
buy.m4.be.af$mobility <- buy.m4.be.af$af.max.blind/buy.m4.be.af$max.blind
buy.m7.be.af.vc<-left_join(buy.m7.before.vc, buy.m7.after.vc, by ="channeluserid")

barplot(table(buy.m6.be.af$mobility))
t.test(buy.m7.be.af$betting, buy.m7.be.af$af.betting, paired = T)
t.test(buy.m7.be.af$dealerpoint, buy.m7.be.af$af.dealerpoint, paired = T)
t.test(buy.m7.be.af$real, buy.m7.be.af$af.real, paired = T)
t.test(buy.m7.be.af.vc$vc.x, buy.m7.be.af.vc$vc.y, paired = T)
#calculating VC
buy.m1.before.vc<- summarise_each(buy.m1.before, funs(mean, sd), totalmypoint)
buy.m1.before.vc$vc <- buy.m1.before.vc$sd/buy.m1.before.vc$mean
buy.m1.after.vc<- summarise_each(buy.m1.after, funs(mean, sd), totalmypoint)
buy.m1.after.vc$vc <- buy.m1.after.vc$sd/buy.m1.after.vc$mean
buy.m1.before.wfl<- dcast(buy.m1.before, channeluserid~result)
buy.m1.before.wfl$total<- buy.m1.before.wfl$f+buy.m1.before.wfl$l+buy.m1.before.wfl$w
buy.m1.before.wfl$winrate = buy.m1.before.wfl$w/buy.m1.before.wfl$total
buy.m1.before.wfl$foldrate = buy.m1.before.wfl$f/buy.m1.before.wfl$total

#calculateing other users'vc
play.m7 <- group_by(play.monthly.7, channeluserid)
play.m7$real <- play.m7$totalmypoint/play.m7$blindstate
play.m7.v2 <- summarise(play.m7, dealerpoint=sum(dealerpoint), min.blind=min(blindstate),max.blind=max(blindstate), betting=sum(bettingpoint), real=mean(real))
#play.m1.v1 <- summarise_each(play.m1, funs(mean,sd), totalmypoint)
#play.m1.v1$vc<-play.m1.v1$sd/play.m1.v1$mean 
sample.m7 <- play.m7.v2[sample(c(1:nrow(play.m7.v2)), nrow(buy.m7.total)),] #sampling
sample.m7.v1 <- play.m7[sample(c(1:nrow(play.m7.v2)), nrow(buy.m7.total)),] #sampling

buy.m4.before$real <- buy.m4.before$totalmypoint/buy.m4.before$blindstate
buy.p.m7.v1 <- summarise(buy.p.m7, dealerpoint=sum(dealerpoint), min.blind=min(blindstate),max.blind=max(blindstate), betting=sum(bettingpoint), real=mean(real))
#inequality test
sample.m7.v1 <- select(sample.m7, channeluserid, real)
buy.p.m7.v2 <- select(buy.p.m7.v1, channeluserid, real)
buy.plus.nonbuy.m7 <- rbind(sample.m7.v1, buy.p.m7.v2)
ineq(buy.plus.nonbuy.m7$real)


#ttest
t.test(buy.m6.before.vc$vc, sample.m6$vc)
mean(buy.m4.before.vc$vc, na.rm=T)
mean(sample.m4$vc)

dau.pay <- read.csv("D:/4ones/task/payment_freechip.csv", header = T)

#before/after buying items
buy.before <- data.frame()
for( i in 1:nrow(buy.fb))
{
  buy.before <- rbind(buy.before, filter(buynplay, channeluserid==buy.fb$channeluserid[i])%>%filter(now_2<buy.fb$now_2[i]))
}

buy.after <- data.frame()
for( i in 1:nrow(buy.fb))
{
  buy.after <- rbind(buy.after, filter(buynplay, channeluserid==buy.fb$channeluserid[i])%>%filter(now_2>buy.fb$now_2[i]))
}
buy.before$real <- buy.before$totalmypoint/buy.before$blindstate
buy.after$real <- buy.after$totalmypoint/buy.after$blindstate
buy.before.v1 <- filter(buy.before, yearmon > '2 2016')
buy.after.v1 <- filter(buy.after, yearmon > '2 2016')
buy.before.access <- dcast(buy.before, channeluserid~date)
buy.after.access <- dcast(buy.after, channeluserid~date)
buy.after.access$num.access <- 0
for (i in 1:nrow(buy.after.access))
{
  for( j in 2:211)
  {
    if(buy.after.access[i,j]!=0){
      buy.after.access$num.access[i]<- buy.after.access$num.access[i]+1
    }
  }
}
buy.only1.access <-left_join(buy.only1.v1, buy.access.be.af, by="channeluserid") # join only1 into access data
buy.over1.access <-left_join(buy.over1.v1, buy.access.be.af, by="channeluserid")

buy.before.summary <- summarise(buy.before, playtime = sum(playtime), dealerpoint=sum(dealerpoint), getpoint=sum(getpoint),betting=sum(bettingpoint),min.blind= min(blindstate), max.blind=max(blindstate))
buy.after.summary <- summarise(buy.after, af.playtime = sum(playtime), af.dealerpoint=sum(dealerpoint), af.getpoint=sum(getpoint),af.betting=sum(bettingpoint),af.min.blind= min(blindstate), af.max.blind=max(blindstate))
buy.be.af <- left_join(buy.after.summary, buy.before.summary, by="channeluserid")
buy.only1.before <- left_join(buy.only1.v1, buy.before.summary)
buy.only1.after <- left_join(buy.only1.v1, buy.after.summary)
buy.only1.be.af <- left_join(buy.only1.after, buy.only1.before)

buy.over1.before <- left_join(buy.over1.v1, buy.before.summary)
buy.over1.after <- left_join(buy.over1.v1, buy.after.summary)
buy.over1.be.af <- left_join(buy.over1.after, buy.over1.before)
buy.only1.access$du.date<- buy.only1.access$af.num.access-buy.only1.access$num.access.y
buy.over1.access$du.date<- buy.over1.access$af.num.access-buy.over1.access$num.access.y
####
buy.over1.be.af.v1 <- select(buy.over1.be.af, channeluserid, af.getpoint, af.betting, getpoint,betting)
buy.over1.be.af.v1$revenue <- buy.over1.be.af.v1$getpoint+buy.over1.be.af.v1$betting
buy.over1.be.af.v1$af.revenue <- buy.over1.be.af.v1$af.getpoint+buy.over1.be.af.v1$af.betting
buy.over1.be.af.v1$diff <- buy.over1.be.af.v1$af.revenue-buy.over1.be.af.v1$revenue
buy.over1.be.af.v1$growthrate <- buy.over1.be.af.v1$diff/buy.over1.be.af.v1$revenue

####
buy.over1.be.af.v1[which(is.infinite(buy.over1.be.af.v1$growthrate)),9]<-0
buy.only1.be.af.v1[which(is.infinite(buy.only1.be.af.v1$growthrate)),9]<-0
t.test(buy.only1.be.af.v1$growthrate, buy.over1.be.af.v1$growthrate)
####
buy.m7.before.v1 <- summarise(buy.m7.before, avg.betting=mean(bettingpoint), total.dealer=sum(dealerpoint),total.playtime=sum(playtime), max.blind=max(blindstate))
buy.m7.after.v1 <- summarise(buy.m7.after, avg.betting=mean(bettingpoint), total.dealer=sum(dealerpoint),total.playtime=sum(playtime), max.blind=max(blindstate))
