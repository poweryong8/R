library(reshape2)
library(arules)
library(dplyr)

for( i in 6:30){
  assign(paste("over", i, sep = ""),filter(over0408, duration==i))
}
over30

library(reshape2)
du6 <- tbl_df(dcast(over6, channeluserid~du.date))
num.over6 <- as.data.frame(table(over6$du.date))

c6 <- list()
for( i in 1:6)
{ 
  c6[i] <- as.list(count(du6[du6[,i+1]>0,]))
}
num.over6$uid <- as.numeric(cbind(c6))
num.over6$mean <- num.over6$Freq/num.over6$uid

plot(num.over29$mean, type="l", main = "2ndlt 29")+grid()
#
dc9.1<- left_join(dc9,join.t )
dc9.1 <- select(dc9.1, channeluserid,xx,duration,blindstate, result, playtime, mypoint, mylevel,allintype, bettingpoint, now_2, du.date)
sp9.1 <- filter(dc9.1, xx==3)
dc9.1$wdays <- weekdays(dc9.1$now_2)

#dc6.2 <- filter(dc6.1.1, xx==3)
#dc6.3 <- filter(dc6.1.1, xx==4)
sp9 <- dc9.1[dc9.1$channeluserid == uid.dc9.1[3],]
sp23 <- dc6.1.1[dc6.1.1$channeluserid == dc6.2$channeluserid[3],]
plot(sp23$du.date,sp23$bettingpoint)

#data preparing for analysis 'association'
cast.dc9.1 <- dcast(dc9.1, channeluserid+du.date~wdays)
rules9<- cast.dc9.1[,1:2]
rules9$channeluserid<-as.character(rules9$channeluserid)
#rules2$du.date<-as.character(rules2$du.date)
rules.list9<-split(rules9$du.date, rules9$channeluserid)
rules.trans9 <- as(rules.list9, "transactions")
association9 <- apriori(rules.trans9, parameter = list(support=0.20))
inspect(association9)
as(association9, "data.frame")
# one association model on game play data
gameend_dt.v1 <- left_join(gameend_dt, game.user)
gameend_dt.v1 <- filter(gameend_dt.v1, duration >5)
gameend_dt.v1 <- select(gameend_dt.v1, channeluserid, min.date, now_2)
gameend_dt.v1$now_2 <- as.Date.character(gameend_dt.v1$now_2)
#wholedata <- left_join(gameend_dt.v1, game.user)
#wholedata
wholedata <- mutate(gameend_dt.v1, du.date = now_2-min.date+1, wdays=weekdays(now_2))
wholedata.du <- select(wholedata, channeluserid, du.date, wdays)
wholedata.du$du.date <- as.numeric(wholedata.du$du.date)
cast.whole <- dcast(wholedata.du, channeluserid+du.date~wdays)
rules.all <- cast.whole[,1:2]
rules.all$channeluserid<-as.character(rules.all$channeluserid)
#rules2$du.date<-as.character(rules2$du.date)
rules.all.list<-split(rules.all$du.date, rules.all$channeluserid)
rules.trans.all <- as(rules.all.list, "transactions")
association.all <- apriori(rules.trans.all, parameter = list(support=0.30))
inspect(association.all)
rules10
association.all.dt <- as(association.all, "data.frame")
write.csv(association.all.dt, file="association30.csv")
