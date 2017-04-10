dc7 <- tbl_df(dcast(dura7, channeluserid~du.date))
counta7 <- list(0)
for(i in 1:length(dc7$channeluserid))
{counta.basic <-0
for ( j in 2:ncol(dc7))
{
  if (dc7[i,j]!=0) counta.basic <- counta.basic+1
  counta7[i]<- counta.basic
}
}
dc7$xx<- as.numeric(cbind(counta7))


dc7.1<- left_join(dc7,join.t )
dc7.1 <- select(dc7.1, channeluserid,xx,duration,blindstate, result, playtime, mypoint, mylevel,allintype, bettingpoint, now_2, du.date)
dc7.1$wdays <- weekdays(dc7.1$now_2)
dc7.1.5 <- filter(dc7.1, xx==5)


#dc6.2 <- filter(dc6.1.1, xx==3)
#dc6.3 <- filter(dc6.1.1, xx==5)
#sp9.1.3 <- dc9.1[dc9.1$channeluserid == uid.dc9.1[3],]
#sp23 <- dc6.1.1[dc6.1.1$channeluserid == dc6.2$channeluserid[3],]
plot(sp23$du.date,sp23$bettingpoint)

#data preparing for analysis 'association'
cast.dc7.1.5 <- dcast(dc7.1.5, channeluserid+du.date~wdays)
rules7.5<- cast.dc7.1.5[,1:2]
rules7.5$channeluserid<-as.character(rules7.5$channeluserid)
#rules2$du.date<-as.character(rules2$du.date)
rules.list7.5<-split(rules7.5$du.date, rules7.5$channeluserid)
listData <- list()
for (i in 1:length(rule.p17.list)) {
  listData[[i]] <- as.character(rule.p17.list[[i]][!duplicated(rule.p17.list[[i]])])
}
rules.trans7.5 <- as(rules.list7.5, "transactions")
association7.5 <- apriori(rules.trans7.5, parameter = list(support=0.01))
inspect(association7.5)
as(association9, "data.frame")


