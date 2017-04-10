
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
rules12
association.all.dt <- as(association.all, "data.frame")
write.csv(association.all.dt, file="association30.csv")