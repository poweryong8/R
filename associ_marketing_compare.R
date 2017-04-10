p27.m7$channeluserid<-as.character(p27.m7$channeluserid)

p37.m11<-left_join(p37.m11, play.3rd, by = "channeluserid")


p27.m7.melt<- melt(p27.m7, id.vars = c("channeluserid", "blindstate"), measure.vars = c("totalmypoint"))
p27.m7.dcast <- dcast(p27.m7.melt, channeluserid+blindstate~variable, median)
p27.m7.dcast<-filter(p27.m7.dcast, totalmypoint>0)
rule.p27.m7 <- p27.m7.dcast[,1:2]
rule.p27.m7.list<-split(rule.p27.m7$blindstate, rule.p27.m7$channeluserid)
p27.m7.list<- list()
for (i in 1:length(rule.p27.m7.list)) {
  p27.m7.list[[i]] <- as.character(rule.p27.m7.list[[i]][!duplicated(rule.p27.m7.list[[i]])])
}

rule.p27.m7.trans<-as(rule.p27.m7.list, "transactions")
associ.p37.m2 <- apriori(rule.p37.m2.trans, parameter = list(support=0.01))
inspect(associ.p37.m2)

p17.buynplay <-left_join(buy.1st.7days, play.1st.7days, by = "channeluserid")%>%group_by(channeluserid)
p17.buynplay.melt <- melt(p17.buynplay)
