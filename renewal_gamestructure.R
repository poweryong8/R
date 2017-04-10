#blind characteristic
p37.blind.v1 <- dcast(play.2nd.7days, channeluserid~blindstate, value.var = c("totlamypoint"), median)
p37.blind.totalchip <- dcast(play.3rd, channeluserid~blindstate, value.var = c("totalmypoint"), median)
p37.blind.totalchip <- tbl_df(p37.blind.totalchip)
p37.blind.totalchip[is.na(p37.blind.totalchip)]<-0

dau.p27 <- 0
for (i in 2:ncol(p27.blind.totalchip)) {
  dau.p27[i-1] <- length(which(p27.blind.totalchip[,i]!=0))
}
total.p27 <- 0
for (i in 1:(ncol(p27.blind.totalchip)-1)) {
  total.p27[i] <- median(p27.blind.totalchip[,i+1])
}
p27.numplay <- dcast(play.2nd.7days, channeluserid~blindstate)
numplay.p27 <- 0
for (i in 2:ncol(p27.numplay)) {
  numplay.p27[i-1] <- sum(p27.numplay[,i])
}
play.2nd.7days<-group_by(play.2nd.7days, blindstate)
p27.summary <- summarise_each(play.2nd.7days, funs(mean, sum, median), playtime, dealerpoint, getpoint, totalmypoint, bettingpoint)
write.csv(p27.summary, file = "p27_summary.csv")
p27.ac<- left_join(reg.2nd.7days,play.2nd.7days , by = "channeluserid")
p17.ac<- left_join(play.1st.7days,reg.1st.7days , by = "channeluserid")
p27.ac<-group_by(p27.ac, channeluserid)
p27.ac<-filter(p27.ac, blindstate!='NA')
p17.ac.v2 <- dcast(p17.ac.v1, channeluserid~blindstate)

dau.p17.ac <- 0
for (i in 2:ncol(p17.ac.v2)) {
  dau.p17.ac[i-1] <- length(which(p17.ac.v2[,i]!=0))
}

----------------------------------------------------------------------------------------------------------------------------
#outlier in blind10,50
p17.weired <- filter(play.2nd.7days, blindstate<100)%>%tbl_df()
p17.weired<-left_join(p27.weired, reg.1st.7days, by="channeluserid")
p27.weired.dcast <- dcast(p27.weired, channeluserid~platformtype, value.var = c("totalmypoint"), median, margins = T)
p27.weired.dcast[is.na(p27.weired.dcast)]<-0
nrow(p27.weired.dcast)
------------------------------------------------------------------------------------------------------------------------------
#mobility
  p27.blind.totalchip$num <- 0
  for (i in 1:nrow(p27.blind.totalchip))
  {
    for( j in 2:10)
    {
      if(p27.blind.totalchip[i,j]!=0){
        p27.blind.totalchip$num[i]<- p27.blind.totalchip$num[i]+1
      }
    }
  }
p27.blind.totalchip$channeluserid<-as.character(p27.blind.totalchip$channeluserid)
play.2nd.7days$channeluserid<-as.character(play.2nd.7days$channeluserid)
for( i in 1:8){
  assign(paste("p27.m", i, sep=""),filter(p27.blind.totalchip, num==i))
}
p27.m8.dist<-0
for (i in 2:10) {
  p27.m8.dist[i-1] <- length(which(p27.m8[,i]!=0))
}
p27.m8.dist
-------------------------------------------------------------------------------------------
#association 
p37.m2$channeluserid<-as.character(p37.m2$channeluserid)

p27.m6<-left_join(p27.m6, play.3rd, by = "channeluserid")

p27.m6.melt<- melt(p27.m6, id.vars = c("channeluserid", "blindstate"), measure.vars = c("totalmypoint"))
p27.m6.dcast <- dcast(p27.m6.melt, channeluserid+blindstate~variable, median)
p27.m6.dcast<-filter(p27.m6.dcast, totalmypoint>0)
rule.p27.m6 <- p27.m6.dcast[,1:2]
rule.p27.m6.list<-split(rule.p27.m6$blindstate, rule.p27.m6$channeluserid)
p17.m6.list<- list()
for (i in 1:length(rule.p17.m6.list)) {
  p17.m6.list[[i]] <- as.character(rule.p17.m6.list[[i]][!duplicated(rule.p17.m6.list[[i]])])
}

rule.p27.m6.trans<-as(rule.p27.m6.list, "transactions")
associ.p27.m6 <- apriori(rule.p27.m6.trans, parameter = list(support=0.1))
inspect(associ.p27.m6)
--------------------------------------------------------------------------------------------------------------------------
#buying user analysis
p37.buynplay <-left_join(buy.3rd, play.3rd, by = "channeluserid")%>%group_by(channeluserid)
p37.buynplay.totalmoney<- dcast(p37.buynplay, channeluserid~blindstate,value.var = c( "totalmypoint"),median)
p37.buynplay.totalmoney[is.na(p37.buynplay.totalmoney)]<-0
p37.buynplay.dealer<- dcast(p37.buynplay, channeluserid~blindstate,value.var = c( "dealerpoint"),sum)
p37.buynplay.totalmoney$num <- 0
for (i in 1:nrow(p37.buynplay.totalmoney))
{
  for( j in 2:12)
  {
    if(p37.buynplay.totalmoney[i,j]!=0){
      p37.buynplay.totalmoney$num[i]<- p37.buynplay.totalmoney$num[i]+1
    }
  }
}
p37.buy.mobility <- select(p37.buynplay.totalmoney, channeluserid, num)
buy.1st.7days<-left_join(buy.1st.7days, p27.buy.mobility, by="channeluserid")%>%group_by(num)
summarise(buy.3rd, payment = sum(payamount))
p17.summary <-summarise(play.1st.7days, playtime= mean(playtime), blindstate=max(blindstate), totalmypoint=median(totalmypoint), bettingpoint=mean(bettingpoint))
p17.test <- dcast(play.1st.7days, channeluserid~blindstate)
p17.blind.num<-tbl_df(p17.test)
p27.blind.num <- dcast(play.2nd.7days, channeluserid~blindstate)
p17.blind.num$total<-0
for( i in 1:nrow(p17.blind.num))
{
  p17.blind.num$total[i]<-sum(p17.blind.num[i,2:8])
}

p37.buy.summary<-summarise(p37.buynplay, playtime=sum(playtime), dealerpoint=sum(dealerpoint), totalmypoint=median(totalmypoint), getpoint=sum(getpoint), bettingpoint=sum(bettingpoint))
buy.3rd.all<- left_join(buy.3rd.summary, p37.buy.summary, by = "channeluserid")
p37.item.summary<-summarise(item.3rd, usepoint=sum(usepoint))

p37.buy.all <- left_join(buy.3rd.all, p37.item.summary, by="channeluserid")
p37.buy.all <- left_join(p37.buy.all, p3.character.summary, by="channeluserid")
p37.buy.all<-group_by(p37.buy.all, num)
p37.buy.all<-left_join(p37.buy.all, p37.buy.mobility, by = "channeluserid")
summarise(p37.buy.all, dealerpoint= sum(dealerpoint, na.rm =T), totalmoney=median(totalmypoint), usepoint.x=sum(usepoint.x,na.rm=T),usepoint.y=sum(usepoint.y,na.rm=T))
---------------------------------------------------------------------------------------------------------------------------------------
#reached 1M
p37.1m.melt<- melt(play.3rd, id.vars = c("channeluserid","date"), measure.vars = c("totalmypoint"))
p37.1m.melt1<- melt(play.3rd, id.vars = c("channeluserid","date"), measure.vars = c("dealerpoint"))
p37.1m.dcast<- dcast(p37.1m.melt, channeluserid+date~variable, median)
p37.1m.dcast1<- dcast(p37.1m.melt1, channeluserid+date~variable, sum)

p27.1m.dcast2<-dcast(play.2nd.7days, channeluserid+date~blindstate)
p27.1m.dcast2$num[1]<-1


for ( i in 2: nrow(p27.1m.dcast2))
{
  if(p27.1m.dcast2$channeluserid[i-1]!=p27.1m.dcast2$channeluserid[i]) {
    p27.1m.dcast2$num[i]<-1} else {
      p27.1m.dcast2$num[i]<-p27.1m.dcast2$num[i-1]+1
    }
}

p27.1m.dcast2$total<-0
for( i in 1:nrow(p27.1m.dcast2))
{
  p27.1m.dcast2$total[i]<-sum(p27.1m.dcast2[i,3:11])
}

p27.1m.total<- dcast(p27.1m.dcast2, channeluserid~num, value.var = c("total"))
p37.1m.ac.dcast1[is.na(p37.1m.ac.dcast1)]<-0


dau.p27.1m <- 0
for (i in 2:ncol(p27.1m.total)) {
dau.p27.1m[i-1] <- length(which(p27.1m.total[,i]!=0))
}
p17.1m.dcast2$mob<-0
for (i in 1:nrow(p17.1m.dcast2)) {
  p17.1m.dcast2$mob[i] <- length(which(p17.1m.dcast2[i,3:9]!=0))
}
dcast(p17.1m.dcast2, num~mob)

dau.p27.1m.sum <- 0
for (i in 2:ncol(p27.1m.total)) {
  dau.p27.1m.sum[i-1] <- sum(p27.1m.total[which(p27.1m.total[,i]!=0),i])
}
p37.1m.total <- left_join(p37.1m.dcast2, p37.1m.dcast)
p37.1m.total <-left_join(p37.1m.total, p37.1m.dcast1)
dcast(p37.1m.total, num~mob, value.var = c("totalmypoint"), mean)
dcast(p37.1m.total, num~mob, value.var = c("dealerpoint"), sum)

-------------------------------------------------------------------------------------------------------------------------------------------
#currency

play.1st.7days$currency1 <-0
for ( i in 1:nrow(play.1st.7days))
{
  if ( play.1st.7days$result[i]=='l') play.1st.7days$currency1[i]<-play.1st.7days$bettingpoint[i]
  else if( play.1st.7days$result[i]=='f') play.1st.7days$currency1[i]<-play.1st.7days$bettingoint[i]
  else if(play.1st.7days$result[i]=='w') play.1st.7days$currency1[i]<-play.1st.7days$getpoint[i]+play.1st.7days$bettingpoint[i]
}
#===========================================================================================
play.3rd$getpoint <- as.numeric(play.3rd$getpoint)

play.3rd$roic <- play.3rd$getpoint+play.3rd$bettingpoint
play.1st.7days$roic <- play.1st.7days$getpoint+play.1st.7days$bettingpoint
play.2nd.7days$roic <- play.2nd.7days$getpoint+play.2nd.7days$bettingpoint

play.3rd.roic <- summarise(play.3rd, roic = sum(roic), betting = sum(bettingpoint))
play.3rd.roic$roric.rate <- play.3rd.roic$roic/play.3rd.roic$betting
play.3rd.roic<-left_join(play.3rd.roic, p37.blind.totalchip, by = "channeluserid")
#======================================================================================
p27.consume <- dcast(play.2nd.7days, channeluserid~blindstate, value.var = c("bettingpoint"),sum)
playnum.p37 <- 0
for (i in 2:ncol(p37.blind)) {
  playnum.p37[i-1] <-sum(p37.blind[,i],na.rm=T)
}
-------------------------------------------------------------------------------------------------------
umcode.3rd<-tbl_df(umcode.3rd)
umcode.db<-read.csv("D:/4ones/task/data analystic/EDA/game structure/freechip_db_v1.csv", header = T)
umcode.db <- select(umcode.db, umcode, codename, 값)
p37.umcode <- dcast(umcode.3rd, umcode~date)
p37.umcode$total<-0
for( i in 1:nrow(p37.umcode))
{
  p37.umcode$total[i]<-sum(p37.umcode[i,2:8])
}
p37.umcode.v1<- left_join(p37.umcode, umcode.db)
p17.umcode.v1$값 <- as.numeric(p17.umcode.v1$값)
p17.umcode.v1$totalchip <- p17.umcode.v1$total*p17.umcode.v1$값
write.csv(p37.umcode.v1, file = "p37_umcode.csv")
p17.umcode.v1
um.timebonus <- filter(umcode.1st, umcode=='gdd08200')
um.timebonus.1st <- filter(umcode.1st, umcode=='gdd08200')
um.timebonus.2nd <- filter(umcode.2nd, umcode=='gdd08200')
um.timebonus.3rd <- filter(umcode.3rd, umcode=='gdd08200')
um.timebonus.1st.dcast <- dcast(um.timebonus.1st, channeluserid~umcode)
p17.1m.total<-group_by(p17.1m.total, channeluserid)
p17.1m.summary <-summarise(p17.1m.total, num=max(num), totalmypoint=mean(totalmypoint))
p17.summary <- left_join(p17.1m.summary, um.timebonus.1st.dcast)
um.quest.3rd <- filter(umcode.3rd, grepl("gdd180", umcode))


