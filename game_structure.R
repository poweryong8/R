conn_mssql1 <- odbcConnect("4ones", uid = "sa", pwd = "1234Qwer")

#writing quary
sql_registration<- "SELECT channeluserid, platformtype, os_2, now_2
from dbo.TB_LOG_AF_1208
where logid = 1 and logdetailid =1"

registration <- sqlQuery(conn_mssql1, sql_registration)

sql_access.june<- "SELECT channeluserid,os_2, mycash,mypoint,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 1 and logdetailid =2 and datedt >='2016-06-01'"

access <- sqlQuery(conn_mssql1, sql_access.june)

sql_play<- "SELECT channeluserid,playtime,dealerpoint,result, blindstate,blindtype,getpoint,prevtotalmypoint,totalmypoint,bettingpoint,umcode, now_2
from dbo.TB_LOG_AF_1208
where logid = 3 and logdetailid =2 "

play <- sqlQuery(conn_mssql1, sql_play)

sql_item<- "SELECT channeluserid,itemtype,level,itemidx,usecash,usepoint,umcode,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 4 and logdetailid =1"

item <- sqlQuery(conn_mssql1, sql_item)

sql_character<- "SELECT channeluserid,characteridx,level,usecash,usepoint,umcode,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 5 and logdetailid =1"

character <- sqlQuery(conn_mssql1, sql_character)

sql_social<- "SELECT channeluserid,socialtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 10 and logdetailid =100"

social <- sqlQuery(conn_mssql1, sql_social)

sql_social.june<- "SELECT channeluserid,socialtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 10 and logdetailid =100 and datedt >='2016-06-01'"


social.june <- sqlQuery(conn_mssql1, sql_social.june)
----------------------------------------------------------------------------------------------------------------------------------------
  registration <-tbl_df(registration)%>%group_by(channeluserid)
access <- tbl_df(access)%>%group_by(channeluserid)
item<-tbl_df(item)%>%group_by(channeluserid)
character <- tbl_df(character)%>%group_by(channeluserid)
play <- tbl_df(play)%>%group_by(channeluserid)
buy <- tbl_df(buy)%>%group_by(channeluserid)
social <- tbl_df(social)%>%group_by(channeluserid)
#pre-handling buy
buy$date <- as.Date(buy$now_2)
buy$yearmon <-as.yearmon(buy$date)
buy.1st <- filter(buy, date>'2015-12-07')%>%filter(date<'2015-12-23')
buy.1st.7days<-filter(buy.1st, date<'2015-12-15')
buy.2nd <- filter(buy, date>'2016-02-14')%>%filter(date<'2016-03-07')
buy.2nd.7days<-filter(buy.2nd, date.x<'2016-02-22')
buy.3rd <- filter(buy, date>'2016-06-07')

# pre-handling registration
registration$date <- as.Date(registration$now_2)
registration$yearmon <-as.yearmon(registration$date)
reg.1st <- filter(registration, date>'2015-12-07')%>%filter(date<'2015-12-23')
reg.1st.7days<-filter(reg.1st.v1, date<'2015-12-15')
reg.2nd <- filter(registration, date>'2016-02-14')%>%filter(date<'2016-03-07')
reg.2nd.7days<-filter(reg.2nd.v1, date<'2016-02-22')
reg.3rd <- filter(registration, date>'2016-06-07')
# pre-handling item
item$date <- as.Date(item$now_2)
item$yearmon <- as.yearmon(item$date)
access.1st <- filter(access, date>'2015-12-07')%>%filter(date<'2015-12-23')
access.1st.7days <- filter(access.1st, date<'2015-12-15')
access.2nd <- filter(access, date>'2016-02-14')%>%filter(date<'2016-03-07')
access.2nd.7days <- filter(access.2nd, date<'2016-02-22')
access.3rd <- filter(access, date>'2016-06-07')

access.1st.matrix <-dcast(access.1st, channeluserid~date)
access.2nd.matrix <-dcast(access.2nd, channeluserid~date)
access.3rd.matrix <-dcast(access.3rd, channeluserid~date)
# pre-handling play
play$date <- as.Date(play$now_2)
play$yearmon <- as.yearmon(play$date)
play.3rd$bettingpoint<-as.numeric(play.3rd$bettingpoint)

play.1st <- filter(play, date>'2015-12-07')%>%filter(date<'2015-12-23')
play.1st.7days <- filter(play.1st, date<'2015-12-15')
play.2nd <- filter(play, date>'2016-02-14')%>%filter(date<'2016-03-07')
play.2nd.7days <- filter(play.2nd, date<'2016-02-22')
play.3rd <- filter(play, date>'2016-06-07')

play.1st.melt <- melt(play.1st, id.vars = c("blindstate"), measure.vars = c("dealerpoint", "bettingpoint"))
play.1st.dcast <- dcast(play.1st.melt, blindstate~variable,mean)
play.2nd.melt <- melt(play.2nd, id.vars = c("blindstate"), measure.vars = c("dealerpoint", "bettingpoint"))
play.2nd.dcast <- dcast(play.2nd.melt, blindstate~variable,sum)
play.3rd.melt <- melt(play.3rd, id.vars = c("blindstate"), measure.vars = c("dealerpoint", "bettingpoint"))
play.3rd.dcast <- dcast(play.3rd.melt, blindstate~variable,sum)

# pre-handling item
item$date <- as.Date(item$now_2)
item$yearmon <-as.yearmon(item$date)
item.1st <- filter(item, date>'2015-12-07')%>%filter(date<'2015-12-23')
item.1st.7days <- filter(item.1st, date<'2015-12-15')
item.2nd <- filter(item, date>'2016-02-14')%>%filter(date<'2016-03-07')
item.2nd.7days <- filter(item.2nd, date<'2016-02-22')
item.3rd <- filter(item, date>'2016-06-07')

# pre-handling character
character$date <- as.Date(character$now_2)
character$yearmon <-as.yearmon(character$date)
character.1st <- filter(character, date>'2015-12-07')%>%filter(date<'2015-12-23')
character.1st.7days <- filter(character.1st, date<'2015-12-15')
character.2nd <- filter(character, date>'2016-02-14')%>%filter(date<'2016-03-07')
character.2nd.7days <- filter(character.2nd, date<'2016-02-22')
character.3rd <- filter(character, date>'2016-06-07')

# pre-handling social
social$date <- as.Date(social$now_2)
social$yearmon <-as.yearmon(social$date)
social.1st <- filter(social, date>'2015-12-07')%>%filter(date<'2015-12-23')
social.2nd <- filter(social, date>'2016-02-14')%>%filter(date<'2016-03-07')
social.3rd <- filter(social, date>'2016-06-07')
----------------------------------------------------------------------------------------------------------------------------------------
#finding new approach of ARPDAU 
dau.2nd <- 0
for (i in 1:ncol(access.2nd.matrix)) {
  dau.2nd[i] <- length(which(access.2nd.matrix[,i+1]!=0))
}
blind.dau3 <-0
for (i in 1:ncol(play.3rd.blind)) {
  blind.dau3[i] <- length(which(play.3rd.blind[,i+1]!=0))
}

date.3rd <-unique(access.3rd$date)

unique.uid.3rd<- unique(filter(access.3rd, date==date.3rd[1])%>%select(channeluserid))
unique.3rd <- nrow(unique(filter(access.3rd, date==date.3rd[1])%>%select(channeluserid)))

for (i in 2:length(date.3rd))
{
  unique.uid.3rd2<- unique(filter(access.3rd, date==date.3rd[i])%>%select(channeluserid))
  unique.uid.3rd <- unique(rbind(unique.uid.3rd, unique.uid.3rd2))
  unique.3rd[i]<- nrow(unique.uid.3rd)
   
}

intersect.1st.2nd <- intersect(play.1st.uid, play.2nd.uid)
intersect.2nd.3rd <- intersect(play.3rd.uid, play.2nd.uid)
intersect.1st.3rd <- intersect(play.3rd.uid, play.1st.uid)
intersect.all <- intersect(intersect.1st.2nd, play.3rd.uid)
intersect.all<-tbl_df(as.data.frame(intersect.all))
names(intersect.all)[1]<-c("channeluserid")
intersect.all <-left_join(intersect.all, access.summary, by ="channeluserid")
# spliting up channeluserid through the blindstate
play.3rd.blind <- dcast(play.3rd, channeluserid~blindstate)
play.3rd.blind<-tbl_df(play.3rd.blind)

play.3rd.blind$num <- 0

for (i in 1:nrow(play.3rd.blind))
{
  for( j in 2:8)
  {
    if(play.3rd.blind[i,j]!=0){
      play.3rd.blind$num[i]<- play.3rd.blind$num[i]+1
    }
  }
}

buy.3rd <- left_join(buy.3rd, registration, by="channeluserid")
buy.3rd$du.date <- buy.3rd$date.x-buy.3rd$date.y
# split up into blind
blind.3rd <- sort(unique(play.3rd$blindstate))

for ( i in 1:length(blind.3rd))
{
  assign(paste("play.3rd.b",i, sep = ""),filter(play.3rd, blindstate== blind.3rd[i]))
}

play.3rd.b11.wfl<- dcast(play.3rd.b11, channeluserid~result)
play.3rd.b11.wfl$sum <- play.3rd.b11.wfl$f+play.3rd.b11.wfl$l+play.3rd.b11.wfl$w
play.3rd.b11.wfl$foldrate <- play.3rd.b11.wfl$f/play.3rd.b11.wfl$sum

nrow(play.3rd.b10.wfl)
play.3rd.b9.wfl<-play.3rd.b9.wfl[-55,]

play.3rd.b11.summary<- summarise_each(play.3rd.b11, funs(sum,sd,mean, median), playtime, dealerpoint, getpoint, totalmypoint, bettingpoint)

play.3rd.b11.reg <-left_join(play.3rd.b11.summary, registration.v1, by = "channeluserid")%>%group_by(channeluserid)
play.1st.b2.totalndate<- select(play.1st.b2.reg, channeluserid, totalmypoint_median, date,os_2, platformtype)
summarise_each(play.1st.b2.totalndate,funs(mean, median, sd), totalmypoint_median)
table(play.2nd.b3.totalndate$platformtype)
----------------------------------------------------------------------------------------------------------------------------------------------
#acqusition user analysis
play.2nd.b2.reg<- play.2nd.b2.reg[,-24]
play.2nd.b2.reg.v1<- unique(play.2nd.b2.reg)
table(play.2nd.b1.reg.v1$yearmon)
filter(play.3rd.b11.reg, date>'2016-06-07')
------------------------------------------------------------------------------------------------------------------------------
play.3rd.b1.melt<- melt(play.3rd.b1, id.vars = c("channeluserid","date"), measure.vars = c("totalmypoint"))
play.3rd.b1.dcast<- dcast(play.3rd.b1.melt, channeluserid+date~variable, max)
play.3rd.b1.dcast<-left_join(play.3rd.b1.dcast, reg.3rd.v1, by = "channeluserid")%>%tbl_df()
play.3rd.b1.dcast.ac <- filter(play.3rd.b1.dcast, date.y>0)

play.3rd.b1.dcast.ac$du.date <- (play.3rd.b1.dcast.ac$date.x-play.3rd.b1.dcast.ac$date.y)+1
dcast(play.3rd.b1.dcast.ac, channeluserid~num, value.var = "totalmypoint")
dcast(play.3rd.b1.dcast.ac, channeluserid+date~totalmypoint, value.var = "totalmypoint")
dcast(play.3rd.regnplay, channeluserid+date~totalmypoint)
play.3rd.b1.dcast.ac$num[1]<-1

  
for ( i in 2: nrow(play.3rd.b1.dcast.ac))
  {
  if(play.3rd.b1.dcast.ac$channeluserid[i-1]!=play.3rd.b1.dcast.ac$channeluserid[i]) {
    play.3rd.b1.dcast.ac$num[i]<-1} else {
    play.3rd.b1.dcast.ac$num[i]<-play.3rd.b1.dcast.ac$num[i-1]+1
    }
}
----------------------------------------------------------------------------------------------
#moblity check
play.1st.mobility.totalchip <- dcast(play.1st, channeluserid~blindstate, value.var = c("totalmypoint"),median)%>%tbl_df()
play.1st.mobility.totalchip$num <- 0

for (i in 1:nrow(play.1st.mobility.totalchip))
{
  for( j in 2:8)
  {
    if(play.1st.mobility.totalchip[i,j]!=0){
      play.1st.mobility.totalchip$num[i]<- play.1st.mobility.totalchip$num[i]+1
    }
  }
}
play.1st.mobility.totalchip$total<-0
for ( i in 1:nrow(play.1st.mobility.totalchip))
{
  play.1st.mobility.totalchip$total[i] <- sum(play.1st.mobility.totalchip[i,2:10])
}
play.1st.mobility.totalchip<-group_by(play.1st.mobility.totalchip, num)
summarise_each(play.1st.mobility.totalchip, funs(sum,mean), total)

---------------------------------------------------------------------------------------------------------------------
#3rd buying user who were not ac
play.3rd.melt.v2<- melt(play.3rd, id.vars = c("channeluserid", "blindstate"), measure.vars = c("dealerpoint"))
play.3rd.dcast.v2 <- dcast(play.3rd.melt.v2, channeluserid+blindstate~variable,sum)
reg.3rd.play$num[1]<-1

for ( i in 2: nrow(reg.3rd.play))
{
  if(reg.3rd.play$channeluserid[i-1]!=reg.3rd.play$channeluserid[i]) {
    reg.3rd.play$num[i]<-1} else {
      reg.3rd.play$num[i]<-reg.3rd.play$num[i-1]+1
    }
}
buy.3rd.na<- select(buy.3rd.na, channeluserid, ravenitemcd, payamount, date.x)
buy.3rd.notac <- select(buy.3rd.notac, channeluserid, ravenitemcd, payamount, date.x)
buy.3rd.notac <-left_join(buy.3rd.notac, play.3rd.dcast.v2, by = "channeluserid")%>%group_by(channeluserid)

