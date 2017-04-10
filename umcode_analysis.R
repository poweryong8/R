invite0601
invitebigger0601<- filter(invite, date>'2016-05-31')
face0603
invite0603 <- filter(invite0601, date=='2016-06-03')d
invite0604 <- filter(invite0601, date=='2016-06-04')
invite0601 <- filter(invite0601, date=='2016-06-01')

for (i in 1:nrow(table.invite))
{
  for( j in 2:6)
  {
    if(table.invite[i,j]!=0){
      table.invite$num[i]<- table.invite$num[i]+1
    }
  }
}

conn_mssql1 <- odbcConnect("4ones", uid = "sa", pwd = "1234Qwer")

#writing quary
sql_play<- "SELECT channeluserid,playtime, blindstate,result,getpoint,bettingpoint,dealerpoint, prevtotalmypoint,totalmypoint, now_2
from dbo.TB_LOG_AF_1208
where logid = 3 and logdetailid =2 and datedt > '2016-05-31'"

data.play <- sqlQuery(conn_mssql1, sql_play)
data.play<-group_by(data.play, channeluserid)
data.play$date <- as.Date(data.play$now_2)

table.invite$channeluserid <- as.character(table.invite$channeluserid)
data.play.2$channeluserid<-as.character(data.play.2$channeluserid)
join.play2 <- left_join(table.invite, data.play.2, by="channeluserid")
join.play2

play0604<-filter(data.play, date=='2016-06-04')
play0605.2 <- summarise(play0605, avg.playtime=mean(playtime), min.blind=min(blindstate), max.blind=max(blindstate), avg.betting=mean(bettingpoint), avg.getpoint=mean(getpoint), avg.dealer=mean(dealerpoint), total.dealer=sum(dealerpoint), avg.totalpoint = mean(totalmypoint))
access0601<-filter(table.invite, `2016-06-01`>0)
join.0605 <- left_join(access0605, play0605.2, by="channeluserid")
mean(join.0605$avg.totalpoint, na.rm =T)
median(join.0605$avg.dealer, na.rm =T)
sd(join.0604$avg.betting, na.rm =T)

play0605.wfl <- dcast(play0605, channeluserid~result)
play0605.wfl$total <- play0605.wfl$f+play0605.wfl$w+play0605.wfl$l
play0603.wfl$foldrate <- play0603.wfl$f/play0603.wfl$total
join.0605<-left_join(join.0605, play0605.wfl, by="channeluserid")
mean(join.0605$total, na.rm =T)
mean(play0603.wfl$foldrate, na.rm =T)
median(join.0603$total, na.rm =T)
#socialtype help
data.reg<-group_by(data.reg, channeluserid)
help6 <- filter(data.reg, socialtype==6)
help6.0605 <- filter(help6, date=='2016-06-05')
help6.0605 <- dcast(help6.0601, channeluserid~socialtype)
help0605.join<-left_join(help0605.join, play0605.2, by ="channeluserid")
help0605.join<-left_join(help6.0605, play0605.wfl, by ="channeluserid")
mean(help0605.join$avg.totalpoint.y, na.rm = T)
mean(help0604.join$avg.dealer, na.rm = T)
mean(help0605.join$total, na.rm = T)
median(help0605.join$avg.betting, na.rm = T)
median(help0605.join$avg.dealer, na.rm = T)
#registration
help.06 <- filter(data.reg, date>'2016-05-31')
help.0605 <- filter(data.reg, date=='2016-06-05')
help.0605.1<-dcast(help.0605, channeluserid~socialtype, margins = )

invite.0605<-filter(help.0605.1, `1`>0)
help8.0605 <- left_join(help8.0605, registration, by ="channeluserid")
help8.0605$yearmon <- as.yearmon(help8.0605$date)
table(help8.0605$yearmon)

invite0605<-c()
for( i in 1:(ncol(invite.0605)-1)){
    invite0605[i] <- length(which(invite.0605[,i+1] !=0))
 }
#total access
t0605 <- filter(data.play, date=='2016-06-05')
t0605 <- left_join(t0605, registration, by="channeluserid")
t0605$yearmon <- as.yearmon(t0605$date.y)
t0605<-group_by(t0605,channeluserid)
t0605.1<-summarise(t0605, yearmon = max(yearmon) )
table(t0605.1$yearmon)
#help-7
help80605.join<-left_join
(help8.0605, play0605.2, by ="channeluserid")
help80605.join<-left_join(help80605.join, play0605.wfl, by ="channeluserid")
nrow(help80605.join)
mean(help80605.join$avg.betting, na.rm = T)
median(help80605.join$avg.betting, na.rm = T)
mean(help80605.join$avg.dealer, na.rm = T)
median(help80605.join$avg.dealer, na.rm = T)
mean(help80605.join$total, na.rm = T)
mean(help80605.join$avg.totalpoint, na.rm = T)
median(help0605.join$avg.betting, na.rm = T)
median(help0605.join$avg.dealer, na.rm = T)