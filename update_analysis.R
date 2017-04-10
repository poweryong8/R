#after 0831
sql_regi<-c()

for ( i in 1:length(date.904)){
  sql_1<- paste("select channeluserid,level,mypoint,os_2,platformtype, now_2, datedt from gholdem.TB_ParsingGameLog_",date.904[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 1 and logdetailid=1", sep=" ")
  sql_regi<- c(sql_regi, sql_2)
}

regi.data<-data.frame()
for ( i in 1:length(sql_regi)){
  regi1 <- assign(paste("wau_",date.904[i],sep = ""),sqlQuery(conn_mssql, sql_regi[i]))
  regi.data<-rbind(regi.data, regi1)
}

date.901 <-read.csv("D:/4ones/task/date_901.csv", header=T)
date.904 <- date.901[1:4,]
#play data
sql_play<-c()

for ( i in 1:length(date.904)){
  sql_1<- paste("select channeluserid,playtime, bettingpoint, dealerpoint, blindstate, mylevel, datedt from gholdem.TB_ParsingGameLog_",date.904[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid= 2", sep=" ")
  sql_play<- c(sql_play, sql_2)
}

play.data<-data.frame()
for ( i in 1:length(sql_play)){
  play1 <- assign(paste("play_",date.904[i],sep = ""),sqlQuery(conn_mssql, sql_play[i]))
  play.data<-rbind(play.data, play1)
}
#pre-processing
play.data$channeluserid<-as.character(play.data$channeluserid)
play.data$bettingpoint <- as.numeric(play.data$bettingpoint)
regi.uid<- left_join(regi.uid, regi.data)
regi.uid<- regi.uid[,c(1,7)]
names(regi.uid)[2]<-c("regit.date")
regi.play <-left_join(regi.uid,play.data)

regit.melt <-melt(regi.play, id.vars = c("channeluserid","datedt"), measure.vars = "blindstate")
regi.play <- na.omit(regi.play)
blind.regi <-dcast(regi.play, channeluserid~blindstate)



#before 0831------------------------------------------------------------------------------------------------------------------------------
sql_regi.827<-c()

for ( i in 1:length(date.827)){
  sql_1<- paste("select channeluserid,os_2,platformtype, now_2, datedt from gholdem.TB_ParsingGameLog_",date.827[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 1 and logdetailid=1", sep=" ")
  sql_regi.827<- c(sql_regi.827, sql_2)
}

regi.827.data<-data.frame()
for ( i in 1:length(sql_regi.827)){
  regi1 <- assign(paste("reg827_",date.827[i],sep = ""),sqlQuery(conn_mssql, sql_regi.827[i]))
  regi.827.data<-rbind(regi.827.data, regi1)
}
regi.827.data$channeluserid<- as.character(regi.827.data$channeluserid)
#play data
sql_play.827<-c()

for ( i in 1:length(date.827)){
  sql_1<- paste("select channeluserid,playtime, bettingpoint, dealerpoint, blindstate, mylevel, datedt from gholdem.TB_ParsingGameLog_",date.827[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid= 2", sep=" ")
  sql_play.827<- c(sql_play.827, sql_2)
}

play.827.data<-data.frame()
for ( i in 1:length(sql_play.827)){
  play1 <- assign(paste("play_",date.827[i],sep = ""),sqlQuery(conn_mssql, sql_play.827[i]))
  play.827.data<-rbind(play.827.data, play1)
}
#pre-processing
play.827.data$channeluserid<-as.character(play.827.data$channeluserid)
play.827.data$bettingpoint <- as.numeric(play.827.data$bettingpoint)
regi.uid<- left_join(regi.uid, regi.data)
regi.827.uid<- regi.827.data[,c(1,5)]
names(regi.827.uid)[2]<-c("regit.date")
regi.play.827 <-left_join(regi.827.uid,play.827.data)

regit.melt <-melt(regi.play, id.vars = c("channeluserid","datedt"), measure.vars = "blindstate")
regi.play.827 <- na.omit(regi.play.827)
blind.regi.827 <-dcast(regi.play.827, channeluserid~blindstate)
blind.betting.901 <- dcast(regi.play, channeluserid~blindstate, value.var = "bettingpoint", mean)
blind.betting.827 <- dcast(regi.play.827, channeluserid~blindstate, value.var = "bettingpoint", mean)
write.csv(blind.regi.827, "blind_regi827.csv")
write.csv(blind.betting.827, "blind_betting827.csv")
