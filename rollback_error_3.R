library(RODBC)
conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
date.810 <-read.csv("D:/4ones/task/date_810.csv", header=T)

sql_play.810<-c()
for ( i in 1:length(date.810)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,blindstate,, now_2, datedt from gholdem.TB_ParsingGameLog_",date.810[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.810<- c(sql_play.810, sql_2)
}
play.data.810<-data.frame()
for ( i in 1:length(sql_play.810)){
  play1 <- assign(paste("play810_",date.810[i],sep = ""),sqlQuery(conn_mssql, sql_play.810[i]))
  play.data.810<-rbind(play.data.810, play1)
}
play.data.810<-tbl_df(play.data.810)
#play.data.810<-play.data.810[,-c(7:8)]
play.data.810 <- arrange(play.data.810, channeluserid, now_2)
play.data.810$lvindex <- play.data.810$mylevel*1000000
play.data.810$seq <- play.data.810$lvindex+play.data.810$myexp

library(doParallel)
cl <- makeCluster(detectCores(),type = 'PSOCK')
registerDoParallel(cl)

play.data$num[1]<-1

for ( i in 2: nrow(play.data))
{
  if(play.data$channeluserid[i-1]!=play.data$channeluserid[i]) {
    play.data$num[i]<-1} else {
      play.data$num[i]<-play.data$num[i-1]+1
    }
}
play.810.uid <- unique(play.data.810$channeluserid)
play.error.uid.810 <-c()

for ( i in 1:length(play.810.uid)){
  ifelse(diff(filter(play.data.810, channeluserid==play.810.uid[i])[8]$seq)<0, play.error.uid.810<-c(play.error.uid.810, play.810.uid[i]),0)
}
play.error.810 <-data.frame()

for ( i in 1:length(play.error.uid.810))
{
  sample1 <- play.data.810[play.data.810$channeluserid==play.error.uid.810[i],]
  sample.data <-sample1[which(diff(play.data.810[play.data.810$channeluserid==play.error.uid.810[i],8]$seq)<0),]
  play.error.810<-rbind(play.error.810, sample.data)
}

# searching cff424aca48f48d3a09a2b0c5756fa02
sample1<- play.data[play.data$channeluserid=='cff424aca48f48d3a09a2b0c5756fa02',]
diff(sample1$seq)
play.error
