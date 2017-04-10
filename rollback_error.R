conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
date.904 <-read.csv("D:/4ones/task/date_904.csv", header=T)

sql_play.904<-c()
for ( i in 1:length(date.904)){
  sql_1<- paste("select channeluserid,allintype,mylevel,totalmypoint,myexp, now_2, datedt from gholdem.TB_ParsingGameLog_",date.904[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.904<- c(sql_play.904, sql_2)
}
play.904.data<-data.frame()
for ( i in 1:length(sql_play.904)){
  play1 <- assign(paste("play904_",date.904[i],sep = ""),sqlQuery(conn_mssql, sql_play.904[i]))
  play.904.data<-rbind(play.904.data, play1)
}
play.data<-tbl_df(play.data)
play.904.data <- arrange(play.904.data, channeluserid, now_2)


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
play.904.data <- arrange(play.904.data, channeluserid, now_2)
play.904.data.v1$lvindex <- play.904.data$mylevel*1000000
play.904.data.v1$seq <- play.904.data.v1$lvindex+play.904.data.v1$myexp

play.904.uid <- unique(play.904.data.v1$channeluserid)
play.error.uid.904 <-c()
for ( i in 1:length(play.904.uid)){
  ifelse(diff(filter(play.904.data.v1, channeluserid==play.904.uid[i])[8]$seq)<0, play.error.uid.904<-c(play.error.uid.904, play.904.uid[i]),0)
}

play.error.904 <-data.frame()

for ( i in 1:length(play.error.uid.904))
{
  sample1 <- play.data[play.904.data.v1$channeluserid==play.error.uid.904[i],]
  sample.data <-sample1[which(diff(play.904.data.v1[play.904.data.v1$channeluserid==play.error.uid.904[i],8]$seq)<0),]
  play.error.904<-rbind(play.error.904, sample.data)
}

# searching cff424aca48f48d3a09a2b0c5756fa02
sample1<- play.data[play.data$channeluserid=='cff424aca48f48d3a09a2b0c5756fa02',]
diff(sample1$seq)
write.csv(play.error.uid, "rollback_error_v2.csv")
play.error.v1 <-filter(play.error, mylevel!=100)
