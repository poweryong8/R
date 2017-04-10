
library(doParallel)
cl <- makeCluster(detectCores(),type = 'PSOCK')
registerDoParallel(cl)

logout.af.aug$num[1]<-1

for ( i in 2: nrow(logout.af.aug))
{
  if(logout.af.aug$channeluserid[i-1]!=logout.af.aug$channeluserid[i]) {
    logout.af.aug$num[i]<-1} else {
    logout.af.aug$num[i]<-logout.af.aug$num[i-1]+1
    }
}
logout.af.aug$seq <- logout.af.aug$level+logout.af.aug$num
logout.af.uid <- unique(logout.af.aug$channeluserid)
af.aug.uid <-c()
for ( i in 1:length(logout.af.uid)){
  ifelse(diff(filter(logout.af.aug, channeluserid==logout.af.uid[i])[9]$seq)<=0, af.aug.uid<-c(af.aug.uid, logout.af.uid[i]),0)
}
logout.overmay.v1[logout.overmay.v1$channeluserid=='85679a0456fd4868b2fa514666cddef6',]
as.data.frame(logout.overmay.v1[logout.overmay.v1$channeluserid=='85679a0456fd4868b2fa514666cddef6',])
#after 8/3 
date.801 <-read.csv("D:/4ones/task/date_801.csv", header=T)

sql_logout<-c()

for ( i in 1:nrow(date.801)){
  sql_1<- paste("select channeluserid,level,mypoint,os_2,platformtype, now_2, datedt from gholdem.TB_ParsingGameLog_",date.801[i,1],sep = "")
  sql_2 <-paste(sql_1, "where logid = 1 and logdetailid=3", sep=" ")
  sql_logout<- c(sql_logout, sql_2)
}
logout.data<-data.frame()
for ( i in 1:length(sql_logout)){
  logout1 <- assign(paste("logout_",date.801[i,1],sep = ""),sqlQuery(conn_mssql, sql_logout[i]))
  logout.data<-rbind(logout.data, logout1)
}
