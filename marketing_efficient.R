impression <-read.csv("D:/4ones/task/data analystic/EDA/marketing/impression.csv", header=T)
impression <- tbl_df(impression)
for( i in 1:13){
  impression.v1[,i]<-as.numeric(impression.v1[,i])
}
