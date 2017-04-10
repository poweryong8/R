get52 <- filter(data_gameend, date=='2016-05-02')
get52 <- group_by(get52,channeluserid)
get52.blind<- dcast(get52,channeluserid~blindstate)
get52.blind$num <- 0

for (i in 1:nrow(get52.blind))
{
  for( j in 2:10)
  {
    if(get52.blind[i,j]!=0){
      get52.blind$num[i]<- get52.blind$num[i]+1
    }
  }
}
get52.blind1<-filter(get52.blind, num==1)
get52.blind2<-filter(get52.blind, num>1)
cluster.may <- read.csv("D:/4ones/task/data analystic/currency/cluster_may.csv", header = T)
cluster.may <- tbl_df(cluster.may)
cluster.may$channeluserid <- as.character(cluster.may$channeluserid)
get52.blind1<- left_join(get52.blind1, cluster.may, by="channeluserid")
get52.blindstate <- select(get52, channeluserid, blindstate)
#dcast cluster 0 and cluster 3
get52.cluster1 <- filter(get52.blind1, cluster=="cluster_1")

get52.cluster3$play <- 0
for (i in 1:nrow(get52.blind2))
{
  get52.blind2$sum[i]<- sum(get52.blind2[i,2:10])
  
}

get52.melt1 <- melt(get52.cluster0, id.vars = c("blindstate"), measure.vars = c("bettingpoint_mean","getpoint_mean","foldrate", "sum"))
get52.melt2 <- melt(get52.cluster3, id.vars = c("blindstate"), measure.vars = c("bettingpoint_mean","getpoint_mean","foldrate", "sum"))
get52.melt3 <- melt(get52.cluster1, id.vars = c("blindstate"), measure.vars = c("bettingpoint_mean","getpoint_mean","foldrate", "sum"))
dcast(get52.melt3, blindstate~variable, mean, na.rm=T)

blind2.uid <- select(get05.blind2, channeluserid) 
blind2.uid <- left_join(blind2.uid, get05) 
blind2.uid<-group_by(blind2.uid, channeluserid) 
blind2.uid<- summarise(blind2.uid, min.blind = min(blindstate), max.blind=max(blindstate))

access.melt <- melt(access.during.marketing, id.vars = c("date"), measure.vars = c("count"))
