get05.melt <- melt(get05, id.vars = c("channeluserid", "blindstate"), measure.vars = c("bettingpoint"))
get05.melt1 <- melt(get05, id.vars = c("channeluserid", "blindstate"), measure.vars = c("getpoint"))
get05.dcast1 <- dcast(get05.melt, channeluserid+blindstate~variable, sum)
get05.get <- dcast(get05.melt1, channeluserid+blindstate~variable, sum)
get05.result <-melt(get05, id.vars = c("channeluserid", "result"), measure.vars = c("bettingpoint"))
result <-dcast(get05, channeluserid~result)
get05.dcast2 <- dcast(get05.result, channeluserid+result~variable, mean)
left_join(get05.wfl,get05.dcast1)
get05.all$totalplay <- get05.all$f+get05.all$w+get05.all$l

get52.blind1$num <- 0

for (i in 1:nrow(get52.blind1))
{
  for( j in 2:10)
  {
    if(get52.blind1[i,j]!=0){
      get52.blind1$num[i]<- get05.blind$num[i]+1
    }
  }
}
sp5 <-get05[get05$channeluserid==get05.blind1$channeluserid[5],]
msp1 <-get05[get05$channeluserid==get05.blind2$channeluserid[3],]
msp1<- select(msp1,channeluserid, result, blindstate,blindtype, getpoint, bettingpoint, prevtotalmypoint, totalmypoint, now_2, earnrate )


get52.blind1$play <- 0
for (i in 1:nrow(get52.blind1))
{
  get52.blind1$sum[i]<- sum(get52.blind1[i,2:10])
  
}

get05$wi <- get05$bettingpoint/get05$blindstate
get05$earnrate <- ifelse(get05$bettingpoint!=0, (get05$getpoint/get05$bettingpoint)*get05$wi, 0)

get05.wfl$totalmatch <- get05.wfl$f+get05.wfl$l+get05.wfl$w
get05.wfl$foldrate<- ifelse(get05.wfl$f!=0,get05.wfl$f/get05.wfl$totalmatch,0)
get05.charcter <- summarise(get05,avg.wi =mean(wi),total.get =sum(getpoint))
get05.charcter<- left_join(get05.charcter, result)
get05.charcter$totalmatch<- get05.charcter$w+get05.charcter$l+get05.charcter$f
get05.charcter$foldrate<- get05.charcter$f/get05.charcter$totalmatch

# data on may
may<-filter(data_gameend, date>'2016-04-30')
may<-group_by(may, channeluserid)
may$wi <- may$bettingpoint/may$blindstate
may.cluster <- select(may, channeluserid, getpoint, bettingpoint,wi)
may.cluster <- summarise_each(may.cluster,funs(mean, sum), getpoint, bettingpoint, wi)
may.wfl <- dcast(may, channeluserid~result)
may<- left_join(may.cluster,may.wfl)
may.cluster$foldrate <- ifelse(may.cluster$ may.cluster$f/may.cluster$totalplay)

may.cluster$play<- may.cluster$w+may.cluster$l+may.cluster$f)

cluster_may <- read.csv("D:/4ones/task/data analystic/currency/cluster_may.csv", header = T)
