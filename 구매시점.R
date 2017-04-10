point.buy2 <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/Global holdem/분석용/구매시점 분석.csv", header= T)
head(point.buy)
buy2<- list()
last.buy <- list()
data <- c()
for( i in 1:length(point.buy$X1)){
      for (o in 2:length(point.buy[1,])) {
      if(point.buy[i,o]!=0){
      buy2[[i]]<- c(o)
      break
      }
    }
}
point.buy$first <- result
point.buy$first <- as.numeric(point.buy$first)
point.buy$frist <- point.buy$first-1
write.csv(point.buy, file = "buy_point.csv", row.names = T)
# last level users buy it.
for( i in 1:length(point.buy2$X1)){
  for (o in 2:length(point.buy2[1,])) {
    if(point.buy2[i,o]!=0){
      last.buy[[i]]<- c(o)
    }
  }
}
last <- cbind(last.buy)
point.buy$last <- last
point.buy$last <- as.numeric(point.buy$last)
point.buy$last <- point.buy$last-1
write.csv(point.buy, file = "buy2_point.csv", row.names = T)
