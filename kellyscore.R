kelly <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/Global holdem/분석용/kelly.csv", header= T)
kelly.for<- function(b,p,q){
  x <- (b*p-q)/b
  return(x)
}
kelly <- kelly[,-1]
kelly.result<-c()

for(i in 1: length(kelly[,1])){
  temp<- NULL
  b <- kelly[i,1]
  p <- kelly[i,2]
  q <- kelly[i,3]
  temp <- kelly.for(b,p,q)
  kelly.result<-c(kelly.result,temp)
}
kelly.result <- cbind(kelly.result)
kelly$kelly <- kelly.result
write.csv(kelly, file = "kelly.score.csv", row.names = T)
