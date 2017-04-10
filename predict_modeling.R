profit <- function(z) 0.000008*z^2-0.0309*z+119.17
cost <- function(z) 1035*log(z)-6335

z1<- seq(1000,50500,by=500)
x1<-seq(100,6000,by=50)

cost.result <- c()
for ( i in 1:length(z1))
{
  cost.result<-c(cost.result, cost(z1[i]))
}
cost.result

profit.result <- c()
for ( i in 1:length(z1))
{
  profit.result<-c(profit.result, profit(z1[i]))
}
profit.result

