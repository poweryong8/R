spinfl_non<- read.csv("D:/4ones/task/programe_data/Global holdem/분석용/buying user analysis/inflection_nonbuy.csv", header= T)
head(infl)
xl <- seq(min(infl$X.400),max(infl$X.400), (max(infl$X.400) - min(infl$X.400))/100)
out <- predict(lo,xl)
lines(xl, out, col='red', lwd=2)
infls<- c(FALSE, diff(diff(out)>0)!=0)
xl[infls]
dy< -diff(infl3$X.400)
infl3$X.400
dy_non <- diff(infl_non$X.10000)
dy_non1 <- diff(dy_non)
which(dy_non1==0)

connect <- odbcConnectExcel("D:/4ones/task/programe_data/Global holdem/분석용/buying user analysis/sample12.xlsx")
require(xlsx)
file <- system.file("sample12.xlsx", package = "xlsx")
read.xlsx(file,1)
infltest10<- read.csv("D:/4ones/task/programe_data/Global holdem/분석용/buying user analysis/infl_test10.csv", header= T)
