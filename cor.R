analysis<- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/Global holdem/분석용/buying user analysis/previous_activities_before_buying.csv", header= T)
analysis
ana.test <- analysis[,-1]
summary(analysis)
head(ana.test)
cor(ana.test)
cor.test(ana.test$first_payment,ana.test$cum_getpoint)
test.1<- lm( first_payment ~ num_game+ playtime+num_allin+win+lose+fold+cum_getpoint+cum_battingpoint+b+p+kelly+slope, data = ana.test)
library(lmtest)
library(sandwich)
bptest(test.1)
summary(test.1)
coeftest(test.1, vcov. = vcovHC)
waldtest(test.1, vcov= vcovHC)
library(gmm)
attach(ana.test)
equation <- first_payment ~ num_game+ playtime+num_allin+win+lose+fold+cum_getpoint+cum_battingpoint+b+p+kelly+slope
ist.var <- ~num_game+ playtime+num_allin+win+lose+fold+cum_getpoint+cum_battingpoint+b+p+kelly+slope
gmm.res <- gmm(g=equation, x=ist.var, data = ana.test)
detach(ana.test)
summary(gmm.res)
AIC(test.1)
AIC(gmm.res)
