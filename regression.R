pre_ret <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/prediction_retention.csv", header= T)
head(pre_ret)
pre_ret_test <- pre_ret[,-1]
pre_ret_test
head(pre_ret_test)
cor(pre_ret_test)
lm.pre <- lm(retention_period ~ total_payment + total_payment.1 + total_playtime + num_login , data= pre_ret_test)
lm.pre <- lm(retention_period ~ total_payment + total_playtime + num_login , data= pre_ret_test)
shapiro.test(resid(lm.pre))
plot(lm.pre)
bptest(lm.pre)
library(lmtest)
library(sandwich)
coeftest(lm.pre, vcov. = vcovHC)
waldtest(lm.pre, vcov= vcovHC)
lm.glm <- glm(retention_period ~ total_payment + total_payment.1 + total_playtime + num_login , data= pre_ret_test, family = )

#retention
