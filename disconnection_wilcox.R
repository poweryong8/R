discon <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/groupby_disconnected.csv", header= T)
con<-read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/groupby_connected.csv", header= T)
head(discon)
#every variables is not normal distribution, so we use wilcox text.
shapiro.test(sha_discon$total_payment)
shapiro.test(sha_discon$totalpoint)
shapiro.test(sha_discon$total_playtime)

#wilcox
wilcox.test(discon$total_payment, con$total_payment)
wilcox.test(discon$totalpoint, con$totalpoint)
wilcox.test(discon$total_playtime, con$total_playtime)

#lm
reg_con <- lm(total_playtime ~ num_dis_2+I(num_dis_2^2), data=discon)
plot(reg_con)
# after heteroscadasticity check, we recaculate S.E
library(sandwich)
coeftest(reg_con, vcov. = vcovHC)
waldtest(reg_con, vcov= vcovHC)
