getpoint <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/payuser_Fab.csv", header= T)
head(getpoint)
getpoint$uid <- NULL
getpoint
getpoint_lm <- lm(total_point ~ total_payment+level, data=getpoint)
summary(getpoint_lm)
#paired test

p.test <- read.csv("C:/Users/fourones_jsw/Documents/task/programe_data/pairedttest_test.csv", header= T)
head(p.test)
#variables: playtime
t.test(p.test$playtime_2, p.test$playtime_3, paired = T)
#variables: payment
t.test(p.test$pay_2, p.test$pay_3, paired = T)
#variables: point
t.test(p.test$point_2, p.test$point_3, paired = T)
#variables: level
t.test(p.test$level_2, p.test$level_3, paired = T)
# if it is not normal distribution, use wilcox.test, below case is only groups are  paired group.
wilcox.test(p.test$pay_2,p.test$pay_3, paired = T)

