library(dplyr)
library(reshape2)


#login.v1$now_2 <- as.Date(login.v1$now_2)
#login.v1 <- mutate(login.v1, du.date=(now_2-min.date)+1)
#login.v1 <- left_join(login.v1, char.user)

#comparing non.churn users with churn users at only churn day
churn7<- filter(join.t,duration==7)
churn7<-group_by(churn7, channeluserid)
churn7 <- filter(churn7, du.date==7)
churn7.1 <- summarise(churn7, bindstate = max(blindstate), avg.playtime = mean(playtime),total.playtime=sum(playtime) ,max.point = max(mypoint), ave.point= mean(mypoint), ave.betting = mean(bettingpoint))
non.churn7<-filter(join.t, duration>7)
non.churn7<- group_by(non.churn7, channeluserid)
non.churn7 <- filter(non.churn7, du.date==7)
non.churn7.1 <- summarise(non.churn7, bindstate = max(blindstate), avg.playtime = mean(playtime),total.playtime=sum(playtime) ,max.point = max(mypoint), ave.point= mean(mypoint), ave.betting = mean(bettingpoint))
#sampling data frame
sample.non7 <- non.churn7.1[sample(nrow(non.churn7.1),1138),]

#cumlative ones
cum.churn2<- filter(join.t,duration==2)
cum.churn2<-group_by(cum.churn2, channeluserid)
cum.churn2.1 <- summarise(cum.churn2, bindstate = max(blindstate), avg.playtime = mean(playtime),total.playtime=sum(playtime) ,max.point = max(mypoint), ave.point= mean(mypoint), ave.betting = mean(bettingpoint))
cum.non.churn2<-filter(join.t, duration>2)
cum.non.churn2<- group_by(cum.non.churn2, channeluserid)
cum.non.churn2 <- filter(cum.non.churn2, du.date<=2)
cum.non.churn2.1 <- summarise(cum.non.churn2, bindstate = max(blindstate), avg.playtime = mean(playtime),total.playtime=sum(playtime) ,max.point = max(mypoint), ave.point= mean(mypoint), ave.betting = mean(bettingpoint))
#sampling data frame
cum.sample.non2 <- non.churn2.1[sample(nrow(non.churn2.1),5199),]

colMeans(sample.non3[,2:7])
t.test(sample.non7$avg.playtime, churn7.1$avg.playtime)
t.test(sample.non7$total.playtime, churn7.1$total.playtime)
t.test(sample.non7$ave.betting, churn7.1$ave.betting)
t.test(sample.non7$ave.point, churn7.1$ave.point)
churn1.1$churn <- 0
sample.non1$churn <- 1
regression2 <- rbind(churn2.1, sample.non2)
regression1$churn<-as.factor(regression1$churn)
model1<- glm(formula = churn~avg.playtime+total.playtime+max.point+ave.point, family = "binomial", data = regression)
summary(model1)
m.list1<- list(upper=update(model1, ~.+bindstate+avg.playtime+total.playtime+max.point+ave.point+ave.betting))
step(model1, scope = m.list1, direction = "backward")

equation <- churn~avg.playtime+total.playtime+ave.point+ave.betting
intrumental <- ~avg.playtime+total.playtime+ave.point+ave.betting
library(gmm)
gmm.churn1 <- gmm(equation, intrumental, family= binomial(link = logit), data = regression1, na.action=na.pass)
summary(gmm.churn1)
user.inform <- dcast(data_login, channeluserid+devicecd+os_2~platformtype)
user.inform.1 <- user.inform[,1:3]
user.inform.1<- tbl_df(user.inform.1)

uniq.du <- unique(char.user$duration)
for ( i in 1:length(uniq.du))
{
  assign(paste("du", i, sep=""),filter(login.v2, duration == i))
}
