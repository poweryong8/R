conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
startdate <- as.Date("2016-08-01")
enddate <- as.Date("2016-08-31")
date_aug<- makeDate(startdate, enddate)
####play data####
sql_play.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,bettingpoint,playtime, blindstate,dealerpoint,result,win,lose,fold, allintype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.aug<- c(sql_play.aug, sql_2)
}
play.data.aug<-data.frame()
for ( i in 1:length(sql_play.aug)){
  play1 <- assign(paste("playaug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_play.aug[i]))
  play.data.aug<-rbind(play.data.aug, play1)
}
play.data.aug<-tbl_df(play.data.aug)
play.data.aug$channeluserid <- as.character(play.data.aug$channeluserid)
play.data.aug$result<- as.character(play.data.aug$result)
play.data.aug$now_2<- as.character(play.data.aug$now_2)
####buying data####
sql_buy.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,ravenitemcd,payamount, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 2 and logdetailid=1", sep=" ")
  sql_buy.aug<- c(sql_buy.aug, sql_2)
}
buy.data.aug<-data.frame()
for ( i in 1:length(sql_buy.aug)){
  buy1 <- assign(paste("buy.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_buy.aug[i]))
  buy.data.aug<-rbind(buy.data.aug, buy1)
}
####item_deco####
sql_item.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,itemidx, usepoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 4 and logdetailid=1", sep=" ")
  sql_item.aug<- c(sql_item.aug, sql_2)
}
item.data.aug<-data.frame()
for ( i in 1:length(sql_item.aug)){
  item1 <- assign(paste("item.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_item.aug[i]))
  item.data.aug<-rbind(item.data.aug, item1)
}
####item_char####
sql_char.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,characteridx, usepoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 5 and logdetailid=1", sep=" ")
  sql_char.aug<- c(sql_char.aug, sql_2)
}
char.data.aug<-data.frame()
for ( i in 1:length(sql_char.aug)){
  char1 <- assign(paste("char.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_char.aug[i]))
  char.data.aug<-rbind(char.data.aug, char1)
}
####randombox####
sql_random.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,boxidx, usepoint,usecash, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 108 and logdetailid=100", sep=" ")
  sql_random.aug<- c(sql_random.aug, sql_2)
}
random.data.aug<-data.frame()
for ( i in 1:length(sql_random.aug)){
  random1 <- assign(paste("random.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_random.aug[i]))
  random.data.aug<-rbind(random.data.aug, random1)
}
####freechip_attendent####
sql_attend.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,resulttype, resultcount, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 113 and logdetailid=100", sep=" ")
  sql_attend.aug<- c(sql_attend.aug, sql_2)
}
attend.data.aug<-data.frame()
for ( i in 1:length(sql_attend.aug)){
  attend1 <- assign(paste("attend.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_attend.aug[i]))
  attend.data.aug<-rbind(attend.data.aug, attend1)
}
####freechip_newuser####
sql_new.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,promotiongrade, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 112 and logdetailid=100", sep=" ")
  sql_new.aug<- c(sql_new.aug, sql_2)
}
new.data.aug<-data.frame()
for ( i in 1:length(sql_new.aug)){
  new1 <- assign(paste("new.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_new.aug[i]))
  new.data.aug<-rbind(new.data.aug, new1)
}

####freechip_request####
sql_req.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,umcode,getpoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 101 and logdetailid=101 and umcode IN('gdd12205','gdd12206','gdd12207','gdd12208')", sep=" ")
  sql_req.aug<- c(sql_req.aug, sql_2)
}
req.data.aug<-data.frame()
for ( i in 1:length(sql_req.aug)){
  req1 <- assign(paste("req.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_req.aug[i]))
  req.data.aug<-rbind(req.data.aug, req1)
}
########invite buddy#######
sql_invite.aug<-c()
for ( i in 1:length(date_aug)){
  sql_1<- paste("select channeluserid,hashed_talk_user_id, now_2, datedt from gholdem.TB_ParsingGameLog_",date_aug[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 6 and logdetailid=101", sep=" ")
  sql_invite.aug<- c(sql_invite.aug, sql_2)
}
invite.data.aug<-data.frame()
for ( i in 1:length(sql_invite.aug)){
  invite1 <- assign(paste("invite.aug_",date_aug[i],sep = ""),sqlQuery(conn_mssql, sql_invite.aug[i]))
  invite.data.aug<-rbind(invite.data.aug, invite1)
}
############pre-precess################
##########1.to tbl type
play.data.aug<-tbl_df(play.data.aug)
buy.data.aug<- tbl_df(buy.data.aug)
buy.data.aug<- group_by(buy.data.aug, channeluserid)
item.data.aug<- tbl_df(item.data.aug)
item.data.aug<- group_by(item.data.aug, channeluserid)
char.data.aug<- tbl_df(char.data.aug)
char.data.aug<- group_by(char.data.aug, channeluserid)
random.data.aug<- tbl_df(random.data.aug)
random.data.aug<- group_by(random.data.aug, channeluserid)
attend.data.aug<- tbl_df(attend.data.aug)
attend.data.aug<- group_by(attend.data.aug, channeluserid)
new.data.aug<- tbl_df(new.data.aug)
new.data.aug<- group_by(new.data.aug, channeluserid)
req.data.aug<- tbl_df(req.data.aug)
req.data.aug<- group_by(req.data.aug, channeluserid)
fold.data.aug <- filter(play.data.aug, result=='f')
invite.data.aug<- tbl_df(invite.data.aug)
invite.data.aug<- group_by(invite.data.aug, channeluserid)
invite.data.aug$channeluserid<- as.character(invite.data.aug$channeluserid)
invite.aug.summary$channeluserid<- as.character(invite.aug.summary$channeluserid)
###########2. summarise###############
play.aug.summary<- summarise(play.data.aug,blindstate=max(blindstate), result=length(result), betting=sum(bettingpoint), level=max(mylevel), playtime=sum(playtime))
play.aug.summary$channeluserid<-as.character(play.aug.summary$channeluserid)
buy.aug.summary <- summarise(buy.data.aug, num_buy = length(ravenitemcd), payment=sum(payamount)/100)
buy.aug.summary$channeluserid <- as.character(buy.aug.summary$channeluserid)
item.aug.summary <- summarise(item.data.aug, num_buy_item=length(itemidx), usepoint_item=sum(usepoint))
item.aug.summary$channeluserid<-as.character(item.aug.summary$channeluserid)
char.aug.summary <- summarise(char.data.aug, num_buy_char=length(characteridx), usepoint_char=sum(usepoint))
char.aug.summary$channeluserid<-as.character(char.aug.summary$channeluserid)
random.aug.summary <- summarise(random.data.aug, num_buy_random = length(boxidx), usepoint_random=sum(usepoint),usecash=sum(usecash))
random.aug.summary$channeluserid<-as.character(random.aug.summary$channeluserid)
attend.aug.summary <-summarise(attend.data.aug, num_attend=length(resultcount))
attend.aug.summary$channeluserid<-as.character(attend.aug.summary$channeluserid)
new.aug.summary<- summarise(new.data.aug, num_new=length(promotiongrade))
new.aug.summary$channeluserid<-as.character(new.aug.summary$channeluserid)
req.aug.summary <- summarise(req.data.aug, num_req=length(umcode))
req.aug.summary$channeluserid<-as.character(req.aug.summary$channeluserid)
fold.aug.summary <- summarise(fold.data.aug, fold = length(result))
invite.aug.summary <- summarise(invite.data.aug, try.invite=length(hashed_talk_user_id))
###########3. join all tables#############
aug.total.info<- left_join(play.aug.summary, buy.aug.summary)
aug.total.info1<- left_join(aug.total.info,item.aug.summary)
aug.total.info2<- left_join(aug.total.info1,char.aug.summary)
aug.total.info3<- left_join(aug.total.info2,random.aug.summary)
aug.total.info4<- left_join(aug.total.info3,attend.aug.summary)
aug.total.info5<- left_join(aug.total.info4,new.aug.summary)
aug.total.info6<- left_join(aug.total.info5,req.aug.summary)
aug.total.info6[is.na(aug.total.info6)]<-0
aug.total.info7<- left_join(aug.total.info6,fold.aug.summary)
aug.total.info8<- left_join(aug.total.info7,invite.aug.summary)
#####making binary for logit regression###########
for(i in 1:nrow(aug.total.info6)){
  if(aug.total.info6$num_buy[i]>0){
    aug.total.info6$logit[i]<-1
  }else{
    aug.total.info6$logit[i]<-0
  }
}

#########sampling#############
train_x <- c((nrow(aug.total.info8)*0.7))
train_ind <- sample(seq_len(nrow(aug.total.info8)),size = train_x, replace = FALSE)
train_set<- aug.total.info8[train_ind,]
test_set<-aug.total.info8[-train_ind,]

#########logistic############
formula_glm <- c("logit~blindstate+result+betting+level+playtime+num_buy_item+num_buy_char+usepoint+usepoint_char+num_buy_random+usepoint_random+usecash+num_attend+num_new+num_req+foldrate")
formula_glm.v1 <- c("logit~num_attend + num_new + blindstate + num_buy_char + level + betting + usepoint_char + usecash + result + playtime + usepoint+foldrate")
formula_glm.v2 <- c("logit ~ num_attend + num_new + level + foldrate + blindstate + betting + num_buy_char + usepoint_char + usecash + result + playtime+try.invite")
model_buy.possi.v2<- glm(formula_glm.v2, data=train_set, family = binomial(link = "logit"))
summary(model_buy.possi.v2)
#####selecting variables######
object<- glm(logit~1, family = binomial, data=train_set)
s.list<- list(upper=update(object,~.+blindstate+result+betting+level+playtime+num_buy_item+num_buy_char+usepoint+usepoint_char+num_buy_random+usepoint_random+usecash+num_attend+num_new+num_req+foldrate))
step(object, scope=s.list, direction="forward")
#####
library(pscl)
pscl::pR2(model_buy.possi.v2)
#R^2 0.159 , after putting foldrate 0.167, after putting foldrate 0.3196

library(ROCR)
p <- predict(model_buy.possi.v2, newdata=test_set, type="response")
table(p, test_set$logit)
pr <- prediction(p, test_set$logit)
prf<- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc<- auc@y.values[[1]]
auc
############svm###########
library(e1071)
model_svm <- e1071::svm(logit~num_attend + num_new + level + foldrate + blindstate + betting + num_buy_char + usepoint_char + usecash + result + playtime+try.invite, data=train_set)
pred_svm <- predict(model_svm, test_set)
table(pred_svm, test_set$logit)
####ensemble#########
library(caret)
library(caretEnsemble)
library(kernlab)
control <- caret::trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
test_model <- caretList(logit~num_attend + num_new + blindstate + num_buy_char + level + betting + usepoint_char + usecash + result + playtime + usepoint, data=train, trControl = control, methodList = 'svmRadial')
test_model1 <- caretList(logit~ blindstate  + level + betting +usecash + result + playtime + usepoint, data=train, trControl = control, methodList = 'svmRadial')
pred_model1<-predict(test_model, test_set)
##########deep learning############
