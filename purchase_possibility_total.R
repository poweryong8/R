conn_mssql <- odbcConnect("gholdem", uid = "BI_BS_Admin", pwd = "cjsqortlqdlf1111")
startdate <- as.Date("2016-08-01")
enddate <- as.Date("2016-09-30")
date_total<- makeDate(startdate, enddate)
####play data####
sql_play.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,mylevel,totalmypoint,bettingpoint,playtime, blindstate,dealerpoint,result,win,lose,fold, allintype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 3 and logdetailid=2", sep=" ")
  sql_play.sep<- c(sql_play.sep, sql_2)
}
play.data.sep<-data.frame()
for ( i in 1:length(sql_play.sep)){
  play1 <- assign(paste("playsep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_play.sep[i]))
  play.data.sep<-rbind(play.data.sep, play1)
}
play.data.sep<-tbl_df(play.data.sep)
play.data.sep$channeluserid <- as.character(play.data.sep$channeluserid)
play.data.sep$result<- as.character(play.data.sep$result)
play.data.sep$now_2<- as.character(play.data.sep$now_2)
####buying data####
sql_buy.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,ravenitemcd,payamount, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 2 and logdetailid=1", sep=" ")
  sql_buy.sep<- c(sql_buy.sep, sql_2)
}
buy.data.sep<-data.frame()
for ( i in 1:length(sql_buy.sep)){
  buy1 <- assign(paste("buy.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_buy.sep[i]))
  buy.data.sep<-rbind(buy.data.sep, buy1)
}
####item_deco####
sql_item.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,itemidx, usepoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 4 and logdetailid=1", sep=" ")
  sql_item.sep<- c(sql_item.sep, sql_2)
}
item.data.sep<-data.frame()
for ( i in 1:length(sql_item.sep)){
  item1 <- assign(paste("item.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_item.sep[i]))
  item.data.sep<-rbind(item.data.sep, item1)
}
####item_char####
sql_char.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,characteridx, usepoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 5 and logdetailid=1", sep=" ")
  sql_char.sep<- c(sql_char.sep, sql_2)
}
char.data.sep<-data.frame()
for ( i in 1:length(sql_char.sep)){
  char1 <- assign(paste("char.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_char.sep[i]))
  char.data.sep<-rbind(char.data.sep, char1)
}
####randombox####
sql_random.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,boxidx, usepoint,usecash, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 108 and logdetailid=100", sep=" ")
  sql_random.sep<- c(sql_random.sep, sql_2)
}
random.data.sep<-data.frame()
for ( i in 1:length(sql_random.sep)){
  random1 <- assign(paste("random.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_random.sep[i]))
  random.data.sep<-rbind(random.data.sep, random1)
}
####freechip_attendent####
sql_attend.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,resulttype, resultcount, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 113 and logdetailid=100", sep=" ")
  sql_attend.sep<- c(sql_attend.sep, sql_2)
}
attend.data.sep<-data.frame()
for ( i in 1:length(sql_attend.sep)){
  attend1 <- assign(paste("attend.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_attend.sep[i]))
  attend.data.sep<-rbind(attend.data.sep, attend1)
}
####freechip_newuser####
sql_new.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,promotiongrade, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 112 and logdetailid=100", sep=" ")
  sql_new.sep<- c(sql_new.sep, sql_2)
}
new.data.sep<-data.frame()
for ( i in 1:length(sql_new.sep)){
  new1 <- assign(paste("new.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_new.sep[i]))
  new.data.sep<-rbind(new.data.sep, new1)
}

####freechip_request####
sql_req.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,umcode,getpoint, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 101 and logdetailid=101 and umcode IN('gdd12205','gdd12206','gdd12207','gdd12208')", sep=" ")
  sql_req.sep<- c(sql_req.sep, sql_2)
}
req.data.sep<-data.frame()
for ( i in 1:length(sql_req.sep)){
  req1 <- assign(paste("req.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_req.sep[i]))
  req.data.sep<-rbind(req.data.sep, req1)
}
########invite buddy#######
sql_invite.sep<-c()
for ( i in 1:length(date_sep)){
  sql_1<- paste("select channeluserid,hashed_talk_user_id, now_2, datedt from gholdem.TB_ParsingGameLog_",date_sep[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 6 and logdetailid=101", sep=" ")
  sql_invite.sep<- c(sql_invite.sep, sql_2)
}
invite.data.sep<-data.frame()
for ( i in 1:length(sql_invite.sep)){
  invite1 <- assign(paste("invite.sep_",date_sep[i],sep = ""),sqlQuery(conn_mssql, sql_invite.sep[i]))
  invite.data.sep<-rbind(invite.data.sep, invite1)
}
########social#######
sql_social.total<-c()
for ( i in 1:length(date_total)){
  sql_1<- paste("select channeluserid,socialtype, now_2, datedt from gholdem.TB_ParsingGameLog_",date_total[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 10 and logdetailid=100", sep=" ")
  sql_social.total<- c(sql_social.total, sql_2)
}
social.data.total<-data.frame()
for ( i in 1:length(sql_social.total)){
  social1 <- assign(paste("social_",date_total[i],sep = ""),sqlQuery(conn_mssql, sql_social.total[i]))
  social.data.total<-rbind(social.data.total, social1)
}
social.data.total<-tbl_df(social.data.total)
social.data.total$datedt<-as.character(social.data.total$datedt)
social.data.aug<-filter(social.data.total, datedt<'2016-08-31')
social.data.sep<-filter(social.data.total, datedt>'2016-08-31')
########sell item#######
sql_sell.total<-c()
for ( i in 1:length(date_total)){
  sql_1<- paste("select channeluserid,itemidx, sellcount, getpoint, getcash, now_2, datedt from gholdem.TB_ParsingGameLog_",date_total[i],sep = "")
  sql_2 <-paste(sql_1, "where logid = 109 and logdetailid=100", sep=" ")
  sql_sell.total<- c(sql_sell.total, sql_2)
}
sell.data.total<-data.frame()
for ( i in 1:length(sql_sell.total)){
  sell1 <- assign(paste("sell_",date_total[i],sep = ""),sqlQuery(conn_mssql, sql_sell.total[i]))
  sell.data.total<-rbind(sell.data.total, sell1)
}
sell.data.total<-tbl_df(sell.data.total)
sell.data.total$datedt<-as.character(sell.data.total$datedt)
sell.data.aug<-filter(sell.data.total, datedt<'2016-08-31')
sell.data.sep<-filter(sell.data.total, datedt>'2016-08-31')

############pre-precess################
##########1.to tbl type
play.data.sep<-tbl_df(play.data.sep)
play.data.sep<-group_by(play.data.sep, channeluserid)
buy.data.sep<- tbl_df(buy.data.sep)
buy.data.sep<- group_by(buy.data.sep, channeluserid)
item.data.sep<- tbl_df(item.data.sep)
item.data.sep<- group_by(item.data.sep, channeluserid)
char.data.sep<- tbl_df(char.data.sep)
char.data.sep<- group_by(char.data.sep, channeluserid)
random.data.sep<- tbl_df(random.data.sep)
random.data.sep<- group_by(random.data.sep, channeluserid)
attend.data.sep<- tbl_df(attend.data.sep)
attend.data.sep<- group_by(attend.data.sep, channeluserid)
new.data.sep<- tbl_df(new.data.sep)
new.data.sep<- group_by(new.data.sep, channeluserid)
req.data.sep<- tbl_df(req.data.sep)
req.data.sep<- group_by(req.data.sep, channeluserid)
fold.data.sep <- filter(play.data.sep, result=='f')
invite.data.sep<- tbl_df(invite.data.sep)
invite.data.sep<- group_by(invite.data.sep, channeluserid)
invite.data.sep$channeluserid<- as.character(invite.data.sep$channeluserid)
social.data.aug<-group_by(social.data.aug, channeluserid)
social.data.sep<-group_by(social.data.sep, channeluserid)
sell.data.aug<-group_by(sell.data.aug, channeluserid)
sell.data.sep<-group_by(sell.data.sep, channeluserid)
sell.data.aug$channeluserid<-as.character(sell.data.aug$channeluserid)
sell.data.sep$channeluserid<-as.character(sell.data.sep$channeluserid)
###########2. summarise###############
play.sep.summary<- summarise(play.data.sep,blindstate=max(blindstate), result=length(result), betting=sum(bettingpoint), level=max(mylevel), playtime=sum(playtime))
play.sep.summary$channeluserid<-as.character(play.sep.summary$channeluserid)
buy.sep.summary <- summarise(buy.data.sep, num_buy = length(ravenitemcd), payment=sum(payamount)/100)
buy.sep.summary$channeluserid <- as.character(buy.sep.summary$channeluserid)
item.sep.summary <- summarise(item.data.sep, num_buy_item=length(itemidx), usepoint=sum(usepoint))
item.sep.summary$channeluserid<-as.character(item.sep.summary$channeluserid)
char.sep.summary <- summarise(char.data.sep, num_buy_char=length(characteridx), usepoint_char=sum(usepoint))
char.sep.summary$channeluserid<-as.character(char.sep.summary$channeluserid)
random.sep.summary <- summarise(random.data.sep, num_buy_random = length(boxidx), usepoint_random=sum(usepoint),usecash=sum(usecash))
random.sep.summary$channeluserid<-as.character(random.sep.summary$channeluserid)
attend.sep.summary <-summarise(attend.data.sep, num_attend=length(resultcount))
attend.sep.summary$channeluserid<-as.character(attend.sep.summary$channeluserid)
new.sep.summary<- summarise(new.data.sep, num_new=length(promotiongrade))
new.sep.summary$channeluserid<-as.character(new.sep.summary$channeluserid)
req.sep.summary <- summarise(req.data.sep, num_req=length(umcode))
req.sep.summary$channeluserid<-as.character(req.sep.summary$channeluserid)
fold.sep.summary <- summarise(fold.data.sep, fold = length(result))
invite.sep.summary <- summarise(invite.data.sep, try.invite=length(hashed_talk_user_id))
invite.sep.summary$channeluserid<- as.character(invite.sep.summary$channeluserid)
social.aug.summary.v1<-social.aug.summary[,c(1,4,5,6,8,9)]
social.sep.summary.v1<-social.sep.summary[,c(1,4,5,6,8,9)]
sell.sep.summary <-summarise(sell.data.sep, sellcount=sum(sellcount), sell.getpoint=sum(getpoint), sell.getcash=sum(getcash)) 
sell.aug.summary <-summarise(sell.data.aug, sellcount=sum(sellcount), sell.getpoint=sum(getpoint), sell.getcash=sum(getcash)) 
###########3. join all tables#############
sep.total.info<- left_join(play.sep.summary, buy.sep.summary)
sep.total.info1<- left_join(sep.total.info,item.sep.summary)
sep.total.info2<- left_join(sep.total.info1,char.sep.summary)
sep.total.info3<- left_join(sep.total.info2,random.sep.summary)
sep.total.info4<- left_join(sep.total.info3,attend.sep.summary)
sep.total.info5<- left_join(sep.total.info4,new.sep.summary)
sep.total.info6<- left_join(sep.total.info5,req.sep.summary)
sep.total.info6[is.na(sep.total.info6)]<-0
sep.total.info7<- left_join(sep.total.info6,fold.sep.summary)
sep.total.info8<- left_join(sep.total.info7,invite.sep.summary)
sep.total.info9<- left_join(sep.total.info8,social.sep.summary.v1)
aug.total.info9<- left_join(aug.total.info8,social.aug.summary.v1)
sep.total.info10<- left_join(sep.total.info9,sell.sep.summary)
aug.total.info10<- left_join(aug.total.info9,sell.aug.summary)
#####making binary for logit regression###########
for(i in 1:nrow(sep.total.info6)){
  if(sep.total.info6$num_buy[i]>0){
    sep.total.info6$logit[i]<-1
  }else{
    sep.total.info6$logit[i]<-0
  }
}
sep.total.info8[is.na(sep.total.info8)]<-0

##########merge some tables########
total.data.info <-rbind(aug.total.info8, sep.total.info8)
total.data.info.v1 <-rbind(aug.total.info9, sep.total.info9)
total.data.info.v2 <-rbind(aug.total.info10, sep.total.info10)
total.data.info.v2[is.na(total.data.info.v2)]<-0
#########sampling#############
train_x3 <- c((nrow(total.data.info.v2)*0.7))
train_ind3 <- sample(seq_len(nrow(total.data.info.v2)),size = train_x3, replace = FALSE)
train_set_total2<- total.data.info.v2[train_ind,]
test_set_total2<-total.data.info.v2[-train_ind,]

#########logistic############
formula_glm <- c("logit~blindstate+result+betting+level+playtime+num_buy_item+num_buy_char+usepoint+usepoint_char+num_buy_random+usepoint_random+usecash+num_attend+num_new+num_req+foldrate")
formula_glm.v1 <- c("logit~num_attend + num_new + blindstate + num_buy_char + level + betting + usepoint_char + usecash + result +I(result^2)+ playtime + usepoint+foldrate")
formula_glm.v2 <- c("logit ~ num_attend + num_new + level + foldrate + blindstate + betting + num_buy_char + usepoint_char + usecash + result + playtime+I(result^2)")
formula_glm.v3 <- c("logit ~ num_attend + num_new + blindstate + foldrate + level + num_buy_char + betting + usepoint_char + usecash + sell.getcash + usepoint + try.invite + result + sell.getpoint + sellcount + playtime + num_buy_random")
model_buy.possi.v3<- glm(formula_glm.v2, data=train_set_total, family = binomial(link = "logit"))
model_buy.possi.v4<- glm(formula_glm.v3, data=train_set_total2, family = binomial(link = "logit"))
summary(model_buy.possi.v3)
#####selecting variables######
object<- glm(logit~1, family = binomial, data=train_set_total2)
s.list<- list(upper=update(object,~.+blindstate+result+betting+level+playtime+num_buy_item+num_buy_char+usepoint+usepoint_char+num_buy_random+usepoint_random+usecash+num_attend+num_new+num_req+foldrate+try.invite+giftchip_send+giftchip_reply+giftchip_accept+help_reply+help_accept+sellcount+sell.getpoint+sell.getcash))
step(object, scope=s.list, direction="forward")

#####
library(pscl)
pscl::pR2(model_buy.possi.v3)
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
