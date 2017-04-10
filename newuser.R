sql_new0608<- "SELECT channeluserid,blindstate, bettingpoint,getpoint,playtime, dealerpoint, now_2
  FROM  [gholdem].[TB_ParsingGameLog_20160608]
where logid =3 and logdetailid =2 and channeluserid = 
any(select channeluserid from [gholdem].[TB_ParsingGameLog_20160608] where logid =1 and logdetailid =1)"

new0608 <- sqlQuery(conn_mssql, sql_new0608)

new0608 <- tbl_df(new0608)
new0608 <- group_by(new0608, channeluserid)
new0608$wi <- new0608$bettingpoint/new0608$blindstate
new0608.summary <- summarise_each(new0608, funs(sum,mean), bettingpoint, getpoint, playtime, dealerpoint)
table.new0608<-dcast(new0608, channeluserid~blindstate)
table.new0608<- left_join(table.new0608, new0608.summary, by = "channeluserid")
write.csv(table.new0608, file = "new0608.csv")
