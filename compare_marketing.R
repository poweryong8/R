conn_mssql1 <- odbcConnect("4ones", uid = "sa", pwd = "1234Qwer")

#writing quary
sql_registration<- "SELECT channeluserid, platformtype, os_2, now_2
from dbo.TB_LOG_AF_1208
where logid = 1 and logdetailid =1"

registration <- sqlQuery(conn_mssql1, sql_registration)

sql_access<- "SELECT channeluserid,os_2, mycash,mypoint,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 1 and logdetailid =2"

access <- sqlQuery(conn_mssql1, sql_access)

sql_play.june<- "SELECT channeluserid,playtime,level, result, blindstate,blindtype,getpoint,prevtotalmypoint,totalmypoint,bettingpoint,umcode, now_2
from dbo.TB_LOG_AF_1208
where logid = 3 and logdetailid =2 and datedt>='2016-06-01'"

play.june <- sqlQuery(conn_mssql1, sql_play.june)

sql_item<- "SELECT channeluserid,itemtype,level,itemidx,usecash,usepoint,umcode,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 4 and logdetailid =1"

item <- sqlQuery(conn_mssql1, sql_item)

sql_character<- "SELECT channeluserid,characteridx,level,usecash,usepoint,umcode,platformtype, now_2
from dbo.TB_LOG_AF_1208
where logid = 5 and logdetailid =1"

character <- sqlQuery(conn_mssql1, sql_character)

----------------------------------------------------------------------------------------------------------------------------------------
registration <-tbl_df(registration)%>%group_by(channeluserid)
access <- tbl_df(access)%>%group_by(channeluserid)
item<-tbl_df(item)%>%group_by(channeluserid)
character <- tbl_df(character)%>%group_by(channeluserid)
play <- tbl_df(play)%>%group_by(channeluserid)
# pre-handling registration
registration$date <- as.Date(registration$now_2)
registration$yearmon <-as.yearmon(registration$date)
reg.feb <- filter(registration, yearmon =='2 2016')
reg.march <- filter(registration, yearmon =='3 2016')
# pre-handling item
item$date <- as.Date(item$now_2)
item$yearmon <- as.yearmon(item$date)
item.feb <- filter(item, yearmon=='2 2016')
item.march <- filter(item, yearmon=='3 2016')
# pre-handling access
access$date <- as.Date(access$now_2)
access$yearmon <- as.yearmon(access$date)
access.feb <- filter(access, yearmon=='2 2016')
access.march <- filter(access, yearmon=='3 2016')
# pre-handling play
play$date <- as.Date(play$now_2)
play$yearmon <- as.yearmon(play$date)
play.feb <- filter(play, yearmon == '2 2016')
play.march <- filter(play, yearmon == '3 2016')
play.feb.15 <- filter(play.feb, date=='2016-02-15')

