
#connting server using odbc function
conn_mssql <- odbcConnect("4ones", uid = "sa", pwd = "1234Qwer")

#writing quary
sql_gameend<- "select 
channeluserid, stage, result,platformtype,r_key, blindstate,dealerpoint,playtime,getpoint, blindtype, prevmypoint,mypoint,prevtotalmypoint,totalmypoint,bettingpoint,allintype,prevlevel,mylevel,win,lose,fold,now_2
from dbo.TB_LOG_AF_1208
where logid = 3 and logdetailid =2 and channeluserid not like 'nm%'"

# doing quary
data_gameend <- sqlQuery(conn_mssql, sql_gameend)
library(dplyr)
library(hflights)
data_gameend <- tbl_df(data_gameend)
game.group <- group_by(data_gameend, channeluserid)
game.group$now_2 <- as.Date(as.character(game.group$now_2))
game.user <- summarise(game.group, total_playtime = sum(playtime)/60 , mu.mypoin = mean(totalmypoint, na.rm = FALSE),avg.dealerpoint =mean(dealerpoint), avg.getpoint = mean(getpoint) ,min.date = min(now_2), max.date = max(now_2))
game.user <- mutate(game.user, duration = (max.date - min.date)+1)

sp4 <- data_gameend[data_gameend$channeluserid == game.user$channeluserid[4],]
sum(sp4$getpoint)
sum(sp4$bettingpoint)
select(sp4, getpoint, mypoint,bettingpoint,prevtotalmypoint, totalmypoint, allintype,now_2)
cor(sp4$bettingpoint, sp4$getpoint)
