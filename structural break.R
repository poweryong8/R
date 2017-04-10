library(RODBC)
#connting server using odbc function
conn_mssql <- odbcConnect("4ones", uid = "sa", pwd = "1234Qwer")

#writing quary
sql_reg<- "select 
distinct channeluserid, os_2, mycash, mypoint,regdatetime, now_2
from dbo.TB_LOG_AF_1208
where logid =1 and logdetailid=1"


sql_pay <- 'select 
channeluserid,sum(payamount) as total_sum, max(now_2) as last_buy
from dbo.TB_LOG_AF_1208
where logid = 2 and logdetailid =1
group by channeluserid'


# doing quary
data_reg <- sqlQuery(conn_mssql, sql_reg)
data_buy <- sqlQuery(conn_mssql, sql_pay)
library(dplyr)
library(hflights)
data_reg <- tbl_df(data_reg)
data_reg <- select(data_reg, channeluserid, os_2, regdatetime)
data_buy <- tbl_df(data_buy)
data_buy$last_buy <- as.Date(as.character(data_buy$last_buy))
data_reg$regdatetime <- as.Date(as.character(data_reg$regdatetime))

unique.date <- unique(data_reg[order(data_reg$regdatetime),])
unique.date <- unique(unique.date$regdatetime)

for (i in 1:length(unique.date))
{
  assign(paste("reg.user", i,sep = ""), filter(data_reg, regdatetime==unique.date[i]))
}

reg.0229 <- inner_join(reg.user84, data_buy, by = "channeluserid")
sum(reg.0229$total_sum)
library(ggplot2)
ggplot(num.login21, aes(y= mean, x=Var1,colour="Average Frequancy", size=2))+geom_point()+ geom_point(aes(x=Var1, y=log(Freq, base = exp(4)),colour="Frequancy"))
plot(num.login6$mean, type="l")

sp2 <- dura30[dura30$channeluserid==uid30[2],]
sp2.summary <- summarise(sp2, ave_playtime = mean(playtime), ave_betting = log(mean(bettingpoint)), num_access = count(sp2, sp2$du.date))

inf.duration <- rbind(t5, t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19,t20,t21,t22,t23,t24,t25,t26,t27,t28,t29,t30)
