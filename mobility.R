play.3rd.mobility.totalchip <- dcast(play.3rd, channeluserid~blindstate, value.var = c("totalmypoint"),median)%>%tbl_df()
play.3rd.mobility.totalchip$num <- 0
play.3rd.mobility.totalchip[is.na(play.3rd.mobility.totalchip)]<-0

for (i in 1:nrow(play.3rd.mobility.totalchip))
{
  for( j in 2:12)
  {
    if(play.3rd.mobility.totalchip[i,j]!=0){
      play.3rd.mobility.totalchip$num[i]<- play.3rd.mobility.totalchip$num[i]+1
    }
  }
}
play.3rd.mobility.totalchip$total<-0
for ( i in 1:nrow(play.3rd.mobility.totalchip))
{
  play.3rd.mobility.totalchip$total[i] <- sum(play.3rd.mobility.totalchip[i,2:10])
}
play.3rd.mobility.totalchip<-group_by(play.3rd.mobility.totalchip, num)
summarise_each(play.3rd.mobility.totalchip, funs(sum,mean), total)
-------------------------------------------------------------------------------------------------------------------------------
#ac payment
reg.3rd.pay <- left_join(reg.3rd.v1, buy.3rd, by= "channeluserid")%>%group_by(channeluserid)
reg.3rd.pay.ac<- filter(reg.3rd.pay, payamount>0)
table(reg.3rd.pay.ac$ravenitemcd)
