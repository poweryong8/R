buy.data.1208$yearmon <-as.yearmon(buy.data.1208$datedt)
july.bu <- filter(buy.data.1208, yearmon=='8 2016')
july.bu$now_2<- as.character(july.bu$now_2)
july.bu.melt <-melt(july.bu, id.vars = c("channeluserid", "now_2", "ravenitemcd"),measure.vars = "payamount")
july.bu.melt <-arrange(july.bu.melt, channeluserid, now_2)
july.bu.melt <- tbl_df(july.bu.melt)
july.bu.melt <-group_by(july.bu.melt, channeluserid)
july.id <- unique(july.bu.melt$channeluserid)
july.first <- data.frame()
#searching first buying point each day per id
for( i in 1: length(unique(july.bu.melt$channeluserid))){
  temp = july.bu.melt[july.bu.melt$channeluserid==july.id[i],]
  temp <- group_by(temp, date)
  temp1 <- summarise(temp, channeluserid=max(channeluserid), now_2 = min(now_2))
  july.first <- rbind(july.first, temp1)
}

buy.before.aug <- data.frame()
buy.before.aug <- rbind(buy.801.before.v1,buy.802.before.v1,buy.803.before.v1,buy.804.before.v1,buy.805.before.v1,buy.806.before.v1,buy.807.before.v1,buy.808.before.v1,buy.809.before.v1,buy.810.before.v1,buy.811.before.v1,buy.812.before.v1,buy.813.before.v1,buy.814.before.v1,buy.815.before.v1,buy.816.before.v1,buy.817.before.v1,buy.818.before.v1,buy.819.before.v1,buy.820.before.v1,buy.821.before.v1,buy.822.before.v1,buy.823.before.v1,buy.824.before.v1,buy.825.before.v1,buy.826.before.v1,buy.827.before.v1,buy.828.before.v1,buy.829.before.v1,buy.830.before.v1)
