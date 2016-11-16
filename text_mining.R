test <- read.csv("voc_4op.csv", stringsAsFactors = FALSE)

data_table <- filter(test, Date=='2016-11-01')

pos.words <- scan("positive-words.txt", what = "character", comment.char = ";")
neg.words <- scan("negative-words.txt", what = "character", comment.char = ";")

tm_model<-function(data_table){
mycorpus <- Corpus(VectorSource(data_table$Title.and.Contents))

mycorpus<- tm_map(mycorpus, content_transformer(tolower))
mycorpus<- tm_map(mycorpus, removePunctuation)
mycorpus<- tm_map(mycorpus, removeNumbers)
mystopwords <- c(stopwords("english"),"game")
mycorpus <- tm_map(mycorpus, removeWords, mystopwords)
mycorpus<- tm_map(mycorpus, stemDocument)

mytm <- DocumentTermMatrix(mycorpus)
tdm<- TermDocumentMatrix(mycorpus)
freq <- colSums(as.matrix(mytm))

##basic positive/negative words
neg.words <-c(neg.words, "unfair","uninstall","uninstal")
pos.matches <- match(names(freq), pos.words)
neg.matches <- match(names(freq), neg.words)
pos.matches.v1 <- !is.na(pos.matches)
neg.matches.v1 <- !is.na(neg.matches)
score_strength <- sum(pos.matches.v1)/sum(neg.matches.v1)
pos.score <- sum(pos.matches.v1)/(sum(pos.matches.v1)+sum(neg.matches.v1))
neg.score <- sum(neg.matches.v1)/(sum(pos.matches.v1)+sum(neg.matches.v1))
total_score<- cbind(data_table[1,1],score_strength, pos.score, neg.score)
return(total_score)
}
findFreqTerms(mytm, lowfreq = 4)

findAssocs(mytm, "game", corlimit = 0.3)
plot(mytm, terms = findFreqTerms(mytm, lowfreq = 10), corThreshold = 0.5)
wordcloud(names(freq), freq,min.freq = 20)
####daily score table######

startdate <- as.Date("2016-11-01")
enddate <- as.Date("2016-11-14")
date_test<- makeDateX(startdate, enddate)
test_table<-data.frame()
for( i in 1:length(date_test)){
  table1<- filter(test, Date==date_test[i])
  table2<- tm_model(table1)
  test_table <- rbind(test_table, table2)
}
test_table <- tbl_df(test_table)
test_table<- test_table[-c(4:6),]
test_table$score_strength<- as.numeric(as.character(test_table$score_strength))
test_table$pos.score<- as.numeric(as.character(test_table$pos.score))
test_table$neg.score<- as.numeric(as.character(test_table$neg.score))
ggplot(data=test_table, aes(x=V1, y=neg.score))+geom_point()
