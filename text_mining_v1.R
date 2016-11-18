test <- read.csv("voc_4op.csv", stringsAsFactors = FALSE)

pos.words <- scan("positive-words.txt", what = "character", comment.char = ";")
neg.words <- scan("negative-words.txt", what = "character", comment.char = ";")

mycorpus <- Corpus(VectorSource(test$Title.and.Contents))

mycorpus<- tm_map(mycorpus, content_transformer(tolower))
mycorpus<- tm_map(mycorpus, removePunctuation)
mycorpus<- tm_map(mycorpus, removeNumbers)
mystopwords <- c(stopwords("english"),"game","app","can","let","one","will")
mycorpus <- tm_map(mycorpus, removeWords, mystopwords)
mycorpus<- tm_map(mycorpus, stemDocument)
  
mytm.v1 <- DocumentTermMatrix(mycorpus)
tdm<- TermDocumentMatrix(mycorpus)
freq <- colSums(as.matrix(mytm.v1))
  
##basic positive/negative words
neg.words <-c(neg.words, "unfair","uninstall","uninstal","cant")
pos.matches <- match(names(freq), pos.words)
neg.matches <- match(names(freq), neg.words)
pos.matches.v1 <- !is.na(pos.matches)
neg.matches.v1 <- !is.na(neg.matches)
score_strength <- sum(pos.matches.v1)/sum(neg.matches.v1)
pos.score <- sum(pos.matches.v1)/(sum(pos.matches.v1)+sum(neg.matches.v1))
neg.score <- sum(neg.matches.v1)/(sum(pos.matches.v1)+sum(neg.matches.v1))
total_score<- cbind(data_table[1,1],score_strength, pos.score, neg.score)


findFreqTerms(mytm, lowfreq = 4)
findAssocs(mytm, "game", corlimit = 0.3)

plot(mytm, terms = findFreqTerms(mytm, lowfreq = 10), corThreshold = 0.5)
wordcloud(names(freq), freq,min.freq = 20)
ggplot(data=test_table, aes(x=V1, y=score_strength))+geom_point()

##topic modeling
burnin <- 400
iter <- 200
thin <- 50
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k<-3
ldaout <- LDA(mytm.v1,k, method="Gibbs", control=list(nstart=nstart,seed=seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaout.topic <- as.matrix(topics(ldaout))
ldaout.terms <- as.matrix(terms(ldaout,10))
