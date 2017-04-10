tm1 <- read.csv("C:/Users/fourones_jsw/Documents/task/review_slotmania.csv", header= T)
library(tm)
mycorpus<- Corpus(VectorSource(tm1$review))
mycorpus<- tm_map(mycorpus, tolower)
mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<- tm_map(mycorpus, removeNumbers)
mystopwords <-c(stopwords('english'),"I", "like", "love") 
idx<- which(mystopwords %in% c("like", "love"))
mycorpus <- tm_map(mycorpus,removeWords,mystopwords)
# copy version
mycorpuscopy <- mycorpus
mycorpus <- tm_map(mycorpus, PlainTextDocument)
matrix_corpus <- DocumentTermMatrix(mycorpus)
findFreqTerms(matrix_corpus,lowfreq = 9)
termfrequancy <- rowSums(as.matrix(matrix_corpus))
termfrequancy <- subset(, termfrequancy>=9)
library(ggplot2)


library(wordcloud)
