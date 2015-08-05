tweets = read.csv("./tweets (1).csv")
#perform the following pre-processing tasks (like we did in Unit 5), 
#noting that we don't stem the words in the document or remove sparse terms:

library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))

basicFiltrins <- function(corpus)
{
  transformed = tm_map(corpus,tolower)
  transformed = tm_map(transformed, PlainTextDocument)
  transformed = tm_map(transformed,removePunctuation)
  tm_map(transformed,removeWords,c(stopwords("english")))
}

corpusFilt = basicFiltrins(corpus)
dtm = DocumentTermMatrix(corpusFilt)
tweetFrame = as.data.frame(as.matrix(dtm),row.names = FALSE)
#unique words:
ncol(tweetFrame)

#1.2

#2.1
install.packages("wordcloud")
library(wordcloud)

#2.3
?wordcloud
wordcloud(colnames(tweetFrame),colSums(tweetFrame))

#2.4 
#with appl removed
tweetFrame2 = subset(tweetFrame, select=-c(apple))
wordcloud(colnames(tweetFrame2),colSums(tweetFrame2))

#3.1
?wordcloud
                   
#3.5
