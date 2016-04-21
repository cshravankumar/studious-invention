library(tm)
library(SnowballC)
library(wordcloud)


#WORDCLOUD


###the column sentiment$reviewText is the column with the text
corpus=Corpus(VectorSource(sentiment$reviewText))
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,removePunctuation)
orpus=tm_map(corpus,removeWords,stopwords('english'))
corpus=tm_map(corpus,stemDocument)

###random has to be FALSE if we wanna have different plotss each time (I think it's better)
###maxword is the number of words to display (pick the best!)
wordcloud(corpus,max.words=200,random.order=FALSE)
corpus=tm_map(corpus,removeWords,c("time","just"))