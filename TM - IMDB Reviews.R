library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(ggplot2)


##IMDB Reviews
aurl <- "https://www.imdb.com/title/tt0944947/reviews?ref_=tt_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"GOT.txt",row.names = F)


got1 <- read.delim('GOT.txt')
str(got1)
View(got1)

# Build Corpus and DTM/TDM
library(tm)
corpus <- got1[-1,]
head(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Clean the text 
corpus_clean <- tm_map(corpus,tolower)
#remove punctuation
corpus_clean <- tm_map(corpus,removePunctuation)
#removeNumbers
corpus_clean <- tm_map(corpus,removeNumbers)
corpus_clean<-tm_map(corpus,stripWhitespace)
corpus_clean<-tm_map(corpus,removeWords, stopwords('english'))
corpus_clean <- tm_map(corpus, stemDocument)
corpus_clean<-tm_map(corpus,removeWords, c('movie','movies'))
inspect(corpus[1:5])

##build DTM/TDM
Cleancorpus_dtm <- TermDocumentMatrix(corpus)
Cleancorpus_dtm

Cleancorpus_dtm1 <- as.matrix(Cleancorpus_dtm)

Cleancorpus_dtm[1:10,1:20]




w <- rowSums(Cleancorpus_dtm1)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 50) # Pull words that were used more than 50 times.
barplot(w, las = 2, col = rainbow(50))


#generate word cloud
w <- sort(rowSums(Cleancorpus_dtm1), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)

##sentiment analysis 
# Read File 
IMDB_reviews <- read.delim(file.choose())
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)

s <- get_nrc_sentiment(reviews)
head(s)

barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for GOT')
