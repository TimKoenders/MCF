
####################################
### I. Create DTM and Word Cloud ###
####################################

#install.packages("malaytextr")

library(tokenizers)
library(NLP)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(dplyr)
library(tidyverse)
library(malaytextr)


#getwd()
#setwd("C:/Users/fajli/Documents/R/data/project/small corpus")
setwd("C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV")
list.files()


news_url <- read.csv2("smallcorpus.csv", header=TRUE, sep=";", 
                        stringsAsFactors=FALSE, colClasses=c("character","character"))
str(news)

#Removing URLs
news <- data.frame(text=remove_url(news_url[,1]),date=news_url[,2])


to_corpus<-data.frame(cbind(news$date, news$text))

str(to_corpus)

names(to_corpus)<-c("doc_id","text")
to_corpus$doc_id<-as.character(to_corpus$doc_id)
to_corpus$text<-as.character(to_corpus$text)

to_corpus<-DataframeSource(to_corpus)


corpus<-VCorpus(to_corpus, readerControl=list(language="english"))
str(corpus)
length(corpus)
corpus[[1]][1]





inspect(corpus)



### Tokenizer for only n-grams: the parameters 1:2 below produce one-grams (i.e. single words) and bi-grams
### Adjust the numbers below to produce other n-grams. E.g., 4:4 would produce only 4-grams

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:2), paste, collapse = "_"), use.names = FALSE)
}

### The command below produces the central document term matrix, containing all words/tokens in the columns
### and the word counts in the documents in the rows
### Note the options for the control list including stopwords, switching to lower case letters, stemming,
### removing punctuation etc. Change these option to obtain alternative results.
### Your system may take several seconds to produce the DTM.

custom_stopwords <- c("issue", "pages", "ofpublication", "inth", "onth", "ofth", "inth", 
                      "theeconomist", "publication", "issn", "coden", "subject", "classification", "volume",
                      "issue", "publisher", "in", "th", "id", "and", "are", "for", "that", "the", "economist",
                      "has", "with", "have", "amp", "title", "magazine", "btitle", "jtitle", "spage", "url",
                      "article", "rft_id", "some", "date", "this","au", "isbn", "atitle", "sid", "genre",
                      "openurl.wu.ac.at","https", "abstract", "links", "copyright", "their", "aabiglobal",
                      "fmt:kev:mtx:journal", "info:ofi", "aabiglob", "proqproq", "resolve", "infodoi",
                      "which", "docview", "infoer", "urlver", "magazine", "type", "his", "her", "proq:proq",
                      "www.proquest.com", "accountid", "be", "been", "rft_val_fmt", "newspaper", "abi", 
                      "url_ver", "info:doi", "info:eric", "them", "into", "how", "anonymous", "Aabiglobal",
                      "yet", "%3Aabiglobal")

# Custom tokenizer that performs more controlled tokenization
custom_tokenizer <- function(x) {
  tokens <- unlist(tokenize_words(x))
  tokens <- tolower(tokens)
  tokens <- tokens[!(tokens %in% custom_stopwords)]
  return(tokens)
}

# Apply the custom tokenizer to each document in the corpus
corpus <- tm_map(corpus, content_transformer(custom_tokenizer))


# Create Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(
  corpus,
  control = list(
    removeNumbers = TRUE,
    stopwords = TRUE,
    tolower = TRUE,
    stemming = TRUE,
    removePunctuation = TRUE
  )
)


### Remove terms that appear in only two percent of the documents
# can make it more strict here maybe?
dtm <- removeSparseTerms(dtm, 0.98)

### Explore the DTM
dim(dtm)
str(dtm)
inspect(dtm)


### Show the word frequencies in the DTM

term.frequencies<-colSums(as.matrix(dtm))
order.frequencies<-term.frequencies[order(term.frequencies, decreasing=TRUE)]
str(order.frequencies)


dtm_matrix <- as.matrix(dtm) 
dimnames(dtm_matrix)=="false"
dtm_words <- sort(colSums(dtm_matrix),decreasing=TRUE) 
cloud_frame <- data.frame(word = names(dtm_words),freq=dtm_words)
cloud_frame<-cloud_frame[cloud_frame$word!="fals",]

# edit(cloud_frame)

set.seed(123)
wordcloud(words = cloud_frame$word, freq = cloud_frame$freq, min.freq = 5, 
          max.words=70, random.order=FALSE, rot.per=0.2,
          colors=brewer.pal(8, "Dark2"))


## Transform date in dtm to format DD.MM.YYYY
dtm_df<-data.frame(dtm_matrix)
dtm_df$date1<-row.names(dtm_df)
dtm_df$date1<-ifelse(substr(dtm_df$date1,1,2)=="99",
                     paste("19",dtm_df$date1,sep=""),paste("20",dtm_df$date1,sep=""))

dtm_df$date2<-paste(substr(dtm_df$date1,7,8),substr(dtm_df$date1,5,6),
                    substr(dtm_df$date1,1,4), sep=".")

dtm_df$date3<-as.numeric(paste(substr(dtm_df$date1,1,4),substr(dtm_df$date1,5,6),sep=""))



