#### Set-up ----------------------------------------------------------------
rm(list = ls())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("dplyr",
              "plyr",                          
              "readr",
              "readxl",
              "tidyverse",
              "ggplot2",
              "stringr",
              "tokenizers",
              "NLP",
              "tm",
              "wordcloud",
              "RColorBrewer",
              "SnowballC",
              "dplyr",
              "malaytextr",
              "vader",
              "schrute",
              "data.table",
              "tictoc"
              )

ipak(packages)

setwd("C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV")
list.files()


#### Data processing ---------------------------------------------------------

# news_url_s <- read.csv2("smallcorpus.csv", header=TRUE, sep=";", 
                      stringsAsFactors=FALSE, colClasses=c("character","character"))

news_url <- read.csv2("merged corpus.csv", header=TRUE, sep=";", 
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


# # Create Document-Term Matrix (DTM)
# dtm <- DocumentTermMatrix(
#   corpus,
#   control = list(
#     removeNumbers = TRUE,
#     stopwords = TRUE,
#     tolower = TRUE,
#     stemming = TRUE,
#     removePunctuation = TRUE
#   )
# )


# ### Remove terms that appear in only two percent of the documents
# # can make it more strict here maybe?
# dtm <- removeSparseTerms(dtm, 0.98)
# 
# ### Explore the DTM
# dim(dtm)
# str(dtm)
# inspect(dtm)
# 
# 
# ### Show the word frequencies in the DTM
# 
# term.frequencies<-colSums(as.matrix(dtm))
# order.frequencies<-term.frequencies[order(term.frequencies, decreasing=TRUE)]
# str(order.frequencies)
# 
# 
# dtm_matrix <- as.matrix(dtm) 
# dimnames(dtm_matrix)=="false"
# dtm_words <- sort(colSums(dtm_matrix),decreasing=TRUE) 
# cloud_frame <- data.frame(word = names(dtm_words),freq=dtm_words)
# cloud_frame<-cloud_frame[cloud_frame$word!="fals",]
# 
# # edit(cloud_frame)
# 
# set.seed(123)
# wordcloud(words = cloud_frame$word, freq = cloud_frame$freq, min.freq = 5, 
#           max.words=70, random.order=FALSE, rot.per=0.2,
#           colors=brewer.pal(8, "Dark2"))
# 
# 
# ## Transform date in dtm to format DD.MM.YYYY
# dtm_df<-data.frame(dtm_matrix)
# dtm_df$date1<-row.names(dtm_df)
# dtm_df$date1<-ifelse(substr(dtm_df$date1,1,2)=="99",
#                      paste("19",dtm_df$date1,sep=""),paste("20",dtm_df$date1,sep=""))
# 
# dtm_df$date2<-paste(substr(dtm_df$date1,7,8),substr(dtm_df$date1,5,6),
#                     substr(dtm_df$date1,1,4), sep=".")
# 
# dtm_df$date3<-as.numeric(paste(substr(dtm_df$date1,1,4),substr(dtm_df$date1,5,6),sep=""))

#### Sentiment analysis with vader------------------------------------------------------

inspect(corpus)

# Extract sentiment from corpus
text_list <- lapply(corpus , function(x) { as.character(x) })
sentiment <- vader_df(text_list)
sentiment_copy <- sentiment
sentiment_copy_copy <- sentiment_copy

# Redo
sentiment <- sentiment_copy

# Now continue
sentiment$document_id <- as.numeric(row.names(sentiment))


# Get relevant indictor differentiating whether to extract 3,4 or 5 digits from document_id
sentiment$row <- seq_len(nrow(sentiment))


# Create document_id based on the conditions
sentiment$document_id <- ifelse(sentiment$row <= 143086, 
                                 substr(sentiment$document_id, 1, 3),
                                 ifelse(sentiment$row > 143086 & sentiment$row < 186430, 
                                        substr(sentiment$document_id, 1, 4),
                                        substr(sentiment$document_id, 1, 5)))

# Ensure that document_id is a character vector
sentiment$document_id <- as.character(sentiment$document_id)

# Convert sentiment to data.table

setDT(sentiment)

# Calculate weighted positive, negative, and total occurrences for each document
document_sentiment <- sentiment[, .(total_pos = sum(pos),
                                    total_neg = sum(neg),
                                    total = sum(but_count)),
                                by = document_id]

# Calculate sentiment scores using the formula
document_sentiment$sentiment_score <- (document_sentiment$total_pos - document_sentiment$total_neg) / (document_sentiment$total + 1)

# Remove observations
document_sentiment <- document_sentiment[total_pos >= 2, ]

# View the resulting data frame
print(document_sentiment)


#### Sentiment analysis with vader using Word Scores --------------------

sentiment$word_scores <- gsub("\\{|\\}", "", sentiment$word_scores) # remove brackets
sentiment$word_scores <- as.numeric(sentiment$word_scores) # convert to numeric

# Create a new data.table for document-level sentiment
document_sentiment_ws <- sentiment[, .(weighted_pos = sum(pos * word_scores),
                                    weighted_neg = sum(neg * -1*word_scores)),
                                by = document_id]


# Calculate sentiment scores using the formula
document_sentiment_ws$sentiment_score_ws <- (document_sentiment_ws$weighted_pos - document_sentiment_ws$weighted_neg) / (document_sentiment$total +1)

# Remove observations
document_sentiment_ws <- document_sentiment_ws[weighted_neg >= 4 | sentiment_score_ws == 2.1, ]

# View the resulting data frame
print(document_sentiment_ws)

document_sentiment_ws$document_id <- as.character(document_sentiment_ws$document_id)

# Create a new variable 'date' based on document_id
document_sentiment_ws$date <- ifelse(nchar(document_sentiment_ws$document_id) == 3, 
                                     paste0("20000", document_sentiment_ws$document_id), 
                                     ifelse(nchar(document_sentiment_ws$document_id) == 4, 
                                            paste0("2000", document_sentiment_ws$document_id), 
                                            ifelse(nchar(document_sentiment_ws$document_id) == 5, 
                                                   paste0("200", document_sentiment_ws$document_id), 
                                                   NA)))

document_sentiment_ws$date <- paste0(substr(document_sentiment_ws$date, 1, 4), "-", substr(document_sentiment_ws$date, 5, 6), "-", substr(document_sentiment_ws$date, 7, 8))
head(document_sentiment_ws)

## Merge with number of articles

art <- read.csv("numberofarticles.csv", sep = ";")
art <- aggregate(numberart ~ date, data = art, sum)
names(art)[names(art) == "date"] <- "document_id"
head(art)
head(document_sentiment_ws)
document_sentiment_ws <- merge(art, document_sentiment_ws, by = "document_id")


#### Create dataframe --------------------------------------------------------
head(document_sentiment)
head(document_sentiment_ws)
# Convert document_id to character type in both data frames
document_sentiment$document_id <- as.character(document_sentiment$document_id)
document_sentiment_ws$document_id <- as.character(document_sentiment_ws$document_id)

scores <- merge(document_sentiment, document_sentiment_ws, by = "document_id")  
scores <- data.frame(
  date = scores$date,
  sentiment_score = scores$sentiment_score,
  sentiment_score_ws = scores$sentiment_score_ws,
  art = scores$numberart
)
# Define the file path
file_path <- "C:/Users/koend/OneDrive/Bureaublad/WU 2023-2024/Courses/Seminar MCF/Project_MCF/MCF/CSV/scores.csv"

# Write the DataFrame to a CSV file
write.csv(scores, file = file_path, row.names = FALSE)

