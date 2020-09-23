#twitter sentimental analysis

#install.packages("twitteR", dependencies=TRUE)
#install.packages("RCurl")
#install.packages('bitops')
#install.packages('base64enc')
#install.packages('httpuv')
#install.packages('tm')
#install.packages('wordcloud')
#install.packages("stringr")

library(stringr)
library(twitteR)
library(RCurl)
library(bitops)
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)


options(stringsAsFactors = FALSE)


#A space is to be added after 
cred_file = "/Users/vaibhav/Desktop/twitterOauth.txt"
oauthCreds=read.table(cred_file,header = TRUE)


setup_twitter_oauth(oauthCreds$consumer_key,
                    oauthCreds$consumer_secret,
                    oauthCreds$access_token,
                    oauthCreds$access_secret) 

searchterms = c("India + sa + south africa")
numberofTweets =300

tweets_list = searchTwitter(searchterms,lang="en",n=numberofTweets,resultType="recent")


tweets_list [1]

getTweets_text = function(tweets_list){
  
  tweets_text = sapply(tweets_list, function(x) x$getText())
  tweets_text
  #str(tweets_text)
  class(tweets_text)
  return (tweets_text)
  
tweets_text[1]


tweets_corpus = Corpus(VectorSource(tweets_text))
tweets_corpus
#see the first tweet in the tweet corpus
inspect(tweets_corpus[1:3])
#return(tweets_corpus)


#preprocessing

tweets_corpus_clean = tm_map(tweets_corpus, removePunctuation)
tweets_corpus_clean = tm_map(tweets_corpus_clean, stripWhitespace)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeNumbers)
tweets_corpus_clean = tm_map(tweets_corpus_clean, removeWords, stopwords("english"))
tweets_corpus_clean = tm_map(tweets_corpus_clean, content_transformer(tolower))
toSpace = content_transformer(function(x, pattern) gsub(pattern,"",x))
tweets_corpus_clean = tm_map(tweets_corpus_clean, toSpace,"https*|youtu*")
#tweets_corpus_clean = tm_map(tweets_corpus_clean, stemDocument)
#return(tweets_corpus_clean)

tweets_corpus_clean


tweets_tdm=TermDocumentMatrix(tweets_corpus_clean)
#class(tweets_tdm)
#[1] "TermDocumentMatrix" "simple_triplet_matrix"
str(tweets_tdm)

tweets_tdm = as.matrix(tweets_tdm)
class(tweets_tdm)
str(tweets_tdm)

###############################

tdm_term_freq_sort = sort(rowSums(tweets_tdm), decreasing=TRUE)
class(tdm_term_freq_sort)
tdm_term_freq_sort_inc = sort(rowSums(tweets_tdm), decreasing=FALSE)
tdm_term_freq_df = data.frame(word = names(tdm_term_freq_sort),
                              freq = tdm_term_freq_sort)
str(tdm_term_freq_df)
head(tdm_term_freq_df,10)
#set rownames to number

wordcloud(words = tdm_term_freq_df$word,
                  freq= tdm_term_freq_df$freq,
            min.freq=15,
            max.words=300,
            random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(8,'Dark2'),
            scale=c(3,0.5))
