#tm = librerya para mineria de texto
#Corpus coleccion de documentos de texto
library(tm)
library(qdap)
library(ggplot2)
#carga de los datos
tweets <- read.csv(file = "/home/newuser/Descargas/trumptw.csv")
#Separando la columna texto de interes
tweets_text <- (tweets$text)
tweets_text_frame <- as_data_frame(tweets$text)

#Corpus
#1) Crear corpus = Convertir vector en objeto
library(tm)
tweets_sorce <- VectorSource(tweets_text)

#2) Convertir a volatir Volatile = significa que se guardara en
#memora RAM y no en disco esto es para mejor rendimienti
library(tm)
tweets_corpus <- VCorpus(tweets_sorce)
#Imprime el total de documentos = 23402
tweets_corpus
head(tweets_corpus)
#Imprime los primeros 15 tweeats
tweets_corpus[[5]]
#Conteo de palabras
library(tm)
terminos_frecu <- freq_terms(tweets_text)
plot(terminos_frecu)
head(terminos_frecu)

#Text transformation

#=================CLEANING TEXT====================================
# Convert the text to lower case

tweets_corpus <- tm_map(tweets_corpus, content_transformer(tolower))
# Remove numbers
tweets_corpus <- tm_map(tweets_corpus, removeNumbers)
# Remove english common stopwords
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
tweets_corpus <- tm_map(tweets_corpus, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
tweets_corpus <- tm_map(tweets_corpus, removePunctuation)
# Eliminate extra white spaces
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)
clean_corp <- tm_map(clean_corp, removeWords, stopwords("en"))

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}
clean_corp <- clean_corpus(tweets_corpus)

clean_corpus <- tm_map(tweets_corpus, content_transformer(tolower))
clean_corpus <- tm_map(tweets_corpus, removeWords, stopwords('english'))
clean_corpus <- tm_map(tweets_corpus, removePunctuation)
clean_corpus <- tm_map(tweets_corpus, PlainTextDocument)
clean_corpus <- tm_map(tweets_corpus, stemDocument)



wordcloud(tweets_corpus, max.words = 200, random.order = FALSE,
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(clean_corpus, max.words = 200, random.order = FALSE,
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#Cleaning 
#tm_map() funcion para remplazar caracteres especiales
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
clean_corp <- tm_map(tweets_corpus, toSpace, "/")
clean_corp <- tm_map(tweets_corpus, toSpace, "@")
clean_corp <- tm_map(tweets_corpus, toSpace, "\\|")

library(dplyr)
clean_corp<-tweets_corpus%>%
  tm_map(removePunctuation)%>% ##elimina puntuacion
  tm_map(removeNumbers)%>% #elimina numeros
  tm_map(stripWhitespace)#espacios en blanco
#Delete stopwords
clean_corp<-tweets_corpus%>%
  tm_map(tolower)%>% ##make all words lowercase
  tm_map(removeWords, stopwords("english"))

#Delete other words
clean_corp <- tm_map(tweets_corpus, removeWords, c("the", "and","for","this",
                                                  "that","with","will","also",
                                                  "i'm")) 
#Steamming
clean_corp <-tm_map(tweets_corpus, stemDocument)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(dplyr)

clean_corpus <- tm_map(tweets_corpus, content_transformer(tolower))
clean_corpus <-tm_map(clean_corpus, stemDocument)
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("english"))
clean_corpus <- tm_map(clean_corpus, removePunctuation)
clean_corpus <- tm_map(clean_corpus, stripWhitespace)
word.counts<-as.matrix(TermDocumentMatrix(clean_corpus))
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)

##Top de palabras frecuentes?

clean_corpus <- tm_map(clean_corpus, removeWords, c("the","for","twitter","false","to"
                                                     ,"and","a","of","is","in","you",
                                                     "realdonaldtrump","i","on","be",
                                                     "will","trump","great","that","are",
                                                     'i', 'me', 'my', 'myself', 'we', 'our', 
                                                     'ours', 'ourselves', 'you', "you're", "you've", 
                                                     "you'll", "you'd", 'your', 'yours', 'yourself', 
                                                     'yourselves', 'he', 'him', 'his', 'himself', 'she', 
                                                     "she's", 'her', 'hers', 'herself', 'it', "it's",
                                                     'its', 'itself', 'they', 'them', 'their', 'theirs', 
                                                     'themselves', 'what', 'which', 'who', 'whom', 'this',
                                                     'that', "that'll", 'these', 'those', 'am', 'is', 'are', 
                                                     'was', 'were', 'be', 'been', 'being', 'have', 'has',
                                                     'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 
                                                     'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 
                                                     'while', 'of', 'at', 'by', 'for', 'with', 'about',
                                                     'against', 'between', 'into', 'through', 'during', 'before', 
                                                     'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 
                                                     'out', 'on', 'off', 'over', 'under', 'again', 'further', 
                                                     'then', 'once', 'here', 'there', 'when', 'where', 'why', 
                                                     'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 
                                                     'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 
                                                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 
                                                     'just', 'don', "don't", 'should', "should've", 'now', 'd', 
                                                     'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 
                                                     'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 
                                                     'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 
                                                     'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', "mustn't", 
                                                     'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 
                                                     'wasn', "wasn't", 'weren', "weren't", 'won', "won't", 'wouldn',
                                                     "wouldn't"
))
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(dplyr)
wordcloud(words=names(word.freq), freq=word.freq, scale=c(4,.3),max.words = 150, 
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))

dtm <- TermDocumentMatrix(clean_corp)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)



