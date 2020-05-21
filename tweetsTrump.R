#==============================================#
#NOMBRE: Lorena Leal Garcia
#ID: 208313
#MINERIA DE DATOS NRC:16816
#==============================================#
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(qdap)
#carga de los datos
tweets <- read.csv(file = "/home/newuser/Descargas/trumptw.csv")
#Separando la columna texto de interes
tweets_text <- (tweets$text)
#Conteo inicial sin limpieza
#Tabla con la frecuencia de las palabras DataSet inicial.
terminos_frecu <- freq_terms(tweets_text)#terminos frecuentes
plot(terminos_frecu)#Grafica de frecuencia
head(terminos_frecu)#Cabecera de terminos frecuentes conteo
#=========================Corpus==============================
#1) Crear corpus = Convertir vector en objeto
tweets_sorce <- VectorSource(tweets_text)
#2) Convertir a volatir Volatile = significa que se guardara en
#memora RAM y no en disco esto es para mejor rendimienti
tweets_corpus <- VCorpus(tweets_sorce)
#Primer wordCloud SIN LIMPIEZA
wordcloud(tweets_corpus, max.words = 200, random.order = FALSE,
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#========================Cleanning===========================
clean_corpus <- tm_map(tweets_corpus, content_transformer(tolower))#minisculas
clean_corpus <-tm_map(clean_corpus, stemDocument)#stemming wordStructure
clean_corpus <- tm_map(clean_corpus, removeWords, stopwords("english")) #stopwords
clean_corpus <- tm_map(clean_corpus, removePunctuation) # puntuacion
clean_corpus <- tm_map(clean_corpus, stripWhitespace) #espacion en blanco
#limpieza otras palabras
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
#Convertir corpus resultante en Matriz 
word.counts<-as.matrix(TermDocumentMatrix(clean_corpus))
#Se realiza el conteo de palabras frecuentes en la matriz
word.freq<-sort(rowSums(word.counts), decreasing=TRUE)
#COnteo de palabras como dataFrame
word.freq.table <- as.data.frame(word.freq)
head(word.freq)#COnteo palabras
#Segundo wordCloud LIMPIO
wordcloud(words=names(word.freq), freq=word.freq, scale=c(4,.3),max.words = 150, 
          random.order = FALSE, colors=brewer.pal(8, "Dark2"))



