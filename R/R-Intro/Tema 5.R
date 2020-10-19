#--------------------------------------------------------------------------------
#                             MINERÍA DE TEXTO
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Analizar noticias de periódicos 

# MATERIAL: Texto.

#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Crear corpus
#       4. Limpiar texto
#           4.1 Definimos función
#           4.2 Limpieza
#       5. Palabras vacías
#       6. Matriz
#       7. Nube de palabras
#       8. Frecuencias de palabras
#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls())                  
graphics.off()                 
options(warn=-1)              


#1. Cargar librerías
install.packages(" ")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(foreign)  
library(dplyr)
library(ggplot2)
library(igraph)


#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/Desktop/FLACSO-R-2020-main/Datos")

#3. Crear corpus

docs <- Corpus(VectorSource(readLines("TEXTO.txt", encoding = "UTF-8")))

#4. Limpiar texto

#4.1 Definimos función
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

#4.2 Limpieza
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, ";")
rdocs <- tm_map(docs, toSpace, "¿")

docs <- tm_map(docs, toSpace, ":")

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("spanish"))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)

inspect(docs)


#5. Palabras vacías

docs <- tm_map(docs, removeWords, c("pues","tenia")) 


#6. Matriz

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#7. Nube de palabras

set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=400, random.order=FALSE, rot.per=0.7, 
          colors=brewer.pal(8, "Dark2"))


#8. Frecuencias de palabras

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Palabras más frecuentes",
        ylab = "Frecuencias")


  #---------------------------------------
  # RETO-6:
  # ¿De qué habla el artículo? 
  #---------------------------------------




#9. Análisis de discurso

datalist = list()
ter<-c("presidente")
i<-1
for (t in ter){
  temp<-data.frame(findAssocs(dtm, terms = t, corlimit = 0.3))
  temp<-mutate(temp,word=t,termino=row.names(temp))
  names(temp)<-c("n","word","termino")
  datalist[[i]] <-temp
  i=i+1
}
base<-do.call(rbind, datalist)
write.table(base, file = "base.csv", row.names = FALSE, sep=",")
base2<- read.csv("base.csv", sep=",", header=TRUE)


s   <- c(base2[,2])
r   <- c(base2[,3])
w   <- c(base2[,1])
el  <- matrix(c(s,r,w), length(s), 3)

#
labels<- unique(c(as.character(base2[,2]),as.character(base2[,3])))
A<- matrix(0, length(labels), length(labels))
rownames(A) <- colnames(A) <- labels
A[el[,1:2]]<-as.numeric(el[,3])
m=as.matrix(A)
net=graph.adjacency(m,mode="directed",weighted=TRUE,diag=FALSE) #the only difference between this and the weighted network code is that mode="directed"
plot.igraph(net,vertex.label=V(net)$name,layout=layout.fruchterman.reingold, vertex.label.color="black",edge.color="black",edge.width=E(net)$weight/3, edge.arrow.size=0.05)

