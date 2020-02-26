rm(list=ls())
# Load
library(tm)
require("NLP")
require("openNLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library(NMF)

###########################################
###########  Corpus and TDM    ############
###########################################
docs <- Corpus(DirSource("~/Desktop/all/UR/2019 FALL B/CIS 434 social media/hw3/yelp"))
dtm.control <- list(tolower=T, removePunctuation=T, removeNumbers=T,
                    stopwords=c(stopwords("english"), stopwords("spanish"),stopwords("portuguese")),
                    stemming=F, weighting=weightTfIdf)

tdm.full <- TermDocumentMatrix(docs, control = dtm.control )
tdm<- removeSparseTerms(tdm.full, 0.97)
idx <- colSums(as.matrix(tdm))>0
tdm = tdm[,idx]
#Use the function res=nmf(X, 20) from the R library NMF to project, the terms and documents into a 20-dimensional semantic space.
X= as.matrix (tdm)
dim(X)
r= 20
res = nmf(X, r)

#Use res@fit@W to retrieve the base matrix W.
W= res@fit@W
H= res@fit@H

approxi = W%*%H
diff=X-approxi
norm(diff, 'F')
approxi[1:3, 1:3]
X[1:3, 1:3]


#Plot a word cloud for the following semtantic feature: 1, 2, 10,17, 20
k=1
wordcloud(names(W[, k]), W[, k], max.words=15, scale=c(0.8,1.5))
k=2
wordcloud(names(W[, k]), W[, k], max.words=15, scale=c(0.8,1.5))
k=10
wordcloud(names(W[, k]), W[, k], max.words=15, scale=c(0.8,1.5))
k=17
wordcloud(names(W[, k]), W[, k], max.words=15, scale=c(0.8,1.5))
k=20
wordcloud(names(W[, k]), W[, k], max.words=15, scale=c(0.8,1.5))



#Place the 300 documents in a 2-dimensional semantic space and Use k-means to cluster the documents.
library(ggplot2)
docs1 <- Corpus(DirSource("~/Desktop/all/UR/2019 FALL B/CIS 434 social media/hw3/specialty3"))
tdm.control <- list(tolower = T, removePunctuation = T, removeNumbers = T,
                    stopwords = c(stopwords('english'),stopwords('spanish'),stopwords('portuguese')),
                    stemming = F,
                    weighting = weightTfIdf)
tdm.full <- TermDocumentMatrix(docs1,control = tdm.control)
tdm <- removeSparseTerms(tdm.full,0.99)

X = as.matrix(tdm)
X.svd = svd(X)
D = diag(X.svd$d)
U <- X.svd$u
V <- X.svd$v

###########################################
#####   2-dimensional Reconstruction  #####
###########################################
dim=2
Uk = U[,seq(1,dim)]
Dk = D[seq(1,dim),seq(1,dim)]
Vk = V[,seq(1,dim)]
rownames(Uk) = rownames(X)
rownames(Vk) = colnames(X)

R = Uk %*% Dk %*% t(Vk) 
term.proj = Uk %*% Dk
doc.proj = Dk %*% t(Vk)
dim(doc.proj)
### 2. Place the 300 documents in a 2-dimensional semantic space.
require(ggplot2)

doc.plot <- data.frame(x=doc.proj[1,], y=doc.proj[2,], names=colnames(doc.proj))
doc.plot$clr = gsub('-\\d+', "", doc.plot$names)
ggplot(doc.plot, aes(x,y, color=clr)) + geom_point() 
#Cluster three kinds of doctors by finding the patterns

doc.plot2 <- data.frame(x=doc.proj[1,],
                        y=doc.proj[2,])

km.out = kmeans(doc.plot2, 3)
doc.plot2$cluster  = km.out$cluster 
doc.plot2$names = gsub("-\\d+", "", colnames(doc.proj))
doc.plot2$DocID=1:300

ggplot(doc.plot2, aes(DocID, cluster, color=names)) + geom_point()
#Three clusters

doc.plot2 <- data.frame(x=doc.proj[1,],
                        y=doc.proj[2,])

km.out = kmeans(doc.plot2, 4)
doc.plot2$cluster  = km.out$cluster 
doc.plot2$names = gsub("-\\d+", "", colnames(doc.proj))
doc.plot2$DocID=1:300
ggplot(doc.plot2, aes(DocID, cluster, color=names)) + geom_point()
#Four Clusters




