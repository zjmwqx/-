csv =  read.csv("train.csv", header = T, sep='\t', stringsAsFactors = F, encoding = 'UTF-8')
mystopwords <- unlist(read.table("StopWords.txt", stringsAsFactors = F))
library(NLP)
library(tm)
library(Rwordseg)
removeNumbers = function(x) {ret = gsub("[0-9０１２３４５６７８９]", "", x)}
removeStopWords = function(x, words) {
  ret = character(0)
  index <- 1
  it_max <- length(x)
  while (index <= it_max) {
    if (length(words[words == x[index]]) < 1)ret <- c(ret, x[index])
    index <= index + 1
  }
  ret
}
sample.words <- lapply(csv$text, removeNumbers)
sample.words <- lapply(sample.words, segmentCN)
sample.words <- lapply(sample.words, removeStopWords, mystopwords)

corpus = Corpus(VectorSource(sample.words))
meta(corpus, "cluster") <- csv$type
unique_type = unique(csv$type)

sample.dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf)))
library(wordcloud)

sample.tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf)))
tdm.matrix <- as.matrix(sample.tdm)
png("sample_comparison.png", width = 1500, height = 1500)
par(family='STKaiti')
comparison.cloud(family='STKaiti', tdm.matrix)
title(main = "smaple comparision")
dev.off()

n<-nrow(csv)
zz1 = 1:n
cluster_matrix<-sapply(unique_type, function(type){apply(tdm.matrix[,zz1[csv$type==type]], 1, sum)})
png("sample_cluster_comparision.png", width = 800, height = 800)
comparison.cloud(family='STKaiti',cluster_matrix)
title(main="smaple cluster comparison")
dev.off()

sample.cloud<-function(cluster, maxwords = 100) {
  words <- sample.words[which(csv$type == cluster)]
  allwords <- unlist(words)
  wordsfreq <- sort(table(allwords), decreasing = T)
  wordsname = names(wordsfreq)
  png(paste("sample_", cluster, ".png", sep = ""), width = 600, height = 600)
  wordcloud(wordsname, wordsfreq, family='STKaiti', scale= c(6, 1.5), min.freq=2, max.words = maxwords, 
            colors = rainbow(100))
  title(main = paste("cluster:", cluster))
  dev.off()
}
lapply(unique_type, sample.cloud)


#特征提取
library(slam)
summary(col_sums(sample.dtm))
term_tfidf <- tapply(sample.dtm$v/row_sums(sample.dtm)[sample.dtm$i], sample.dtm$j, mean) * log2(nDocs(sample.dtm)/col_sums(sample.dtm > 0))
summary(term_tfidf)

#聚类
sample_matrix = as.matrix(sample.dtm)
rownames(sample_matrix) <- csv$type
k <- length(unique(csv$type))
sample_KMeans <- kmeans(sample_matrix, k)
library(clue)
cl_agreement(sample_KMeans, as.cl_partition(csv$type), "diag")

n<-nrow(csv)
set.seed(100)
zz1 <- 1:n
zz2 <- rep(1:k, ceiling(n/k))[1:n]
zz2 <- sample(zz2, n)
train <- sample_matrix[zz2 < 10,]
test <- sample_matrix[zz2 == 10,]
trainC1 <- as.factor(rownames(train))

#knn分类
library(class)
sample_knnCl <- knn(train, test, trainC1)
trueC1 <- as.factor(rownames(test))

(nnTable <-table("1-NN" = sample_knnCl, sample = trueC1))
sum(diag(nnTable)) / nrow(test)
