library(amap)
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
cl <- kmeans(x, 2)
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)
result1 = cbind(x, cl$cluster)
write.csv(result1, "result.csv")
cl <- kmeans(x, 5, nstart = 25)
c1
c1
points(cl$centers, col = 1:5, pch = 8, cex = 2)


x = c(rnorm(200, 30, 1), rnorm(200, 10, 1.5), rnorm(100, 5, 0.5))
y = c(rnorm(200, 30, 1), rnorm(200, 10, 1.5), rnorm(100, 5, 0.5))
data = data.frame(x, y)
str(data)
library(colorspace)
library(rattle)
library(cluster)
require(amap, quietly = TRUE)
require(fpc, quietly = TRUE)
require(cba, quietly = TRUE)
chcluster <-  hclusterpar(na.omit(data[, c(1:2)]), method = 'manhattan', link = 'ward')
chcluster
centers.hclust(na.omit(data[, c(1:2)]), chcluster, 3)
par(bg='grey')
plot(chcluster, main="", sub = "", xlab = "",  labels =  FALSE,  hang = 0)
rect.hclust(chcluster, k=3)

par(bg='yellow')
plotcluster(na.omit(data[, c(1:2)]), cutree(chcluster, 3))
plot(data[, c(1:2)], col=cutree(chcluster, 3))

cluster.stats(dist(data[, c(1:2)]), cutree(chcluster, 3))

?dist

library(klaR)
set.seed(1)
x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
           matrix(rbinom(250, 2, 0.75), ncol = 5))
colnames(x) <- c('a', 'b', 'c', 'd', 'e')

head(x)
cl = kmodes(x, 2)

plot(jitter(x), col=cl$cluster)
