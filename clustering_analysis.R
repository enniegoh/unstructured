# clustering analysis

################################################################################
# Task 2: Perform text clustering.
## 1. Using the dataset created in Data acquisition section, construct data clustering by using k-means, hierarchical and HDBScan algorithms.
### import data
library(tm)
cleaned_news <- Corpus(DirSource("Task2", encoding = "UTF-8"))
writeLines(as.character(cleaned_news[[1]]))



### document-term matrix
cleaned_tdm <- DocumentTermMatrix(cleaned_news)
cleaned_tdm
tdm.tfidf <- weightTfIdf(cleaned_tdm)
tdm_matrix <- as.matrix(tdm.tfidf)



### Cosine distance matrix  
library(proxy)
distance_matrix <- dist(tdm_matrix, method = "cosine")

truth.K = 4
set.seed(1234)
### Perform clustering
library(dbscan)
clustering.kmeans <- kmeans(tdm_matrix, truth.K) 
clustering.hierarchical <- hclust(distance_matrix, method = "ward.D2") 
clustering.dbscan <- hdbscan(distance_matrix, minPts = 10)



### k-means
library(cluster)
clusplot(as.matrix(distance_matrix), clustering.kmeans$cluster, color = T, shade = T, labels = 2, lines = 0)



### hierarchical clustering
plot(clustering.hierarchical)
rect.hclust(clustering.hierarchical, 4)



### HDBScan
plot(as.matrix(distance_matrix), col = clustering.dbscan$cluster + 1L)



## 2. Perform the relevant analysis (including the relevant visualization) on each of the clustering algorithms and compare the result
### combine results
master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = truth.K) 
slave.dbscan <- clustering.dbscan$cluster  



### k-means
table(master.cluster)

### plot the combined results for k-means
library(colorspace)
points <- cmdscale(distance_matrix, k = truth.K) 
palette <- diverge_hcl(truth.K)

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')



### hierarchical clustering
table(slave.hierarchical)

### plot the combined results for hierarchical clustering
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')



### HDBScan
table(slave.dbscan)

### plot the combined results for HDBScan
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

