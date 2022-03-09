setwd('E:/tai lieu nam 4/phan tich du lieu')
data = read.table('data5.txt', header = T)
head(data)
data_dist <- dist(data, method = "euclidean")
hc_complete <- hclust(data_dist, method = "complete")
plot(hc_complete, cex = 0.75)
rect.hclust(hc_complete, k = 3)
install.packages("dendextend")
library(dendextend)
hc_complete_dend <- as.dendrogram(hc_complete)
plot(colour_branches(hc_complete_dend, k=3))
set.seed(12)
data_kmeans <- kmeans(data, 3)
install.packages("factoextra")
library(factoextra)
colnames(data)
fviz_cluster(object = list(data = data,
                           cluster = cutree(hc_complete, k = 3)),
             choose.vars = c(1, 2),
             geom = "point",
             show.clust.cent = FALSE,
             main = "Agglomerative HC - Complete Linkage")+
  theme(legend.position = "none")
fviz_cluster(object = list(data = data,
                           cluster = data_kmeans$cluster),
             choose.vars = c(1, 2),
             geom = "point",
             show.clust.cent = FALSE,
             main = "K-Means")+
  theme(legend.position = "none")
