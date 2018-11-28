#Sys.setenv('R_MAX_VSIZE'= 40000000000)
#install.packages(c('ggplot2', 'fpc', 'dbscan', 'factoextra', 'clustertend', 'clValid', 'cluster'), dependencies = T)

library('fpc')
library('dbscan')
library('factoextra')
library('ggplot2')
library(clustertend)
library(clValid)
library(clusterCrit)

df <- read.csv('../dataframes/enh/enh_grouped_2016.csv')
data <- df[, 3:62]
data <- as.matrix(data)
#data <- scale(x = data)
#data$id <- df[, 1] + 1

# Hopkins statistic
set.seed(123)
hopkins <- clustertend::hopkins(data = data, n = nrow(data)-1)
hopkins

# dist
dist_euc <- stats::dist(x = data, method = "euclidean")

# VAT
vat <- factoextra::fviz_dist(dist.obj = dist_euc, show_labels = F)
vat + labs(title='data')

dunn_km <- c()
range_km <- c(2:10)
i <- 1

data <- lapply(data, as.numeric)


for (h in range_km) {
  set.seed(123)
  km <- stats::kmeans(x = data, centers = h, nstart = 25)
  
  index_internal_km <- clusterCrit::intCriteria(traj = data,
                                                part = as.integer(km$cluster), 
                                                crit = "dunn")
  dunn_km[i] <- index_internal_km$dunn
  
  i <- i + 1
}

plot_dunn_km <- ggplot2::qplot(x = range_km, 
                               y = dunn_km, 
                               geom = 'line', 
                               main = 'Dunn Index: k-means', 
                               xlab = 'Clusters', ylab = 'Dunn Index')

plot_dunn_km + 
  theme_bw() +
  geom_vline(xintercept = range_km[which.max(dunn_km)], linetype = 2, color='red') 
  ggsave(filename = "dunn_kmeans.png", dpi = 300)




km <- stats::kmeans(x = data, centers = range_km[which.max(dunn_km)], nstart = 25)

plot_km <- factoextra::fviz_cluster(object = km, 
                                    # shape = nrow(data),
                                    data = data,
                                    stand = F, 
                                    geom =  'point', 
                                    ellipse.type = 'convex',
                                    ellipse = T,
                                    main = 'Clustering:k-means k:7',
                                    outlier.color = 'black',
                                    show.clust.cent = T)

plot_km + 
  theme_bw() +
  theme(legend.position = 'bottom')
  ggsave(filename = "plot_kmeans.png", dpi = 300)

pca <- stats::prcomp(x = data)
plot_pca <- factoextra::fviz_eig(X = pca, main = "PCA: ENH") +
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(filename = "plot_pca.png", dpi = 300)

# compactness <- diceR::compactness(data = data, labels = km$cluster)
# stats_km <- fpc::cluster.stats(d = dist, clustering = km$cluster)

library(clusterCrit)
index_internal_km <- clusterCrit::intCriteria(traj = data,
                                              part = as.integer(km$cluster), 
                                              crit = "all")
# index_internal_km$calinski_harabasz
# index_internal_km$silhouette

write.csv(x = km$cluster, file = "enh_cluster.csv")
