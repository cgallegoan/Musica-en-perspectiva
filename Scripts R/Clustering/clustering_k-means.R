library(cluster)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(clValid)


###############################################################################
###                           VARIABLES A INDICAR                           ###
###############################################################################
   
load(file = file.choose())  #Cargad ncdata
load(file = file.choose())  #Cargad desc_data

excludes = c(3, 10, 15)
pop_filter = 50  #Filtro de popularidad
year_min_filter = 1970   #Filtro de año (menor)
year_max_filter = 1980   #Filtro de año (mayor)

###############################################################################
###                                  CÓDIGO                                 ###
###############################################################################

songs = ncdata[desc_data$type == "numerical" | desc_data$type == "binary"]
songs = songs[songs$year >= year_min_filter & songs$year <= year_max_filter,]
songs = songs[songs$popularity >= pop_filter,]
songs = songs[,-excludes]
songs = scale(songs)

midist = get_dist(songs, method = "euclidean")

fviz_nbclust(x = songs, FUNcluster = kmeans, method = "silhouette", 
             k.max = 15, verbose = FALSE) +
  labs(title = "Num. clusters")

fviz_nbclust(x = songs, FUNcluster = kmeans, method = "wss", 
             k.max = 15, verbose = FALSE) +
  labs(title = "Num. clusters")

###############################################################################
###                            VARIABLES A INDICAR                          ###
###############################################################################

K = c(2, 4, 6)

set.seed(12710)

clustering_2 <- kmeans(songs, centers = 2, nstart = 50, iter.max = 50)
clustering_4 <- kmeans(songs, centers = 4, nstart = 50, iter.max = 50)
clustering_6 <- kmeans(songs, centers = 6, nstart = 50, iter.max = 50)
table(clustering_2$cluster)
table(clustering_4$cluster)
table(clustering_6$cluster)

fviz_cluster(object = list(data=songs, cluster=clustering_2$cluster), stand = FALSE,
             ellipse.type = "convex", geom = "point", show.clust.cent = TRUE,
             labelsize = 8)  +
  labs(title = "K-MEDIAS + Proyeccion PCA",
       subtitle = "Dist euclidea, K=2") +
  theme_bw() +
  theme(legend.position = "bottom")

fviz_cluster(object = list(data=songs, cluster=clustering_4$cluster), stand = FALSE,
             ellipse.type = "convex", geom = "point", show.clust.cent = TRUE,
             labelsize = 8)  +
  labs(title = "K-MEDIAS + Proyeccion PCA",
       subtitle = "Dist euclidea, K=4") +
  theme_bw() +
  theme(legend.position = "bottom")

fviz_cluster(object = list(data=songs, cluster=clustering_6$cluster), stand = FALSE,
             geom = "point", show.clust.cent = TRUE,
             labelsize = 8)  +
  labs(title = "K-MEDIAS + Proyeccion PCA",
       subtitle = "Dist euclidea, K=6") +
  theme_bw() +
  theme(legend.position = "bottom")

plot(silhouette(clustering_2$cluster, midist), col=rainbow(2), border=NA, main = "K-medias")
plot(silhouette(clustering_4$cluster, midist), col=rainbow(4), border=NA, main = "K-medias")
plot(silhouette(clustering_6$cluster, midist), col=rainbow(6), border=NA, main = "K-medias")

data = ncdata[desc_data$type == "numerical" | desc_data$type == "binary"]
data = data[data$year >= year_min_filter & data$year <= year_max_filter,]
data = data[data$popularity >= pop_filter,]

mianova_2 = aov(data$popularity ~ factor(clustering_2$cluster))
mianova_4 = aov(data$popularity ~ factor(clustering_4$cluster))
mianova_6 = aov(data$popularity ~ factor(clustering_6$cluster))

boxplot(data$popularity ~ factor(clustering_2$cluster), col = rainbow(2))
boxplot(data$popularity ~ factor(clustering_4$cluster), col = rainbow(4))
boxplot(data$popularity ~ factor(clustering_6$cluster), col = rainbow(6))

TukeyHSD(mianova_2)
TukeyHSD(mianova_4)
TukeyHSD(mianova_6)
