library(cluster)

recommend = function (song, dataset, clustering){
  centroids = clustering$centers
  includes = setdiff(colnames(song), "id")
  datamean = colMeans(dataset[,includes])
  datasd = apply(dataset[,includes], 2, FUN = sd)
  song[includes] = scale(song[includes], center = datamean, scale = datasd)
  candidates = as.data.frame(rbind(song[includes], centroids))
  rownames(candidates) = c("Song", paste("Cluster", 1:nrow(clustering$centers)))
  
  closeness = as.data.frame(as.matrix(get_dist(candidates)))
  
  #print(closeness)
  
  closest = min(closeness[1,-1])
  chosen_cluster = which(closeness[1,-1] == closest)

  dataset_sc = dataset
  dataset_sc[,-1] = scale(dataset[,-1])
  
  songs_cluster = dataset_sc[clustering$cluster == chosen_cluster,]
  all_together = as.data.frame(rbind(song, songs_cluster))
  finally = as.data.frame(as.matrix(get_dist(all_together[,-1])), row.names = all_together$id)
  colnames(finally) = all_together$id
  
  index = which(finally[[song$id]] == min(finally[[song$id]][-1]))
  chosen_one_id = colnames(finally)[index]
  chosen_one = dataset[dataset$id == chosen_one_id,]
  
  return (chosen_one)
}

'
A continuación, se da un código muy simple para comprobar el funcionamiento de la funcion "Recomendador".
Lo único que hay que tener en cuenta es que para utilizar la función, se ha facilitado un fichero de datos 
llamado "new_songs.csv" en el que se encuentran los datos de unas pocas canciones para pasar como parámetros.
'
nuevos = read.csv("new_songs.csv", as.is = TRUE)
song = nuevos[3,c('id','acousticness','instrumentalness','speechiness','loudness')]
#song
songs = ncdata[ncdata$duration_ms > 18000,]
songs = songs[songs$duration_ms < 300000,]
songs = songs[songs$popularity > 70,]
songs = songs[,c('id','acousticness','instrumentalness','speechiness','loudness')]

rec =recommend(song,songs,clustering_4)
#rec
rec1 = as.matrix(rec)
rec1[1,'id']
names=c('you dont know me','vivir asi es morir de amor','Pareja del año','O-o-h child','Univers')





print('Si escuchas "La pareja del año", te recomendamos: ')
print(ncdata[which(ncdata$id==rec1[1,'id']),c('name','artists')])
