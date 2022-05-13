
install.packages('readr')
library('readr')
datos=read_csv(file.choose(),col_names = FALSE)
colnames(datos)=c('track_id','song_id','artist_name','song_title')
users=read_tsv(file.choose(),col_names = FALSE)
colnames(users)=c('user_id','song_id','times_list')
mas_escuchadas=users[users$times_list>1000,'song_id']
library('dbplyr')
mas_escuchadas=merge(mas_escuchadas,datos,by='song_id')
datos[datos$artist_name=='Michael Jackson',]
users[users$song_id=='SOTLHDK12AB018A405',]
