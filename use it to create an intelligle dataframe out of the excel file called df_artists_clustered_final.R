setwd('C:/Users/Administrator/Desktop/Big Data Analytics/project')
df_final_cluster <- read.csv('df_artists_clustered_final.csv',header = TRUE, sep = ";")
col_to_be <- c('artists', 'clusters')
df_final_cluster_artists <- df_final_cluster[,col_to_be] 

df_artists <- read.csv('data_by_artist.csv',header = TRUE, sep = ",")
df_artists_clustered_new <- left_join(df_artists, df_final_cluster_artists, by = 'artists')
df_artists_clustered_new <- na.omit(df_artists_clustered_new)

