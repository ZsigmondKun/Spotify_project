
library(dplyr)
library(data.table)

setwd('C:/Users/Administrator/Desktop/Big Data Analytics/project')

# intelligent dataframe out of the excel file -----------------------------
df_final_cluster <- read.csv('df_artists_clustered_final.csv',header = TRUE, sep = ";")
col_to_be <- c('artists', 'clusters')
df_final_cluster_artists <- df_final_cluster[,col_to_be] 

df_artists <- read.csv('data_by_artist.csv',header = TRUE, sep = ",")
df_artists_clustered_new <- left_join(df_artists, df_final_cluster_artists, by = 'artists')
df_artists_clustered_new <- na.omit(df_artists_clustered_new)

#renumbering the clusters.... :(
df_artists_clustered_new_sorted <-  df_artists_clustered_new[with(df_artists_clustered_new, order(clusters)), ]
first_occurance <- df_artists_clustered_new_sorted[match(unique(df_artists_clustered_new_sorted$clusters), df_artists_clustered_new_sorted$clusters),]
rownames(df_artists_clustered_new_sorted) <- NULL
df_artists_clustered_new_sorted$clusters[1:6894] <- df_artists_clustered_new_sorted[1:6894,16]-1
df_artists_clustered_new_sorted$clusters[6895:15833] <- df_artists_clustered_new_sorted[6895:15833,16]-2

# cluster overview --------------------------------------------------------
df_cluster_overview <- read.csv('cluster_final.csv',header = TRUE, sep = ";")
df_cluster_overview <- as.data.frame(df_cluster_overview)

# ENGINE components -------------------------------------------------------   
col_cluster_name <- c('cluster','name','top.10.artists') 
df_cluster_name <- df_cluster_overview[,col_cluster_name] 
colnames(df_cluster_name)[1] <-'clusters'
high_low_cols <- c('acousticness','danceability','energy',
              'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence','artists','clusters','name','top.10.artists')
user_cols <- c('acousticness','danceability','energy',
               'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence') 
user_volume <- c('low', 'high') 
further_info<- c('yes', 'no') 

#adding the name and top 10 artists columns to the df_artists_clustered_new_sorted
df_artists_clustered_new_sorted <- left_join(df_artists_clustered_new_sorted,df_cluster_name, by = 'clusters')

#high_low conversion
df_high_low <- df_artists_clustered_new_sorted[,high_low_cols]
df_high_low$acousticness <- ifelse(df_high_low$acousticness > mean(df_high_low$acousticness), 'high', 'low')
df_high_low$danceability <- ifelse(df_high_low$danceability > mean(df_high_low$danceability), 'high', 'low')
df_high_low$energy <- ifelse(df_high_low$energy > mean(df_high_low$energy), 'high', 'low')
df_high_low$instrumentalness <- ifelse(df_high_low$instrumentalness > mean(df_high_low$instrumentalness), 'high', 'low')
df_high_low$key <- ifelse(df_high_low$key > mean(df_high_low$key), 'high', 'low')
df_high_low$liveness <- ifelse(df_high_low$liveness > mean(df_high_low$liveness), 'high', 'low')
df_high_low$loudness <- ifelse(df_high_low$loudness > mean(df_high_low$loudness), 'high', 'low')
df_high_low$mode <- ifelse(df_high_low$mode > mean(df_high_low$mode), 'high', 'low')
df_high_low$speechiness <- ifelse(df_high_low$speechiness > mean(df_high_low$speechiness), 'high', 'low')
df_high_low$tempo <- ifelse(df_high_low$tempo > mean(df_high_low$tempo), 'high', 'low')
df_high_low$valence <- ifelse(df_high_low$valence > mean(df_high_low$valence), 'high', 'low')

# ENGINE ------------------------------------------------------------------
spotify_engine <- function(){
                  select_cluster <- select.list(unique(df_cluster_name$name), title="Please choose your desired cluster",graphics = getOption("menu.graphics"))
                  select_variable <- select.list(user_cols,title = "Please choose the characteric you are looking for the most; if you want further descriptions on them please visit: https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/ ",graphics = getOption("menu.graphics"))
                  select_volume <- select.list(user_volume, title = "Please choose your desiered intensity of the previously selected characteristic", graphics = getOption("menu.graphics"))  
             
                    if (select_volume == 'high') {
                    index <- which(df_high_low[,14] == select_cluster & df_high_low[,select_variable] == 'high')
                    df_user_high <- df_high_low[index,]
                    df_user_high <- data.table(df_user_high)
                    high_sample <- df_user_high[sample(.N, 5)]
                    high_sample <- as.data.frame(high_sample)
                    print(high_sample$artists)
                    }
                    else {
                    index2 <- which(df_high_low[,14] == select_cluster & df_high_low[,select_variable] == 'low')
                    df_user_low <- df_high_low[index2,]
                    df_user_low <- data.table(df_user_low)
                    low_sample <- df_user_low[sample(.N, 5)]
                    low_sample <- as.data.frame(low_sample)
                    print(low_sample$artists)
                    }
                    
                  select_info <- select.list(further_info, title="Would you like to have further information about the recommended artists? (Y/N)",graphics = getOption("menu.graphics"))
                    if (select_info == 'yes'& select_volume == 'high') {
                      further_cols <- c('acousticness','danceability','energy',
                                        'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence','artists')
                      print(high_sample[,further_cols])
                    } else if(select_info == 'yes'& select_volume == 'low') {
                      further_cols <- c('acousticness','danceability','energy',
                                        'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence','artists')
                      print(low_sample[,further_cols])
                    } else if(select_info =='no') {
                       print('Enjoy!')
                    }}

spotify_engine()                    
                      
                    
                     










