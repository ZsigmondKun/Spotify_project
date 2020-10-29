# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggstatsplot)
library(NbClust)
library(VIM)
library(boot)
library(table1)
library(DT)

# K-means clustering ------------------------------------------------------ # for the artist dataset the recommended cluster was 2 , so I try to get the some reasonable number using the overall dataset, and then extrapolate it to the artists
df_full <- read.csv('data.csv',header = TRUE, sep = ",")
col_for_clustering <- c('popularity','duration_ms','acousticness','danceability','energy',
                        'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence')
df_full_scaled <- scale(df_full[complete.cases(df_full),col_for_clustering])

#num_clusters <- NbClust(sample(df_full, size = 1000),method = 'kmeans',max.nc = 25) it yields 23, just takes ages for it to run, therefore it is commented out

df_full_clust <- kmeans(df_full_scaled, centers = 23, nstart = 25)
df_full_with_clusters <- cbind(df_full, clusters = df_full_clust$cluster) 

cluster_distribution <- df_full_with_clusters %>% group_by(clusters) %>% tally()
ggplot(cluster_distribution, aes(x = clusters, y = n)) +                  #besides one cluster (it has 408 songs), each one is populated 'enough', I think it is fine :)
  geom_bar(stat="identity") 

#since the cluster is done on the basis of songs, we have to find the most frequent cluster for each artist
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}
artists_by_clusters <- aggregate(clusters ~ artists, df_full_with_clusters, calculate_mode)

#in order to match the clustered artists with the column in the data_by_artists, we have to format them alike, and get rid of the rows containing multiple artists 
artists_by_clusters$artists <- sapply(artists_by_clusters$artists, function(x) gsub("\\[|]|'","",x))
artists_by_clusters <- artists_by_clusters[!grepl(",", artists_by_clusters$artists),] #16692 artists clustered

# now we have the main data clustered, lets extrapolate it to the data_by_artists
df_artists <- read.csv('data_by_artist.csv',header = TRUE, sep = ",")
df_artists_clustered <- left_join(df_artists, artists_by_clusters, by = 'artists')
df_artists_clustered <- na.omit(df_artists_clustered)

#lets explore the clusters!
cluster_analysis <- df_artists_clustered
cluster_analysis$artists <-  NULL
table1(~. | clusters, data=cluster_analysis, overall = F, transpose = TRUE, topclass="Rtable1-grid Rtable1-shade Rtable1-times") 

