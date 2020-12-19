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

#determining the appropriate number of clusters
#num_clusters <- NbClust(sample(df_full, size = 1000),method = 'kmeans',max.nc = 25) it yields 23, just takes ages for it to run, therefore it is commented out

#clustering and joining them to the df_full dataframe 
df_full_clust <- kmeans(df_full_scaled, centers = 23, nstart = 25)
df_full_with_clusters <- cbind(df_full, clusters = df_full_clust$cluster) 

#checking how good the fit is 
cluster_fit <- ceiling(df_full_clust$betweenss/df_full_clust$totss*100) # this yield 60% which is pretty good, we can further experiment by removing variables to see if that causes any improvement

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

#cluster distribution histogram
cluster_distribution <- df_artists_clustered %>% group_by(clusters) %>% tally()
ggplot(cluster_distribution, aes(x = clusters, y = n)) +                  
  geom_bar(stat="identity") 
cluster_distribution_matrix <- as.matrix(cluster_distribution)

# lets explore the clusters! ---------------------------------------------- we have to shortly describe each clusters (we could even give some fancy name to each that the user can select as his/her input) a good starting would be do highlight the variable which is describes each cluster the most.i cannot seem to figure out how to do that non-manually 
cluster_analysis <- df_artists_clustered
cluster_analysis$artists <-  NULL
table1(~ acousticness + danceability + duration_ms + energy + instrumentalness + key + liveness + loudness + mode +  popularity + speechiness + tempo + valence | clusters, data=cluster_analysis, overall = F, transpose = TRUE, topclass="Rtable1-grid Rtable1-shade Rtable1-times") 

#some initial lines on our predictive model

#cluster_analysis %>% 
  #filter(clusters == x, danceability == y, duration_ms == z...) %>% 
  #sample_n(5)

#practically we have to write a function, which takes the values x-y-z etc. from the user and outputs some number of artists being the closest to these input numbers. Definitely doable 








