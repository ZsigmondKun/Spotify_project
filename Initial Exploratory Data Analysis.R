
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggstatsplot)
library(lubridate)
library(reshape2)

# Opening the dataset -----------------------------------------------------

setwd('C:/Users/Administrator/Desktop/Big Data Analytics/project')
df <- read.csv('data_by_artist.csv',header = TRUE, sep = ",")
df <- as.data.frame(df)

# Reordering the columns, converting the duration variable to minutes, and the popularity to integers for easier grouping -------------

colorder <- c('artists','count', 'popularity','duration_ms','acousticness','danceability','energy',
              'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence')
df <- df[,colorder]
df$duration_ms <- (df$duration_ms/1000)/60
df$popularity <- sapply(df$popularity,ceiling) 

# Exploratory Data Analysis -----------------------------------------------

unique_artists <- unique(df$artists) #there are 27261 artists in our dataset
lapply(df,class) # the formats of the variables seem to be in order - no categorical variables that would complicate things 

#studying popularity distribution
popularity_distribution <- ggplot(df, aes(x= popularity)) +
                            geom_histogram(binwidth=.7, colour="black", fill="white") +
                            geom_vline(aes(xintercept=mean(popularity, na.rm=T)),           
                            color="red", linetype="dashed", size=1) +
                            geom_text(aes(x= 28,label= 'mean', y = 2000))

by_popularity <- df %>% group_by(popularity) %>% tally() # a great chunk of artists(4057) have 0 popularity, whereas the mean is 34.48

#creating a joint histogram encompassing all the descriptive variables...
descriptive_variables <- c('duration_ms','acousticness','danceability','energy',
              'instrumentalness','key', 'liveness','loudness','mode','speechiness','tempo','valence')
df_descriptive <- df[,descriptive_variables] 


