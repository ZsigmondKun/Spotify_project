
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggstatsplot)
library(lubridate)
library(reshape2)
library(ggthemes)
library(xts)
library(rpart)
library(corrplot)
library(Hmisc)
library(factoextra)
library(VIM)
library(NbClust)

# Opening the dataset -----------------------------------------------------

setwd('C:/Users/Administrator/Desktop/Big Data Analytics/project')
df <- read.csv('data_by_artist.csv',header = TRUE, sep = ",")
df <- as.data.frame(df)
aggr(df) #there seems to be no missing data

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
by_popularity <- as.matrix(by_popularity)
colnames(by_popularity)[colnames(by_popularity) == "n"] <- "number of artists"

#creating a joint histogram encompassing all the descriptive variables...
descriptive_variables <- c('duration_ms','acousticness','danceability','energy',
              'instrumentalness','key', 'liveness','speechiness','tempo','valence')
df_descriptive <- df[,descriptive_variables] 
df_descriptive_scaled <- data.frame(scale(df_descriptive)) # since the variables have varying distribution, we first must normalize, if we want to visualize them in one histogram

df_descriptive_scaled %>%
  # Reshape
  gather(key = indicator, value = val) %>%
  # Basic chart
  ggplot(aes(x =val)) +
  geom_histogram(colour = "darkgreen", fill = "gray",bins = 100) +
  coord_cartesian(xlim = c(-2, 1), expand = FALSE) +
  facet_wrap(~indicator, nrow = 2) +                                             
  ## Theme and looks 
  theme_economist() +
  ggtitle("The distribution of the descriptive variables") +
  theme(strip.background = element_rect(fill = "gray80", colour = "black",
                                        size = 1, linetype = "solid"),
        strip.text = element_text(face = "bold"), axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_blank())

#what determines popularity? - step-wise selection of relevant variables
df_null <- lm(popularity ~ 1, df)
df_full <- formula(lm(popularity ~.,df))
df_step <- step(df_null, scope = df_full, direction = 'both')
stepwise_model_popularity <- as.data.frame(df_step$anova) #acousticness seems to be the most important factor predicting popularity, all the variables can explain 46 % of the variance behind popularity

#linear regression with the most relevant variable
df_lm <- lm(popularity ~ acousticness, df)
summary(df_lm) # a linear regression consisting of only acousticness explains 39 % of the variance, thus the remaining 12 only explains 7 % 

#correlation table
df_corr <- rcorr(as.matrix(df_descriptive)) 
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    variable1 = rownames(cormat)[row(cormat)[ut]],
    variable2 = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p_value = pmat[ut]
  )}
df_corr_flattened <- flattenCorrMatrix(df_corr$r,df_corr$P)
corr_visualization <- corrplot(df_corr$r, type="upper", order="hclust", 
                      p.mat = df_corr$P, sig.level = 0.01, insig = "blank")  #this table nicely complements the previous regression analysis by shedding light on the negative relationship between acousticness and popularity

#annual average of the descriptive variables
df_yearly <- read.csv('data_by_year.csv',header = TRUE, sep = ",")
df_yearly <- as.data.frame(df_yearly)
#acousticness 
yearly_acousticness <- ggplot(df_yearly) +
                        aes(x = year, y = acousticness) +
                        geom_point(size = 1L, colour = "#0c4c8a") +
                        geom_smooth(span = 0.75) +
                        ggthemes::theme_gdocs()
#danceability
yearly_danceability <- ggplot(df_yearly) +
                        aes(x = year, y = danceability) +
                        geom_point(size = 1L, colour = "#0c4c8a") +
                        geom_smooth(span = 0.75) +
                        ggthemes::theme_gdocs()






