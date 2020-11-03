#linear regression for the cluster
for(i in 2:23){
  if(i == 12) next 
  lin_reg <- lm(clusters == i ~ acousticness + danceability + duration_ms + energy + instrumentalness + key + liveness + loudness + mode +  popularity + speechiness + tempo + valence, data = df_artists_clustered_2)
  print(ols_step_both_p(lin_reg))
}
