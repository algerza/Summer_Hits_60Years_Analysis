#################################################################
#####                                                       #####
#####            STEP 0: SET UP FOR THE PROJECT             #####
#####                                                       #####
#################################################################

## Install and load the necessary packages
#install.packages('spotifyr')
library("spotifyr")
library(RCurl)
library(dplyr)
library(fmsb)
library(httr)
library(readr)


## Get the credentials to access Spotify's API. You get this from 
##    Spotify here: https://developer.spotify.com/my-applications/#!/applications
Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')
access_token <- get_spotify_access_token()



#################################################################
#####                                                       #####
#####           STEP 1: GET AND TRANSFORM THE DATA          #####
#####                                                       #####
#################################################################


## Get all the playlist for a specific user, in case you don't know your 
##    playlist uri. Once you know the Spotify username and playlist's uri,
##    you can call the package's function to get all the data you need.
##    For this analysis I used this playlist: https://open.spotify.com/playlist/2y9YTrLFbtdIm1fmChWZq3
##    To make it work, you need to specify below the username and playlist uri
##    You can try it out with the playlist link I provided above


## Get all the palylist for a given user 
playlists <- get_user_playlists('username')

## Get the data from a specific playlist owned by a user
data <- get_playlist_audio_features('username', 'playlist_uris',
                                    authorization = get_spotify_access_token())


## Once we get the data, we need to select only some variables for our analysis
data <- data %>% select(track.name, danceability, energy, loudness, acousticness, valence, tempo)

## Alternatively, if you want to skip the previous steps, you can find the dataset on csv format
##    on the GitHub repository: https://github.com/algerza/Summer_Hits_60Years_Analysis
urlfile1="https://raw.githubusercontent.com/algerza/Summer_Hits_60Years_Analysis/master/Summer_Songs_1958_2018_The_Top_Tunes_of_Each_Summer.csv"
data <-read_csv(url(urlfile1))

## In order to plot the data on a radar chart, we need to have similar range of values
##    between 0 and 1. Some of our variables are way above that range, so we need to normalize them
##    with a function we will set
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
  }

## Normalize loudness and tempo on our dataset
data$loudness<-normalize(df$loudness)
data$tempo<-normalize(df$tempo)


# Our tracks are ordered by years but aren't always ordered on the right ranking.
##    Therefore, I created manually a mapping table for each song. You can create
##    your ranking and year based on Billboard's list: https://www.billboard.com/articles/news/513524/summer-songs-1985-present-top-10-tunes-each-summer-listen
##    Or get the mapping table on the GitHub repository

urlfile2="https://raw.githubusercontent.com/algerza/Summer_Hits_60Years_Analysis/master/Summer_Songs_Mapping_Table.csv"
Summer_Songs_Mapping_Table<-read_csv(url(urlfile2))


## Create vectors to help us handle the data easily: decade, year and yearly ranking:
##    1 = Top #1 summer song, 10 = Top #10 summer song
year <- c(Summer_Songs_Mapping_Table$year)
decades <- c(Summer_Songs_Mapping_Table$decade)
rank <- c(Summer_Songs_Mapping_Table$rank)
df <- data.frame(decades, year, rank, data)


#################################################################
#####                                                       #####
#####       STEP 2: SELECT DATA FOR VISUALIZATIONS          #####
#####                                                       #####
#################################################################


## We need a dataframe with each decade's average to plot the overall shift over time: decades
decades_avg <- df %>% 
  group_by(decades) %>%
  summarise_at(c("danceability", "energy", "loudness", "acousticness", "valence", "tempo"), mean, na.rm = TRUE)

decades_avg["track.name"] <- c("Decade Average", "Decade Average", "Decade Average", "Decade Average","Decade Average", "Decade Average", "Decade Average")
decades_avg <- decades_avg[, c("decades", "track.name", "danceability", "energy", "loudness", "acousticness", "valence", "tempo")]


## We need a dataframe with the most popular summer song per year: top_songs
top_songs <- df[df$rank == 1,]
top_songs <- top_songs[, c("decades", "track.name", "danceability", "energy", "loudness", "acousticness", "valence", "tempo")]

## We need to concatenate decades and top_songs dataframes to create a visualization
df_visualization <- rbind(top_songs, decades_avg)


## In order to conclude the data preparation part, we need to divide our dataset by decades, 
##    so we can plot them individually on different radar charts
decade_dataframe_60 <- df_visualization[df_visualization$decades == "60s",]
rownames(decade_dataframe_60) <- decade_dataframe_60$track.name
decade_dataframe_60 <- select (decade_dataframe_60,-c(track.name,decades))

decade_dataframe_70 <- df_visualization[df_visualization$decades == "70s",]
rownames(decade_dataframe_70) <- decade_dataframe_70$track.name
decade_dataframe_70 <- select (decade_dataframe_70,-c(track.name,decades))

decade_dataframe_80 <- df_visualization[df_visualization$decades == "80s",]
rownames(decade_dataframe_80) <- decade_dataframe_80$track.name
decade_dataframe_80 <- select (decade_dataframe_80,-c(track.name,decades))

decade_dataframe_90 <- df_visualization[df_visualization$decades == "90s",]
rownames(decade_dataframe_90) <- decade_dataframe_90$track.name
decade_dataframe_90 <- select (decade_dataframe_90,-c(track.name,decades))

decade_dataframe_00 <- df_visualization[df_visualization$decades == "00s",]
rownames(decade_dataframe_00) <- decade_dataframe_00$track.name
decade_dataframe_00 <- select (decade_dataframe_00,-c(track.name,decades))

decade_dataframe_10 <- df_visualization[df_visualization$decades == "10s",]
rownames(decade_dataframe_10) <- decade_dataframe_10$track.name
decade_dataframe_10 <- select (decade_dataframe_10,-c(track.name,decades))



#################################################################
#####                                                       #####
#####              STEP 3: VISUALIZE THE DATA              #####
#####                                                       #####
#################################################################


## To use the fmsb package, we have to add 2 lines to the dataframe: the max and min of each variable to show on the plot
decade_dataframe_60 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_60)
decade_dataframe_70 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_70)
decade_dataframe_80 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_80)
decade_dataframe_90 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_90)
decade_dataframe_00 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_00)
decade_dataframe_10 <- rbind(rep(1,6) , rep(0,6) , decade_dataframe_10)


## We set colors individually for each song and the average decade values. In this case, we want to 
##    see all the songs with the same color, and the decade's average with a different one, so we 
##    can observe how different or similar are the top hits compared to each decade's songs


## We set a vector with all the colors we need (the last one is decade's average, with a different color)
colors_in=c( rgb(0.2,0.5,0.5,0.4) , rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.9) )

## Set our radar chart for the 60's decade
radarchart( decade_dataframe_60  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 60's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_60[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1)

## Set our radar chart for the 70's decade
radarchart( decade_dataframe_70  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 70's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_70[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1)

## Set our radar chart for the 80's decade
radarchart( decade_dataframe_80  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 80's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_80[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1)


## Set our radar chart for the 90's decade
radarchart( decade_dataframe_90  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 90's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_90[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1)


## Set our radar chart for the 00's decade
radarchart( decade_dataframe_00  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 2000's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_00[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=0.8, pt.cex=1)
            

## Our dataset is missing 2019 (Old Town Road), so we need 
##    to delete one parameter in order to assign the correct color to the decade's average
colors_in2010=c( rgb(0.2,0.5,0.5,0.4) , rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4),rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.9) )
            
            
## Set our radar chart for the 10's decade
radarchart( decade_dataframe_10  , axistype=1 , 
            # Set our polygon parameters
            pcol=colors_in2010, plwd=1 , plty=1,
            # Custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            # Custom the labels and add title
            vlcex=0.8, 
            title="Most popular summer songs 2010's" 
            ) 
            # Add a legend
            legend(x=1.2, y=1.5, legend = rownames(decade_dataframe_10[-c(1,2),]), bty = "n", pch=20 , col=colors_in2010 , text.col = "grey", cex=0.8, pt.cex=1)