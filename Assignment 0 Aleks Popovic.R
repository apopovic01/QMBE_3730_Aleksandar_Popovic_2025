# First step we did is import our data and here we will load packages for use
library(tidyverse)
library(dplyr)

#Our second step will be to be to get rid of NAs in our dataset
netflix<-drop_na(netflix)
#Third step to have further anaylis we will filter the data to seperate TV shows and movies
netflix_shows<-netflix%>%filter(type=="TV Show")
netflix_movies<-netflix%>%filter(type=="Movie")
#One interesting thing to note is that there are only 147 observations for TV shows without NAs meaning thousands of TV shows have in complete data in our dataset
#Our next step will be convert the duration category into numbers so we can analyze those
numeric_duration <- as.numeric(gsub("[^0-9]", "", netflix_shows$duration))
mean(numeric_duration)
#1.9
median(numeric_duration)
#1
#This is a right skewed distribution as mean is greater then median
#Now we will do the same thing for movies
numeric_duration1 <- as.numeric(gsub("[^0-9]", "", netflix_movies$duration))
mean(numeric_duration1)
#102.7
median(numeric_duration1)
#101
#This is also a slightly right skewed distribution as mean is greater then median
#Now we will create some simple graphs to show the above results
hist(x=numeric_duration,main = "Duration of TV shows",ylab="TV Shows", xlab="Seasons",col = "red")
hist(x=numeric_duration1,main = "Duration of Movies",ylab="Movies", xlab="Minutes",col = "blue")
#both these graphs reenforce our previous findings tha the data is right skewed
#Now we are going to filter by country and only look at media created in the US for the same topics as above
netflix1<-netflix%>%filter(country=="United States")
netflix_shows1<-netflix1%>%filter(type=="TV Show")
netflix_movies1<-netflix1%>%filter(type=="Movie")
#Now we will find the mean and median durations for US made netflix titles
numeric_duration2 <- as.numeric(gsub("[^0-9]", "", netflix_shows1$duration))
mean(numeric_duration2)
#2.62
median(numeric_duration2)
#1
#This has the same median as before although the mean is significantly higher at 2.6 seasons instead of 1.9
#Now we will do the same thing for movies
numeric_duration3 <- as.numeric(gsub("[^0-9]", "", netflix_movies1$duration))
mean(numeric_duration3)
#92.72
median(numeric_duration3)
#93
#This data is ever so slightly left skewed as median is higher then the mean but it is so insignificant since it is by 0.28
#Now we will make charts of this to finalize it
hist(x=numeric_duration2,main = "Duration of TV shows in the US",ylab="TV Shows", xlab="Seasons",col = "green")
hist(x=numeric_duration3,main = "Duration of Movies in the US",ylab="Movies", xlab="Minutes",col = "gold")
#We see in these graphs the results we previously saw in our prior analysis