library(dplyr)
library(ggplot2)
library(caTools)
library(rpart) 
library(rpart.plot) 
library(MASS)
library(caret)
library(randomForest)
library(softImpute)
library(randomForest)
library(ranger)
library(dplyr)
library(tidyverse)
library(reshape2)

cast_final <- read.csv('clean_cast.csv')
crew_final <- read.csv('crew_final.csv')
Rate <- read.csv('rating_features_small.csv')

# check if the number of movieid in these dataset are the same
num_id_cast <- length(unique(cast_final$movieid))
num_id_crew <- length(unique(crew_final$movieId)) 
num_id_rate <- length(unique(Rate$movieId)) # contain the same number of movies 

# create a new dataframe and then paste these information into these new dataframe
# first, create a new dataframe with only rate, userid and movieid columns
Rating <- data.frame(Rate$rating, Rate$movieId, Rate$userId)

# drop unuseful column 'Id' in cast_final and crew_final dataframe
cast_final$credit_id <- NULL
cast_final$character <- NULL
cast_final$gender <- NULL
cast_final$X <- NULL
cast_final$id <- NULL
cast_final$order <- NULL
cast_final$profile_path <- NULL #check there are 1321 movies


######################### characters are not used, this part can be ignored#################################################
# find that there are missing values in the column 'character'
# find which rows do not have character info
char_na <- which(cast_final$character == '')
char_na  #there are 346 NA values

# remove rows with NA
cast_NONA <- read.csv("cast_final.csv")
num_id_cast_NoNa <- length(unique(cast_NONA$movieid)) # but now there are only 1312 unique movies

# check which movie misses
setA <- c(unique(cast_final$movieid)) # 1321 movies
setB <- c(unique(cast_NONA$movieid)) # 1312 movies
omit_movieid <- setdiff(setA,setB)  #get the 9 missing movies

# find the name of the movie based on movieid
meta <-read.csv("movies_metadata.csv")
name1 <- which(meta$id == omit_movieid[1])
name1  #index 5694: Soul Assassin
name2 <- which(meta$id == omit_movieid[2])
name2  #index 7508: Berlin: Die Sinfonie der Grosstadt
name3 <- which(meta$id == omit_movieid[3])
name3  #index 11586: Shiza
name4 <- which(meta$id == omit_movieid[4])
name4  #index 11804: ‡πÅ‡∏™‡∏á‡∏®‡∏ï‡∏ß‡∏£‡∏£‡∏©
name5 <- which(meta$id == omit_movieid[5])
name5  #index 14523: Flaming Creatures
name6 <- which(meta$id == omit_movieid[6])
name6  #index 27793: Megacities
name7 <- which(meta$id == omit_movieid[7])
name7  #index 30392: Big Time
name8 <- which(meta$id == omit_movieid[8])
name8  #index 43369: Boat
name9 <- which(meta$id == omit_movieid[9])
name9  #index 44718: Illusions funambulesques

# filling in the missing character values in the cast_final csv
# check the index of the missing values of the first movie
which(cast_final$movieid == omit_movieid[1])
# [1] 11637 11638 11639 11640 11641 11642 11643
cast_final$character <- as.character(cast_final$character)
cast_final$character[11637] <- 'Kevin Burke'
cast_final$character[11638] <- 'Tessa Jansen'
cast_final$character[11639] <- 'Karl Jorgensen'
cast_final$character[11640] <- 'Karina'
cast_final$character[11641] <- 'Karl Jorgensen Jr.' 

# 2
which(cast_final$movieid == omit_movieid[2])
cast_final$character[14210] <- 'Himself'

# 3
which(cast_final$movieid == omit_movieid[3])
cast_final$character[18844] <- 'Mustafa (Shiza)'
cast_final$character[18845] <- 'Zinka'
cast_final$character[18846] <- 'Sakura'
cast_final$character[18847] <- 'Doctor'
cast_final$character[18848] <- 'Kulyash'
cast_final$character[18849] <- 'Sandzhik'
cast_final$character[18850] <- 'Almaz'

# forth movie
which(cast_final$movieid == omit_movieid[4])
#cast_final$character[19191] <- 
#cast_final$character[19192] <- 
#cast_final$character[19193] <- 
#cast_final$character[19194] <- 
#cast_final$character[19195] <- 
#cast_final$character[19196] <- 

# fifth movie
which(cast_final$movieid == omit_movieid[5]) 
cast_final$character[21577] <- 'The Spanish Girl'
cast_final$character[21578] <- 'Himself'
cast_final$character[21579] <- 'Almaz'

# sixth
which(cast_final$movieid == omit_movieid[6]) 
# cast_final$character[23529] <- 

# 7
which(cast_final$movieid == omit_movieid[7]) 
# cast_final$character[23912] <- 

# 8
which(cast_final$movieid == omit_movieid[8]) 
cast_final$character[25012] <- 'Himself'
cast_final$character[25013] <- 'Voice'

# 9
which(cast_final$movieid == omit_movieid[9]) 
cast_final$character[25122] <- 'Magician'

# save into a new csv
write.csv(cast_final, "cast_final_character.csv")
################################################################################################################


# Goal: create a new dataframe with only 1321 rows (there are 1321 unique movies)
crew_final$X <- NULL

# replace the name of Movieid
colnames(cast_final)[3] <- "MovieId"
colnames(cast_final)[2] <- "ActorName"
colnames(crew_final)[1] <- 'DirectorName'
colnames(crew_final)[2] <- "MovieId"

# merge crew_final and cast_final
cast_crew <- merge(cast_final, crew_final, by = 'MovieId') # check: there are 1321 unique MovieId
# we will not use the castid, remove the entire column
cast_crew$cast_id <- NULL

# count the frequency of each actor
frequent_actor <- as.data.frame(table(cast_crew$ActorName)) # there are 18900 actors in total
frequent_actor <- frequent_actor[order(-frequent_actor$Freq),] #sort dataframe according to actor frequency
# replace the column names in frequent_actor to make merge easy 
colnames(frequent_actor)[1] <- "ActorName"
colnames(frequent_actor)[2] <- "ActorFrequent"

# the type of 'actor' is factor, convert factor to character
frequent_actor$ActorName <- as.character(frequent_actor$ActorName)
cast_crew$ActorName <- as.character(cast_crew$ActorName)

# extract the top 150 actors according to the frequency, and change the other actors' names to be others

for (i in 1:25162)
{
  if (cast_crew$ActorName[i] %in% frequent_150)
  {
    cast_crew$ActorName[i] <- cast_crew$ActorName[i]
  }else{
    cast_crew$ActorName[i] <- 'Other'
  }
}   #replace successfully

# deal with the directors' information
cast_crew$DirectorName <- as.character(cast_crew$DirectorName) # convert directornames to string
frequent_director$DirectorName <- as.character(frequent_director$DirectorName)

# count the frequency of each director
frequent_director <- as.data.frame(table(cast_crew$DirectorName)) # there are 914 directors in total
frequent_director <- frequent_director[order(-frequent_director$Freq),] #sort dataframe according to the directors' frequency
# replace the column names in frequent_director to make merge easy
colnames(frequent_director)[1] <- "DirectorName"
colnames(frequent_director)[2] <- "DirectorFrequent"

# extract top 50 directors and replace other directornames into others
frequent_50 = frequent_director$DirectorName[1:50]

for (i in 1:25162)
{
  if (cast_crew$DirectorName[i] %in% frequent_50)
  {
    cast_crew$DirectorName[i] <- cast_crew$DirectorName[i]
  }else{
    cast_crew$DirectorName[i] <- 'OtherDirector'
  }
}   # replace successfully


library(caret)
str(cast_crew) #character
# convert into factor
cast_crew$ActorName <- factor(cast_crew$ActorName)
cast_crew$DirectorName <- factor(cast_crew$DirectorName)
# check the number of actors and directors
length(unique(cast_crew$ActorName)) # 151
length(unique(cast_crew$DirectorName)) # 51

# dummy features: ActorName and DirectorName 
dummy <- dummyVars(" ~ .", data = cast_crew)
cast_crew_dummy <- data.frame(predict(dummy, newdata = cast_crew)) # 203 columns(including movieid) and there are 1321 unique movies

# aggregate by movieid
library(plyr)
cast_crew_agg <- ddply(cast_crew_dummy,.(MovieId),numcolwise(sum))  #1321 unique movies

# remove two unuseful columns: otherdirector and other
cast_crew_agg$ActorName.Other <- NULL
cast_crew_agg$DirectorName.OtherDirector <- NULL #201 columns in this dataframe, 1321 movies

write.csv(cast_crew_agg, "cast_crew_agg.csv")