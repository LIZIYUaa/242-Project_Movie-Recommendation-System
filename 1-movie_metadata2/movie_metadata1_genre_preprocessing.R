install.packages("rjson")
install.packages("RJSONIO")
install.packages("jsonlite")
library(softImpute)
library(randomForest)
library(ranger)
library(dplyr)
library(tidyverse)
library(reshape2)
library(rjson)
library(RJSONIO)
library(jsonlite)

movies_metadata <- read.csv("movies_metadata.csv")

str(movies_metadata)

# drop columns like belongs_to_collection, homepage, poster_path, original title, spoken language
movies_metadata <- subset(movies_metadata,select=-c(belongs_to_collection,homepage,
                                                    poster_path,original_title,
                                                    spoken_languages))
str(movies_metadata)

 
# ========1.dealing with adult, only keep whose value is "False", then drop this column======================
table(movies_metadata$adult)
movies_metadata <- filter(movies_metadata,adult == 'False')
movies_metadata <- subset(movies_metadata,select=-adult)

# ======================2.Budget==========================
table(movies_metadata$budget == 0) 
# 36565 missing raws in 45454 rows, maybe need to fill it by checking other sources

# ======================3.genres===========================
genres <- data.frame(matrix(0,nrow(movies_metadata),22))
names(genres) <- c("id","Action","Adventure","Animation","Family","Comedy","Crime","Documentary",
                 "Drama","Fantasy","History","Horror","Music","Mystery","Romance","Science Fiction",
                 "Thriller","War","Western","Foreign","TV Movie","Other")

vector <- c("Action","Adventure","Animation","Family","Comedy","Crime","Documentary",
             "Drama","Fantasy","History","Horror","Music","Mystery","Romance","Science Fiction",
             "Thriller","War","Western","Foreign","TV Movie","Other")

# History  ScienceFiction
ToBeDropped <- list()
#OthersName <- list()
k = 1 
#p = 1

for (i in 1:nrow(movies_metadata)){
  genres[i,"id"] <- as.integer(as.character(movies_metadata[i,"id"]))
  matrix <- gsub("\'", '\"' , movies_metadata[i,"genres"]) %>% fromJSON()
  if (length(matrix) == 0) {
    ToBeDropped[k] <- as.integer(as.character(movies_metadata[i,"id"]))
    k = k+1
    next
  }
  

  
  for (j in 1:nrow(matrix)){
    flag <- 0
    for(genre in vector){
      if(matrix[j,2] == genre){
        genres[i,genre] <- genres[i,genre] + 1
        flag <- 1
        break
      }
    }
  if(flag == 0){
    genres[i,"Other"] <- genres[i,"Other"] + 1
    #OthersName[p] = matrix[j,2]
    #p = p+1
  }
  }
}

length(ToBeDropped) #2442
OthersName

for(n in 2:22){
  print(sum(genres[,n]))
}
#Others is 0

#drop Other from genres
genres <- subset(genres,select=-Other)

#drop raws that has no genres
for(n in 1:length(ToBeDropped)){
  movies_metadata <- filter(movies_metadata,id != ToBeDropped[n])
  genres <- filter(genres,id != ToBeDropped[n])
}


movies_metadata$id <- as.integer(as.character(movies_metadata$id))
genres <- subset(genres,select=-id)

metadata_new <- cbind(movies_metadata,genres)
metadata_new <- subset(metadata_new,select=-genres)


#===============the Final dataframe to combine yingxin！=====================
metadata_new1 <- subset(metadata_new,select=-(3:17))

# drop repeated
metadata_new2<- metadata_new1[!duplicated(metadata_new1$id), ]

movie_genres <- subset(metadata_new2,select=-1)
write.csv(movie_genres,'movie_genres.csv') 


#=================Combine====================================================
movies_metadata2 <- read.csv("movies_metadata2.csv")
# drop repeated
movies_metadata2<- movies_metadata2[!duplicated(movies_metadata2$id), ]

metadata_combine <- inner_join(metadata_new2,movies_metadata2,by=c("id"="id"))
metadata_combine <- subset( metadata_combine,select=-X)

write.csv(metadata_combine,'movie_features.csv') 


table(metadata_combine$budget == 0 | metadata_combine$revenue == 0) 
# FALSE  TRUE 
# 4756   33720 

#============== Only leave movies that have both budget and revenue data ==============
metadata_combine <- filter(metadata_combine, budget != 0)
metadata_combine <- filter(metadata_combine, revenue != 0)





