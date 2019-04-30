#install.packages("lubridate"
#install.packages("jsonlite")
# Load packages
library(dplyr)
library(lubridate)
library(tm)
library(tm.plugin.webmining)
library(jsonlite)


# Load the data set
movies_metadata_origin <- read.csv("movies_metadata.csv",stringsAsFactors=FALSE)
movies_metadata <- subset(movies_metadata_origin, 
                         select = -c(belongs_to_collection,
                                     homepage,poster_path,
                                     original_title,
                                     spoken_languages) )


#movies_metadata$vote_count: delete movies which have zero vote count
table(movies_metadata_origin$vote_count != 0) #FALSE  TRUE : 2899 42561  
movies_metadata <- filter(movies_metadata, vote_count != 0)
table(movies_metadata$vote_count == 0) 


#movies_metadata$vote_average: it is ok to level rows with vote_average equals to 0
table(movies_metadata_origin$vote_average != 0) #FALSE  TRUE : 2998 42462 


#movies_metadata$video: delete video feature
table(movies_metadata_origin$video) #      False  True : 45367    93 
movies_metadata <- filter(movies_metadata, video == "False")
table(movies_metadata$video)
movies_metadata <- subset(movies_metadata, select = -c(video))


#movies_metadata$title: to be determined



#movies_metadata$tagline: to be determined



#movies_metadata$status: only deal with Released Movie, delete status feature
table(movies_metadata_origin$status) 
#                 Canceled   In Production         Planned Post Production        Released         Rumored 
# 87               2              20              15              98           45014             230 
movies_metadata <- filter(movies_metadata, status == "Released")
table(movies_metadata$status) 
movies_metadata <- subset(movies_metadata, select = -c(status))


#movies_metadata$runtime
# find the data structure of runtime
hist(movies_metadata_origin$runtime)
# there few movies whose runtime is bigger than 200, delete it
table(movies_metadata_origin$runtime>1000)
table(movies_metadata_origin$runtime>500)
table(movies_metadata_origin$runtime>200)
movies_metadata <- filter(movies_metadata, runtime <= 200)
hist(movies_metadata$runtime)
# delete movies whose runtime equals to 0
table(movies_metadata_origin$runtime==0)
movies_metadata <- filter(movies_metadata, runtime != 0)
hist(movies_metadata$runtime)
table(movies_metadata$runtime!=0)


#movies_metadata$revenue: need to add vacant value
table(movies_metadata_origin$revenue != 0) # FALSE  TRUE : 38052  7408 
table(movies_metadata$revenue != 0)


#movies_metadata$release_date: get year, month, day from date, and delete column release_date
release_date_year <- format(as.Date(movies_metadata$release_date),"%Y")
release_date_year <- as.numeric(release_date_year)
hist(release_date_year)
release_date_month <- format(as.Date(movies_metadata$release_date),"%m")
release_date_month <- as.numeric(release_date_month)
hist(release_date_month)
release_date_day <- format(as.Date(movies_metadata$release_date),"%d")
release_date_day <- as.numeric(release_date_day)
hist(release_date_day)

movies_metadata <- cbind(movies_metadata, release_date_year, release_date_month, release_date_day)
movies_metadata <- subset(movies_metadata, select = -c(release_date))


#movies_metadata$production_countries: get abbreviation of production country from JSON data cell
production_countries <- list()
movies_metadata$production_countries[4131]<-"[{'iso_3166_1': 'CI', 'name': \"Cote DIvoire\"}, {'iso_3166_1': 'BF', 'name': 'Burkina Faso'}, {'iso_3166_1': 'FR', 'name': 'France'}, {'iso_3166_1': 'IT', 'name': 'Italy'}, {'iso_3166_1': 'CH', 'name': 'Switzerland'}]"
movies_metadata$production_countries[6682]<-"[{'iso_3166_1': 'CI', 'name': \"Cote DIvoire\"}, {'iso_3166_1': 'FR', 'name': 'France'}, {'iso_3166_1': 'DE', 'name': 'Germany'}, {'iso_3166_1': 'CH', 'name': 'Switzerland'}]"
movies_metadata$production_countries[21881] <- "[{'iso_3166_1': 'AU', 'name': 'Australia'}, {'iso_3166_1': 'LA', 'name': \"Lao Peoples Democratic Republic\"}, {'iso_3166_1': 'TH', 'name': 'Thailand'}]"
movies_metadata$production_countries[28127] <- "[{'iso_3166_1': 'LA', 'name': \"Lao Peoples Democratic Republic\"}]"
movies_metadata$production_countries[35784] <- "[{'iso_3166_1': 'CA', 'name': 'Canada'}, {'iso_3166_1': 'LA', 'name': \"Lao Peoples Democratic Republic\"}]"
for(i in 1:40468) {
  a <- movies_metadata$production_countries[i]
  a <- gsub("\'", '\"' , a)
  print(i)
  a =  fromJSON(a)
  print(i)
  if (length(a) == 0) {
    production_countries[i] <- NA
  }else{
    production_countries[i] <- a[1,1]
  }
}
production_countries<-data.frame(matrix(unlist(production_countries), nrow=length(production_countries)))
colnames(production_countries)[1] <- "production_countries_ab"
table(production_countries$production_countries_ab)
movies_metadata <- cbind(movies_metadata, production_countries)
movies_metadata <- subset(movies_metadata, select = -c(production_countries))


#movies_metadata$production_companies: delete production_companies
movies_metadata <- subset(movies_metadata, select = -c(production_companies))
# table(movies_metadata$production_companies == "[]")
# movies_metadata$production_companies[29]<- "[{'name': 'Procirep', 'id': 311}]"
# movies_metadata$production_companies[129]<-"[{'name': 'Cigua Films', 'id': 93488}]"
# movies_metadata$production_companies[163]<-"[{'name': 'The Booking Office', 'id': 12909}]"
# production_companies <- list()
# for(i in 1:40468) {
#   a <- movies_metadata$production_companies[i]
#   a <- gsub("\'", '\"' , a)
#   print(i)
#   a <-  fromJSON(a)
#   print(i)
#   if (length(a) == 0) {
#     production_companies[i] <-"NA"
#   }else{
#     production_companies[i] <- a[1,1]
#   }
# }


#movies_metadata$popularity: delete popularity is bigger than 15
movies_metadata_origin$popularity = as.numeric(movies_metadata_origin$popularity)
hist(movies_metadata_origin$popularity)
table(movies_metadata_origin$popularity>15)
movies_metadata$popularity = as.numeric(movies_metadata$popularity)
movies_metadata <- filter(movies_metadata, popularity <= 15)
hist(movies_metadata$popularity)


#movies_metadata$overview: to be determined


#movies_metadata$original_language: 84 languages to 28 languages
tobeDelete <- list()
j <- 0
for (i in 1:length(table(movies_metadata$original_language))){
  if (table(movies_metadata$original_language)[i] >= 50){
    tobeDelete[j] <- i
    j<-j+1
  }
}
length(tobeDelete)

left <- c("cn","cs","da","de","el",
         "en","es","fa","fi","fr","he","hi","hu","it",
         "ja","ko",
         "nl","no","pl","pt","ru","sr",
         "sv","ta","th","tr","zh")
for (i in 1:39765){
  flag <- 0
  for (j in left){
    if (movies_metadata$original_language[i] == j){
      flag <- 1
      break
    }
  }
  if(flag == 0){
      movies_metadata$original_language[i]<-"other"
  }
}
table(movies_metadata$original_language)
length(table(movies_metadata$original_language))


## subset
movies_metadata<- movies_metadata[!duplicated(movies_metadata$id), ]
movies_metadata <- subset(movies_metadata, select = -c(adult,budget,genres))
write.csv(movies_metadata,'movies_metadata2.csv')


# merge dataset
movies_metadata_1 <- read.csv("movies_metadata1.csv",stringsAsFactors=FALSE)
movies_metadata_1<- movies_metadata_1[!duplicated(movies_metadata_1$id), ]
movies_metadata_2 <- read.csv("movies_metadata2.csv",stringsAsFactors=FALSE)
movies_metadata_2<- movies_metadata_2[!duplicated(movies_metadata_2$id), ]
movies_metadata_final <- inner_join(movies_metadata_1, movies_metadata_2, by = c("id"="id"))
movies_metadata_final <- subset(movies_metadata_final, select = -c(X.x,X.y))

movies_metadata_test <- filter(movies_metadata_final, budget !=0)
movies_metadata_test <- filter(movies_metadata_test, revenue !=0)
write.csv(movies_metadata_test,'movies_metadata_final.csv')

