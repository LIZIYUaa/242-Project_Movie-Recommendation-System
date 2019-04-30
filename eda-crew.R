setwd("/Users/apple/desktop/242/project")
library(jsonlite)
library(data.table)
library(dplyr)

credits <- read.csv("credits.csv")
length(unique(credits$id))   #45432
movie <-read.csv("rating_features_small.csv")
movie <- movie[ -c(1) ]

crew <- subset(credits,select = c(2,3))
crew$crew[1]

# basic cleaning
crew_merge <- inner_join(crew,movie, by = c("id" = "movieId"))
crew_merge <- subset(crew_merge,select = c(1,2))
colnames(crew_merge)[colnames(crew_merge)=="id"] <- "movieId"
crew_merge <- crew_merge[!duplicated(crew_merge$movieId), ]
length(unique(crew_merge$movieId))  #1321
glimpse(crew_merge)

# replace empty list with 000
crew_merge$crew <- gsub("\\[]", "[{\'credit_id\': \'000\', \'department\':\'000\', \'gender\':\'000\', \'id\':\'000\', \'job\':\'000\', \'name\':\'000\', \'profile_path\':\'000\'}]",crew_merge$crew)

write.csv(crew_merge, "crew_merge.csv")

#========================= double quotes transformation was done in python ===================================

# read crew with double quotes dataframe transformed in python
crew_double <- read.csv("crew_double_quote.csv")
crew_double <- subset(crew_double,select = c(3,5))
glimpse(crew_double)
crew_double$crew_dict[1]

# remove all line breaks "\n" from the string
crew_double$crew_dict <- gsub("[\n]", "", crew_double$crew_dict)
# remove :null
crew_double$crew_dict <- gsub("null", '\"000\"', crew_double$crew_dict)
crew_double$crew_dict <- as.character(crew_double$crew_dict)
crew_double$crew_dict[1]
write.csv(crew_double, "crew_double2.csv")

colnames(crew_double)[colnames(crew_double)=="movieId"] <- "id"
# check
for(i in 1:nrow(crew_double)){
  print(i)
  test <- f(crew_double$crew_dict[i],crew_double$id[i])
  print(i)
}
crew_double$crew_dict[498]
#========================= convert json column into dataframe ===========================
# 1) First, make a transformation function that works for a single entry
f <- function(json, id){
  tmp    <- jsonlite::fromJSON(json)   # transform json to list
  tmp    <- as.data.frame(tmp)        # transform list to data.frame
  tmp$movieid <- id                        # add id
  return(tmp)
}
# 2) apply it via mapply 
json <- mapply(f, crew_double$crew_dict, crew_double$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
clean_crew <- data.table::rbindlist(json)
# check the number of unique movieid: 
length(unique(clean_crew$movieid))  #1321

write.csv(clean_crew, "clean_crew.csv")

# only want info about director
clean_crew <- subset(clean_crew,select = c(5,6,8))
df = subset(clean_crew, job == 'Director')

# for those movies that have multiple directors, keep the first appeared director 
df1 <- df[!duplicated(df$movieid), ]
length(unique(df1$movieid))  #1319

# find which movie that we do not have director info
df2 <- right_join(df1,clean_crew, by = c("movieid" = "movieid"))
length(unique(df2$movieid))   #1321

df2 <- subset(df2,select = c(2,3))
df3 <- df2[!duplicated(df2$movieid), ]  
length(unique(df3$movieid))  #1321
colnames(df3)[colnames(df3)=="name.x"] <- "name"
colnames(df3)[colnames(df3)=="movieid"] <- "movieId"

# find which 2 rows do not have director info
whichna <- which(is.na(df3$name))
whichna # 1124  movieid: 26131
        # 1176  movieid: 1260
# find the name of the movie based on movieid
meta <-read.csv("movies_metadata.csv")
name1 <- which(meta$id == 26131)
name1  #index 22199: Straight From the Heart
name2 <- which(meta$id == 1260)
name2  #index 26721: Leiutajateküla Lotte

glimpse(df3)
df3$name <- as.character(df3$name)
df3$name[1124] <- "David S. Cass Sr."
df3$name[1176] <- "Janno Põldma"
length(unique(df3$name))  #914

write.csv(df3, "crew_final.csv")


