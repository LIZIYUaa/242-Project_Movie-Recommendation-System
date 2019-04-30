library(jsonlite)
library(data.table)
library(dplyr)

setwd("/Users/apple/desktop/242/project")
key <- read.csv("keywords.csv")
movie <-read.csv("rating_features_small.csv")

#============================== remove duplicated rows with same id ==============================
key <- key[!duplicated(key$id), ]

# check our result
list_key <- table(key$id)
for(i in 1:length(list_key)){
  if (list_key[i] != 1)
    print("exist")
  
}
list_meta <- table(meta$id)
for(i in 1:length(list_meta)){
  if (list_meta[i] != 1)
    print("exist")
  
}

key_merge <- inner_join(key,movie, by = c("id" = "movieId"))
key_merge <- subset(key_merge,select = c(1,2))
key_merge <- key_merge[!duplicated(key_merge$id), ]
write.csv(key_merge, "key_merge.csv")
#============================== deal with format ==============================
# convert 'id' into "id" without changing's inside the list
key_merge$keywords <- gsub("'(?![a-z])|(?<=\\{|\\s)'", '"', key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('ladies"', "ladies", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('artists"', "artists", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('boys"', "boys", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('girls"', "girls", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('boys}', "boys\\\"}", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('girls}', "girls\\\"}", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('rock "n" roll', "rock n roll", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('workers"', "workers", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('years"', "years", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('students"', "students", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub('students}', "students\\\"}", key_merge$keywords, perl = TRUE)
key_merge$keywords <- gsub("\\\\xa0", "",key_merge$keywords)
# replace empty list with 000
key_merge$keywords <- gsub("\\[]", "[{\"id\": \"000\", \"name\":\"000\"}]",key_merge$keywords)
# check
for(i in 1:nrow(key_merge)){
  print(i)
  test <- f(key_merge$keywords[i],key_merge$id[i])
  print(i)
}
glimpse(key_merge)
write.csv(key_merge, "key_merge2.csv")
#========================= convert json column into dataframe ===========================
# 1) First, make a transformation function that works for a single entry
f <- function(json, id){
  tmp    <- jsonlite::fromJSON(json)   # transform json to list
  tmp    <- as.data.frame(tmp)        # transform list to data.frame
  tmp$movieid <- id                        # add id
  return(tmp)
}
# 2) apply it via mapply 
json <- mapply(f, key_merge$keywords, key_merge$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
clean_key <- data.table::rbindlist(json)
# check the number of unique keywords id: 4689
length(unique(clean_key$id))
length(unique(clean_key$movieid))
# rename movieid and id
colnames(clean_key)[colnames(clean_key)=="id"] <- "keywordsId"
colnames(clean_key)[colnames(clean_key)=="movieid"] <- "movieId"
# count the number of keywords_id > 40
n_keyid <- as.data.frame(table(clean_key$keywordsId))
greater40 <- n_keyid[which(n_keyid$Freq > 40),]
# find the maximum frequence keywordid
range(n_keyid$Freq)
n_keyid[which(n_keyid$Freq == 265),]
clean_key[which(clean_key$keywords_id == 179431),]

write.csv(clean_key, "clean-key.csv")


# dummy all features
library(caret)
str(clean_key)
clean_key <-read.csv("clean-key.csv")
key_name <- subset(clean_key,select = c(3,4))
key_name_switch <- key_name[,c("movieId","name")]
length(unique(key_name_switch$movieId))   #1321

key_name_switch$name <- as.factor(key_name_switch$name)
length(unique(key_name_switch$movieId))  #1321

dummy <- dummyVars(" ~ .", data = key_name_switch)
keywords_dummy <- data.frame(predict(dummy, newdata = key_name_switch))
length(unique(keywords_dummy$movieId))   #1321

# drop columns name.000 and name.
keywords_dummy <- keywords_dummy[ -c(2, 3) ]
# aggregate by movieid
library(plyr)
key_agg <- ddply(keywords_dummy,.(movieId),numcolwise(sum))
length(unique(key_agg$movieId))   #1321
key_agg <- key_agg[ -c(2,3) ]

write.csv(key_agg, "key_agg.csv")

# add ratings
key_agg <- read.csv("key_agg.csv")
movie <-read.csv("rating_features_small.csv")
movie_rating <- subset(movie,select = c(2,3,4))
key_rating <- inner_join(movie_rating, key_agg, by = "movieId")

write.csv(key_rating, "key_rating.csv")

# extract 100 keywords from 4689 unique keywords
key_rating <- read.csv('key_rating.csv')
keyword <- key_rating[,-c(1:4)]
str(keyword)
sum(keyword$name.16th.century)
sort(colSums(keyword,na.rm = TRUE),decreasing = TRUE)[1:100]
keyword = keyword[ ,colSums(keyword,na.rm = TRUE) > 989]
str(keyword)
movieid <- key_rating[,c(3)]
keyword$movieid <-key_rating[,c(3)]
str(keyword)
keyword <- keyword[!duplicated(keyword$movieid), ]
str(keyword)
write.csv(keyword,'keyword_movieid2.csv')





