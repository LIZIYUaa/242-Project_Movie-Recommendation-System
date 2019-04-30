setwd("/Users/apple/desktop/242/project")
library(jsonlite)
library(data.table)
library(dplyr)

credits <- read.csv("credits.csv")
length(unique(credits$id))   #45432
movie <-read.csv("rating_features_small.csv")
movie <- movie[ -c(1) ]

cast <- subset(credits,select = c(1,3))
cast$cast[1]

cast_merge <- inner_join(cast,movie, by = c("id" = "movieId"))

cast_merge <- subset(cast_merge,select = c(1,2))
colnames(cast_merge)[colnames(cast_merge)=="id"] <- "movieId"
cast_merge <- cast_merge[!duplicated(cast_merge$movieId), ]
length(unique(cast_merge$movieId))   #1321
glimpse(cast_merge)

# replace empty list with 000
cast_merge$cast <- gsub("\\[]", "[{\'cast_id\': \'000\', \'character\':\'000\', \'credit_id\':\'000\', \'gender\':\'000\', \'id\':\'000\', \'name\':\'000\', \'order\':\'000\', \'profile_path\':\'000\'}]",cast_merge$cast)

write.csv(cast_merge, "cast_merge.csv")

#========================= double quotes transformation was done in python ===================================

# read crew with double quotes dataframe transformed in python
cast_double <- read.csv("cast_double_quote.csv")
cast_double <- subset(cast_double,select = c(3,5))
glimpse(cast_double)
cast_double$cast_dict[1]

# remove all line breaks "\n" from the string
cast_double$cast_dict <- gsub("[\n]", "", cast_double$cast_dict)
# remove :null
cast_double$cast_dict <- gsub("null", '\"000\"', cast_double$cast_dict)
cast_double$cast_dict <- as.character(cast_double$cast_dict)
cast_double$cast_dict[1]
write.csv(cast_double, "cast_double2.csv")

colnames(cast_double)[colnames(cast_double)=="movieId"] <- "id"
# check
for(i in 1:nrow(cast_double)){
  print(i)
  test <- f(cast_double$cast_dict[i],cast_double$id[i])
  print(i)
}

#========================= convert json column into dataframe ===========================
# 1) First, make a transformation function that works for a single entry
f <- function(json, id){
  tmp    <- jsonlite::fromJSON(json)   # transform json to list
  tmp    <- as.data.frame(tmp)        # transform list to data.frame
  tmp$movieid <- id                        # add id
  return(tmp)
}
# 2) apply it via mapply 
json <- mapply(f, cast_double$cast_dict, cast_double$id, SIMPLIFY = FALSE)
# 3) combine the fragments via rbindlist
clean_cast <- data.table::rbindlist(json)
# check the number of unique movieid: 
length(unique(clean_cast$movieid))  #1321
write.csv(clean_cast, "clean_cast.csv")

































# substruct columns that we want
clean_cast <- subset(clean_cast,select = c(1,2,5,6,7,9))

# find which rows do not have character info
char_na <- which(clean_cast$character == "")
length(char_na)      # 346

# find which rows do not have cast_id info
castid_na <- which(clean_cast$cast_id == "")
length(castid_na)   # 0

# find which rows do not have id info
id_na <- which(clean_cast$id == "")
length(id_na)       # 0

# find which rows do not have name info
name_na <- which(clean_cast$name == "")
length(name_na)      # 0

# find which rows do not have order info
order_na <- which(clean_cast$order == "")
length(order_na)    # 0

# remove rows where character == ""
df<- clean_cast[-which(clean_cast$character == ""), ]

# double check
char_na1 <- which(df$character == "")
length(char_na1)      # 0

# check the number of unique movieid
length(unique(df$movieid))   # 1312

write.csv(df, "cast_final.csv")
str(df)


