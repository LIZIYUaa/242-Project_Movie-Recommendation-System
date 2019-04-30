#install.packages("dummies")
#install.packages("fastDummies")
#install.packages("xgboost")
#install.packages("onehot")
#install.packages("neuralnet")
library(softImpute) #CF model
library(randomForest) # Random Forest
library(xgboost) #XGboost
library(ranger) # Random Forest
library(neuralnet) # Neural Network
library(fastDummies) # Dummy
library(onehot) # Dummy
library(caret)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape2)

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}



rating <- read.csv("6040Rating.csv")
rating <- subset(rating,select = -1)
#users <- read.csv('User.csv')
users <- read.csv('user_withlecture.csv')
users <- subset(users,select = -1)
movie_feature <- read.csv('movie_features.csv')
movie_feature <- subset(movie_feature,select = -1)
#lecture <- read.csv('MovieLensFeatures.csv')
#lecture_user <- subset(lecture,select = c(22:28,userID))

length(unique(rating$movieId))

#===============dealing with movie features===========
movie_feature <- subset(movie_feature,select = -c(imdb_id,overview,tagline,title,production_countries_ab))

# filling missing value in budget & revenue with mean
movie_feature$budget[movie_feature$budget == 0] = mean(movie_feature$budget, na.rm = T)
movie_feature$revenue[movie_feature$revenue == 0] = mean(movie_feature$revenue, na.rm = T)


#===============dealing with users data===========
str(users)
table(users$age)
#    1   18   25   35   45   50   56 
#   222 1103 2096 1193  550  496  380
table(users$occupation)
#  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
#711 528 267 173 759 112 236 679  17  92 195 129 388 142 302 144 241 502  70  72 281 

#****in this step, drop zipcode firstly
#users <- subset(users,select = -5)
#users <- left_join(users,lecture_user, by = c("userID" = "userID"))
#users <- subset(users,select = -2)
# drop repeated
#users<- users[!duplicated(users$userID), ]
#write.csv(users,'user_withlecture.csv') 

#===============combine several dataframe==============
rating_features <- inner_join(rating,movie_feature, by = c("movieId"= "id"))
rating_features <- inner_join(rating_features,users, by = c("userId"= "userID"))



#===================exploration========================
length(unique(rating_features$userId)) #5919
length(unique(rating_features$movieId)) #3875

unique(rating_features$rating) # what are unique values of ratings
table(rating_features$rating)
# 0.5     1   1.5     2   2.5     3   3.5     4   4.5     5 
# 2636  7469  2845 16271 10388 52094 25130 62725 17340 35302 

vector_ratings <- factor(rating_features$rating)
qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")


# each user has rated how many movies
User_summary <- rating_features %>% group_by(userId) %>% summarise(User_viewN = n())
User_summary_sub <- filter(User_summary, User_viewN <= 500)
ggplot(User_summary, aes(x = User_viewN)) + geom_histogram() + ggtitle("User Summary")
ggplot(User_summary_sub, aes(x = User_viewN)) + geom_histogram() + ggtitle("User Summary Sub")
table(User_summary$User_viewN <= 30)
 
# each movie has been rated for how many times
Movie_summary <- rating_features %>% group_by(movieId) %>% summarise(Movie_viewedN = n())
ggplot(Movie_summary, aes(x = Movie_viewedN)) + geom_histogram() + ggtitle("Movie Summary")
ggplot(filter(Movie_summary, Movie_viewedN <= 50), aes(x = Movie_viewedN)) + geom_histogram() + ggtitle("Movie Summary")
table(Movie_summary$Movie_viewedN <= 20)



rating_features_count <- inner_join(rating_features,User_summary, by = c("userId"="userId"))
rating_features_count <- inner_join(rating_features_count,Movie_summary, by = c("movieId"="movieId"))
# drop a movie if it is rated less than 20 and drop a user if he/she rates less than 30 times
rating_features_small <- filter(rating_features_count, User_viewN > 30 & Movie_viewedN >20)
length(unique(rating_features_small$userId)) #1944
length(unique(rating_features_small$movieId)) #1321
#rating_features_small <- subset(rating_features_small,select = -c(27,28))

write.csv(rating_features_small,'rating_features_small_5.0.csv') 


1000209/(6040*3900) #****lecture observation ratio = 0.0424609


# ～～～～～～～～～～～Movie_User_Keyword run from here～～～～～～～～～～～～
library(softImpute) #CF model
library(randomForest) # Random Forest
library(xgboost) #XGboost
library(ranger) # Random Forest
library(neuralnet) # Neural Network
library(fastDummies) # Dummy
library(onehot) # Dummy
library(caret)
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape2)

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

rating_features_small = read.csv("rating_features_small_5.0.csv")  
rating_features_small = subset(rating_features_small, select = -c(1))

# keyword= read.csv("keyword_movieid2.csv")  
# keyword = subset(keyword, select = -c(1))
# summary(keyword)
# 
# rating_features_small <- left_join(rating_features_small,keyword, by = c("movieId" = "movieid"))
# names(rating_features_small)

#===================as factor==================
str(rating_features_small)
## as factor
#rating_features_small$userId <- as.factor(rating_features_small$userId)
#rating_features_small$movieId <- as.factor(rating_features_small$movieId)
# rating_features_small$release_date_year <- as.factor(rating_features_small$release_date_year)
# rating_features_small$release_date_month <- as.factor(rating_features_small$release_date_month)
# rating_features_small$release_date_day <- as.factor(rating_features_small$release_date_day)
# rating_features_small$age <- as.factor(rating_features_small$age)
# rating_features_small$occupation <- as.factor(rating_features_small$occupation)
# rating_features_small$Male <- as.factor(rating_features_small$Male)
# rating_features_small$Urban <- as.factor(rating_features_small$Urban)
# rating_features_small$RegionMidwest <- as.factor(rating_features_small$RegionMidwest)
# rating_features_small$RegionNortheast <- as.factor(rating_features_small$RegionNortheast)
# rating_features_small$RegionSouth <- as.factor(rating_features_small$RegionSouth)
# rating_features_small$RegionWest <- as.factor(rating_features_small$RegionWest)
# 
# rating_features_small = subset(rating_features_small, select = -c(release_date_day))

## dummy original language
#language <- fastDummies::dummy_cols(rating_features_small$original_language)
#language <- subset(language, select = -.data)
#rating_features_small_test <- cbind(rating_features_small, language)
#rating_features_small_test <- subset(rating_features_small_test, select = -original_language)
#rating_features_small<-rating_features_small_test

## dummy all features
# rating_features_small_dummy <- dummyVars(" ~ .", data = rating_features_small)
# rating_features_small <- data.frame(predict(rating_features_small_dummy, newdata = rating_features_small))


# write.csv(rating_features_small,'rating_features_small_3.0.csv') 

#===================split data==================
set.seed(500)
train.ids <- sample(nrow(rating_features_small), 0.92*nrow(rating_features_small))
train <- rating_features_small[train.ids,]
test <- rating_features_small[-train.ids,]

# split training into real training and validation set
set.seed(500)
val1.ids <- sample(nrow(train), (4/92)*nrow(train))
val1 <- train[val1.ids,]
train <- train[-val1.ids,]

set.seed(500)
val2.ids <- sample(nrow(train), (4/88)*nrow(train))
val2 <- train[val2.ids,]
train <- train[-val2.ids,]


# ～～～～～～～～～～～～～～～～Model～～～～～～～～～～～～～～～～～

#===================basic cf model=====================
mat.train <- Incomplete(train$userId, train$movieId, train$rating)

# compute validation set MAE for rank = 1,2,...,20
mae.vals = rep(NA, 10)
#set.seed(100)
for (rnk in seq_len(10)) {
  set.seed(100)
  print(str_c("Trying rank.max = ", rnk))
  mod <- softImpute(mat.train, rank.max = rnk, lambda = 0, maxit = 1000)
  preds <- impute(mod, val1$userId, val1$movieId)  %>% pmin(5) %>% pmax(0.5)
  mae.vals[rnk] <- mean(abs(preds - val1$rating))
}

mae.val.df <- data.frame(rnk = seq_len(10), mae = mae.vals)
ggplot(mae.val.df, aes(x = rnk, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of Archetypal Users") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


# choose k = 4
set.seed(100)
mod.final <- softImpute(mat.train, rank.max = 4, lambda = 0, maxit = 1000)
preds <- impute(mod.final, test$userId, test$movieId) %>% pmin(5) %>% pmax(0.5)
mean(abs(preds - test$rating))/4.5 # 0.1473483(k=4)  
sqrt(mean((preds - test$rating)^2))/4.5 # 0.1938611(k=4)  
OSR2(preds, train$rating, test$rating) # 0.264254(k=4)  



# ==================Now try a linear regression without CF as a varible========
lin.mod <- lm(rating ~ . -movieId -userId, data = train)
summary(lin.mod)
preds.lm <- predict(lin.mod, newdata = test) %>% pmin(5) %>% pmax(0.5)
mean(abs(preds.lm - test$rating))/4.5 # 0.1716093 #with keyword0.1709485
sqrt(mean((preds.lm - test$rating)^2))/4.5 # 0.2173623         0.2168004
OSR2(preds.lm, train$rating, test$rating) # 0.075057          0.07983356


# ==================try random forest================
library(ranger)
library(randomForest)

#first train
mae.vals.rf = rep(NA, length(seq(101, 201, 10)))
i = 1
for (mtry in seq(101, 201, 10)) {
  set.seed(100)
  print(str_c("Trying mtry = ", mtry))
  rf.mod <-  ranger(rating ~ .  -movieId -userId, 
                    data = train, 
                    mtry = mtry, 
                    num.trees = 500,
                    verbose = TRUE)
  preds.rf <- predict(rf.mod, data = val1)
  preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)
  mae.vals.rf[i] <- mean(abs(preds.rf - val1$rating))
  i = i+1
}

mae.val.df_rf <- data.frame(mtry = seq(101, 201, 10), mae = mae.vals.rf)
ggplot(mae.val.df_rf, aes(x = mtry, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of mtry") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

mae.val.df_rf_below100 <- mae.val.df_rf
mae.val.df_rf_larger100 <- mae.val.df_rf[1:10,]

#train 1 plot 
mae.val.df_rf_train1 <- rbind(mae.val.df_rf_below100,mae.val.df_rf_larger100)
ggplot(mae.val.df_rf_train1[-1,], aes(x = mtry, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of mtry") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


# second train
mae.vals.rf = rep(NA, length(seq(112, 130, 3)))
i = 1
for (mtry in seq(112, 130, 3)) {
  set.seed(100)
  print(str_c("Trying mtry = ", mtry))
  rf.mod <-  ranger(rating ~ .  -movieId -userId, 
                    data = train, 
                    mtry = mtry, 
                    num.trees = 500,
                    verbose = TRUE)
  preds.rf <- predict(rf.mod, data = val1)
  preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)
  mae.vals.rf[i] <- mean(abs(preds.rf - val1$rating))
  i = i+1
}

#train 2 plot 
mae.val.df_rf_train2 <- data.frame(mtry = seq(112, 130, 3), mae = mae.vals.rf)
mae.val.df_rf_train2 <- rbind(mae.val.df_rf_train1[12,],mae.val.df_rf_train2,mae.val.df_rf_train1[14,])

ggplot(mae.val.df_rf_train2, aes(x = mtry, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of mtry") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))


#final model: choose mtry = 120
set.seed(3592)
rf.mod <- ranger(rating ~ .  -movieId -userId, 
                 data = train, 
                 mtry = 120, #floor((ncol(train) - 3)/3), 
                 num.trees = 500,
                 verbose = TRUE,
                 importance = "impurity")
preds.rf <- predict(rf.mod, data = test)
preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)

mean(abs(preds.rf - test$rating))/4.5 #0.1549859  #with keyword 0.154931
sqrt(mean((preds.rf - test$rating)^2))/4.5 #0.2001288  #with keyword 0.2002545
OSR2(preds.rf, train$rating, test$rating) #0.2159108  #with keyword 0.2149251

rf.mod$variable.importance

# ==================XGBoost=========================
library(xgboost)

train_matrix <- subset(train, select = -c(movieId,userId))
dtrain <- xgb.DMatrix(data = as.matrix(train_matrix[,-1]) , label = as.matrix(train$rating))
test_matrix <- subset(test, select = -c(movieId,userId))
dtest <- xgb.DMatrix(data = as.matrix(test_matrix[,-1]) , label = as.matrix(test$rating))

set.seed(123)
xgb.mod <- xgboost(data = dtrain,
                  objective = "reg:linear", eval_metric = "rmse", 
                  nfold = 5,nrounds = 500, verbose = FALSE,
                  nthread = 8, eta = 0.01, gamma = 0.0468, max_depth = 20, min_child_weight = 1.7817, 
                  subsample = 0.5213, colsample_bytree = 0.4603)
# print(xgbFit)

# Predictions
xgb.preds <- predict(xgb.mod, newdata = dtest)%>% pmin(5) %>% pmax(0.5)

mean(abs(xgb.preds - test$rating))/4.5 # 0.1499456(max_depth = 15)  #keyword 0.1482095
sqrt(mean((xgb.preds - test$rating)^2))/4.5   # 0.1926818(max_depth = 15)  #keyword 0.1911825
OSR2(xgb.preds, train$rating, test$rating) # 0.2731787(max_depth = 15)  #keyword 0.2844459

# ===========Tuning Parameter of XGBoost
# cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 5)
# xgb.grid <- expand.grid(nrounds = 500,
#                         max_depth = 15,
#                         eta = c(0.01,0.3, 1),
#                         gamma = 0.04,
#                         colsample_bytree = 0.5,
#                         min_child_weight=seq(1,3,0.5),
#                         subsample = 0.5213)
# 
# xgb_tune <-train(rating ~ .  -movieId -userId, 
#                  data = train, 
#                  method="xgbTree",
#                  metric = "RMSE",
#                  trControl=cv.ctrl,
#                  tuneGrid=xgb.grid)
# prediction <- predict(xgb_tune, test_data)

# ===================Neural Network ================
# library(neuralnet)
# 
# nn.mod <- neuralnet(rating ~ .  -movieId -userId, 
#                    data = train,  
#                    hidden=c(10,5), 
#                    linear.output=TRUE, 
#                    threshold=0.01)
# # ?neuralnet
# # nn$result.matrix
# # plot(nn)
# 
# # predictions
# nn.preds <- predict(nn.mod, newdata = test)%>% pmin(5) %>% pmax(0.5)
# 
# mean(abs(nn.preds - test$rating))/4.5 # 
# sqrt(mean((nn.preds - test$rating)^2))/4.5  # 
# OSR2(nn.preds, train$rating, test$rating) # 


#～～～～～～～～～～cast_crew run from here～～～～～～～～～～～～
cast_crew_user_model = read.csv("cast_crew_user_model.csv") 
cast_crew_user_model = subset(cast_crew_user_model, select = -c(1))
cast_crew_user_model$ClusterType <- as.factor(cast_crew_user_model$ClusterType)

set.seed(500)
cast_train.ids <- sample(nrow(cast_crew_user_model), 0.92*nrow(cast_crew_user_model))
cast_train <- cast_crew_user_model[cast_train.ids,]
cast_test <- cast_crew_user_model[-cast_train.ids,]

# split training into real training and validation set
set.seed(500)
cast_val1.ids <- sample(nrow(cast_train), (4/92)*nrow(cast_train))
cast_val1 <-cast_train[cast_val1.ids,]
cast_train <- cast_train[-cast_val1.ids,]

set.seed(500)
cast_val2.ids <- sample(nrow(cast_train), (4/88)*nrow(cast_train))
cast_val2 <- cast_train[cast_val2.ids,]
cast_train <- cast_train[-cast_val2.ids,]

#======================try Linear regression ====================================== 
cast_lin.mod <- lm(Rating ~ . -MovieId -UserId, data = cast_train)
summary(cast_lin.mod)
cast_preds.lm <- predict(cast_lin.mod, newdata = cast_test) %>% pmin(5) %>% pmax(0.5)
mean(abs(cast_preds.lm - cast_test$Rating))/4.5 # 0.1729005
sqrt(mean((cast_preds.lm - cast_test$Rating)^2))/4.5 # 0.2179789
OSR2(cast_preds.lm, cast_train$Rating, cast_test$Rating) # 0.066723
#================================Random Forest==============================
library(ranger)
library(randomForest)

set.seed(3592)
cast_rf.mod <- ranger(Rating ~ .  -MovieId -UserId, 
                 data = cast_train, 
                 mtry = 68, #floor((ncol(train) - 3)/3), 
                 num.trees = 500,
                 verbose = TRUE,
                 importance = "impurity")
cast_preds.rf <- predict(cast_rf.mod, data = cast_test)
cast_preds.rf <- cast_preds.rf$predictions%>% pmin(5) %>% pmax(0.5)

mean(abs(cast_preds.rf - cast_test$Rating))/4.5 #0.1622767
sqrt(mean((cast_preds.rf - cast_test$Rating)^2))/4.5 #0.2096969
OSR2(cast_preds.rf, cast_train$Rating, cast_test$Rating) #0.136294

# ～～～～～～～～～～～～～～Blending～～～～～～～～～～～～～～～～～
val2_matrix <- subset(val2, select = -c(movieId,userId))
dval2 <- xgb.DMatrix(data = as.matrix(val2_matrix[,-1]) , label = as.matrix(val2$rating))

val.preds.cf <- impute(mod.final, val2$userId, val2$movieId) %>% pmin(5) %>% pmax(0.5)
val.preds.lm <- predict(lin.mod, newdata = val2) %>% pmin(5) %>% pmax(0.5)
val.preds.rf <- predict(rf.mod, data = val2)$predictions %>% pmin(5) %>% pmax(0.5)
val.preds.xg <- predict(xgb.mod, newdata = dval2)%>% pmin(5) %>% pmax(0.5)
val.preds.lm.cast <- predict(cast_lin.mod, newdata = cast_val2) %>% pmin(5) %>% pmax(0.5)
val.preds.rf.cast <- predict(cast_rf.mod, data = cast_val2)$predictions %>% pmin(5) %>% pmax(0.5)

# Build validation set data frame
val.blending_df <- data.frame(rating = val2$rating, cf_preds = val.preds.cf, 
                             lm_preds = val.preds.lm, rf_preds = val.preds.rf, 
                             xg_preds = val.preds.xg, lm_preds_cast = val.preds.lm.cast,
                             rf_preds_cast = val.preds.rf.cast)

# Train blended model
blend.mod <- lm(rating ~ . -1, data = val.blending_df)
summary(blend.mod)



# Get predictions on test set
test.preds.cf <- impute(mod.final, test$userId, test$movieId) %>% pmin(5) %>% pmax(0.5)
test.preds.lm <- predict(lin.mod, newdata = test) %>% pmin(5) %>% pmax(0.5)
test.preds.rf <- predict(rf.mod, data = test)$predictions %>% pmin(5) %>% pmax(0.5)
test.preds.xg <- predict(xgb.mod, newdata = dtest)%>% pmin(5) %>% pmax(0.5)
test.preds.lm.cast <- predict(cast_lin.mod, newdata = cast_test) %>% pmin(5) %>% pmax(0.5)
test.preds.rf.cast <- predict(cast_rf.mod, data = cast_test)$predictions %>% pmin(5) %>% pmax(0.5)

test.blending_df = data.frame(rating = test$rating, cf_preds = test.preds.cf, 
                              lm_preds = test.preds.lm, rf_preds = test.preds.rf,
                              xg_preds = test.preds.xg,lm_preds_cast = test.preds.lm.cast,
                              rf_preds_cast = test.preds.rf.cast) 

test.preds.blend <- predict(blend.mod, newdata = test.blending_df) %>% pmin(5) %>% pmax(0.5)

mean(abs(test.preds.blend - test$rating))/4.5 # 0.1413476 # 0.1412085 #keyword 0.1413724 #0.1413705
sqrt(mean((test.preds.blend - test$rating)^2))/4.5 # 0.1838554 # 0.1836525 #keyword 0.1837938 # all 0.1837949
OSR2(test.preds.blend, train$rating, test$rating) # 0.3382424 #0.3397021 #keyword 0.3386857 #all 0.3386777

 

# All

#～～～～～～～～～～～～～～～～Recommendation Movie for One User～～～～～～～～～～～～～～～
library(plyr)

# ～～～Movie User Keyword～～～
## get ratings of one user for 1321 movies
unique_movie <- sort(unique(rating_features_small$movieId)) 
# length(unique_movie) #1321
unique_user <- unique(rating_features_small$userId)
#length(unique_user) #1944
rec_user <- rep(unique_user[2],length(unique_movie) )
rec_df <- data.frame(userId = rec_user,movieId = unique_movie) 

user_feature <- subset(rating_features_small,select = c(userId,age.1:User_viewN))
user_feature <- user_feature[!duplicated(user_feature$userId), ]
movie_feature <- subset(rating_features_small,select = -c(userId,rating,age.1:User_viewN))
#length(unique(movie_feature$movieId))
movie_feature <- movie_feature[!duplicated(movie_feature$movieId), ]

rec_df$rating <- 0
rec_df <- join(rec_df, movie_feature, by = c("movieId"),type = "inner")
rec_df <- join(rec_df, user_feature, by = c("userId"),type = "inner")

rec_df <- rec_df[,c(1:66,167:203,67:166)]

#～～～～～Cast_User ～～～～
## get ratings of one user for 1321 movies
cast_unique_movie <- unique(cast_crew_user_model$MovieId)
# length(unique(cast_crew_user_model$movieId)) #1321
cast_unique_user <- unique(cast_crew_user_model$UserId)
#length(unique_user) #1944
cast_rec_user <- rep(11,length(unique_movie))
cast_rec_df <- data.frame(UserId = cast_rec_user,MovieId = cast_unique_movie) 

cast_user_feature <- subset(cast_crew_user_model,select = c(UserId,LogAge:ClusterType))
cast_user_feature <- cast_user_feature[!duplicated(cast_user_feature$UserId), ]
cast_movie_feature <- subset(cast_crew_user_model,select = -c(UserId,LogAge:ClusterType))
#length(unique(movie_feature$movieId))
cast_movie_feature <- cast_movie_feature[!duplicated(cast_movie_feature$MovieId), ]

cast_rec_df$rating <- 0
cast_rec_df <- join(cast_rec_df, cast_movie_feature, by = c("MovieId"),type = "inner")
cast_rec_df <- join(cast_rec_df, cast_user_feature, by = c("UserId"),type = "inner")

## using blend model to do prediction
rec_df_matrix <- subset(rec_df, select = -c(movieId,userId))
drec_df <- xgb.DMatrix(data = as.matrix(rec_df_matrix[,-1]) , label = as.matrix(rec_df$rating))
# Get predictions on rec_df 
rec_df.preds.cf <- impute(mod.final, rec_df$userId, rec_df$movieId) %>% pmin(5) %>% pmax(0.5)
rec_df.preds.lm <- predict(lin.mod, newdata = rec_df) %>% pmin(5) %>% pmax(0.5)
rec_df.preds.rf <- predict(rf.mod, data = rec_df)$predictions %>% pmin(5) %>% pmax(0.5)
rec_df.preds.xg <- predict(xgb.mod, newdata = drec_df)%>% pmin(5) %>% pmax(0.5)
rec_df.preds.lm.cast <- predict(cast_lin.mod, newdata = cast_rec_df) %>% pmin(5) %>% pmax(0.5)
rec_df.preds.rf.cast <- predict(cast_rf.mod, data = cast_rec_df)$predictions %>% pmin(5) %>% pmax(0.5)

rec_df.blending_df <- data.frame(rating = rec_df$rating, cf_preds = rec_df.preds.cf, 
                              lm_preds = rec_df.preds.lm, rf_preds = rec_df.preds.rf,
                              xg_preds = rec_df.preds.xg,lm_preds_cast = rec_df.preds.lm.cast,
                              rf_preds_cast = rec_df.preds.rf.cast) 

rec_df.preds.blend <- predict(blend.mod, newdata = rec_df.blending_df) %>% pmin(5) %>% pmax(0.5)


## get movies with top ratings 
rec_df$rating <-  rec_df.preds.blend
cast_rec_df$rating <-  rec_df.preds.blend

ordered_rec_df <- rec_df[order(rec_df$rating,decreasing = TRUE),]
rec_movie_id <- head(ordered_rec_df$movieId)
head(ordered_rec_df$rating)

movie_name <- read.csv("movies_metadata.csv")
rec_movie_name <- list()
for (i in 1:length(rec_movie_id)){
  rec_movie_name[i] <- as.character( movie_name[movie_name$id == rec_movie_id[i], ]$title ) 
}

# userID = 8
# [[1]]
# [1] "The Million Dollar Hotel"
# 
# [[2]]
# [1] "Once Were Warriors"
# 
# [[3]]
# [1] "Solaris"
# 
# [[4]]
# [1] "Sleepless in Seattle"
# 
# [[5]]
# [1] "The 39 Steps"
# 
# [[6]]
# [1] "Scarface"

# userID = 11
# [[1]]
# [1] "La Chienne"
# 
# [[2]]
# [1] "License to Wed"
# 
# [[3]]
# [1] "Boat"
# 
# [[4]]
# [1] "Solaris"
# 
# [[5]]
# [1] "The Million Dollar Hotel"
# 
# [[6]]
# [1] "Once Were Warriors"
