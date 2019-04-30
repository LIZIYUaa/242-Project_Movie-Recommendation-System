setwd("/Users/apple/desktop/242/project")
model2 <-read.csv("cast_crew_user_model2.csv")
length(unique(model2$MovieId))
length(unique(model2$UserId))

# Change model2$cluster into dummy variables
library(caret)
str(model2)
length(unique(model2$ClusterType))   #30
model2$ClusterType <- as.factor(model2$ClusterType)
dummy <- dummyVars(" ~ .", data = model2)
model2_dummy <- data.frame(predict(dummy, newdata = model2))
write.csv(model2_dummy, "model2_dummy.csv")



library(softImpute) #CF model
library(randomForest) # Random Forest
library(xgboost) #XGboost
library(ranger) # Random Forest
library(neuralnet) # Neural Network
library(fastDummies) # Dummy
library(onehot) # Dummy
library(dplyr)
library(data.table)
library(tidyverse)
library(reshape2)
library(ranger)
library(randomForest)

OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

#============================ split data ================================
set.seed(500)
train.ids <- sample(nrow(model2_dummy), 0.92*nrow(model2_dummy))
train <- model2_dummy[train.ids,]
test <- model2_dummy[-train.ids,]

# split training into real training and validation set
set.seed(500)
val1.ids <- sample(nrow(train), (4/92)*nrow(train))
val1 <- train[val1.ids,]
train <- train[-val1.ids,]

set.seed(500)
val2.ids <- sample(nrow(train), (4/88)*nrow(train))
val2 <- train[val2.ids,]
train <- train[-val2.ids,]

#================= compare the osr2 btwn dummy and non-dummy ====================
set.seed(3592)
rf.mod <- ranger(Rating ~ .  -MovieId -UserId -X, 
                 data = train, 
                 mtry = 67, #floor((ncol(train) - 3)/3), 
                 num.trees = 500,
                 verbose = TRUE,
                 importance = "impurity")
preds.rf <- predict(rf.mod, data = test)
preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)

mean(abs(preds.rf - test$Rating))/4.5 #0.1624504
sqrt(mean((preds.rf - test$Rating)^2))/4.5 #0.2100681
OSR2(preds.rf, train$Rating, test$Rating) #0.1332385

# Since this OSR2 is lower than without dummy(0.1351315)
# Then we will still use model2

#============================ split data ================================
set.seed(500)
train.ids <- sample(nrow(model2), 0.92*nrow(model2))
train <- model2[train.ids,]
test <- model2[-train.ids,]

# split training into real training and validation set
set.seed(500)
val1.ids <- sample(nrow(train), (4/92)*nrow(train))
val1 <- train[val1.ids,]
train <- train[-val1.ids,]

set.seed(500)
val2.ids <- sample(nrow(train), (4/88)*nrow(train))
val2 <- train[val2.ids,]
train <- train[-val2.ids,]

#======================try Linear regression ====================================== 
lin.mod <- lm(Rating ~ . -MovieId -UserId -X, data = train)
summary(lin.mod)
preds.lm <- predict(lin.mod, newdata = test) %>% pmin(5) %>% pmax(0.5)
mean(abs(preds.lm - test$Rating))/4.5 # 0.1737025
sqrt(mean((preds.lm - test$Rating)^2))/4.5 # 0.218938
OSR2(preds.lm, train$Rating, test$Rating) # 0.05849714

#================================Random Forest============================
#first train
mae.vals.rf = rep(NA, length(seq(1, 100, 10)))
i = 1
for (mtry in seq(1, 100, 10)) {
  set.seed(100)
  print(str_c("Trying mtry = ", mtry))
  rf.mod <-  ranger(Rating ~ .  -MovieId -UserId -X, 
                    data = train, 
                    mtry = mtry, 
                    num.trees = 500,
                    verbose = TRUE)
  preds.rf <- predict(rf.mod, data = val1)
  preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)
  mae.vals.rf[i] <- mean(abs(preds.rf - val1$Rating))
  i = i+1
}

mae.val.df_rf <- data.frame(mtry = seq(1, 100, 10), mae = mae.vals.rf)
ggplot(mae.val.df_rf, aes(x = mtry, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of mtry") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

# Since when mtry=31, mae is the lowst = 0.7283766, so shrink our range to 21-41
mae.vals.rf = rep(NA, length(seq(21, 41, 1)))
i = 1
for (mtry in seq(21, 41, 1)) {
  set.seed(100)
  print(str_c("Trying mtry = ", mtry))
  rf.mod <-  ranger(Rating ~ .  -MovieId -UserId -X, 
                    data = train, 
                    mtry = mtry, 
                    num.trees = 500,
                    verbose = TRUE)
  preds.rf <- predict(rf.mod, data = val1)
  preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)
  mae.vals.rf[i] <- mean(abs(preds.rf - val1$Rating))
  i = i+1
}

mae.val.df_rf <- data.frame(mtry = seq(21, 41, 1), mae = mae.vals.rf)
ggplot(mae.val.df_rf, aes(x = mtry, y = mae)) + geom_point(size = 3) + geom_line(size = 1) +
  ylab("Validation MAE") + xlab("Number of mtry") + 
  theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

# when mtry=27, we reach the lowest mae = 0.7275158
set.seed(3592)
rf.mod <- ranger(Rating ~ .  -MovieId -UserId -X, 
                 data = train, 
                 mtry = 27, 
                 num.trees = 500,
                 verbose = TRUE,
                 importance = "impurity")
preds.rf <- predict(rf.mod, data = test)
preds.rf <- preds.rf$predictions%>% pmin(5) %>% pmax(0.5)

mean(abs(preds.rf - test$Rating))/4.5 #0.1607902
sqrt(mean((preds.rf - test$Rating)^2))/4.5 #0.2073432
OSR2(preds.rf, train$Rating, test$Rating) #0.1555789





