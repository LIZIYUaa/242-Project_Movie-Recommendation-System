library(caret)
library(dplyr)
library(ggplot2)


#============================movie feature group by year and season==================
# Load the data:
rating_features_small <- read.csv("rating_features_small_3.0.csv")

# Take a look:
head(rating_features_small)

rating_features_small <- subset(rating_features_small,select=-c(Male.1,Urban.1,RegionMidwest.1,
                                                                RegionNortheast.1,
                                                                RegionWest.1,RegionSouth.1))

head(rating_features_small)

release_spring <- rating_features_small$release_date_month.3 +
  rating_features_small$release_date_month.4 + rating_features_small$release_date_month.5
release_summer <- rating_features_small$release_date_month.6 +
  rating_features_small$release_date_month.7 + rating_features_small$release_date_month.8
release_autumn <- rating_features_small$release_date_month.9 +
  rating_features_small$release_date_month.10 + rating_features_small$release_date_month.11
release_winter <- rating_features_small$release_date_month.12 +
  rating_features_small$release_date_month.1 + rating_features_small$release_date_month.2

rating_features_small<-cbind(rating_features_small,release_spring,release_summer,release_autumn,release_winter)

rating_features_small <- subset(rating_features_small,select=-c(153:164)) # remove release_month


#===========take a look at the distribution of release year=============
metadata <- read.csv("rating_features_small.csv")
#drop repeated
metadata<- metadata[!duplicated(metadata$movieId), ]
ggplot(metadata, aes(x = release_date_year)) + geom_histogram() + ggtitle("release_date_year")
table(metadata$release_date_year)


release_1930 <- rating_features_small$release_date_year.1896 +
  +   rating_features_small$release_date_year.1903 + rating_features_small$release_date_year.1910 +
  +   rating_features_small$release_date_year.1916 + rating_features_small$release_date_year.1919 +
  +   rating_features_small$release_date_year.1920 + rating_features_small$release_date_year.1921 + 
  +   rating_features_small$release_date_year.1922 + rating_features_small$release_date_year.1924 + 
  +   rating_features_small$release_date_year.1925 + rating_features_small$release_date_year.1926 + 
  +   rating_features_small$release_date_year.1927 + rating_features_small$release_date_year.1928 + 
  +   rating_features_small$release_date_year.1929 + rating_features_small$release_date_year.1930
release_1940 <- rating_features_small$release_date_year.1931 +
  +   rating_features_small$release_date_year.1932 + rating_features_small$release_date_year.1933 +
  +   rating_features_small$release_date_year.1934 + rating_features_small$release_date_year.1935 +
  +   rating_features_small$release_date_year.1936 + rating_features_small$release_date_year.1937 + 
  +   rating_features_small$release_date_year.1938 + rating_features_small$release_date_year.1939 + 
  +   rating_features_small$release_date_year.1940
release_1950 <- rating_features_small$release_date_year.1941 +
  +   rating_features_small$release_date_year.1942 + rating_features_small$release_date_year.1943 +
  +   rating_features_small$release_date_year.1944 + rating_features_small$release_date_year.1945 +
  +   rating_features_small$release_date_year.1946 + rating_features_small$release_date_year.1947 + 
  +   rating_features_small$release_date_year.1948 + rating_features_small$release_date_year.1949 + 
  +   rating_features_small$release_date_year.1950
release_1960 <- rating_features_small$release_date_year.1951 +
  +   rating_features_small$release_date_year.1952 + rating_features_small$release_date_year.1953 +
  +   rating_features_small$release_date_year.1954 + rating_features_small$release_date_year.1955 +
  +   rating_features_small$release_date_year.1956 + rating_features_small$release_date_year.1957 + 
  +   rating_features_small$release_date_year.1958 + rating_features_small$release_date_year.1959 + 
  +   rating_features_small$release_date_year.1960
release_1970 <- rating_features_small$release_date_year.1961 +
  +   rating_features_small$release_date_year.1962 + rating_features_small$release_date_year.1963 +
  +   rating_features_small$release_date_year.1964 + rating_features_small$release_date_year.1965 +
  +   rating_features_small$release_date_year.1966 + rating_features_small$release_date_year.1967 + 
  +   rating_features_small$release_date_year.1968 + rating_features_small$release_date_year.1969 + 
  +   rating_features_small$release_date_year.1970
release_1980 <- rating_features_small$release_date_year.1971 +
  +   rating_features_small$release_date_year.1972 + rating_features_small$release_date_year.1973 +
  +   rating_features_small$release_date_year.1974 + rating_features_small$release_date_year.1975 +
  +   rating_features_small$release_date_year.1976 + rating_features_small$release_date_year.1977 + 
  +   rating_features_small$release_date_year.1978 + rating_features_small$release_date_year.1979 + 
  +   rating_features_small$release_date_year.1980
release_1990 <- rating_features_small$release_date_year.1981 +
  +   rating_features_small$release_date_year.1982 + rating_features_small$release_date_year.1983 +
  +   rating_features_small$release_date_year.1984 + rating_features_small$release_date_year.1985 +
  +   rating_features_small$release_date_year.1986 + rating_features_small$release_date_year.1987 + 
  +   rating_features_small$release_date_year.1988 + rating_features_small$release_date_year.1989 + 
  +   rating_features_small$release_date_year.1990
release_2000 <- rating_features_small$release_date_year.1991 +
  +   rating_features_small$release_date_year.1992 + rating_features_small$release_date_year.1993 +
  +   rating_features_small$release_date_year.1994 + rating_features_small$release_date_year.1995 +
  +   rating_features_small$release_date_year.1996 + rating_features_small$release_date_year.1997 + 
  +   rating_features_small$release_date_year.1998 + rating_features_small$release_date_year.1999 + 
  +   rating_features_small$release_date_year.2000
release_2010 <- rating_features_small$release_date_year.2001 +
  +   rating_features_small$release_date_year.2002 + rating_features_small$release_date_year.2003 +
  +   rating_features_small$release_date_year.2004 + rating_features_small$release_date_year.2005 +
  +   rating_features_small$release_date_year.2006 + rating_features_small$release_date_year.2007 + 
  +   rating_features_small$release_date_year.2008 + rating_features_small$release_date_year.2009 + 
  +   rating_features_small$release_date_year.2010
release_2015 <- rating_features_small$release_date_year.2011 +
  +   rating_features_small$release_date_year.2012 + rating_features_small$release_date_year.2013 +
  +   rating_features_small$release_date_year.2015

rating_features_small<-cbind(rating_features_small,release_1930,release_1940,release_1950,release_1960,
                             release_1970,release_1980,release_1990,release_2000,release_2010,release_2015)


rating_features_small <- subset(rating_features_small,select=-c(54:152)) # remove release_year


rating_features_small = rating_features_small[, c(1:25, 91:104, 26:90)] # rearrange order

write.csv(rating_features_small,'rating_features_small_4.0.csv')

head(rating_features_small)


# ===========================users cluster ==========================================
# cluster
user_feature = rating_features_small[, c(2, 68:103)] 

user_feature<- user_feature[!duplicated(user_feature$userId), ]
# We need to preprocess the data so that we treat each column
# equally to compute the clusters
# we use the preProcess function from Caret that does
# all the work for us

# preprocessing steps:
# First, center the data (substract the mean to each column)
# => mean becomes 0 for each column
# Then, scale the data, by dividing by the standard deviation
# => std becomes 1 for each column
# https://medium.com/@rrfd/standardize-or-normalize-examples-in-python-e3f174b65dfc

str(user_feature)
user_feature_numeric <- user_feature[, c(31,37)] 
head(user_feature_numeric)

#step 1: create the pre-processor using preProcess (part of caret)
pp <- preProcess(user_feature_numeric, method=c("center", "scale"))
#step 2: apply it to our dataset

user_feature_numeric.scaled <- predict(pp, user_feature_numeric) # newdata?

head(user_feature_numeric.scaled)

user_feature <- subset(user_feature, select = -c(31,37))
head(user_feature)

user_feature.scaled <- cbind(user_feature,user_feature_numeric.scaled)

summary(user_feature.scaled)
# What does a negative value represent?


### Clustering: K-Means

# the kmeans function is part of stats package (default in R)
# we can set an upper bound to the number of iterations of the algorithm
# here we set k=8
# setting the seed..why?
set.seed(144)
head(user_feature.scaled[,-1])

km <- kmeans(user_feature.scaled[,-1], iter.max=100, 30)

# Let's explore the results!

km 

# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km$withinss

# betweenss: sum of squared distances between each cluster mean and
# the data mean
km$betweenss

# number of iters
km$iter
# cluster centroids
km$centers
# cluster for each point
cluster_type <- km$cluster

# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km$tot.withinss

# the number of observations in each cluster -- table(km$cluster) also works
km$size


user_feature.scaled$cluster_type <- cluster_type


# Selecting the value of K
# we want to create the scree plot
# to do this, we want to compute the cluster dissimilarity for
# all the values of k we want to try
# we store this result in the dataframe "dat"

# here we test all k from 1 to 100

dat <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat$SS <- sapply(dat$k, function(k) {
  set.seed(144)
  kmeans(user_feature.scaled[,-1], iter.max=100, k)$tot.withinss
})

# let's plot it using ggplot!
ggplot(dat, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 20, color = "blue")
# Was k=8 a good choice ?
# remember that we are looking for a "knee" in the curve / a place where rocks stop falling
# https://scikit-learn.org/stable/auto_examples/cluster/plot_kmeans_silhouette_analysis.html

 

#============================log age & median income===================================
user_notdummy <- read.csv("rating_features_small.csv")
#drop repeated
user_notdummy<- user_notdummy[!duplicated(user_notdummy$userId), ]
user_notdummy <- user_notdummy[,c(2,35,38)]

#age
table(user_notdummy$age)
user_notdummy$age[user_notdummy$age == 1] = 15
user_notdummy$age[user_notdummy$age == 18] = (18+25)/2
user_notdummy$age[user_notdummy$age == 25] = 30
user_notdummy$age[user_notdummy$age == 35] = 40
user_notdummy$age[user_notdummy$age == 45] = 47.5
user_notdummy$age[user_notdummy$age == 50] = 53
user_notdummy$age[user_notdummy$age == 56] = 60

user_notdummy$age = log(user_notdummy$age)

#median income
user_notdummy$MedianIncome = log(user_notdummy$MedianIncome)
head(user_notdummy)

#add cluster type
user_notdummy$cluster_type <- cluster_type
names(user_notdummy)<-c("userId","LogAge","LogMedianIncome","ClusterType")

write.csv(user_notdummy,'user_features_version2.csv')

test <- subset(rating_features_small,select=68)

head(test)
summary(test$spring)


