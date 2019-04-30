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

