# 242-Project_Movie-Recommendation-System

1-movie_metadata2:
  1. R code converting json files(genres) into csv files, and then do some exploratory data analytics and data cleaning on movie features.
  
2-user cluster:
  1. R code cluster user based on age, gender, occupation, etc.
  2. R code to preprocess movie release time.

3-movie keywords:
  1. R code converting json files(keywords) into csv files, and then extract 100 unqiue keywords based on high frequency.

4-movie cast crew:
  1. Python code converting json files(credits) into csv files, and then we extract the cast and crew informations to create corresponding cast and crew csv files.
  2. R code to preprocess the cast and crew dataset. For example, we only extract three actors and 1 directors per movie, and then according to the frequency, we extract top 50 directors and top 150 actors.

5-Model_Blend_rating_small_2.0:
  1. R code to train several models like cf/rf by cross validation.
  2. R code to ensemble several models.
  3. R code to recommendation movies to users.
