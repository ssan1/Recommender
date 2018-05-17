# Prep - install package "arules" and "recommenderlab" for setting the system

install.packages('arules', repos="http://cran.us.r-project.org")
install.packages('recommenderlab', repos="http://cran.us.r-project.org")


# Creating Data Frame for uploading the input file

data = read.table('u.data')

# Renaming columns of data columns
colnames(data) = c("user_id", "item_id", "rating", "timestamp")

# Removing "timestamp" from data as its not needed for recommendation

data = data[ , -which(names(data) %in% c("timestamp"))]

# Display data summary

summary(data)


# hist(data$rating)

hist(data$rating)

#**************** SPARSITY  ****************#
# Calculate total number of Rows in Data

num_ratings = nrow(data)

# Calculate number of movies in Data
num_movies = length(unique(data$item_id))

# Calculate number of users in Data
num_users = length(unique(data$user_id))

 
sparsity = num_ratings / (num_movies * num_users) * 100
print(c('Sparsity of Dataset is', sparsity))

# To improve Sparsity , Data Partition is included  which means include only thoses users who have atleast rated 50 movies.

data = data[ data$user_id %in% names(table(data$user_id))[table(data$user_id) > 50] , ]

num_ratings = nrow(data)
num_movies = length(unique(data$item_id))
num_users = length(unique(data$user_id))
sparsity = num_ratings / (num_movies * num_users) * 100
print(c('UPDATED Sparsity of Dataset is', sparsity))


# PART B - Train and Test Model

  # Download Recommender Lab package
library('recommenderlab')

# Converting data type to a recommenderlab compatible type - 
# method to convert a data-frame into a realRatingMatrix 
data_new = as(data, "realRatingMatrix")

# Splitting data into 70% training and 30% in testing data.

# Need to tell recommender lab what the top rating is (5)
data_split = evaluationScheme(data_new, method="split", train=0.7, given=15, goodRating=5)
train = getData(data_split, "train")
test_X = getData(data_split, "known")
test_Y = getData(data_split, "unknown")


rec_pop = Recommender(train, method="POPULAR")

# Model is tested on Data using "Popularity Recommender"

predicted = predict(rec_pop, test_X, type="ratings")
error = calcPredictionAccuracy(predicted, test_Y)
error


# ********************Integrating a Collaborative Filtering Recommender************************#


val_split = evaluationScheme(train, method="split", train=0.7, given=15, goodRating=5)
train_real = getData(val_split, "train")
val_X = getData(val_split, "known")
val_Y = getData(val_split, "unknown")


# Create model
rec_ubcf = Recommender(train_real, "UBCF")
predicted_ubcf = predict(rec_ubcf, val_X, type="ratings")
# Now, we compute the RMSE
error_ubcf = calcPredictionAccuracy(predicted_ubcf, val_Y)
print(error_ubcf)


# *********************Integrating an Item-Item Similarity Recommender ************************#

# Create model
rec_ibcf = Recommender(train, "IBCF")
predicted_ibcf = predict(rec_ibcf, test_X, type="ratings")
# Now, we compute the RMSE
error_ibcf = calcPredictionAccuracy(predicted_ibcf, test_Y)
print(error_ibcf)




# ** How to Calculate Top recommendation for each model . K = 5 OR 10 **#


results_pop <- evaluate(data_split, method="POPULAR", type="topNList", n=c(5, 10))

results_ubcf <- evaluate(data_split, method="UBCF", type="topNList", n=c(5, 10))

results_ibcf <- evaluate(data_split, method="IBCF", type="topNList", n=c(5, 10))

 # top prediction for specific user - userid = 1
user_recs = predict(rec_pop, test_X, n=5, type="topNList")
user_recs@items[1]


# **************** PART C - Confusion Matrix *************************** #

# Popularity

print(c('Popularity', results_pop@results))

# Collaborative Filtering 

print(c('Collaborative Filtering', results_ubcf@results))

# Item-Item Similarity

print(c('Item-Item Similarity', results_ibcf@results))









