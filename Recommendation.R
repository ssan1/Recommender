install.packages('arules', repos="http://cran.us.r-project.org")
install.packages('recommenderlab', repos="http://cran.us.r-project.org")
library(xlsx)
data = read.table('u.data')

colnames(data) = c("user_id", "item_id", "rating", "timestamp")
data = data[ , -which(names(data) %in% c("timestamp"))]
print(data)
summary(data)
hist(data$rating)
num_ratings = nrow(data)
num_movies = length(unique(data$item_id))
num_users = length(unique(data$user_id))
print(num_users)
sparsity = num_ratings / (num_movies * num_users) * 100
print(c('Sparsity of Dataset is', sparsity))
data = data[ data$user_id %in% names(table(data$user_id))[table(data$user_id) > 50] , ]
print(data)


num_ratings = nrow(data)
num_movies = length(unique(data$item_id))
num_users = length(unique(data$user_id))
print(num_users)
sparsity = num_ratings / (num_movies * num_users) * 100
print(c('UPDATED Sparsity of Dataset is', sparsity))

# Train & Test Split

library('recommenderlab')
# First, we convert our data type to a recommenderlab compatible type
data_new = as(data, "realRatingMatrix")


# Now, we apply a split of 0.7 train, 0.3 test
# Need to tell recommender lab what the top rating is (5)
data_split = evaluationScheme(data_new, method="split", train=0.7, given=15, goodRating=5)
train = getData(data_split, "train")
test_X = getData(data_split, "known")
test_Y = getData(data_split, "unknown")



# 1-----> Integrating a **Collaborative Filtering** Recommender
val_split = evaluationScheme(train, method="split", train=0.7, given=15, goodRating=5)
train_real = getData(val_split, "train")
val_X = getData(val_split, "known")
val_Y = getData(val_split, "unknown")

## Create model  - COLLOBORATIVE FILTERING ---------
rec_ubcf = Recommender(train_real, "UBCF")
predicted_ubcf = predict(rec_ubcf, val_X, type="ratings")

# Getting top 5 recommendation of movie
results_ubcf.df <- evaluate(data_split, method="UBCF", type="topNList", n=c(5, 10))


#names(results_ubcf.df)
user_recs.df = predict(rec_ubcf, test_X, n=5, type="topNList")
#print(user_recs)

# print top 5 recommendation of movie based on user id .

user_recs@items[data$user_id]

movie_id <- user_recs@items[]
username <- user_recs@itemLabels[]

out1 <- cbind(username[data$user_id],movie_id)
write.csv(out1,"C:/Documents/colab.csv")

# 2------> # Integrating a Popularity Recommender

rec_pop = Recommender(train, method="POPULAR")

predicted = predict(rec_pop, test_X, type="ratings")
error = calcPredictionAccuracy(predicted, test_Y)
error

# Getting top 5 recommendation of movie

results_pop.df <- evaluate(data_split, method="POPULAR", type="topNList", n=c(5, 10))


user_recs.df = predict(rec_pop, test_X, n=5, type="topNList")

# print top 5 recommendation of movie based on user id .

user_recs@items[data$user_id]

movie_id <- user_recs@items[]
username <- user_recs@itemLabels[]

out2 <- cbind(username[data$user_id],movie_id)
write.csv(out2,"C:/Documents/pop.csv")
 

# 3------>  Integrating an Item-Item Similarity Recommender



# Create model
rec_ibcf = Recommender(train, "IBCF")
predicted_ibcf = predict(rec_ibcf, test_X, type="ratings")
# Now, we compute the RMSE
error_ibcf = calcPredictionAccuracy(predicted_ibcf, test_Y)
print(error_ibcf)

results_ibcf <- evaluate(data_split, method="IBCF", type="topNList", n=c(5, 10))


user_recs.df = predict(rec_ibcf, test_X, n=5, type="topNList")


# print top 5 recommendation of movie based on user id .

user_recs@items[data$user_id]

movie_id <- user_recs@items[]
username <- user_recs@itemLabels[]

out3 <- cbind(username[data$user_id],movie_id)
write.csv(out3,"C:/Documents/item.csv")
user_recs@items[6]
