install.packages("jsonlite")
install.packages("tidyjson")


library(tidyr)
library(jsonlite)
library(dplyr)
library(randomForest)
library(MASS)
library(neuralnet)


ted <- read.csv("IMT_574/Data/ted_main.csv", stringsAsFactors = FALSE)

ted_ratings <- ted[,c("url","ratings")]

for (i in 1:nrow(ted_ratings)) {
  row_ratings <- ted[i, "ratings"]
  row_url <- ted[i, "url"]
  ratings_json <-  fromJSON(gsub("'", '"', row_ratings))[-1]
  total_ratings <- sum(ratings_json$count)
  ted_ratings[i,"total_ratings"] <- total_ratings
  for (rating_index in 1:nrow(ratings_json)){
    rating_name<-ratings_json[rating_index,1]
    rating_count <- ratings_json[rating_index,2]/total_ratings
    ted_ratings[i,rating_name] <- rating_count
  }
}
ted_ratings <- ted_ratings[,-c(2)] # remove original ratings column 
write.csv(ted_ratings,"IMT_574/Data/ratings_expanded.csv", row.names = FALSE)


# Cleaning up the Tags Column
ted_tags_clean <- ted[,c("url", "tags")]
ted_tags_clean$tags <- gsub("Alzheimer's","Alzheimer", ted_tags_clean$tags)

for (i in 1:nrow(ted_tags_clean)) {
  row_i_tags_list <- fromJSON(gsub("'", '"', ted_tags_clean$tags[i]))
  for (tag in row_i_tags_list){
    ted_tags_clean[i,tag] <- 1
  }
}
ted_tags_clean[is.na(ted_tags_clean)] <-  0
ted_tags_clean <- ted_tags_clean[,-c(2)] # remove the original tags column 
ted_tags_clean[2:417] <- lapply(ted_tags_clean[2:417] , factor) # convert from numeric to factor
write.csv(ted_tags_clean,"IMT_574/Data/tags_expanded.csv", row.names = FALSE)

tags_DF[is.na(tags_DF)] <-  0
sum((tags_DF$children))

ted_discussions <- ted[,c("url","views", "comments")]
ted_discussions["comment_ratio"] <-  ted_discussions$comments/ted_discussions$views
ted_discussions<- ted_discussions[,c("url", "comment_ratio")]
joined1 <- inner_join(ted_discussions, ted_ratings,"url")
joined2 <- inner_join(ted_discussions, ted_tags_clean,"url")

# Scaling the new dataset that has engagement data along with ratio of ratings
joined1 <- joined1 %>% rename(Jaw_dropping=`Jaw-dropping`)
joined1<- joined1[-1]
max = apply(joined1, 2, max)
min = apply(joined1, 2, min)
scaled = as.data.frame(scale(joined1, center=min, scale=max-min))
scaled_tanh = 
index = sample(1:nrow(joined1),round(0.75*nrow(joined1)))
training = scaled[index,]
testing = scaled[-index,]


decision_training <- training[,"comment_ratio"]
predictors_training <- training[, -c(1)]
rf_m <- randomForest(predictors_training, decision_training)
rf_m
rf_pred <- predict(rf_m, newdata = testing[-1])
rf_RMSE <- sqrt(sum((rf_pred - testing$comment_ratio)^2)/length(rf_pred))
plot(rf_pred~testing$comment_ratio, main = "Random Forest Predictions")


nn = neuralnet(comment_ratio ~ .,data=training, act.fct = "logistic")
plot(nn, rep ='best')
nn_pred <- compute(nn, testing)$net.result
nn_RMSE <- sqrt(sum((nn_pred - testing$comment_ratio)^2)/length(nn_pred))
plot(nn_pred~testing$comment_ratio, main = "Neural Network Predictions")

lm_m <- lm(log(comment_ratio)~., data=training)
summary(lm_m)
tmp <- stepAIC(lm_m, direction = "backward")
lm_pred <- predict(lm_m, newdata = testing)
lm_RMSE <- sqrt(sum((lm_pred - testing$comment_ratio)^2)/length(lm_pred))
plot(lm_pred~testing$comment_ratio, main = "Linear Regression Predictions")

write.csv(joined2,"IMT_574/Data/ted_discussion.csv", row.names = FALSE)
