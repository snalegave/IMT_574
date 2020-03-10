install.packages("jsonlite")
install.packages("tidyjson")

library(tidyr)
library(jsonlite)
library(dplyr)


ted <- read.csv("IMT_574/Data/ted_main.csv", stringsAsFactors = FALSE)

ted_ratings <- ted[,c("url","ratings")]
json <- ted_ratings$ratings[1]
tmp <- fromJSON(gsub("'", '"', json))[-1]
tmp <- tmp[order(tmp$name),] 
ratings <- tmp$name
ratings <- ratings[order(ratings)]

ratings_df <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(ratings_df) <- c(ratings, "url")


for (i in 1:nrow(ted)) {
  row_ratings <- ted[i, "ratings"]
  row_url <- ted[i, "url"]
  ratings_json <-  fromJSON(gsub("'", '"', row_ratings))[-1]
  ratings_json <- ratings_json[order(ratings_json$name),]
  
  ratings_df[nrow(ratings_df)+1,] <- c(ratings_json$count, row_url)
}
write.csv(ratings_df,"IMT_574/Data/ratings_expanded.csv", row.names = FALSE)


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
