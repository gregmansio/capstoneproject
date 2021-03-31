##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(lubridate)
library(rmarkdown)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Split edx in train and test sets
test_index <- createDataPartition(y = edx$rating, p = 0.2, times = 1, list = FALSE)
train_set <- edx[-test_index]
test_set <- edx[test_index]
rm(test_index)

# Remove movies that are only in one of the two sets

test_set <- test_set[test_set$movieId %in% train_set$movieId]

# Average rating
avg <- mean(train_set$rating)

# Basic - RMSE
avg_rmse <- RMSE(test_set$rating, avg)

# Average rating by movie
avg_mov <- train_set %>% group_by(movieId) %>% summarize(bi = mean(rating - avg))

# Movie - RMSE
mov_pred <- avg + test_set %>%
  left_join(avg_mov, by='movieId') %>%
  pull(bi)
mov_rmse <- RMSE(mov_pred, test_set$rating)

# Average rating by user
avg_usr <- train_set %>% group_by(userId) %>% summarize(bu = mean(rating - avg))

# Date loess smoothing
Smooth_date_day <- train_set %>% mutate(dateday = as.numeric(round_date(as_datetime(timestamp), "day"))) %>%
                    group_by(dateday) %>%
                    summarize(avg_rating_day = mean(rating)) %>%
                    loess(formula = avg_rating_day ~ dateday) %>%
                    .$fitted
avg_day = Smooth_date_day - avg - 

# Average rating by genre
# Read genre then spread

genre_effect <- train_set %>% 
  separate_rows(genres, sep = "\\|") %>% 
  group_by(genres) %>%
  summarize(avg_rate_genre = mean(rating) - avg - avg_mov$bi - avg_usr$bu)

# RMSE results
#
#
#
#

#usr_pred <- avg + test_set %>%
#  left_join(avg_usr, by='userId') %>%
# pull(bu
#usr_rmse <- RMSE(usr_pred, test_set$rating)#

#usr_mov_pred <- test_set %>%
#  left_join(avg_mov, by='movieId') %>%
#  left_join(avg_usr, by='userId') %>%
#  mutate(pred= avg + bi + bu) %>%
#  pull(pred)
#usr_mov_rmse <- RMSE(usr_mov_pred, test_set$rating)


#rmse_results <- tibble(method = "Basic average", RMSE = avg)
#rmse_results <- 