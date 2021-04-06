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

# New date variable, First step: finding the best grouping by looking at graphs
train_set %>%
  mutate(daily = as.numeric(round_date(as_datetime(timestamp), "day"))) %>%
  group_by(daily) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(daily, rating)) +
  geom_point() +
  geom_smooth()

train_set %>%
  mutate(weekly = as.numeric(round_date(as_datetime(timestamp), "week"))) %>%
  group_by(weekly) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(weekly, rating)) +
  geom_point() +
  geom_smooth()

train_set %>%
  mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month"))) %>%
  group_by(monthly) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(monthly, rating)) +
  geom_point() +
  geom_smooth()

# New date variable, Second step: creating the new date varaible in both datasets

test_set <- test_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))
train_set <- train_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))

# Remove movies and dates that are only in one of the two sets
test_set <- test_set[test_set$movieId %in% train_set$movieId]
test_set <- test_set[test_set$monthly %in% train_set$monthly]

# Average rating
avg <- mean(train_set$rating)

# Basic - RMSE
avg_rmse <- RMSE(test_set$rating, avg)

# Movie effect
avg_mov <- train_set %>% 
  group_by(movieId) %>% 
  summarize(bi = mean(rating - avg))

# Movie - RMSE
mov_pred <- avg + test_set %>%
  left_join(avg_mov, by='movieId') %>%
  pull(bi)
mov_rmse <- RMSE(mov_pred, test_set$rating)

# User effect (ajusted for movie effect)
avg_usr <- train_set %>% 
  left_join(avg_mov, by='movieId') %>%
  group_by(userId) %>% 
  summarize(bu = mean(rating - avg - bi))

# Movie & User - RMSE
usr_mov_pred <- test_set %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  mutate(pred = avg + bi + bu) %>%
  .$pred
usr_mov_rmse <- RMSE(usr_mov_pred, test_set$rating)

# Date effect (ajusted for movie and user effects)
monthly <-  train_set %>% 
              left_join(avg_usr, by='userId') %>%
              left_join(avg_mov, by='movieId') %>%
              group_by(monthly) %>%
              summarize(bd = mean(rating - avg - bi - bu)) %>%
              loess(formula = bd ~ monthly) %>%
              .$x
monthly <- c(monthly)

monthly_beta <- train_set %>% 
              left_join(avg_usr, by='userId') %>%
              left_join(avg_mov, by='movieId') %>%
              group_by(monthly) %>%
              summarize(bd = mean(rating - avg - bi - bu)) %>%
              loess(formula = bd ~ monthly) %>%
              .$fitted

date_effect <- tibble(monthly, monthly_beta)
date_effect <- rename(date_effect, bd = monthly_beta)                 
  
# Movie, User & Date - RMSE
date_usr_mov_pred <- test_set %>%
  left_join(avg_usr, by='userId') %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(date_effect, by='monthly', copy=TRUE) %>%
  mutate(pred = avg + bi + bu + bd) %>%
  .$pred

date_usr_mov_rmse <- RMSE(date_usr_mov_pred, test_set$rating)


# Genre effect (adjusted for Movie and User - Date effect dropped)
avg_genre <- train_set %>% 
  separate_rows(genres, sep = "\\|") %>% 
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  group_by(genres) %>%
  summarize(avg_rating_genre = mean(rating  - avg - bi - bu))

#genre_effect <- train_set %>%
#  separate_rows(genres, sep = "\\|") %>%
#  left_join(x=.,y=avg_genre, by='genres') %>%
#  ungroup() %>%
#  group_by(movieId) %>%
#  summarize(bg = mean(avg_rating_genre))

# Genre User Movie - RMSE 
#genre_usr_mov_pred <- test_set %>%
#  left_join(avg_usr, by='userId') %>%
#  left_join(avg_mov, by='movieId') %>%
#  left_join(genre_effect, by='movieId', copy=TRUE) %>%
#  mutate(pred = avg + bi + bu + bg) %>%
# .$pred

#genre_usr_mov_rmse <- RMSE(genre_usr_mov_pred, test_set$rating)

# RMSE results
#rmse_results <- tibble(method = "Basic average", RMSE = avg)
#rmse_results <- 