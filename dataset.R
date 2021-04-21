##########################################################
# Load Packages
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(knitr)
library(lubridate)
library(rmarkdown)

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################


# Note: this process could take a couple of minutes

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
rm(test_index, edx)

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Separate and make sets lighter for RAM saving
train_set <- train_set %>%
  select(userId, movieId, rating, genres, timestamp)
test_set <- test_set %>%
  select(userId, movieId, rating, genres, timestamp)

save.image("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/movielens.RData")
rm(train_set, test_set)
save.image("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/validation.RData")

##########################################################
# Computing effects
##########################################################

load("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/movielens.RData")
rm(validation)

# New date variable, First step: finding the best grouping by looking at graphs
#train_set %>%
#  mutate(daily = as.numeric(round_date(as_datetime(timestamp), "day"))) %>%
#  group_by(daily) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(daily, rating)) +
#  geom_point() +
#  geom_smooth()

train_set %>%
  mutate(weekly = as.numeric(round_date(as_datetime(timestamp), "week"))) %>%
  group_by(weekly) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(weekly, rating)) +
  geom_point() +
  geom_smooth()

#train_set %>%
#  mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month"))) %>%
#  group_by(monthly) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(monthly, rating)) +
#  geom_point() +
#  geom_smooth()

# New date variable, Second step: creating the new date varaible in both datasets

#test_set <- test_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))
#train_set <- train_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))

# Remove potential dates that are only in one of the two sets
#test_set <- test_set[test_set$monthly %in% train_set$monthly]

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
#monthly <-  train_set %>% 
#              left_join(avg_usr, by='userId') %>%
#              left_join(avg_mov, by='movieId') %>%
#              group_by(monthly) %>%
#              summarize(bd = mean(rating - avg - bi - bu)) %>%
#              loess(formula = bd ~ monthly) %>%
#              .$x
#monthly <- c(monthly)
#
#monthly_beta <- train_set %>% 
#              left_join(avg_usr, by='userId') %>%
#              left_join(avg_mov, by='movieId') %>%
#              group_by(monthly) %>%
#              summarize(bd = mean(rating - avg - bi - bu)) %>%
#              loess(formula = bd ~ monthly) %>%
#              .$fitted

#date_effect <- tibble(monthly, monthly_beta)
#date_effect <- rename(date_effect, bd = monthly_beta)                 
  
# Movie, User & Date - RMSE
#date_usr_mov_pred <- test_set %>%
#  left_join(avg_usr, by='userId') %>%
#  left_join(avg_mov, by='movieId') %>%
#  left_join(date_effect, by='monthly', copy=TRUE) %>%
#  mutate(pred = avg + bi + bu + bd) %>%
#  .$pred

#date_usr_mov_rmse <- RMSE(date_usr_mov_pred, test_set$rating)

# Date effect being unsignificant, delete the variable for RAM saving
train_set <- train_set %>%
  select(-timestamp)
test_set <- test_set %>%
  select(-timestamp)

# Genre effect (adjusted for Movie and User - Date effect dropped)
avg_genre <- train_set %>% 
  separate_rows(genres, sep = "\\|") %>% 
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  group_by(genres) %>%
  summarize(avg_rating_genre = mean(rating  - avg - bi - bu))

genre_effect <- train_set %>%
  mutate(id=row_number()) %>%
  separate_rows(genres, sep = "\\|") %>%
  left_join(x=.,y=avg_genre, by='genres') %>%
  select(-genres) %>%
  group_by(id) %>%
  mutate(bg = sum(avg_rating_genre)) 

# Different cleaning and memory saving operations 

genre_effect <- genre_effect %>% select(id, bg)
genre_effect <- as.data.frame(genre_effect)
genre_effect <- unique(genre_effect, by='id')

train_set <- train_set %>%
  mutate(id=row_number()) %>%
  left_join(y = genre_effect, by='id')

genre_effect <- train_set %>%
  select(movieId, bg)

genre_effect <- unique(genre_effect, by='movieId')
genre_effect <- as.data.frame(genre_effect)

train_set <- as.data.frame(train_set)
train_set <- train_set %>% 
  select(-id, -bg)

test_set <- test_set %>%
 select(userId, movieId, rating)

# Genre User Movie - RMSE 
genre_usr_mov_pred <- test_set %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  left_join(genre_effect, by='movieId') %>%
  mutate(pred = avg + bi + bu + bg)
genre_usr_mov_pred <- genre_usr_mov_pred %>%
  select(pred)
genre_usr_mov_rmse <- RMSE(genre_usr_mov_pred$pred, test_set$rating)


##########################################################
# Regularization with Lambda method
##########################################################


# Regularization - Lambda method - First try on movie effect model
lambdaz <- seq(0,40, 0.25)

avg2_mov <- train_set %>%
  group_by(movieId) %>%
  summarize(bi_sum = sum(rating - avg), n_i=n())

mov_rmse_reg <- sapply(lambdaz, function(y){
  mov_pred2 <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    mutate(bi_reg = bi_sum/(n_i+y)) %>%
    mutate(pred = avg + bi_reg) %>%
    .$pred
  return(RMSE(mov_pred2, test_set$rating))
})

qplot(lambdaz, mov_rmse_reg)
lambdaz[which.min(mov_rmse_reg)]

# Best lambda = 1.75

avg2_mov <- train_set %>%
  group_by(movieId) %>%
  summarize(bi_sum = sum(rating - avg), n_i=n())

mov_rmse_final <- sapply(1.75, function(y){
  mov_pred2 <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    mutate(bi_reg = bi_sum/(n_i+y)) %>%
    mutate(pred = avg + bi_reg) %>%
    .$pred
  return(RMSE(mov_pred2, test_set$rating))
})


# User and Movie regularized model - without Genre
lambdaz2 <- seq(0,20,0.25)
usr_mov_rmse_reg <- sapply(lambdaz2, function(y){
  
  avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  
  avg2_mov <- avg2_mov %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  
  avg2_usr <- avg2_usr %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  usr_mov_pred_reg <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    mutate(pred = avg + bi_final + bu_final) %>%
    .$pred
  RMSE(usr_mov_pred_reg, test_set$rating)
})
qplot(lambdaz2, usr_mov_rmse_reg)
lambdaz2[which.min(usr_mov_rmse_reg)]

# RMSE - Best User & Movie regularized model (without genre) - Lambda = 4.75
lambda2 <- 4.75
usr_mov_rmse_final <- sapply(lambda2, function(y){
  
  avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  
  avg2_mov <- avg2_mov %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  
  avg2_usr <- avg2_usr %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  usr_mov_pred_final <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    mutate(pred = avg + bi_final + bu_final) %>%
    .$pred
  RMSE(usr_mov_pred_final, test_set$rating)
})

# Regularized User, Movie and Genre model
# We will use the standard genre effect - see report to understand why
lambdaz <- seq(0,40,0.25)
genre_usr_mov_rmse2 <- sapply(lambdaz, function(y){

   avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
   avg2_mov <- avg2_mov %>% 
     select(movieId, bi_final) %>% 
     group_by(movieId) %>% 
     unique(by='movieId') %>% 
     as.data.frame()
   
   avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
   avg2_usr <- avg2_usr %>% 
     select(userId, bu_final) %>% 
     group_by(userId) %>% 
     unique(by='userId') %>%
     as.data.frame()
   
   genre_usr_mov_pred2 <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    left_join(genre_effect, by='movieId') %>%
    mutate(pred = avg + bi_final + bu_final + bg) %>%
    .$pred
  return(RMSE(genre_usr_mov_pred2, test_set$rating))
})

qplot(lambdaz, genre_usr_mov_rmse2)
lambdaz[which.min(genre_usr_mov_rmse2)]

#Best lambda = 4.5 - Returning corresponding RMSE
lambda_final <- 4.5
genre_usr_mov_rmse_final <- sapply(lambda_final, function(y){
  
  avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg2_mov <- avg2_mov %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg2_usr <- avg2_usr %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  genre_usr_mov_pred_final <- test_set %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    left_join(genre_effect, by='movieId') %>%
    mutate(pred = avg + bi_final + bu_final + bg) %>%
    .$pred
RMSE(genre_usr_mov_pred_final, test_set$rating)
})


##########################################################
# RMSE results table
##########################################################

# RMSE results
rmse_results <- tibble(method = "Basic average model", RMSE = avg_rmse)
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie effect model", RMSE= mov_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie regularized model", RMSE= mov_rmse_final))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User model", RMSE= usr_mov_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User regularized model", RMSE= usr_mov_rmse_final))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User + Genre  model", RMSE= genre_usr_mov_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User + Genre regularized model", RMSE= genre_usr_mov_rmse_final))
rmse_results %>% knitr::kable()



##########################################################
# Validation step
##########################################################

load("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/validation.RData")
validation <- validation %>% 
  select(userId, movieId, rating) %>% 
  as.data.frame()

# Make sure every movie and users are present in train and validation datasets so we don't endup with NA
validation <- validation %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


lambda_final <- seq(0,10,0.25)
Validation_RMSE <- sapply(lambda_final, function(y){
  
  avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg2_mov <- avg2_mov %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg2_usr <- avg2_usr %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  validation_pred <- validation %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    left_join(genre_effect, by='movieId') %>%
    mutate(pred = avg + bi_final + bu_final + bg) %>%
    .$pred
  RMSE(validation_pred, validation$rating)
})

qplot(lambda_final, Validation_RMSE)
lambda_final[which.min(Validation_RMSE)]

# Best lambda = 5 - Final RMSE
best_lambda <- 5
Validation_RMSE <- sapply(best_lambda, function(y){
  
  avg2_mov <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg2_mov <- avg2_mov %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg2_usr <- train_set %>%
    left_join(avg2_mov, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg2_usr <- avg2_usr %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  validation_pred <- validation %>%
    left_join(avg2_mov, by='movieId') %>%
    left_join(avg2_usr, by='userId') %>%
    left_join(genre_effect, by='movieId') %>%
    mutate(pred = avg + bi_final + bu_final + bg) %>%
    .$pred
  RMSE(validation_pred, validation$rating)
})