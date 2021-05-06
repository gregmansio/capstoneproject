##########################################################
# Load Packages
##########################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("data.table", repos = "http://cran.us.r-project.org")


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

# Create 'year of rating' variable
movielens <- movielens %>% 
  mutate(rating_y = year(as_datetime(timestamp)))

# Extract release year from title
movielens <- movielens %>% 
  mutate(release_y = str_extract(string = movielens$title, pattern ="\\(([1-2][0-9][0-9][0-9])\\)"))

movielens <- movielens %>% 
  mutate(release_year = str_extract(string = movielens$release_y, pattern = "\\d{4}"))

movielens$release_year <- as.numeric(movielens$release_year)
  
# New predictor: age of rating
movielens <- movielens %>%
  mutate(age_rating = rating_y - release_year)

# Make set lighter for RAM saving
movielens <- movielens %>%
  select(-rating_y, -release_y, -release_year)


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

# Again, make sure every exploitable observation in train_set as a counterpart in test_set
# This also has a little regularization effect
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = 'age_rating')

# Make sets lighter for RAM saving
train_set <- train_set %>%
  select(userId, movieId, rating, genres, timestamp, age_rating)
test_set <- test_set %>%
  select(userId, movieId, rating, genres, timestamp, age_rating)

save.image("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/movielens.RData")
rm(train_set, test_set)
save.image("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/validation.RData")


##########################################################
# Computing effects
##########################################################
gc()
load("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/movielens.RData")
rm(validation)

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

# Pure date effect being unsignificant, delete timestamp variable for RAM saving
train_set <- train_set %>%
  select(-timestamp)
test_set <- test_set %>%
  select(-timestamp)

# Effect of Age of movie when rated
train_set <- train_set %>% filter(age_rating >=0) #filtering out movies that have been rated before their release
age_grp <- train_set %>%
  group_by(age_rating) %>%
  summarize(age_avg = mean(rating))

age_plot <- ggplot(data = age_grp, aes(age_rating, age_avg)) +
            geom_point() +
            geom_line(aes(y = avg), lty=2, col = 'blue') +
            geom_text(aes(x = 60, y = 3.55), label = "Avg rating", size = 3) + 
            xlab("Age of movie when rated") +
            ylab("Rating") +
            ggtitle("Movie rating against age when rated")
print(age_plot)

# Effect becomes questionable after 50 years. Filtering out massive age gaps between movie release and rating
age_grp <- age_grp %>%
  filter(age_rating <= 60)

# Age effect adjusted for User and Movie
avg_age <- train_set %>% 
  left_join(age_grp, by='age_rating') %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  group_by(age_rating) %>% 
  summarize(ba = mean(rating - avg - bi - bu))

avg_age <- avg_age %>% filter(age_rating >= 0)
test_set <- test_set %>% filter(age_rating >=0) #same operation than in the train_set
gc()
  
# Movie, User and Age RMSE
age_pred <- test_set %>%
  left_join(avg_age, by='age_rating') %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  mutate(pred = avg + ba + bu + bi) %>%
  .$pred

age_rmse <- RMSE(age_pred, test_set$rating)


# Genre effect (adjusted for Movie, User and Age when rated) - take a while to run and need 6+ Go RAM
avg_genre <- train_set %>% 
  separate_rows(genres, sep = "\\|") %>% 
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  left_join(avg_age, by='age_rating') %>%
  group_by(genres) %>%
  summarize(avg_rating_genre = mean(rating  - avg - bi - bu - ba))

genre_effect <- train_set %>%
  mutate(id=row_number()) %>%
  separate_rows(genres, sep = "\\|") %>%
  left_join(x=.,y=avg_genre, by='genres') %>%
  select(-genres) %>%
  group_by(id) %>%
  mutate(bg = sum(avg_rating_genre)) 

# Different cleaning, reordering and memory saving operations 
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


# Genre User Movie Age - RMSE 
genre_usr_mov_age_pred <- test_set %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  left_join(genre_effect, by='movieId') %>%
  left_join(avg_age, by='age_rating') %>%
  mutate(pred = avg + bi + bu + bg + ba)
genre_usr_mov_age_pred <- genre_usr_mov_age_pred %>%
  select(pred)
genre_usr_mov_age_rmse <- RMSE(genre_usr_mov_age_pred$pred, test_set$rating)

gc()

# Other Genre effect computation - mixtures
avg_genre_mix <- train_set %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  left_join(avg_age, by='age_rating') %>%
  group_by(genres) %>%
  summarize(bg_mix = mean(rating - avg - bi - bu - ba))

# Genre_mix, Movie, User and Age - RMSE
avg_mix_pred <- test_set %>%
  left_join(avg_mov, by='movieId') %>%
  left_join(avg_usr, by='userId') %>%
  left_join(avg_age, by='age_rating') %>%
  left_join(avg_genre_mix, by='genres') %>%
  mutate(pred = avg + bi + bu + ba + bg_mix) %>%
  .$pred

avg_mix_rmse <- RMSE(avg_mix_pred, test_set$rating)

gc()

##########################################################
# Regularization with Lambda method
##########################################################

# Regularization - Lambda method - First try on movie effect model
lambda <- seq(0,10, 0.25)

avg_mov_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(bi_sum = sum(rating - avg), n_i=n())

mov_rmse_reg <- sapply(lambda, function(y){
  mov_pred_reg <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    mutate(bi_reg = bi_sum/(n_i+y)) %>%
    mutate(pred = avg + bi_reg) %>%
    .$pred
  return(RMSE(mov_pred_reg, test_set$rating))
})

qplot(lambda, mov_rmse_reg)
lambda[which.min(mov_rmse_reg)]

# Best lambda = 1.75

avg_mov_reg <- train_set %>%
  group_by(movieId) %>%
  summarize(bi_sum = sum(rating - avg), n_i=n())

mov_rmse_final <- sapply(1.75, function(y){
  mov_pred2 <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    mutate(bi_reg = bi_sum/(n_i+y)) %>%
    mutate(pred = avg + bi_reg) %>%
    .$pred
  return(RMSE(mov_pred2, test_set$rating))
})


# User and Movie regularized model - without Genre
lambda <- seq(0,10,0.25)
usr_mov_rmse_reg <- sapply(lambda, function(y){
  
  avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  
  avg_mov_reg <- avg_mov_reg %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  
  avg_usr_reg <- avg_usr_reg %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  usr_mov_pred_reg <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    mutate(pred = avg + bi_final + bu_final) %>%
    .$pred
  RMSE(usr_mov_pred_reg, test_set$rating)
})
qplot(lambda, usr_mov_rmse_reg)
lambda[which.min(usr_mov_rmse_reg)]



# RMSE - Best User & Movie regularized model (without genre) - Lambda = 4.75
usr_mov_rmse_final <- sapply(4.75, function(y){
  
  avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  
  avg_mov_reg <- avg_mov_reg %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  
  avg_usr_reg <- avg_usr_reg %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  usr_mov_pred_final <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    mutate(pred = avg + bi_final + bu_final) %>%
    .$pred
  RMSE(usr_mov_pred_final, test_set$rating)
})

# Regularized User, Movie and Genre model
# We will use the mix_genre effect which returned better RMSE
lambda <- seq(0,10,0.25)
genre_mix_usr_mov_age_rmse2 <- sapply(lambda, function(y){

   avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
   avg_mov_reg <- avg_mov_reg %>% 
     select(movieId, bi_final) %>% 
     group_by(movieId) %>% 
     unique(by='movieId') %>% 
     as.data.frame()
   
   avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
   avg_usr_reg <- avg_usr_reg %>% 
     select(userId, bu_final) %>% 
     group_by(userId) %>% 
     unique(by='userId') %>%
     as.data.frame()
   
   avg_genre_mix_reg <- train_set %>%
     left_join(avg_usr_reg, by='userId') %>%
     left_join(avg_mov_reg, by='movieId') %>%
     group_by(genres) %>%
     mutate(bg_mix_sum = sum(rating - avg - bi_final - bu_final), n_g=n()) %>%
     summarize(bg_final = bg_mix_sum/(n_g+y))
   avg_genre_mix_reg <- avg_genre_mix_reg %>%
     select(genres, bg_final) %>%
     group_by(genres) %>%
     unique(by='genres') %>%
     as.data.frame
     
   
   genre_mix_usr_mov_age_reg_pred <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_genre_mix_reg, by='genres') %>%
    left_join(avg_age, by='age_rating') %>%
    mutate(pred = avg + bi_final + bu_final + bg_final + ba) %>%
    .$pred
  return(RMSE(genre_mix_usr_mov_age_reg_pred, test_set$rating))
})

qplot(lambda, genre_mix_usr_mov_age_rmse2)
lambda[which.min(genre_mix_usr_mov_age_rmse2)]

#Best lambda = 4.75 - Returning corresponding RMSE
genre_mix_usr_mov_age_rmse_final <- sapply(4.75, function(y){
  
  avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg_mov_reg <- avg_mov_reg %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg_usr_reg <- avg_usr_reg %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  avg_genre_mix_reg <- train_set %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(genres) %>%
    mutate(bg_mix_sum = sum(rating - avg - bi_final - bu_final), n_g=n()) %>%
    summarize(bg_final = bg_mix_sum/(n_g+y))
  avg_genre_mix_reg <- avg_genre_mix_reg %>%
    select(genres, bg_final) %>%
    group_by(genres) %>%
    unique(by='genres') %>%
    as.data.frame
  
  genre_mix_usr_mov_pred_age_final <- test_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_genre_mix_reg, by='genres') %>%
    left_join(avg_age, by='age_rating') %>%
    mutate(pred = avg + bi_final + bu_final + bg_final + ba) %>%
    .$pred
RMSE(genre_mix_usr_mov_pred_age_final, test_set$rating)
})



##########################################################
# RMSE results table
##########################################################

# RMSE results
rmse_results <- tibble(method = "Basic average model", RMSE = avg_rmse)
rmse_results <- bind_rows(rmse_results, data_frame(method="M         - Movie effect model", RMSE= mov_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="MU        - Movie + User model", RMSE= usr_mov_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="MUA       - Movie + User + Age  model", RMSE= age_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="MU reg    - Movie + User regularized model", RMSE= usr_mov_rmse_final))
rmse_results <- bind_rows(rmse_results, data_frame(method="MUGA      - Movie + User + Genre + Age model", RMSE= genre_usr_mov_age_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="MUG²A     - Movie + User + Genre_Mix + Age model", RMSE= avg_mix_rmse))
rmse_results <- bind_rows(rmse_results, data_frame(method="MUG²A reg - Movie + User + Genre_Mix + Age regularized model", RMSE= genre_mix_usr_mov_age_rmse_final))

rmse_results %>% knitr::kable()

gc()

##########################################################
# Validation step
##########################################################

load("C:/Users/Gregoire/Desktop/R/EDX formation/Capstone/capstoneproject/validation.RData")
validation <- validation %>% 
  select(userId, movieId, rating, age_rating, genres) %>% 
  as.data.frame()

# Make sure every movie and users are present in train and validation datasets so we don't endup with NA
validation <- validation %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "age_rating")


lambda <- seq(0,10,0.25)
Validation_RMSE <- sapply(lambda, function(y){
  
  
  avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg_mov_reg <- avg_mov_reg %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg_usr_reg <- avg_usr_reg %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  avg_genre_mix_reg <- train_set %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(genres) %>%
    mutate(bg_mix_sum = sum(rating - avg - bi_final - bu_final), n_g=n()) %>%
    summarize(bg_final = bg_mix_sum/(n_g+y))
  avg_genre_mix_reg <- avg_genre_mix_reg %>%
    select(genres, bg_final) %>%
    group_by(genres) %>%
    unique(by='genres') %>%
    as.data.frame
  
 
  validation_pred <- validation %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_genre_mix_reg, by='genres') %>%
    left_join(avg_age, by='age_rating') %>%
    mutate(pred = avg + bi_final + bu_final + bg_final + ba) %>%
    .$pred
  RMSE(validation_pred, validation$rating)
})

qplot(lambda, Validation_RMSE)
lambda[which.min(Validation_RMSE)]

# Best lambda = 5.25 - Final RMSE
best_lambda <- 5.25
Validation_RMSE <- sapply(best_lambda, function(y){
  
  avg_mov_reg <- train_set %>%
    group_by(movieId) %>%
    mutate(bi_sum = sum(rating - avg), n_i=n()) %>%
    summarize(bi_final = bi_sum/(n_i+y))
  avg_mov_reg <- avg_mov_reg %>% 
    select(movieId, bi_final) %>% 
    group_by(movieId) %>% 
    unique(by='movieId') %>% 
    as.data.frame()
  
  avg_usr_reg <- train_set %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(userId) %>%
    mutate(bu_sum = sum(rating - avg - bi_final), n_u=n()) %>%
    summarize(bu_final = bu_sum/(n_u+y))
  avg_usr_reg <- avg_usr_reg %>% 
    select(userId, bu_final) %>% 
    group_by(userId) %>% 
    unique(by='userId') %>%
    as.data.frame()
  
  avg_genre_mix_reg <- train_set %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_mov_reg, by='movieId') %>%
    group_by(genres) %>%
    mutate(bg_mix_sum = sum(rating - avg - bi_final - bu_final), n_g=n()) %>%
    summarize(bg_final = bg_mix_sum/(n_g+y))
  avg_genre_mix_reg <- avg_genre_mix_reg %>%
    select(genres, bg_final) %>%
    group_by(genres) %>%
    unique(by='genres') %>%
    as.data.frame
  
  validation_pred <- validation %>%
    left_join(avg_mov_reg, by='movieId') %>%
    left_join(avg_usr_reg, by='userId') %>%
    left_join(avg_genre_mix_reg, by='genres') %>%
    left_join(avg_age, by='age_rating') %>%
    mutate(pred = avg + bi_final + bu_final + bg_final + ba) %>%
    .$pred
  RMSE(validation_pred, validation$rating)
})

#########################################################
# Abandonned strategies
#########################################################

# New date variable, First step: finding the best grouping by looking at graphs
#train_set %>%
#  mutate(daily = as.numeric(round_date(as_datetime(timestamp), "day"))) %>%
#  group_by(daily) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(daily, rating)) +
#  geom_point() +
#  geom_smooth()

#train_set %>%
#  mutate(weekly = as.numeric(round_date(as_datetime(timestamp), "week"))) %>%
#  group_by(weekly) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(weekly, rating)) +
#  geom_point() +
#  geom_smooth()

#train_set %>%
#  mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month"))) %>%
#  group_by(monthly) %>%
#  summarize(rating = mean(rating)) %>%
#  ggplot(aes(monthly, rating)) +
#  geom_point() +
#  geom_smooth()

# New date variable, Second step: creating the new date variable in both datasets

#test_set <- test_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))
#train_set <- train_set %>% mutate(monthly = as.numeric(round_date(as_datetime(timestamp), "month")))

# Remove potential dates that are only in one of the two sets
#test_set <- test_set[test_set$monthly %in% train_set$monthly]

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
# Effect non-significant: dropped