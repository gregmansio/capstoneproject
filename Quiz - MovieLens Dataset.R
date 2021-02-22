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
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

# Question 1
nrow(edx)
ncol(edx)
dim(edx)

# Question 2
edx %>% group_by(rating) %>% count
edx %>% filter(rating == 0) %>% tally()
edx %>% filter(rating == 3) %>% tally()

# Question 3
edx %>% group_by(movieId) %>% count
n_distinct(edx$movieId)

# Question 4
n_distinct(edx$userId)

# Question 5
library(stringr)
#str_detect(edx$genres, pattern = "Drama") %>% sum()
#str_detect(edx$genres, pattern = "Comedy") %>% sum()
#str_detect(edx$genres, pattern = "Thriller") %>% sum()
#str_detect(edx$genres, pattern = "Romance") %>% sum()

# Q5 str_detect - methode 2 
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# Q5 separate_rows - methode 3
#edx %>% separate_rows(genres, sep = "\\|") %>%
#  group_by(genres) %>%
#  summarize(count = n()) %>%
#  arrange(desc(count))

# Question 6
#edx %>% filter(str_detect(edx$title, pattern = "Forrest Gump")) %>% nrow()
#edx %>% filter(str_detect(edx$title, pattern = "Jurassic Park$")) %>% nrow()
#edx %>% filter(str_detect(edx$title, pattern = "Pulp Fiction")) %>% nrow()
#edx %>% filter(str_detect(edx$title, pattern = "The Shawshank Redemption")) %>% nrow()
#edx %>% filter(str_detect(edx$title, pattern = "Speed 2: Cruise Control")) %>% nrow()

# Deuxieme solution
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Question 7
edx %>% group_by(rating) %>% summarize(count = n()) %>% arrange(desc(count))

# Summary statistics for the report
mean(edx$rating)
