## EACH file is single observation

## 

source("functions/load-movies.R")
source("functions/process-movies.R")
source("functions/load-olympics.R")

df_ol <- load_olympics(50)
df_movies <- load_movies(50)
## SQL 

readLines("data/movies/ratings_small.csv", 5)

## Row binds ----------
set.seed(2000)
df_example <- data.frame(id = 1:99, gender = rep(c("F", "M", "O"), 33),
                         test_result = round(rnorm(99, 100, 20)))

df_example2 <- data.frame(id = 100:198, gender = rep(c("F", "M", "O"), 33),
                          test_result = round(rnorm(99, 100, 20)))

df_combined <- rbind(df_example, df_example2)

df_example2 <- data.frame(gender = rep(c("F", "M", "O"), 33),
                          test_result = round(rnorm(99, 100, 20)), 
                          id = 100:198)

df_combined <- rbind(df_example, df_example2)

## Not mathcng names
df_example2 <- data.frame(gender = rep(c("F", "M", "O"), 33),
                          test_result_new = round(rnorm(99, 100, 20)), 
                          id = 100:198)

df_combined <- rbind(df_example, df_example2)

# rename
df_example2 <- select(df_example2, id, test_result = test_result_new,
                      gender)
rbind(df_example, df_example2)

## Not matching types
df_example2 <- data.frame(gender = rep(c("F", "M", "O"), 33),
                          test_result = round(rnorm(99, 100, 20)), 
                          id = as.character(100:198))
str(rbind(df_example, df_example2))
str(df_example)

## cbind -------
set.seed(2000)
df_example <- data.frame(id = 1:99, gender = rep(c("F", "M", "O"), 33),
                         test_result = round(rnorm(99, 100, 20)))

df_example2 <- data.frame(test_result2 = round(rnorm(99, 100, 20)),
                         blood_type = sample(c("A", "B"), 99, replace=TRUE))

head(cbind(df_example, df_example2))

df_example_error <- data.frame(test_result2 = round(rnorm(95, 100, 20)),
                          blood_type = sample(c("A", "B"), 95, replace=TRUE))
head(cbind(df_example, df_example_error))

## MERGE ------
set.seed(2000)
df_example <- data.frame(id = 1:99, gender = rep(c("F", "M", "O"), 33),
                         test_result = round(rnorm(99, 100, 20)))

df_example2 <- data.frame(test_result2 = round(rnorm(99, 100, 20)),
                          blood_type = sample(c("A", "B"), 99, replace=TRUE))

str(merge(df_example, df_example2))

df_example2 <- data.frame(test_result2 = round(rnorm(99, 100, 20)),
                          blood_type = sample(c("A", "B"), 99, replace=TRUE),
                          id = sample(1:99, 99))
# You can but don't
str(merge(df_example, df_example2))
intersect(names(df_example), names(df_example2))

# ALWAYS USE BY=
str(merge(df_example, df_example2, by="id"))

df_example_two_ids <- data.frame(test_result2 = round(rnorm(99, 100, 20)),
                          blood_type = sample(c("A", "B"), 99, replace=TRUE),
                          id = sample(1:10, 99, replace = TRUE),
                          participant_id = 1:99)

str(merge(df_example, df_example_two_ids, by.x = "id", 
          by.y = "participant_id"))

View(merge(df_example, df_example_two_ids, by.x = "id", 
          by.y = "participant_id"))


# load the ratings
# load the metadata
load_movies()


# only merge in titles to reviews ()
# JOINS


# left_join
# inner_join
# full_join

