library(dplyr)

source("functions/load-movies.R")
df_movies <- load_movies(50)

glimpse(df_movies)
str(df_movies)

## Recap
df_movies[1, ]
USArrests[1,]
USArrests["Alabama", ]

df_movies$adult
df_movies[,"adult"]
df_movies[,c("adult", "revenue", "runtime")]
df_movies[["adult"]]

colnames(df_movies)
head(df_movies[, c(1, 5, 9)])
head(df_movies[, c(1, 1, 1)])

head(df_movies[, 1:5])
## Better selections
head(select(df_movies, adult))
head(select(df_movies, adult, revenue, runtime))

head(select(df_movies, 1, adult, adult))
head(select(df_movies, Adult=adult))

head(select(df_movies, adult:original_title))
head(select(df_movies, 1:5))

head(select(df_movies, -c(overview, production_companies, belongs_to_collection)))

## gplimpse the dataset
glimpse(df_movies)
## using dplyr's select function select only numeric columns
head(select(df_movies, id, vote_count, vote_average, runtime, revenue, popularity,
            budget))
head(select(df_movies, -c(id, vote_count, vote_average, runtime, revenue, popularity,
            budget)))
## select everything exept first 10 columns
head(select(df_movies, -c(1:10)))

## Load the dataset from big5 - big5-data.csv

# readLines - check parameters
readLines("data/big5/big5-data.csv", 3)
# load
df_big5 <- read.table("data/big5/big5-data.csv", nrows = 100, stringsAsFactors = FALSE,
                      header = TRUE)


head(select(df_big5, starts_with("E"), -engnat))
# Select extraversion question but not engnat
?select_helpers

# first three questions from all personality traits

# only last question from each


## Better filtering

## Mutations (data.frame changes)

## grouping / aggregations

## summaries