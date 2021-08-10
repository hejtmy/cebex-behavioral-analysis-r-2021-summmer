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
df_big5 <- read.table("data/big5/big5-data.csv", 
                      nrows = 100, stringsAsFactors = FALSE, header = TRUE)


head(select(df_big5, starts_with("E"), -engnat))
# Select extraversion question but not engnat
?select_helpers

# first three questions from all personality traits
head(select(df_big5, num_range("E", 1:3),
            num_range("A", 1:3)))

# only last question from each
head(select(df_big5, ends_with("10")))


# %in%
head(select(df_big5, one_of("E10", "A1")))

variables_of_interest <- c("E10", "A1")
head(select(df_big5, one_of(variables_of_interest)))
rm(df_big5)
## Better filtering -----
df_movies[df_movies$budget > 50000000,]
df_movies[df_movies$budget > 50000000 & 
            df_movies$vote_average > 5 &
            df_movies$adult == "True",]

filter(df_movies, budget > 50000000)
filter(df_movies, budget > 50000000, vote_average > 6)
filter(df_movies, budget > 50000000 | budget == 0)

## select only those movies which scored above 8 on vote average
filter(df_movies, vote_average > 8)
## select only adult muvies which costed above 30 000 000
filter(df_movies, budget > 30000000, adult == "True")
## select at least 2 hour long movies which costed less then 10 000 000
filter(df_movies, budget < 10000000, runtime > 120)

## sample n
set.seed(1)
df_movies[1:15,]
sample(1:50, 15)
df_movies[sample(1:50, 15),] #traditional way of doing things

df_movies_random <- sample_n(df_movies, 15)

## Mutations (data.frame changes)
df_movies[, "budget_mil"] <- df_movies[, "budget"]/10^6
df_movies[, "adult_log"] <- df_movies[, "adult"] == "True"
head(df_movies[, c("adult_log", "adult")])
filter(df_movies, !adult_log)

df_movies <- mutate(df_movies,
       budget_mil = budget / 10^6,
       adult_log = adult == "True")

## change the revenue to be also in milios of dollars
df_movies <- mutate(df_movies,
                    budget = budget / 10 ^ 6,
                    revenue = revenue / 10^6,
              ## create hours_long which is runtime in whole hours (119m) -> 1, 120 -> 2
                    hours_long = floor(runtime/60)
                    )

## 

## PIPE
df_movies %>%
  filter(budget > 60) %>%
  select(original_title, vote_average)

df_movies %>%
  select(original_title, vote_average, budget) %>%
  filter(budget > 60) %>%
  select(-budget)


## Arrange
df_movies %>%
  filter(budget > 40) %>%
  select(original_title, vote_average, budget) %>%
  arrange(-budget)

df_movies %>%
  filter(budget > 40) %>%
  select(original_title, vote_average, budget) %>%
  arrange(budget, -vote_average)


## summaries
df_movies %>%
  summarise(avg_budget = mean(budget),
            avg_revenue = mean(revenue, na.rm = TRUE),
            n_budget_error = sum(budget == 0))


## grouping / aggregations
df_movies <- load_movies(5000)

df_movies %>%
  group_by(original_language) %>%
  summarise(avg_budget = mean(budget),
            avg_revenue = mean(revenue, na.rm = TRUE),
            n_budget_error = sum(budget == 0))

df_movies %>%
  mutate(budget = budget/10^6) %>%
  group_by(budget > 70) %>%
  summarise(avg_vote = mean(vote_average, na.rm = TRUE),
            avg_revenue = mean(revenue, na.rm = TRUE))

