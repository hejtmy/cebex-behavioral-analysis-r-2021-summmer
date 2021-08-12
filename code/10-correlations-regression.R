library(correlation)
library(corrplot)
library(ggplot2)
library(dplyr)
library(performance)

source("data/big5/load-big5.R")

M = cor(select(df_big5, starts_with("E", ignore.case = FALSE)))
corrplot(M, method = 'number') # colorful number

## Regression ------
source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)
df_olympics <- sample_n(df_olympics, 5000)

## Check height and weight points and smooth

## Check histogram of both values

## color by potential problematic values (gender, year, season)

## check qqplots for height nad width
# geom_qq() + stat_qq_line()

# see separation by season
# see separation by gender

# facet wrap around sports in Summer

# Filter and run the lm model

# predict(lm_height_weight, newdata = data.frame(Height=c(180)))

## Load the movies dataset ----

source("functions/load-movies.R")
source("functions/process-movies.R")
df_movies <- load_movies(50000)
df_movies <- process_movies(df_movies)
df_movies <- sample_n(df_movies, 5000)

## Investigate if budget has any impact on vote counts
ggplot(df_movies, aes(budget, vote_count)) +
  geom_point() +
  geom_smooth(method = "lm")

## Check the qqplots
df_movies %>%
  ggplot(aes(sample=vote_count)) + 
    geom_qq() + stat_qq_line()

## Check the histograms
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.9, na.rm = TRUE)) %>%
  ggplot(aes(vote_count)) + geom_histogram()
# filter error vote counts and 
# filter out bottom and top 5-10 perecent 
# check qqplots again
## Check the qqplots
df_movies %>%
  ggplot(aes(sample=vote_count)) +
    geom_qq() + stat_qq_line()

lm_visits_budget <- lm(vote_count ~ budget, df_movies)
lm_visits_budget <- lm(vote_count ~ budget, data=df_movies)
summary(lm_visits_budget)
performance(lm_visits_budget)

# check_model(lm_visits_budget)

## Log transformations ------
# filter error values

## Plot data

# filter 0 values because of log
# log transform the data

# scale_x_log10 and scale_y_log10

## Transform the data

# run the model again
lm_visits_budget_log <- df_movies %>%
   filter(vote_count > 0,
         budget >0) %>%
  with(lm(log10(vote_count) ~ log10(budget), data=.))
summary(lm_visits_budget_log)
performance(lm_visits_budget)

# compare the models
compare_performance(lm_visits_budget, lm_visits_budget_log)

## FACEBOOK DATA --------
## load facebook data
df_facebook <- read.csv("data/facebook.csv")
df_facebook_plt <- sample_n(df_facebook, 2000)

## check how likes relate to likes received

## do some filtering and transformations

#df_facebook <- df_facebook %>%
#  filter(likes > 10, likes_received > 10) %>%
#  mutate(log_likes = log10(likes),
#         log_likes_received = log10(likes_received))

#lm_log_likes_received <- lm(log_likes_received ~ log_likes, data=df_facebook)
#summary(lm_log_likes_received)

#lm_log_likes_received <- lm(log_likes_received ~ log_likes, data=df_facebook)
#lm_likes_received <- lm(likes_received ~ likes, data=df_facebook)

#compare_performance(lm_log_likes_received, lm_likes_received)
## Transforming the coefficients back
#https://medium.com/@kyawsawhtoon/log-transformation-purpose-and-interpretation-9444b4b049c9
#https://kenbenoit.net/assets/courses/ME104/logmodels2.pdf
# 10^lm_likes_received$coefficients[1] # if no likes are given, person gets this amount's of likes by default

# 1.1^lm_likes_received$coefficients[2] # for every 10% of increased likes, user gains about 7 percent likes back
predict(lm_likes_received)
