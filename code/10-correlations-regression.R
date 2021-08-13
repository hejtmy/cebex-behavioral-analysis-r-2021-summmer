library(correlation)
library(corrplot)
library(ggplot2)
library(dplyr)
library(performance)

source("data/big5/load-big5.R")

M = cor(select(df_big5, starts_with("E", ignore.case = FALSE)))
corrplot(M, method = 'number') # colorful number

# Regression ------
## PERFECT WORLD ------
x <- 1:100
y <- x * 1.5 + rnorm(100, sd = 3)

plot(x, y)

lm_x_y <- lm(y ~ x)
names(lm_x_y)

summary(lm_x_y)
plot(lm_x_y)

# install.packages("performance")

check_model(lm_x_y)

x2 <- log(x)
y2 <- log(y)

plot(1:100, x2)
plot(1:100, y2)
summary(lm(y2 ~ x2))
lm_x_y_log <- lm(y2 ~ x2)
plot(lm_x_y_log)

## Less perfect world, but still good -----
source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)
set.seed(1000)
df_olympics <- sample_n(df_olympics, 10000)

## Check height and weight points and smooth
df_olympics %>%
  ggplot(aes(Height, Weight)) + geom_point() +
  geom_smooth(method = "lm")

## Check histogram of both values
df_olympics %>%
  ggplot(aes(Weight)) + geom_histogram(bins=50)

## color by potential problematic values (gender, season)
df_olympics %>%
  ggplot(aes(Height, Weight, color=Season)) +
    geom_point(alpha=0.2) +
    geom_smooth(method = "lm") + 
    facet_wrap(~Season)

df_olympics %>%
  ggplot(aes(sample=Height)) + 
    geom_qq() + stat_qq_line()

df_olympics %>%
  filter(Season == "Summer",
         Year > 1990) %>%
  ggplot(aes(sample=Weight)) + 
  geom_qq() + stat_qq_line() + facet_wrap(~Sport)

df_olympics %>%
  filter(Season == "Summer",
         Year > 1990) %>%
  ggplot(aes(Height, Weight)) + geom_point() +
  facet_wrap(~Sport)

# facet wrap around sports in Summer

# Filter and run the lm model
lm_weight_height <- lm(Weight ~ Height, data=df_olympics)
summary(lm_weight_height)
predict(lm_weight_height, newdata = data.frame(Height=c(180)))

lm_weight_height_noathletics <- df_olympics %>%
  filter(Season == "Summer",
         Year > 1990,
         Sport != "Athletics") %>%
  with(lm(Weight ~ Height, data=.))

summary(lm_weight_height_noathletics)

library(performance)
compare_performance(lm_weight_height, lm_weight_height_noathletics)

## Load the movies dataset ----
source("functions/load-movies.R")
source("functions/process-movies.R")
df_movies <- load_movies(50000)
df_movies <- process_movies(df_movies)
set.seed(1000)
df_movies <- sample_n(df_movies, 5000)

## Investigate if budget has any impact on vote counts
ggplot(df_movies, aes(budget, vote_count)) +
  geom_point() +
  geom_smooth(method = "lm")

## Check the qqplots
df_movies %>%
  ggplot(aes(sample=vote_count)) + 
    geom_qq() + stat_qq_line()

ggplot(df_movies, aes(sample=budget)) + 
  geom_qq() + stat_qq_line()


ggplot(df_movies, aes(x=budget)) + 
  geom_histogram(binwidth = 10)

ggplot(df_movies, aes(x=vote_count)) + 
  geom_histogram(binwidth = 1000)

lm_vote_budget <- lm(vote_count ~ budget, data=df_movies)
summary(lm_vote_budget)

plot(lm_vote_budget)

## Check the histograms
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.9, na.rm = TRUE)) %>%
  ggplot(aes(vote_count)) + geom_histogram()

# filter error vote counts and 
# filter out bottom and top 5-10 perecent
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.05, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.95, na.rm = TRUE)) %>%
  ggplot(aes(vote_count)) + geom_histogram()
# check qqplots again
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.9, na.rm = TRUE)) %>%
  ggplot(aes(sample=vote_count)) + geom_qq() + stat_qq_line()
## Check the qqplots
df_movies %>%
  ggplot(aes(sample=vote_count)) +
    geom_qq() + stat_qq_line()

lm_visits_budget <- lm(vote_count ~ budget, data=df_movies)
summary(lm_visits_budget)
performance(lm_visits_budget)

# check_model(lm_visits_budget)

## Log transformations ------
# filter error values
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.9, na.rm = TRUE)) %>%
  ggplot(aes(x=vote_count)) + geom_histogram() +
  scale_x_log10()
## Plot data

# filter 0 values because of log
# log transform the data
df_movies %>%
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE),
         vote_count < quantile(vote_count, 0.9, na.rm = TRUE),
         budget > quantile(budget, 0.1, na.rm = TRUE)) %>%
  ggplot(aes(x=budget, y=vote_count)) +
    geom_point() + scale_x_log10() +
    scale_y_log10() + geom_smooth(method="lm")
# scale_x_log10 and scale_y_log10

## Transform the data

# run the model again
lm_visits_budget_log <- df_movies %>%
   filter(vote_count > 0,
         budget >0) %>%
  with(lm(log10(vote_count) ~ log(budget), data=.))

summary(lm_visits_budget_log)
performance(lm_visits_budget)

# compare the models
compare_performance(lm_visits_budget, lm_visits_budget_log)

## FACEBOOK DATA --------
## load facebook data
df_facebook <- read.csv("data/facebook.csv")
df_facebook_plt <- sample_n(df_facebook, 2000)

## check how likes relate to likes received
df_facebook_plt %>%
  ggplot(aes(likes, likes_received)) + geom_point() +
  geom_smooth(method = "lm") + scale_x_log10() +
  scale_y_log10()


df_facebook_plt %>%
  filter(likes > 10) %>%
  ggplot(aes(likes)) + geom_histogram() +
  scale_x_log10()

df_facebook_plt %>%
  filter(likes_received > 10) %>%
  ggplot(aes(likes_received)) + geom_histogram() +
  scale_x_log10()

df_facebook_plt %>%
  filter(likes_received > 10, likes_received > 10) %>%
  ggplot(aes(likes, likes_received)) + geom_point() +
    geom_smooth(method = "lm") + scale_x_log10() +
    scale_y_log10()

df_facebook <- df_facebook %>%
  filter(likes > 10, likes_received > 10) %>%
  mutate(log_likes = log(likes),
         log_likes_received = log(likes_received))

## do some filtering and transformations
df_facebook %>%
  sample_n(2000) %>%
  ggplot(aes(log_likes, log_likes_received)) + geom_point() +
    geom_smooth(method = "lm")

lm_likes_received <- lm(likes_received ~ likes, data = df_facebook)
summary(lm_likes_received)

lm_log_likes_received <- lm(log_likes_received ~ log_likes, data = df_facebook)
summary(lm_log_likes_received)

compare_performance(lm_log_likes_received, lm_likes_received)

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
