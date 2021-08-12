library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)

source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)

df_olympics %>%
  sample_n(5000) %>%
  ggplot(aes(Height)) + geom_histogram(binwidth = 1)

df_olympics %>%
  sample_n(5000) %>%
  ggplot(aes(Age)) + geom_histogram(binwidth = 1)

## Normality assumption

df_olympics 

freq <- rnorm(100, 0, 1)
x <- seq(-4, 4, length = 100)
hist(df_normal, breaks = 25, prob=TRUE)
lines(x, dnorm(x))

source("functions/load-movies.R")
df_movies <- load_movies(10000)
df_ratings <- read.csv("data/movies/ratings_small.csv")

hist(df_ratings$rating, breaks = 5)
skew(df_ratings$rating)
skew(rnorm(100, 0, 1))

qqnorm(df_ratings$rating)

x <- rnorm(100, 0, 1)
qqnorm(x)
qqline(x)

qqnorm(df_movies$vote_average)
qqline(df_movies$vote_average)

df_movies %>%
  ggplot(aes(vote_average)) + geom_histogram(bins=20)

shapiro.test(df_movies$vote_average[1:4000])

votes <- df_movies$vote_average

## remove unreal values
votes <- votes[votes > 0]
## remove extremes
scaled_votes <- scale(votes)
votes[(scaled_votes < 2) | (scaled_votes > -2)]
votes <- votes[abs(scaled_votes) < 2]
qqnorm(votes)
qqline(votes)

## run shapiro test again
shapiro.test(votes[1:4000])
hist(votes, breaks = 25)

## see qqplots again

## Homogenity of variances
df_example <- data.frame(cond1 = rnorm(100, 10, 2), cond2 = rnorm(100, 12, 2),
                         cond3 = rnorm(100, 35, 2), cond4 = rnorm(100, 50, 2))

df_example %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(name, value, value)) + geom_jitter(width = 0.2) +
  geom_errorbar(aes(ymax=avg, ymin=avg))
  
df_example_nonhomogenous <- data.frame(cond1 = rnorm(100, 10, 2), cond2 = rnorm(100, 12, 2),
                         cond3 = rnorm(100, 35, 5), cond4 = rnorm(100, 50, 15))

df_example_nonhomogenous %>%
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  mutate(avg = mean(value)) %>%
  ungroup() %>%
  ggplot(aes(name, value, value)) + geom_jitter(width = 0.2) +
  geom_errorbar(aes(ymax=avg, ymin=avg))

df_example_nonhomogenous %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value, fill=name)) + geom_histogram(alpha=0.6, bins = 50)


df_example_lm <- data.frame(x=1:100, y=1:100 + rnorm(100, 0, 2))
df_example_lm %>%
  ggplot(aes(x,y)) + geom_point() + 
  geom_smooth(method="lm")

df_example_lm_nonhomogenous <- data.frame(x=1:100, 
                                          y=1:100 + rnorm(100, 0, 2)*exp(seq(0, 3, length.out=100)))
df_example_lm_nonhomogenous %>%
  ggplot(aes(x,y)) + geom_point() + 
  geom_smooth(method="lm")

df_example_nonhomogenous <- data.frame(cond1 = rnorm(100, 10, 2), cond2 = rnorm(100, 12, 2),
                                       cond3 = rnorm(100, 35, 5), cond4 = rnorm(100, 50, 15))

df_example_nonhomogenous %>%
  pivot_longer(cols = everything()) %>%
  with(leveneTest(value, name))

df_example_homogenous <- data.frame(cond1 = rnorm(100, 10, 2), 
                                    cond2 = rnorm(100, 12, 2),
                                    cond3 = rnorm(100, 35, 2),
                                    cond4 = rnorm(100, 50, 2))

df_example_homogenous %>%
  pivot_longer(cols = everything()) %>%
  with(leveneTest(value, name))

## Parametric vs non parametric tests -------

t.