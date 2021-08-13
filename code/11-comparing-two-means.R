library(ggplot2)
library(dplyr)
library(tidyr)

## IDEAL world --------
## Generate data
set.seed(20202)
df_experiment <- data.frame(condition = c(rep("condition1", 50), rep("condition2", 50)),
                            result = c(rnorm(50, 10, 2), rnorm(50, 14, 2.1)))

## Visualise data
df_experiment %>%
  ggplot(aes(result, fill = condition)) + 
  geom_histogram(alpha = 0.5, binwidth = 1)

## Test the data
t.test(rnorm(100, 10, 2), rnorm(100, 11, 2))
t.test(rnorm(10, 10, 2), rnorm(10, 11, 2))
t.test(rnorm(10000, 10, 2), rnorm(10000, 10.05, 2))

t.test(result ~ condition, data = df_experiment)

df_experiment_three <- data.frame(condition = c(rep("condition1", 50), 
                                                rep("condition2", 25),
                                                rep("condition3", 25)),
                            result = c(rnorm(50, 10, 2), rnorm(50, 14, 2.1)))

t.test(result ~ condition, data = df_experiment_three)

## T test vs lm -----
t.test(result ~ condition, data = df_experiment)
summary(lm(result ~ condition, data = df_experiment))
## equal vs non equal variances
set.seed(20202)
df_experiment_unequal <- data.frame(condition = c(rep("condition1", 50), rep("condition2", 50)),
                            result = c(rnorm(50, 10, 2), rnorm(50, 14, 5)))
df_experiment_unequal %>%
  ggplot(aes(result, fill = condition)) + 
  geom_histogram(alpha = 0.5, binwidth = 1)


df_experiment_unequal %>%
  ggplot(aes(condition, result)) + geom_boxplot()

t.test(result ~ condition, data = df_experiment_unequal,
       var.equal = FALSE)

## paired test 

## Less ideal world --------

source("functions/load-movies.R")
source("functions/process-movies.R")
set.seed(2010)
df_movies <- load_movies(50000)
df_movies <- process_movies(df_movies)
df_movies <- sample_n(df_movies, 5000)

# Do some types of movies get larger ratings?
# visualise drama vs non drama
# check boxplots
df_movies %>%
  ggplot(aes(is_drama, vote_average, color = is_drama)) + 
    geom_boxplot()

df_movies %>%
  count(is_drama)

# do dramas get better ratings by people?
t.test(vote_average ~ is_drama, data = df_movies)

# Check histograms
df_movies %>%
  ggplot(aes(vote_average, color = is_drama)) + 
  geom_histogram(binwidth = 0.5)

# vote average qqplots
df_movies %>%
  ggplot(aes(sample=vote_average, color = is_drama)) + 
    geom_qq() + stat_qq_line()

# filter for extreme values
df_movies %>%
  filter(vote_average >= 10) %>%
  head()

# filter for extreme values
df_movies %>%
  ggplot(aes(vote_count, fill=is_drama)) +
  geom_histogram() + scale_x_log10()

# plot qqplot again

df_movies %>%
  filter(vote_count > 10, 
         vote_average > 0) %>%
  ggplot(aes(sample=vote_average, color = is_drama)) + 
  geom_qq() + stat_qq_line()

# T test
t_vote_drama <- df_movies %>%
  filter(vote_count > 10, 
         vote_average > 0) %>%
  with(t.test(vote_average ~ is_drama, data = .))

t_vote_drama

library(effsize)
df_movies %>%
  filter(vote_count > 10, 
         vote_average > 0) %>%
  with(cohen.d(vote_average ~ is_drama, data = .))

cohen.d(rnorm(10000, 10, 2), rnorm(10000, 10.05, 2))

## equal variances

df_movies %>%
  sample_n(2000) %>%
  filter(vote_average > 0) %>%
  group_by(is_drama) %>%
  mutate(avg=mean(vote_average)) %>%
  ungroup() %>%
  ggplot(aes(x = is_drama, y = vote_average, color=is_drama)) + 
    geom_point(position = position_jitter(0.2)) +
    geom_errorbar(aes(ymax=avg, ymin=avg)) +
    theme(aspect.ratio = 1)

# compare
t.test(vote_average ~ is_drama, data=df_movies, var.equal = TRUE)
t.test(vote_average ~ is_drama, data=df_movies, var.equal = FALSE)

## One sided t-test comparing to certain value
avg <- mean(df_movies$vote_average, na.rm = TRUE)
t.test(df_movies$vote_average[df_movies$is_drama], mu=avg)

## Paired t-test
results_pre <- rnorm(100, 10, 1)
results_post <- results_pre + 2 + rnorm(100, 0, 1)

t.test(results_pre, results_post, paired = TRUE)
t.test(results_pre, results_post, paired = FALSE)

## 
df_ratings_family <- df_ratings %>%
  mutate(movieId = as.character(movieId)) %>%
  inner_join(select(df_movies_complete, id, is_family),
             by=c("movieId" = "id"))

df_comparing_family <- df_ratings_family %>%
  group_by(userId, is_family) %>%
  summarise(avg = mean(rating),
            n_movies = n()) %>%
  ungroup() %>%
  filter(n_movies > 5) %>%
  select(-n_movies) %>%
  pivot_wider(names_from = "is_family", values_from="avg", names_prefix = "family") %>%
  select(userId, family=familyTRUE, not_family=familyFALSE) %>%
  drop_na()

str(df_comparing_family)
df_comparing_family %>%
  pivot_longer(cols = c("not_family", "family")) %>%
  ggplot(aes(as.numeric(factor(name)), value, color=factor(userId))) + 
  geom_line() + guides(color=FALSE) + geom_point()

t.test(df_comparing_family$family, df_comparing_family$not_family,
       paired = TRUE)

## Non parametric data -----
hist(df_movies$revenue)
t.test(revenue~is_family, data=filter(df_movies_complete, revenue > 0)) #wrong, revenue is not normally distributed
df_movies_complete %>%
  filter(revenue > 0) %>%
  with(cohen.d(revenue ~ factor(is_family)))

wilcox.test(revenue~is_family,
            data=filter(df_movies_complete, revenue > 0))
wilcox.test(revenue~is_drama, data=filter(df_movies_complete, revenue > 0))

wilcox.test(revenue~is_family,
            data=filter(df_movies, revenue > 0))
summary(lm(order(revenue)~is_family,
            data=filter(df_movies, revenue > 0)))
