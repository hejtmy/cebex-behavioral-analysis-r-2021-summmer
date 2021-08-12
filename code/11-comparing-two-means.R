library(ggplot2)
library(dplyr)
library(tidyr)

source("functions/load-movies.R")
source("functions/process-movies.R")
df_movies <- load_movies(50000)
df_movies <- process_movies(df_movies)
df_movies <- sample_n(df_movies, 5000)

# Do some types of movies get larger ratings?

# do dramas get better ratings by people?

# Check histograms

# check boxplots

# vote average qqplots

# filter for extreme values

# plot qqplot again


# T test

library(effsize)

# cohen.d

## equal variances

df_movies %>%
  sample_n(2000) %>%
  group_by(is_drama) %>%
  mutate(avg=mean(vote_average)) %>%
  ungroup() %>%
  ggplot(aes(x = is_drama, y = vote_average, color=is_drama)) + 
    geom_point(position = position_jitter(0.2)) +
    geom_errorbar(aes(ymax=avg, ymin=avg)) +
    theme(aspect.ratio = 1)

# Run equal variances
# non equal variances
# compare

## One sided t-test comparing to certain value

## Paired t-test
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

t.test(df_comparing_family$family, df_comparing_family$not_family, paired = TRUE)


## Non parametric data
hist(df_movies$revenue)
t.test(revenue~is_family, data=filter(df_movies_complete, revenue > 0)) #wrong, revenue is not normally distributed
df_movies_complete %>%
  filter(revenue > 0) %>%
  with(cohen.d(revenue ~ factor(is_family)))

wilcox.test(revenue~is_family, data=filter(df_movies_complete, revenue > 0))
wilcox.test(revenue~is_drama, data=filter(df_movies_complete, revenue > 0))