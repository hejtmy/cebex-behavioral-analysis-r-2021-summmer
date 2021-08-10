library(dplyr)
library(ggplot2)
source("functions/load-movies.R")
source("functions/process-movies.R")

## load the data
df_movies <- load_movies(5000)
df_movies <- process_movies(df_movies)

plt <- ggplot(df_movies, aes(x=year, y=budget, color=is_comedy))

plt$mapping

plt + geom_point()
plt + geom_boxplot()

# GEOM histogram
# %>%
# +
ggplot(df_movies, aes(x=year)) + 
  geom_histogram() + 
  xlab("Release year") + 
  ylab("Number of movies released")


# Create a histogram of average votes


# Create a histogram of vote counts

# Create a histogram of vote counts filled with them being family movie
ggplot(df_movies, aes(x=vote_count, fill=is_family)) + geom_histogram()

# Dplyr 
# filter only the button 90 percent of movies in vote counts
df_movies_filtered <- df_movies %>%
  filter(vote_count < quantile(vote_count, 0.9, na.rm = TRUE))
# plot a histogram of those movies
ggplot(df_movies_filtered, aes(vote_count)) + 
# set the number of bins to 50
  geom_histogram(bins = 50)
rm(df_movies_filtered)


df_movies %>%
  filter(vote_count < quantile(vote_count, 0.9, na.rm = TRUE)) %>%
  # plot a histogram of those movies
  ggplot(aes(vote_count)) + 
  # set the number of bins to 50
    geom_histogram(bins = 50)


df_movies %>%
  # only for movies with vote counts larger than 10th percentile
  filter(vote_count > quantile(vote_count, 0.1, na.rm = TRUE)) %>%
  # fill by is_comedy
  ggplot(aes(vote_average, fill=is_comedy)) +
  # create a histogram of vote average
    geom_histogram(alpha=0.8, bins=25)

# only for family movies
plt <- df_movies %>%
  filter(is_family) %>%
# histogram of votes averages
  ggplot(aes(vote_average)) +
# set size of each histogram bit to 0.5
    geom_histogram(binwidth = 0.5)

plt + xlab("Vote average")

plt <- df_movies %>%
  filter(is_family) %>%
  # histogram of votes averages
  ggplot(aes(x=vote_average))

## styling
plt + geom_histogram(bins=10, fill = "red")
plt + geom_histogram(bins=10, fill = "#d049eb")
# red, blue, yellow, green,


## Multiple layers ----
plt <- df_movies %>%
  filter(is_family) %>%
  # histogram of votes averages
  ggplot(aes(x=vote_average))

plt + 
  geom_histogram(aes(y=..density..), color="black", bins = 10, 
                 fill = "#d049eb")+
  geom_density(fill="#7aa4ff", color="white", alpha = 0.5)

## Create a histogram with density function on top of it for 
plt <- df_movies %>%
  # profits of movies whith at least 30 million dollar budgets
  filter(budget > 30, profit < 10^3) %>%
  # fill the histograms and density functions with "is_drama"
  ggplot(aes(x=profit, fill=is_drama))

plt +
  geom_histogram(aes(y=..density..), bins = 20) +
  geom_density(alpha = 0.5, color="white")

## plotting side by side
plt +
  facet_wrap(~is_drama) +
  geom_histogram()

hist(df_movies$budget, breaks = 25)
ggplot(df_movies, aes(budget)) + geom_histogram(bins = 50)

## Boxplots ----
