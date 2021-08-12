library(ggplot2)
library(dplyr)
source("functions/load-movies.R")
source("functions/process-movies.R")

## Load movies
df_movies <- load_movies(5000)
df_movies <- process_movies(df_movies)

## Labels --------
# plot revenue as function of budget
ggplot(df_movies, aes(budget, revenue)) + geom_point()

ggplot(df_movies, aes(budget, revenue, fill=is_family)) +
  geom_point()

count(df_movies, is_family)

ggplot(df_movies, aes(budget, revenue, color=is_family)) +
  geom_point() +
  xlab("Movie budget (in millions of dollars)") + 
  ylab("Movies revenue (in million of dollars)")


plt_budget_revenue <- ggplot(df_movies, aes(budget, revenue, color=is_family)) +
  geom_point()

plt_budget_revenue +
  xlab("Movie budget (in millions of dollars)") + 
  ylab("Movies revenue (in million of dollars)") +
  labs(color="Family movie")


df_movies %>%
  mutate(family_movie_string = ifelse(is_family, "Family movie", "Other")) %>%
  ggplot(aes(budget, revenue, color=family_movie_string)) +
    geom_point()


plt_budget_revenue + 
  ggtitle("Movie budgets and revenues",
          subtitle = "Movies from 1990 till 2015")

plt_budget_revenue +
  labs( x = "Movie budget (in millions of dollars)",
        y = "Movies revenue (in million of dollars)",
        title = "Movie budgets and revenues",
        subtitle = "Movies from 1990 till 2015",
        color = "Family movie")

## Create a histogram of revenues
# create a new value pre_1990 <- pre 1990 and post 1990
# plot histograms of budget
# fill them with pre_1990
# label everything accordingly

df_movies %>%
  mutate(pre_1990 = year < 1990) %>%
  filter(budget > 0, !is.na(pre_1990)) %>%
  ggplot(aes(x=budget, fill=pre_1990)) + 
    geom_histogram(binwidth = 5) +
    labs(x="Movie budget in millions of dollars",
         y="", title="Number of movies with different budgets",
         fill = "Movie made before 1990")

# GUIDES
df_movies %>%
  mutate(pre_1990 = year < 1990) %>%
  filter(budget > 0, !is.na(pre_1990)) %>%
  ggplot(aes(x=budget, fill=pre_1990)) + 
  geom_histogram(binwidth = 5) +
  labs(x="Movie budget in millions of dollars",
       y="", title="Number of movies with different budgets",
       fill = "Movie made before 1990") +
  facet_wrap(~pre_1990) +
  guides(fill="none")

df_movies %>%
  mutate(pre_1990 = year < 1990) %>%
  filter(budget > 0, !is.na(pre_1990)) %>%
  ggplot(aes(x=budget, y=revenue, shape=is_family, color=pre_1990)) + 
    geom_point(size=1) +
    facet_wrap(~pre_1990) +
    labs(title="Budgets and revenues for movies pre 1990 and post 1990") +
    guides(color="none")

# Scales
df_movies %>%
  filter(revenue > 0) %>%
  group_by(year) %>%
  summarise(avg_revenue = mean(revenue, na.rm=TRUE)) %>%
  ggplot(aes(year, avg_revenue)) +
    geom_line()

df_year_revenues <- df_movies %>%
  filter(revenue > 0) %>%
  group_by(year) %>%
  summarise(avg_revenue = mean(revenue, na.rm=TRUE))
  
ggplot(df_year_revenues, aes(year, avg_revenue)) +
  geom_line() + scale_y_continuous(breaks = c(0, 10, 50, 200), 
                                   limits = c(0, 500))

ggplot(df_year_revenues, aes(year, avg_revenue)) +
  geom_line() + scale_y_continuous(breaks = seq(0, 400, by=20), 
                                   limits = c(0, 400))

## plot budget vs revenue
# set the y limits to 0, 400,
# set the X limits to 0, 400
# set the breaks of each to every 15 million dollars

ggplot(df_movies, aes(budget, revenue)) + geom_point() +
  scale_y_continuous(breaks = seq(0, 400, by=15), limits = c(0, 400)) +
  scale_x_continuous(breaks = seq(0, 400, by=15), limits = c(0, 400))

ggplot(df_movies, aes(budget, revenue, color=is_family)) +
  geom_point() + scale_color_manual(name="Family movie", 
                                    labels=c("Is a family movie", "Not a family movie"),
                                    values=c("red", "blue"))


