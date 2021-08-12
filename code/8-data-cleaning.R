## Duplicate values
df_students_mat <- read.csv("data/student-alcohol-consumption/student-mat.csv") %>%
  mutate(source="math")
df_students_por <- read.csv("data/student-alcohol-consumption/student-por.csv") %>%
  mutate(source="portugese")

colnames(df_students_mat) %in% colnames(df_students_por)
df_students <- rbind(df_students_mat, df_students_por)

sum(duplicated(df_students))

summary(df_students$freetime)
summary(df_students$famrel)

is_duplicated <- duplicated(select(df_students, school:traveltime))
sum(is_duplicated)
which(is_duplicated)


is_duplicated_better <- duplicated(select(df_students, school:traveltime,
                                          famrel:goout))
which(is_duplicated_better)

head(df_students[!is_duplicated_better,])

## 
distinct(df_students, G1, G2, G3)

## Missing values 
source("functions/load-olympics.R")
df_olympics <- load_olympics(50000)
head(df_olympics[!complete.cases(df_olympics), ])

# slightly more complex
head(df_olympics[!complete.cases(select(df_olympics, -Medal)), ])

is_missing <- !complete.cases(select(df_olympics, -Medal))
sum(is_missing)

# mutate()

df_olympics$Height[is.na(df_olympics$Height)] <- mean(df_olympics$Height,
                                                      na.rm = TRUE)

df_olympics$non_na_height <- ifelse(is.na(df_olympics$Height),
                                    mean(df_olympics$Height, na.rm = TRUE),
                                    df_olympics$Height)

## outliers
# set outliers to NA

df_example <- data.frame(x = rnorm(100, 50, 5),
                         y = rnorm(100, 50, 5))
ggplot(df_example, aes(x, y)) +
  geom_point() + scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) + 
  geom_smooth(method = "lm")


df_example <- rbind(df_example, data.frame(x=c(93, 97), y=c(94, 95)))
ggplot(df_example, aes(x, y)) +
  geom_point() + scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) + geom_smooth(method = "lm")

df_example %>%
  mutate(x_norm = scale(x),
         y_norm = scale(y)) %>%
  tail()

# scale
# scale (x1 - mean(x))/sd(x)

source("functions/load-movies.R")
source("functions/process-movies.R")

df_movies <- load_movies(10000)
df_movies <- process_movies(df_movies)

# plot the revenue budged data
# plot the histograms of budgets
# histogram of revenues

hist(df_movies$budget, breaks = 30)

df_movies %>%
  mutate(budget_norm = scale(budget)) %>% # STILL SCALING WITH ZEROS! WRONG!
  select(budget, budget_norm) %>%
  filter(budget > 0, budget_norm < 2) %>%
  head()

df_movies %>%
  filter(budget > 0) %>%
  mutate(budget_norm = scale(budget)) %>%
  select(budget, budget_norm) %>%
  fulter(budget_norm < 2) %>%
  head()
