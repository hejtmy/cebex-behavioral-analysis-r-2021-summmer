library(ggplot2)
library(dplyr)
library(tidyr)
source("code/data-loading-functions.R")
df_movies <- load_movies_metadata(2500)
df_movies_complete <- load_movies_metadata()

## Analysis of variance
set.seed(666)
df_example <- data.frame(mozart = rnorm(100,100,5), bach = rnorm(100,110,5),
                         haydn = rnorm(100,112,5), beethowen = rnorm(100,150,5))

df_example %>% # all groups together
  pivot_longer(cols = everything()) %>%
  mutate(avg=mean(value)) %>%
  ggplot(aes(x=1, y = value)) + 
  geom_point(position = position_jitter(0.2)) +
  geom_errorbar(aes(ymax=avg, ymin=avg)) +
  theme(aspect.ratio = 1)

df_example %>% #groups separated
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  mutate(avg=mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color=value)) + 
  geom_point(position = position_jitter(0.2)) +
  geom_errorbar(aes(ymax=avg, ymin=avg)) +
  theme(aspect.ratio = 1)

df_musicians <- df_example %>%
  pivot_longer(cols = everything(), names_to="musician", values_to="score")
aov_score_musician <- aov(score ~ musician, data=df_musicians)
summary(aov_score_musician)

### Post-hoc test
TukeyHSD(aov_score_musician)
t.test(df_example$haydn, df_example$bach)

## Height in sports
source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)
df_olympics <- sample_n(df_olympics, 5000)

## Plot average height in sports

## Test it with anova

## Filter only years 1990

## run again

## Multiple predictors -------
source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)
df_olympics <- sample_n(df_olympics, 5000)
table(df_olympics$Sex)

# glm for just weight 

# glm for just Sex

# glm for just Height + Sex

# glm for just Height*Sex


# compare_performance()

## Sex seems not to be adding much, why?

# what about sport?

summary(glm(Weight ~ Height + Sport, data=df_olympics))
compare_performance(glm(Weight ~ Height + Sport, data=df_olympics),
                    glm(Weight ~ Height, data=df_olympics))

## Logistic regression --------
log_gender_likes <- glm(factor(gender) ~ likes_received, 
                        data = df_facebook, 
                        family=binomial(link="logit"))

df_facebook_plt %>%
  ggplot(aes(x = log(likes_received), y = 2-as.numeric(factor(gender)))) + 
  geom_point(size = 1.5, position = position_jitter(height = 0.01)) + 
  theme(aspect.ratio = 1) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))

summary(log_gender_likes)
log_gender_likes_2 <- glm(factor(gender) ~ likes_received + likes + age, 
                          data = df_facebook, 
                          family=binomial(link="logit"))

summary(log_gender_likes_2)
