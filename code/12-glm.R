library(ggplot2)
library(dplyr)
library(tidyr)

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

df_example %>% # all groups together
  pivot_longer(cols = everything()) %>%
  mutate(avg=mean(value)) %>%
  ggplot(aes(x=1, y = value, color=name)) + 
  geom_point(position = position_jitter(0.2)) +
  geom_errorbar(aes(ymax=avg, ymin=avg)) +
  theme(aspect.ratio = 1)

df_example %>% #groups separated
  pivot_longer(cols = everything()) %>%
  group_by(name) %>%
  mutate(avg=mean(value)) %>%
  ungroup() %>%
  ggplot(aes(x = name, y = value, color=name)) + 
  geom_point(position = position_jitter(0.2)) +
  geom_errorbar(aes(ymax=avg, ymin=avg)) +
  theme(aspect.ratio = 1)

df_example <- df_example %>%
  pivot_longer(cols = everything(), names_to="musician", values_to="score")

aov_score_musician <- aov(score ~ musician, data=df_example)
summary(aov_score_musician)
aov_score_musician

### Post-hoc test
TukeyHSD(aov_score_musician)

#t.test(df_example$haydn, df_example$bach)
## Correction for multiple comparisons
lm_score_musician <- lm(score ~ musician, data=df_example)
summary(lm_score_musician)
aov_score_musician

## Height in sports
source("functions/load-olympics.R")
set.seed(201)
df_olympics <- load_olympics(-1)
df_olympics <- df_olympics %>%
  filter(Year > 1990, Season == "Winter") %>%
  sample_n(df_olympics, 5000)

## Plot average height in sports
df_olympics <- df_olympics %>%
  filter(Year > 1990, Season == "Winter") %>%
  sample_n(5000)

unique(df_olympics$Sport)

df_olympics %>%
  ggplot(aes(Height)) + geom_histogram() +
  facet_wrap(~Sport)

## Test it with anova
aov_height_sport <- aov(Height ~ Sport, data=df_olympics)
summary(aov_height_sport)
aov_height_sport
aov_post <- TukeyHSD(aov_height_sport)
as.data.frame(aov_post$Sport) %>%
  arrange(`p adj`) %>%
  mutate(p_sig = `p adj` < 0.01)

lm_height_sport <- lm(Height ~ Sport, data=df_olympics)
summary(lm_height_sport)

## Multiple predictors -------
source("functions/load-olympics.R")
df_olympics <- load_olympics(-1)
set.seed(2500)
df_olympics <-  df_olympics %>%
  filter(Year > 1990, Season == "Winter") %>%
  sample_n(5000)

lm_height_sport_sex <- glm(Height ~ Sport + Sex, data=df_olympics)
summary(lm_height_sport_sex)
summary(lm_height_sport)

# glm for just weight 
lm_weight_height <- glm(Weight ~ Height, data=df_olympics)
summary(lm_weight_height)

# glm for just Sex
lm_weight_gender <- glm(Weight ~ Sex, data=df_olympics)
summary(lm_weight_gender)

# glm for just Height + Sex
lm_weight_height_gender <- glm(Weight ~ Height + Sex, data=df_olympics)
summary(lm_weight_height_gender)

# glm for just Height*Sex
ggplot(df_olympics, aes(Height, Weight, color =Sex)) + geom_point() + geom_smooth(method = "lm")
lm_weight_height_gender_inter <- glm(Weight ~ Height*Sex, data=df_olympics) 
lm_weight_height_gender_inter <- glm(Weight ~ Height + Sex + Height:Sex,
                                     data=df_olympics) #the same
summary(lm_weight_height_gender_inter)

# compare_performance()
compare_performance(lm_weight_height, lm_weight_height_gender,
                    lm_weight_gender, lm_weight_height_gender_inter)

t.test(Height ~ Sex, data=df_olympics)
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
