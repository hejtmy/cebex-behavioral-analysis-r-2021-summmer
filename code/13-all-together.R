pth <- "data/jovi/Jovi4_13.json"
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(report)

names(dat)

head(dat$data, 25)

## REGULAR EXPRESSIONS

load_jovi <- function(pth){
  dat <- jsonlite::fromJSON(pth)
  df_jovi <- dat$data %>%
    extract(image, into=c("jitter"), regex="jit([0-9]+)",
            remove = FALSE, convert = TRUE) %>%
    mutate(user_id = dat$id)
  return(df_jovi)
}

df <- load_jovi("data/jovi/Jovi4_13.json")


jovi <- data.frame()
for(pth in list.files("data/jovi/", full.names = TRUE)){
  df <- load_jovi(pth)
  jovi <- rbind(jovi, df)
}
table(jovi$user_id)

table(jovi$user_id, jovi$jitter)

## Reaction times ------
glimpse(jovi)
ggplot(jovi, aes(reactionTime)) + 
  geom_histogram()

jovi_filtered <- jovi %>%
  filter(reactionTime < 2000)

ggplot(jovi_filtered, aes(reactionTime)) + 
  geom_histogram()

ggplot(jovi_filtered, aes(reactionTime)) + 
  geom_histogram() + facet_wrap(~user_id)

summary(jovi_filtered)

jovi_filtered <- jovi_filtered %>%
  filter(!is.na(jitter), !is.na(correctAnswer))

ggplot(jovi_filtered, aes(sample=reactionTime)) + 
  geom_qq() + stat_qq_line() + facet_wrap(~jitter)

lm_reaction_jitter <- lm(reactionTime ~ jitter, data = jovi_filtered)
summary(lm_reaction_jitter)
lm_reaction_jitter$coefficients["jitter"] * 15

lm_reaction_jitter_correct <- jovi_filtered %>%
  filter(correctAnswer) %>%
  with(lm(reactionTime ~ jitter, data = .))

summary(lm_reaction_jitter_correct)

lm_reaction_jitter_correct_inter <- 
  lm(reactionTime ~ jitter*correctAnswer, data = jovi_filtered)
summary(lm_reaction_jitter_correct_inter)

compare_performance(lm_reaction_jitter, 
                    lm_reaction_jitter_correct,
                    lm_reaction_jitter_correct_inter)

# Linear mixed effect models
library(lme4)
library(lmerTest)
lmer_reaction_jitter <- lmer(reactionTime ~ jitter + (1 | user_id),
                             data = jovi_filtered)

summary(lmer_reaction_jitter)

ggplot(jovi_filtered, aes(factor(user_id), reactionTime)) + geom_boxplot()

compare_performance(lm_reaction_jitter,
                    lmer_reaction_jitter)

data.frame()






## Prep ------
dat <- fromJSON(pth)
str(dat)
head(dat$data)
View(dat$data)

txt <- dat$data$image[120]

## REGULAR EXPRESSION
dat$data %>%
  extract(image, into=c("jitter"), regex = "jit([0-9]+)",
          remove=FALSE, convert = TRUE) %>%
  View()

load_jovi <- function(pth){
  dat <- fromJSON(pth)
  dat$data <- dat$data %>%
    extract(image, into=c("jitter"), regex = "jit([0-9]+)",
            remove=FALSE, convert = TRUE)
  return(dat)
}
## Data looks weird - extreme values at the end
# This is a problem because I cannot run t-test
jovi <- load_jovi(pth)
head(jovi$data)

files <- list.files("data/jovi/", full.names = TRUE)

jovi <- data.frame()
for(file in files){
  res <- load_jovi(file)
  res$data$user <- res$user
  jovi <- rbind(jovi, res$data)
}

## Data exploration
table(jovi$user)
table(jovi$user, jovi$jitter)

ggplot(jovi, aes(reactionTime)) +
  geom_histogram()
summary(jovi$reactionTime)

glimpse(jovi)

ggplot(jovi, aes(reactionTime)) +
  geom_histogram() + facet_wrap(~didAnswer)

lm_reaction_jitter <- lm(reactionTime ~ jitter, data=jovi)
summary(lm_reaction_jitter)

ggplot(jovi, aes(y=reactionTime, fill=factor(jitter))) + geom_boxplot()

jovi <- jovi %>%
  mutate(jitter = ifelse(is.na(jitter), 0, jitter))

ggplot(jovi, aes(x = jitter, y=reactionTime, fill=factor(jitter), group=jitter)) +
  geom_boxplot()

jovi %>%
  filter(didAnswer) %>%
  ggplot(aes(jitter, reactionTime)) +
  geom_jitter() + geom_smooth(method = "lm")

lm_reaction_jitter_aswered <- lm(reactionTime ~ jitter, data=filter(jovi, didAnswer))
summary(lm_reaction_jitter_aswered)

lm_reaction_jitter$coefficients[1] + lm_reaction_jitter$coefficients[2]*11

jovi %>%
  filter(didAnswer) %>%
  group_by(jitter, user) %>%
  summarise(mean=mean(reactionTime),
            se=sd(reactionTime)/sqrt(n())) %>%
  ggplot(aes(jitter, mean, color=user)) +
  geom_line(size=1.5) + geom_errorbar(aes(ymin=mean-1.96*se, ymax=mean+1.96*se), width=0.2)

## repeated ANOVA
library(ez)
ez_reaction_jitter <- ezANOVA(jovi[jovi$didAnswer,],
                              dv = reactionTime,
                              wid = user,
                              within = jitter,
                              return_aov = TRUE)

summary(ez_reaction_jitter$aov)

## mixed models
library(lme4)
lmer_reaction_jitter <- lmer(reactionTime ~ jitter + (1|user), data=jovi[jovi$didAnswer,])
summary(lmer_reaction_jitter)
report(lmer_reaction_jitter)

library(lmerTest)
lmer_reaction_jitter <- lmer(reactionTime ~ jitter + (1|user), data=jovi[jovi$didAnswer,])

## 
plt1 <- ggplot(jovi[jovi$didAnswer, ], aes(reactionTime)) + geom_histogram()
plt2 <- ggplot(jovi[jovi$didAnswer, ], 
               aes(x = jitter, y=reactionTime, fill=factor(jitter), group=jitter)) + 
  geom_boxplot() + guides(fill=FALSE)
plt3 <- jovi %>%
  filter(didAnswer) %>%
  ggplot(aes(jitter, reactionTime)) +
  geom_jitter() + geom_smooth(method = "lm")

library(gridExtra)
grid.arrange(plt1, arrangeGrob(plt2, plt3, ncol=2), nrow = 2)
