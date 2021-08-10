# loading olympics dataset
library(dplyr)
source("functions/load-olympics.R")
df_olympics <- load_olympics(n_rows = -1)

glimpse(df_olympics)

format(object.size(df_olympics), units = "auto")

unique(df_olympics$Sport)

# select a sport
# filter the table for that sport
df_olympics %>%
  filter(Sport == "Archery", !is.na(Medal)) %>%
# group by country
  group_by(Team) %>%
# summarise for each coutry number of gold medals
  summarise(n_gold = sum(Medal == "Gold")) %>%
# order the countries acording the number of medals with the "best one" on top
  arrange(-n_gold)

# checkout average height and weight of athletes from different countries/genders
df_olympics %>%
  filter(Year > 2010) %>%
  group_by(Team, Sex) %>%
  summarise(avg_height = mean(Height),
            avg_weight = mean(Weight),
            avg_age = mean(Age)) %>%
# arrange by height
  arrange(avg_height)