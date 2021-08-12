source("data/big5/load-big5.R")
library(dplyr)

df_big5$Extraversion <- df_big5$E1 + df_big5$E3

select(df_big5, starts_with("E", ignore.case = FALSE)) %>%
  head()


select(df_big5, num_range("E", seq(1, 10, by=2))) %>%
  head()

df_big5 %>%
  mutate(extraversion = (E1 + E3 + E5 + E7 + E9) - E2 - E4 - E6- E8 - E10,
         z_extraversion = (extraversion - mean(extraversion))/sd(extraversion)) %>%
  select(contains("extraversion")) %>%
  ggplot(aes(z_extraversion)) + geom_histogram(binwidth = 0.5)

#df_big5 %>%
#  mutate(Extraversion = across(num_range("E", seq(1, 10, by=2), sum))
# across
