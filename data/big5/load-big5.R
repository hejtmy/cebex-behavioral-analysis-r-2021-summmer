pth <- "data/big5/big5-data.csv"
df_big5 <- read.table(pth, nrows = 100,
                      stringsAsFactors = FALSE, header = TRUE)

df_big5 <- df_big5 %>%
  mutate(hand = recode(hand, "1"="right", "2"="left", "3"="both", .default="missed"),
         engnat = recode(engnat, "1"="yes", "2"="no", .default="missed"),
         gender = recode(gender, "1"="male","2"="female","3"="other", .default="missed"),
         english_native = engnat) %>%
  select(-engnat)

rm(pth)