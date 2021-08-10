# There should be always clear way from raw data to final
library(dplyr)
source("functions/load-movies.R")
df_movies <- load_movies(500)

## Tidy data

### Each variable has its own column
### Each observation has to have its own row
### Each value needs to be in its own cell

# If you need to modify some data by hand, keep some record of it WITH the data 
# (so not in your notepad, but in the same folder as the data)

# Mutate --------

df_movies <- load_movies(500)
## Change budget and revenue to millions of dollars
df_movies <- df_movies %>%
  mutate(budget = budget/10^6,
         revenue = revenue/10^6,
## Create a variable profit = revenue - budget
          profit = revenue - budget,
## adult and video variables should be logical TRUE FALSE
          adult = adult == "True",
          video = video == "True") %>%
## remove all columns which you thnik we will not ever analyze
    select(-c(homepage, imdb_id, overview, poster_path, tagline))

# Recoding ------
unique(df_movies$original_language)

?recode

char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
recode(char_vec, a = "Apple")
recode(char_vec, a = "Apple", b = "Banana")

unique(df_movies$original_language)
# en -> English
# it -> Italian
# fr -> French
# es -> Spannish
# de -> German
# ru -> Russian
# jp -> Japanese

df_movies <- df_movies %>%
  mutate(original_language = recode(df_movies$original_language, 
        "en" = "English", "it" = "Italian",
       "fr" = "French", "es" = "Spannish", "de" = "German",
       "ru" = "Russian", "jp" = "Japanese"))

df_big5 <- read.table("data/big5/big5-data.csv", nrows = 100,
                      stringsAsFactors = FALSE, header = TRUE)

df_big5 <- df_big5 %>%
  mutate(hand = recode(hand, "1"="right", "2"="left", "3"="both", .default="missed"),
         engnat = recode(engnat, "1"="yes", "2"="no", .default="missed"),
         gender = recode(gender, "1"="male","2"="female","3"="other", .default="missed"),
         english_native = engnat) %>%
  select(-engnat)

source("data/big5/load-big5.R")

# Factors ------
source("functions/load-olympics.R")
df_olympics <- load_olympics(1000)

df_olympics$Medal 

factor(df_olympics$Medal, levels = c("Gold", "Silver", "Bronze"))

df_olympics <- df_olympics %>%
  mutate(Medal = factor(Medal, levels = c("Gold", "Silver", "Bronze"), ordered = TRUE))

df_olympics %>%
  filter(Medal < "Silver")

df_olympics <- df_olympics %>%
  mutate(Medal = factor(Medal, levels = c("Bronze", "Silver", "Gold"), ordered = TRUE))

df_olympics %>%
  filter(Medal < "Silver")

# make gender a factor (NOT ORDERED)
df_big5 
# hands a factor (NOT ORDERED)

# english_native(or engnat if you dont have english native) 
# (ORDERED factor - english native is higher than english not")

# String operations ------
source("functions/load-movies.R")
source("functions/process-movies.R")

df_movies <- load_movies(1000)
df_movies <- process_movies(df_movies)

## grepl
head(df_movies$genres)
?grepl
genres <- df_movies$genres[1]
genres

grepl("Comedy", df_movies$genres[1:5])

grepl("c", "apple")
grepl("a", "apple")
grepl("p", "apple")

grep("a", c("apple", "car"))
grep("p", c("apple", "car"))

df_movies <- df_movies %>%
  mutate(is_comedy = grepl("Comedy", genres, ignore.case = TRUE),
         is_romance = grepl("Romance", genres, ignore.case = TRUE),
         is_action = grepl("Action", genres, ignore.case = TRUE),
         is_drama = grepl("Drama", genres, ignore.case = TRUE),
         is_family = grepl("Family", genres, ignore.case = TRUE))

## gsub
gsub("Comedy", "fun", c("Comedy", "Drama", "Family", "Comedy"))

df_movies$genres[1]
gsub("\\[", "", df_movies$genres[1])
gsub("\\{", "", df_movies$genres[1])

#? REGULAR EXPRESSIONS
gsub("'id': \\d+", "", df_movies$genres[1])
gsub("'id': \\d+, 'name': ", "", df_movies$genres[1])


## separate
library(tidyr)

df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"), stringsAsFactors = FALSE)
df %>% separate(x, c("A", "B"))

df_movies %>% 
  separate(release_date, c("year", "month", "day"), sep = "-") %>%
  head()

df_movies %>% 
  separate(release_date, c("year", "month", "day"), sep = "-", remove = FALSE) %>%
  glimpse()

df_movies %>% 
  separate(release_date, c("year", "month", "day"), sep = "-", remove = FALSE) %>%
  mutate(year = as.numeric(year)) %>% glimpse()

df_movies <- df_movies %>% 
  separate(release_date, c("year", "month", "day"), 
           sep = "-", convert = TRUE)

df_movies <- load_movies(500)
df_movies <- process_movies(df_movies)

## unite
df_movies %>%
  unite(release_date, year, month, day, sep = "-") %>%
  glimpse()

## 
df_example <- data.frame(ssn = c("2021134mM", "2021138mT"), stringsAsFactors = FALSE)
df_example %>% 
  separate(ssn, into = c("year", "day", "month", "gender", "weight"),
                        sep = c(4, 6, 7, 8)) %>%
  head()

data.frame(ssn = c("26267", "25487"), stringsAsFactors = FALSE) %>%
  separate(ssn, into=c("gender", "weight", "color", "bodytype", "personality"),
           sep = (1:4)) %>%
  mutate(gender = recode(gender, "1"="boy", "2"="girl", "3"="other", .default = "missing"))
# create a new colum name european_date
# day.month.year


# RESHAPING -------


# Scaling -----
