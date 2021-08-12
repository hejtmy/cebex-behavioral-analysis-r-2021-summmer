library(dplyr)

process_movies <- function(df_movies){
  df_movies <- decode_original_language(df_movies)
  
  df_movies <- df_movies %>%
    mutate(budget = budget/10^6,
           revenue = revenue/10^6,
            profit = revenue - budget,
            adult = adult == "True",
            video = video == "True") %>%
      select(-c(homepage, imdb_id, overview, poster_path, tagline))
  
  df_movies <- dummy_code_genres(df_movies)
  df_movies <- separate_release_date(df_movies)
  return(df_movies)
}

decode_original_language <- function(df_movies){
    df_movies <- df_movies %>%
    mutate(original_language = recode(df_movies$original_language, 
          "en" = "English", "it" = "Italian",
         "fr" = "French", "es" = "Spannish", "de" = "German",
         "ru" = "Russian", "jp" = "Japanese"))
    return(df_movies)
}

dummy_code_genres <- function(df_movies){
  df_movies <- df_movies %>%
    mutate(is_comedy = grepl("Comedy", genres, ignore.case = TRUE),
           is_romance = grepl("Romance", genres, ignore.case = TRUE),
           is_action = grepl("Action", genres, ignore.case = TRUE),
           is_drama = grepl("Drama", genres, ignore.case = TRUE),
           is_family = grepl("Family", genres, ignore.case = TRUE))
  return(df_movies)
}

separate_release_date <- function(df_movies){
  df_movies <- df_movies %>% 
    tidyr::separate(release_date, c("year", "month", "day"), 
             sep = "-", convert = TRUE)
  return(df_movies)
}