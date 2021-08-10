process_movies <- function(df_movies){
  df_movies <- df_movies %>%
    mutate(original_language = recode(df_movies$original_language, 
          "en" = "English", "it" = "Italian",
         "fr" = "French", "es" = "Spannish", "de" = "German",
         "ru" = "Russian", "jp" = "Japanese"))
  df_movies <- df_movies %>%
    mutate(budget = budget/10^6,
           revenue = revenue/10^6,
            profit = revenue - budget,
            adult = adult == "True",
            video = video == "True") %>%
      select(-c(homepage, imdb_id, overview, poster_path, tagline))
  
return(df_movies)
}