df_countries <- read.csv("data/world-happinness/world-happiness-country-regions.csv",
                         stringsAsFactors = FALSE)
url_countries <- "https://raw.githubusercontent.com/hejtmy/cebex-behavioral-analysis-r-2021-summmer/main/data/world-happinness/world-happiness-country-regions.csv"

df_countries_url <- read.csv(url_countries, stringsAsFactors = FALSE)

# download.file()
download.file(url_countries, "countries-downloaded.csv")

# BIG DATA -----
## Check files first
pth_movies <- file.path("data", "movies", "movies_metadata.csv")
readLines(pth_movies, n = 5)

df_movies <- read.table(pth_movies, header=TRUE, sep=",",
                        stringsAsFactors = FALSE, fill = TRUE,
                        nrows = 1000)
df_movies <- read.table(pth_movies, header=TRUE, sep=",",
                        stringsAsFactors = FALSE, fill = TRUE,
                        nrows = 1000, quote = "\"")

hamlet <- 'be or not to be, that\'s the question'
hamlet <- "be or not to be, that's the question"

# create a function called load_movies
# single argument n_rows
# default is 1000
# the function loads movies dataset with given numebr of rows
df_movies <- load_movies(1000)


# Loading multiple files -----
data_sources <- list.files(file.path("data", "world-happinness", "individual-years"),
                           full.names = TRUE)
data_sources

ls_happiness_years <- list()
i <- 1
for(data_source in data_sources){
  df_year <- read.csv(data_source, stringsAsFactors = FALSE)
  # save each table into the list under correct name
  ls_happiness_years[[i]] <- df_year
  i <- i + 1
}

data_sources_names <- list.files(file.path("data", "world-happinness", "individual-years"))
i <- 1
ls_happiness_years <- list()
for(data_source in data_sources){
  df_year <- read.csv(data_source, stringsAsFactors = FALSE)
  # save each table into the list under correct name
  file_name <- data_sources_names[i]
  ls_happiness_years[[file_name]] <- df_year
  i <- i + 1
}

ls_happiness_years <- list()
for(data_source in data_sources){
  df_year <- read.csv(data_source, stringsAsFactors = FALSE)
  # save each table into the list under correct name
  file_name <- basename(data_source)
  ls_happiness_years[[file_name]] <- df_year
}

# JSON -----
#install.packages("jsonlite")
library(jsonlite)
reviews <- fromJSON("data/data-formats/reviews.json")

# xlm2 -----
