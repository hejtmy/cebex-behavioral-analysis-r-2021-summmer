#' Load movies function
#'
#' @param n_rows number of rows to read
#'
#' @return data.frame
#' @export
#'
#' @examples
load_movies <- function(n_rows = 1000){
  pth_movies <- file.path("data", "movies", "movies_metadata.csv")
  df_movies <- read.table(pth_movies, header=TRUE, sep=",", fill=TRUE,
                        stringsAsFactors = FALSE, nrows = n_rows,
                        quote = "\"")
  return(df_movies)
}
