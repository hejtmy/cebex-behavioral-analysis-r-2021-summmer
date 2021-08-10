#' Load movies function
#'
#' @param n_rows number of rows to read
#'
#' @return data.frame
#' @export
#'
#' @examples
load_olympics <- function(n_rows = 1000){
  # function which loads the olypics dataset
  # one parameters n_rows
  # if the number of rows is 0 or less - read all the rows
  pth_file <- file.path("data", "olympics/", "athlete_events.csv")
  
  if(n_rows <= 0){
    df_olympics <- read.table("data/olympics/athlete_events.csv",
                              sep = ",", header = TRUE, stringsAsFactors = FALSE)
  } else {
    df_olympics <- read.table("data/olympics/athlete_events.csv",
                              sep = ",", header = TRUE, stringsAsFactors = FALSE,
                              nrows = n_rows)
  }
  df_olympics <- df_olympics %>%
      mutate(Medal = factor(Medal, levels = c("Bronze", "Silver", "Gold"), 
                            ordered = TRUE))
  return(df_olympics)
}

