#' Load airline safety data into a data frame. 
#' Data are accessible in at https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv. 
#' Description of variables can be found here: https://github.com/fivethirtyeight/data/tree/master/airline-safety
#' 
df_airlines <- read.table("airline-safety.csv", header = TRUE, sep=",",
                          stringsAsFactors = FALSE)
df_airlines <- read.csv("airline-safety.csv")

# How many variables and observations are in the dataset?
str(df_airlines)

# What is each column type? Get a "feel" for the data.
summary(df_airlines)

# Print out dataset’s first 10 rows.
head(df_airlines, 10)
tail(df_airlines, 10)

# What is the variable type of incidents between 1985 and 1999?
class(df_airlines$incidents_85_99)

# How many unique airlines are there?
length(unique(df_airlines$airline))

# What is the average number and standard deviation of incidents for an airline 
# between 85-99? Round to 3 digits.
round(mean(df_airlines$incidents_85_99), 3)
round(sd(df_airlines$incidents_85_99), 3)

# What is the correlation between incidents in 85-99 and 00-14?
cor(df_airlines$incidents_85_99, df_airlines$incidents_00_14)

# Plot the number of incidents in 00-14 vs 85-99
plot(df_airlines$incidents_00_14, df_airlines$incidents_85_99)

# Plot the histogram of incidents in 00-14. Use 15 breaks
hist(df_airlines$incidents_00_14, breaks=15)

