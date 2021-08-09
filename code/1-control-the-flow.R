# CONDOTIONALS -----
IQ <- 160
IQ2 <- 114


if(IQ > 135){
  print("GOOD FOR YOU!")
} else {
  print("GOOD")
}

if(IQ > 135){
  print("GOOD FOR YOU!")
  warning("BE CAUTIOUS!")
}

warnings()

# if IQ is larger than IQ2 print out "person 1 has larger IQ than person 2"
# if not then print the opposite

if(IQ > IQ2){
  print("")
} else {
  print("")
}

personality <- ""
extraversion <- 80 #out of 100
if(extraversion > 60){
  personality <- "extrovert"
}

personality <- ""
extraversion <- 40 #out of 100
if(extraversion > 60){
  personality <- "extrovert"
}

df_airlines <- read.table("airline-safety.csv", header = TRUE, sep=",",
                          stringsAsFactors = FALSE)

# dagerous or not dangerous
#df_airlines$dangerous_00_14 <-  
if(df_airlines$incidents_00_14 > 10) {
  print("big")
}
# FOR -----
1:10
for(i in 1:10){
  # 
  print(i)
}

for(i in seq(1,10)){
  print(i)
}

for(carrot in seq(1,10)){
  print(i)
}

for(i in seq(1,10)){
  print(i)
}
rm(i)

for(carrot in seq(1,10)){
  print(i)
}

names <- c("Lukas", "Martin", "Anna", "Monica")
for(name in names){
  print(name)
}

# paste, toupper
# df_airlines
# "Aer Lingus" -> "AIRLINE_AER LINGUS"

for(airline in df_airlines$airline){
  print(toupper(paste("airline", airline, sep="_")))
}

for(year in 2015:2020){
  print(paste(year, "data.csv", sep="_"))
}

?paste

#df_airlines$dangerous_00_14 <- 

worst_10 <- quantile(df_airlines$incidents_00_14, 0.9)

for(row in 1:56){
  if(df_airlines[row,]$incidents_00_14 > worst_10){
    df_airlines$dangerous_00_14 <- "dangerous"
  } else {
    df_airlines$dangerous_00_14 <- "not dangerous"
  }
}

# NEXT
for(i in 1:10){
  if((i %% 2) == 0) next
  print(i)
}

sessionInfo()

install.packages("tidyverse")
head(ggplot2::mpg)

str(iris)

person <- list(name = "Lukas", age = 31)

for(field in person){
  print(field)
}

for(column in iris){
  print(head(column, 3))
}

# USING IF, skip columns where column is not numeric
# is.numeric
for(column in iris){
  if(is.numeric(column)) print(mean(column, 3))
}

for(column in iris){
  if(is.numeric(column)) {
    print(mean(column, 3))
    print(sd(column, 3))
    print(range(column, 3))
  }
}

for(column in iris){
  if(!is.numeric(column)) next
  print(mean(column, 3))
  print(sd(column, 3))
  print(range(column, 3))
}

## BREAK!

for(i in 1:10){
  if(i > 5) break
  print(i)
}

sample <- rnorm(100, 10, 1)
mean(sample)

sample_of_means <- c()
for(i in 1:10000){
  sample <- rnorm(100, 10, 2) # generate sample mean = 10, sd = 2
  m <- mean(sample) # calculate mean of the sample
  sample_of_means <- c(sample_of_means, m)
}

hist(sample_of_means, breaks = 15)

for(i in 1:10000){
  sample <- rnorm(100, 10, 2) # generate sample mean = 10, sd = 2
  m <- mean(sample) # calculate mean of the sample
  if(m > 10.7){
    print("FOUND IT!!")
    break
  }
}

hist(sample)

## WHILE


# CUSTOM FUNCTIONS ------

# DRY CODE! Dont repeat yourself

create_sample_mean <- function(){
  sample <- rnorm(100, 10, 2)
  m <- mean(sample)
  return(m)
}

sample_of_means <- c()
for(i in 1:1000){
  m <- create_sample_mean()
  sample_of_means <- c(sample_of_means, m)
}

for(i in 1:1000){
  sample_of_means <- c(sample_of_means, create_sample_mean())
}

# 
report_mean_and_sd <- function(numbers){
  m <- mean(numbers)
  s <- sd(numbers)
  report <- paste("mean is ", m, " and sd is ", round(s, 2))
  return(report)
}

report_mean_and_sd(1:10)
# the average is M = 5.5(sd = 1)

# Create a function which generates normal distribution of given length
# mean of 10, sd of 2
# return text "the average of the distributions is ...." rounded to 4 decimal points

report_results <- function(n_observations){
  
}
