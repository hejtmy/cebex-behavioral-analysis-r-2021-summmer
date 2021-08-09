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

