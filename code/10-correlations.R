library(correlation)
library(corrplot)

library(performance)

source("data/big5/load-big5.R")

M = cor(select(df_big5, starts_with("E", ignore.case = FALSE)))
corrplot(M, method = 'number') # colorful number
