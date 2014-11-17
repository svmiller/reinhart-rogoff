library(RCurl)
library(Zelig)

data <- getURL("https://raw.githubusercontent.com/svmiller/reinhart-rogoff/master/RR-processed.csv")
Data <- read.csv(text = data)

summary(Data)
