library(RCurl)
library(Zelig)

data <- getURL("https://raw.githubusercontent.com/svmiller/reinhart-rogoff/master/RR-processed.csv")
Data <- read.csv(text = data)

summary(Data)

Data$dgcat <- NA
Data$dgcat[Data$debtgdp > 0 & Data$debtgdp < 30] <- "0-30%"
Data$dgcat[Data$debtgdp >= 30 & Data$debtgdp < 60] <- "30-60%"
Data$dgcat[Data$debtgdp >= 60 & Data$debtgdp < 90] <- "60-90%"
Data$dgcat[Data$debtgdp >= 90] <- "90% and above"

summary(M1 <- lm(dRGDP ~ factor(dgcat), data=Data))

Data$dgcat2 <- NA
Data$dgcat2[Data$debtgdp > 0 & Data$debtgdp < 30] <- "0-30%"
Data$dgcat2[Data$debtgdp >= 30 & Data$debtgdp < 60] <- "30-60%"
Data$dgcat2[Data$debtgdp >= 60 & Data$debtgdp < 90] <- "60-90%"
Data$dgcat2[Data$debtgdp >= 90 & Data$debtgdp < 120] <- "90-120%"
Data$dgcat2[Data$debtgdp >= 120] <- "Above 120%"

summary(M2 <- lm(dRGDP ~ factor(dgcat2), data=Data))
