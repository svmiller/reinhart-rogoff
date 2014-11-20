library(RCurl)
library(Zelig)
library(countrycode)

data <- getURL("https://raw.githubusercontent.com/svmiller/reinhart-rogoff/master/RR-processed.csv")
Data <- read.csv(text = data)

summary(Data)

Data <- subset(Data, select=c("Country","Year","dRGDP","debtgdp"))
Data$ccode <- countrycode(Data$Country, "country.name", "cown")

CPDS <- read.csv("~/Dropbox/data/CPDS/CPDSI1960-2012csv.csv", sep=";")
CPDS$ccode <- countrycode(CPDS$Country,"country.name","cown")

CPDS$govleft <- as.numeric(CPDS$Gov_Left1)
CPDS$govcent <- as.numeric(CPDS$Gov_Cent1)
CPDS$govright <- as.numeric(CPDS$Gov_Right1)

CPDS.sub <- subset(CPDS, select=c("Year","ccode","govleft","govcent","govright"))

Data <- merge(Data, CPDS.sub, by=c("ccode","Year"), all.x = TRUE)

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

Data$dg0030 <- 0
Data$dg0030[Data$debtgdp > 0 & Data$debtgdp < 30] <- 1
Data$dg3060 <- 0
Data$dg3060[Data$debtgdp >= 30 & Data$debtgdp < 60] <- 1
Data$dg6090 <- 0
Data$dg6090[Data$debtgdp >= 60 & Data$debtgdp < 90] <- 1
Data$dg90plus <- 0
Data$dg90plus[Data$debtgdp >= 90] <- 1

Data$dgcat <- as.factor(Data$dgcat)
summary(Z1 <- zelig(dRGDP ~ dgcat, data = Data, model="ls"))

Z1.low <- setx(Z1, dg3060 = 0, dg6090 = 0, dg90plus = 1)
Z1.low <- setx(Z1, dgcat  = "90% and above")
Z1.sim <-sim(Z1, x = Z1.low)
summary(Z1.sim)

