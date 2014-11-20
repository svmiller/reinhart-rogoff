library(RCurl)
library(Zelig)
library(countrycode)

data <- getURL("https://raw.githubusercontent.com/svmiller/reinhart-rogoff/master/RR-processed.csv")
Data <- read.csv(text = data)

summary(Data)

Data <- subset(Data, select=c("Country","Year","dRGDP","debtgdp"))
Data$ccode <- countrycode(Data$Country, "country.name", "cown")

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

Data$dgcat <- as.factor(Data$dgcat)
summary(Z1 <- zelig(dRGDP ~ dgcat, data = Data, model="ls"))

Z1.low <- setx(Z1, dgcat  = "90% and above")
summary(Z1.sim <-sim(Z1, x = Z1.low))

## Compare expected values of Y from Z1.sim with the means.
## The expected value of Y we got makes sense with the means, but not what RR reported.

(RR.correct.mean <- with(Data, tapply( dRGDP, dgcat, mean, na.rm=TRUE )))
RR.correct.mean.df <- data.frame(RR.correct.mean, dgcat=names(RR.correct.mean) )

## Let's start retracing RR's steps, starting first with equal weight means.

(RR.equalwt.mean <- with(Data, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))

## Recall that RR chose to omit Aussieland, New Zealand, and Canada for the first few years after WWII.
## These observations are inconsistent with their findings. They chose to omit it (despite including the US in these years).


RR.selective <- subset(Data,
                       !((Year<1950 & Country=="New Zealand") | (Year<1951 & Country=="Australia") | (Year<1951 & Country=="Canada") ))
(RR.selective.mean <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))

with(RR.selective, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))


RR.selective.spreadsheet <- subset(RR.selective, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
RR.selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE ))

RR.selective.spreadsheet.transcription["New Zealand",4] <- -7.9

(RR.published.mean <- apply(RR.selective.spreadsheet.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )
