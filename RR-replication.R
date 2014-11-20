library(RCurl)
library(Zelig)
library(countrycode)
library(ggplot2)
library(mgcv)

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
## These are inconsistent with their findings (but they chose to include the US in those years).
#######################################################################################################

RR.selective <- subset(Data,
                       !((Year<1950 & Country=="New Zealand") | (Year<1951 & Country=="Australia") | (Year<1951 & Country=="Canada") ))
(RR.selective.mean <- with(RR.selective, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))

# with(RR.selective, tapply( dRGDP, dgcat, mean, na.rm=TRUE ))

# Compare/contrast RR.selective.mean with RR.equalwt.mean. Look especially at New Zealand.

## Recall that RR also had a silly spreadsheet error that came from doing their work in Excel.
## This error is considered random, unless one assumes alphabetical order matters for the findings.

RR.selective.spreadsheet <- subset(RR.selective, ! Country %in% c("Australia","Austria","Belgium","Canada","Denmark") )
(RR.selective.spreadsheet.transcription <- with(RR.selective.spreadsheet, tapply( dRGDP, list(Country,dgcat), mean, na.rm=TRUE )))

## Compare RR.selective.spreadsheet.mean with RR.selective.mean.
## Look at what happened to Belgium in that right column.

## One final spreadsheet error coming up.

RR.selective.spreadsheet.transcription["New Zealand",4] <- -7.9

## And we think we've replicated RR (2010).

(RR.published.mean <- apply(RR.selective.spreadsheet.transcription,2,mean,na.rm=TRUE))
RR.published.mean.df <- data.frame(RR.published.mean , dgcat=names(RR.published.mean) )

(2.9*.143) + (2.4 *.143) + (1*.143) + (.7*.143) + (-7.9*.143) + (2.4*.143) + (-2*.143)

(3.8*.045)+(2.6*.227) + (3*.045) + (2.9*.173) + (2.4*.064) + (1*.091) + (.7*.10) + (2.6*.045) + (2.4*.173) + (-2*.036)

RR.gam <- gam(dRGDP ~ s(debtgdp, bs="cs"),data=Data)

## Cross-validation technique for loess parameters
## http://stats.stackexchange.com/questions/2002/how-do-i-decide-what-span-to-use-in-loess-regression-in-r
Graph <- ggplot(Data, aes(x=debtgdp,y=dRGDP))
Graph <- Graph + geom_vline(xintercept=90,color='lightgray',size=1.5)
Graph <- Graph + geom_point(color='darkgray') + ylab("Real GDP Growth") + xlab("Public Debt/GDP Ratio") + scale_x_continuous(breaks=seq(0,240,30)) + theme_bw()
Graph <- Graph + geom_smooth(method=gam, color='black',formula= y ~ s(x, bs = "cs"))
print(Graph)
