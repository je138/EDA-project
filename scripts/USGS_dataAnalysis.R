getwd()
library(tidyverse)
library(trend)
library(agricolae)
library(FSA)


## Importing data
roanokerapids.discharge <- read.csv("./Data/processedData/roanoke_rapids_USGS_processed.csv")
oakcity.gage <- read.csv("./Data/processedData/oakcity_USGS_processed.csv")

## Checking and setting classes
str(roanokerapids.discharge)
roanokerapids.discharge$Date <- as.Date(roanokerapids.discharge$Date, format = "%Y-%m-%d")
roanokerapids.discharge$mean.daily.discharge <- as.numeric(roanokerapids.discharge$mean.daily.discharge)
str(oakcity.gage)
oakcity.gage$Date <- as.Date(oakcity.gage$Date, format = "%Y-%m-%d")
oakcity.gage$specific.cond <- as.numeric(oakcity.gage$specific.cond)

#### Running a one way ANOVA on mean daily discharge at Roanoke Rapids pre- and post-QRR

## Subsetting into pre- and post-QRR
roanokerapids.discharge <- roanokerapids.discharge %>%
  mutate(qrr = ifelse(Date <= as.numeric(roanokerapids.discharge$Date[3196]),
                      "pre", "post"))

preQRR.discharge <- roanokerapids.discharge %>% filter(qrr == "pre")
postQRR.discharge <- roanokerapids.discharge %>% filter(qrr == "post")

## Checking for normality
shapiro.test(preQRR.discharge$mean.daily.discharge) ## p-value < 0.05, not well approx. by normal distribution
shapiro.test(postQRR.discharge$mean.daily.discharge) ## p-value < 0.05, not well approx. by normal distribution
qqnorm(preQRR.discharge$mean.daily.discharge)
qqnorm(postQRR.discharge$mean.daily.discharge)

## Testing for equal variance
var.test(preQRR.discharge$mean.daily.discharge, postQRR.discharge$mean.daily.discharge)
bartlett.test(roanokerapids.discharge$mean.daily.discharge ~ roanokerapids.discharge$qrr)
# p-value < 0.05; reject null hypothesis of equal variance; one variance is not equal to the other

## Assumptions of normality and equal variance are violated; will run non-parametric Kruskal-Wallis test
discharge.kw <- kruskal.test(roanokerapids.discharge$mean.daily.discharge ~
                               roanokerapids.discharge$qrr)
discharge.kw
# With a p-value < 0.05, we can reject the null hypothesis and conclude that mean daily discharge is
# different pre- and post-QRR implementation

## Post-hoc Dunn test
dunn.test::dunn.test(roanokerapids.discharge$mean.daily.discharge, roanokerapids.discharge$qrr)
# p-value < 0.05 confirms that mean daily discharges differ significantly in time periods
# Based on results of kruskal-wallis test and boxplots, we can conclude that post-QRR discharges are
# greater than pre-QRR discharges, which is consistent with the QRR regime and our hypthesis.

