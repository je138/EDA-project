getwd()
library(tidyverse)
library(trend)
library(agricolae)
library(FSA)
library(GGally)


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

## Running regression of discharge on pre-/post-QRR groups
discharge.regression <- lm(roanokerapids.discharge$mean.daily.discharge ~
                             roanokerapids.discharge$qrr)
summary(discharge.regression)
# Based on statistically significant coefficient (simple linear regression; p-value < 0.05), post-QRR
# gage readings are associated with 2591.3 cubic feet per second higher discharges than pre-QRR gage readings.

#### Time series analysis
## Making time series
roanokerapids.discharge.ts <- ts(roanokerapids.discharge[[4]], frequency = 365)

## Decomposing time series into components
roanokerapids.ts.decomposed <- stl(roanokerapids.discharge.ts, s.window = 365)

plot(roanokerapids.ts.decomposed)

## Converting components to data frame
roanoke.ts.components <- as.data.frame(roanokerapids.ts.decomposed$time.series[,1:3])
## Adding original data and dates
roanoke.ts.components <- roanoke.ts.components %>%
  mutate(original = roanokerapids.discharge$mean.daily.discharge,
         Date = roanokerapids.discharge$Date)

## Graphing original data with trend
ggplot(roanoke.ts.components) +
  geom_line(aes(x= Date, y=original)) +
  geom_smooth(aes(x= Date, y= trend), method = lm) +
  geom_line(aes(x=Date, y=trend), color = "red") +
  ylab(expression(paste("Discharge (ft"^"3","/s)")))

####### Analyzing effect of discharge on downstream water indicators
## Combining roanoke rapids and oak city data frames
rr.oakcity <- left_join(roanokerapids.discharge, oakcity.gage, by = "Date")

## Correlation matrix

scatterplot.matrix <- ggpairs(rr.oakcity, columns = c(4,9,11,12))
scatterplot.matrix

## Adding column with log(mean.daily.discharge)
rr.oakcity <- rr.oakcity %>% mutate(log.discharge = log(mean.daily.discharge))

## Running regressions
conductance.regression <- lm(rr.oakcity$specific.cond ~ rr.oakcity$mean.daily.discharge)
summary(conductance.regression)
conductance.log.regression <- lm(rr.oakcity$specific.cond ~ rr.oakcity$log.discharge)
summary(conductance.log.regression)

ggplot(data = rr.oakcity, aes(y= specific.cond, x=log(mean.daily.discharge))) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Log Mean Daily Discharge") +
  ylab(expression(paste("Specific Conductance at 25",~degree,"C")))

# Coeff has p-value < .05; mean daily discharge is a significant predictor of specific conductance

temp.regression <- lm(rr.oakcity$temperature ~ rr.oakcity$mean.daily.discharge)
summary(temp.regression)
# Coeff has p-value < .05; mean daily discharge is a significant predictor of temperature
temp.log.regression <- lm(rr.oakcity$temperature ~ rr.oakcity$log.discharge)
summary(temp.log.regression)

ggplot(data = rr.oakcity, aes(y=rr.oakcity$temperature, x= rr.oakcity$mean.daily.discharge)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlab("Mean Daily Discharge") +
  ylab(expression(paste("Temperature (",~degree,"C)")))

DO.regression <- lm(rr.oakcity$DO ~ rr.oakcity$mean.daily.discharge)
summary(DO.regression)
# Coeff has p-value < .05
## The coefficients for all three regressions were very small, indicating a significant but weak trend

DO.temp.interaction.regression <- lm(rr.oakcity$DO ~ rr.oakcity$mean.daily.discharge*rr.oakcity$temperature)
summary(DO.temp.interaction.regression)
## Mean daily discharge is no longer a significant predictor of dissolved oxygen when interaction between
## temp and discharge is included

test <- lm(rr.oakcity$temperature ~ roanoke.ts.components$trend)
summary(test)


### ANOVA tests on water quality indicators

test2 <- kruskal.test(rr.oakcity$temperature ~ rr.oakcity$qrr)
test2
