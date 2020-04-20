getwd()
setwd("/Users/jackeynon/Courses/EnvDataAnalytics/Environmental_Data_Analytics_2020/EDA-project/")
library(tidyverse)
library(lubridate)
library(trend)
library(zoo)
library(cowplot)

mytheme <- theme_classic(base_size = 14) +
  theme(axis.text = element_text(color = "black"), 
        legend.position = "top")
theme_set(mytheme)

## Reading in .csv
roanokerapids_discharge <- read_csv("./Data/processedData/roanoke_rapids_USGS_processed.csv")
str(roanokerapids_discharge)
roanokerapids_discharge$Date <- as_date(roanokerapids_discharge$Date, format = "%Y-%m-%d")

interval(first(roanokerapids_discharge$Date), roanokerapids_discharge$Date[3196])

## Making time series
roanokerapids_discharge.ts <- ts(roanokerapids_discharge[[4]], frequency = 365)

## Decomposing time series into components
roanokerapids_ts_decomposed <- stl(roanokerapids_discharge.ts, s.window = 365)

plot(roanokerapids_ts_decomposed)

## Plotting daily discharges over time with marker for QRR implementation
ggplot(data = roanokerapids_discharge, aes(x= Date, y= mean.daily.discharge)) +
  geom_line() +
  ylab("Mean Daily Discharge") +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2)


####################################################################################################

## Subsetting data into pre- and post-QRR
preQRR.roanokerapids <- roanokerapids_discharge %>% filter(
  Date <= as.numeric(roanokerapids_discharge$Date[3196]))
postQRR.roanokerapids <- roanokerapids_discharge %>% filter(
  Date > as.numeric(roanokerapids_discharge$Date[3196]))

roanokerapids_discharge <- roanokerapids_discharge %>% 
  mutate(qrr = ifelse(Date <= as.numeric(roanokerapids_discharge$Date[3196]),
                                               "pre", "post"))

summary(preQRR.roanokerapids$mean.daily.discharge)
summary(postQRR.roanokerapids$mean.daily.discharge)

ggplot(data = roanokerapids_discharge, aes(y= mean.daily.discharge, x= qrr)) +
  geom_violin() +
  xlab("Pre or Post-QRR") +
  ylab("Mean Daily Discharge (cubic feet/sec)")


