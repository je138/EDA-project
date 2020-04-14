getwd()
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)

## Loading data
roanoke_rapids_raw <- read.csv(file = "./rawData/RoanokeRapids_USGS_discharge_gageHt_raw.csv")
oak_city_raw <- read.csv(file = "./rawData/OakCity_USGS_waterquality_raw.csv")
jamesville_raw <- read.csv(file = "./rawData/Jamesville_USGS_waterquality_raw.csv")

## Setting classes
str(roanoke_rapids_raw)
roanoke_rapids_raw$site_no <- as.factor(roanoke_rapids_raw$site_no)
roanoke_rapids_raw$Date <- as.Date(roanoke_rapids_raw$Date, format = "%Y-%m-%d")
roanoke_rapids_raw$X_00060_00003 <- as.numeric(roanoke_rapids_raw$X_00060_00003)

str(oak_city_raw)
oak_city_raw$site_no <- as.factor(oak_city_raw$site_no)
oak_city_raw$Date <- as.Date(oak_city_raw$Date, format = "%Y-%m-%d")
oak_city_raw$X_00095_00003 <- as.numeric(oak_city_raw$X_00095_00003)

str(jamesville_raw)
jamesville_raw$site_no <- as.factor(jamesville_raw$site_no)
jamesville_raw$Date <- as.Date(jamesville_raw$Date, format = "%Y-%m-%d")
jamesville_raw$X_00095_00003 <- as.numeric(jamesville_raw$X_00095_00003)

### Selecting columns and years of interest
min(jamesville_raw$Date)
max(jamesville_raw$Date)
min(oak_city_raw$Date)
max(oak_city_raw$Date)
min(roanoke_rapids_raw$Date)
max(roanoke_rapids_raw$Date)

roanoke_rapids_raw <- roanoke_rapids_raw[,-c(1,2,6,8)] %>% rename(mean.daily.discharge = X_00060_00003)
roanoke_rapids_raw <- roanoke_rapids_raw %>% rename(gage.height = X_00065_00003)
roanoke_rapids_processed <- roanoke_rapids_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-04-08")

jamesville_raw <- jamesville_raw[,-c(1,2,6,8,10,12)]
jamesville_raw <- jamesville_raw %>% rename(gage.height = X_00065_00003)
jamesville_raw <- jamesville_raw %>% rename(temperature = X_00010_00003)
jamesville_raw <- jamesville_raw %>% rename(specific.cond = X_00095_00003)
jamesville_raw <- jamesville_raw %>% rename(DO = X_00300_00003)
jamesville_processed <- jamesville_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-04-08")


oak_city_raw <- oak_city_raw[,-c(1,2,6,8,10,12)] %>% rename(gage.height = X_00065_00003)
oak_city_raw <- oak_city_raw %>% rename(temperature = X_00010_00003)
oak_city_raw <- oak_city_raw %>% rename(specific.cond = X_00095_00003)
oak_city_raw <- oak_city_raw %>% rename(DO = X_00300_00003)
oak_city_processed <- oak_city_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-04-08")

write.csv(roanoke_rapids_processed, file = "./Data/processedData/roanoke_rapids_processed")
write.csv(jamesville_processed, file = "./Data/processedData/jamesville_processed")
write.csv(oak_city_processed, file = "./Data/processedData/oak_city_processed")
