getwd()
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)

## Loading data
roanoke_rapids_raw <- read.csv(file = "./Data/rawData/RoanokeRapids_USGS_discharge_gageHt_raw.csv")
oak_city_raw <- read.csv(file = "./Data/rawData/OakCity_USGS_waterquality_raw.csv")
jamesville_raw <- read.csv(file = "./Data/rawData/Jamesville_USGS_waterquality_raw.csv")

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

### Selecting columns and years of interest (and removing all observations not "Approved for Publication")
roanoke_rapids_appr <- roanoke_rapids_raw %>% filter(X_00065_00003_cd == "A" & X_00060_00003_cd == "A")
min(roanoke_rapids_appr$Date)
max(roanoke_rapids_appr$Date)
jamesville_appr <- jamesville_raw %>% filter(X_00065_00003_cd == "A" & X_00010_00003_cd == "A" &
                                               X_00095_00003_cd == "A" & X_00300_00003_cd == "A")
min(jamesville_appr$Date)
max(jamesville_raw$Date)
oak_city_appr <- oak_city_raw %>% filter(X_00065_00003_cd == "A" & X_00010_00003_cd == "A" &
                                           X_00095_00003_cd == "A" & X_00300_00003_cd == "A")
min(oak_city_appr$Date)
max(oak_city_appr$Date)

roanoke_rapids_raw <- roanoke_rapids_raw[,-c(1,2,6,8)] %>% rename(mean.daily.discharge = X_00060_00003)
roanoke_rapids_raw <- roanoke_rapids_raw %>% rename(gage.height = X_00065_00003)
roanoke_rapids_processed <- roanoke_rapids_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-02-11")

jamesville_raw <- jamesville_raw[,-c(1,2,6,8,10,12)]
jamesville_raw <- jamesville_raw %>% rename(gage.height = X_00065_00003)
jamesville_raw <- jamesville_raw %>% rename(temperature = X_00010_00003)
jamesville_raw <- jamesville_raw %>% rename(specific.cond = X_00095_00003)
jamesville_raw <- jamesville_raw %>% rename(DO = X_00300_00003)
jamesville_processed <- jamesville_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-02-11")


oak_city_raw <- oak_city_raw[,-c(1,2,6,8,10,12)] %>% rename(gage.height = X_00065_00003)
oak_city_raw <- oak_city_raw %>% rename(temperature = X_00010_00003)
oak_city_raw <- oak_city_raw %>% rename(specific.cond = X_00095_00003)
oak_city_raw <- oak_city_raw %>% rename(DO = X_00300_00003)
oak_city_processed <- oak_city_raw %>% filter(Date >= "2007-10-01" & Date <= "2020-02-11")

write.csv(roanoke_rapids_processed, file = "./Data/processedData/roanoke_rapids_USGS_processed.csv")
write.csv(jamesville_processed, file = "./Data/processedData/jamesville_USGS_processed.csv")
write.csv(oak_city_processed, file = "./Data/processedData/oakcity_USGS_processed.csv")
