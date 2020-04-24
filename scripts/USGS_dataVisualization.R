getwd()
setwd("/Users/jackeynon/Courses/EnvDataAnalytics/Environmental_Data_Analytics_2020/EDA-project/")
library(tidyverse)
library(lubridate)
library(trend)
library(zoo)
library(cowplot)
library(here)
library(purrr)
library(sf)
library(ggmap)
library(rgdal)
library(sp)

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
  ylab(expression(paste("Mean Daily Discharge (ft"^"3","/s)"))) +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2)

## Creating variable for pre- and post-QRR
roanokerapids_discharge <- roanokerapids_discharge %>% 
  mutate(qrr = ifelse(Date <= as.numeric(roanokerapids_discharge$Date[3196]),
                      "pre", "post"))

## Aggregating discharge values into monthly averages
monthly.discharge <- roanokerapids_discharge %>% mutate(month = month(Date), year = year(Date))
monthly.discharge <- monthly.discharge %>% select(2:8) %>% group_by(month, year) %>%
  summarise(mean.monthly.discharge = mean(mean.daily.discharge),
            max.monthly.discharge = max(mean.daily.discharge))

monthly.discharge$Date <- as.Date(with(monthly.discharge,
                                        paste(year, month, 1, sep = "-")), "%Y-%m-%d")

monthly.discharge <- monthly.discharge %>%
  mutate(qrr = ifelse(Date <= as.numeric(roanokerapids_discharge$Date[3196]),
                      "pre", "post"))

ggplot(data = monthly.discharge, aes(x=Date, y=mean.monthly.discharge)) +
  geom_line()

ggplot(data = monthly.discharge, aes(x=Date, y=max.monthly.discharge)) +
  geom_line()

####################################################################################################

## Subsetting data into pre- and post-QRR
preQRR.roanokerapids <- roanokerapids_discharge %>% filter(
  Date <= as.numeric(roanokerapids_discharge$Date[3196]))
postQRR.roanokerapids <- roanokerapids_discharge %>% filter(
  Date > as.numeric(roanokerapids_discharge$Date[3196]))

## Getting descriptive statistics of mean daily discharge, pre- and post-QRR
preQRR.discharge.stats <- summary(preQRR.roanokerapids$mean.daily.discharge)
postQRR.discharge.stats <- summary(postQRR.roanokerapids$mean.daily.discharge)

discharge.stats <- rbind(preQRR.discharge.stats, postQRR.discharge.stats)

## Changing factor levels so pre-QRR is shown as leftmost plot
roanokerapids_discharge$qrr <- factor(roanokerapids_discharge$qrr, levels = c("pre", "post"))

## Importing data from Oak City gage station
oakcity_USGS_processed <- read.csv("./Data/processedData/oakcity_USGS_processed.csv")
oakcity_USGS_processed$Date <- as.Date(oakcity_USGS_processed$Date, format = "%Y-%m-%d")
str(oakcity_USGS_processed)
oakcity_USGS_processed$specific.cond <- as.numeric(oakcity_USGS_processed$specific.cond)

rr.oakcity.join <- left_join(roanokerapids_discharge, oakcity_USGS_processed, by = "Date")

rr.oakcity.join <- rr.oakcity.join %>% mutate(over.20k.cfs = ifelse(mean.daily.discharge > 20000, "yes",
                                                                    "no"))

## Creating violin and boxplots of variables of interest (disharge, DO, conductance, temp)
a <- ggplot(data = roanokerapids_discharge, aes(y= mean.daily.discharge, x= qrr, color = qrr)) +
  geom_violin() +
  scale_color_brewer(palette = "Paired", direction = 1) +
  xlab("") +
  ylab(expression(paste("Mean Daily Discharge (ft"^"3","/s)"))) +
  theme(legend.position = "none", axis.title = element_text(size = 10))
b <- ggplot(data = rr.oakcity.join, aes(y=DO, x=qrr, color = qrr)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired", direction = 1) +
  xlab("") +
  ylab("Dissolved Oxygen (mg/L)") +
  theme(legend.position = "none", axis.title = element_text(size = 10))
c <- ggplot(data = rr.oakcity.join, aes(y=specific.cond , x=qrr, color = qrr)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired", direction = 1) +
  xlab("") +
  ylab(expression(paste("Specific Conductance at 25",~degree,"C")) ) +
  theme(legend.position = "none", axis.title = element_text(size = 10))
d <- ggplot(data = rr.oakcity.join, aes(y=temperature , x=qrr, color = qrr)) +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired", direction = 1) +
  xlab("") +
  ylab(expression(paste("Temperature ", ~degree, "C"))) +
  theme(legend.position = "none", axis.title = element_text(size = 10))

## Creating caption
caption <- ggdraw() +
  draw_label("A) Mean daily discharge at Roanoke Rapids gage before and after Army Corps implementation
             of quasi-run-of-river (QRR) flood management regime. B) Dissolved oxygen content in 
             milligrams per liter at downstream Oak City gage station before and after QRR. 
             C) Specific conductance at 25 degrees Celsius at Oak City gage station before and after QRR.
             D) Temperature in degrees Celsius at Oak City gage station before and after QRR.")

## Combining violin and boxplots using cowplot
cowplot <- plot_grid(a, b, c, d, nrow = 2, align = 'v', rel_heights = c(1.25, 1),
                     labels = c('A', 'B', 'C', 'D'), label_x = 0.1)
cowplot

## Examining mean daily discharge at Roanoke Rapids with frequency polygon
ggplot(data = roanokerapids_discharge, aes(lty=roanokerapids_discharge$qrr)) +
  geom_freqpoly(aes(x=mean.daily.discharge))


####################################################################################################

## Visualizing the days where mean daily discharge exceeded 20,000 cfs
ggplot(data = rr.oakcity.join, aes(x= Date, y= mean.daily.discharge, color = over.20k.cfs)) +
  geom_point() +
  ylab(expression(paste("Mean Daily Discharge (ft"^"3","/s)"))) +
  labs(color = "Over 20,000 cfs?", size = 8) +
  scale_color_brewer(palette = 2, direction = 1) +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2) +
  theme(axis.title.y = element_text(size = 11),
        axis.title.x = element_text(size = 11))

## Visualizing mean daily discharge with downstream water quality indicators (specific conductance)
ggplot(data = rr.oakcity.join, aes(x= Date, y= mean.daily.discharge, color = specific.cond)) +
         geom_point() +
  ylab("Mean Daily Discharge (cubic ft/sec)") +
  labs(color = expression(paste("Specific Conductance at 25", ~degree, "C"))) +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2)

## Visualizing mean daily discharge with downstream water quality indicators (temperature)
ggplot(data = rr.oakcity.join, aes(x= Date, y= mean.daily.discharge, color = temperature)) +
  geom_point() +
  ylab("Mean Daily Discharge (cubic ft/sec)") +
  labs(color = expression(paste("Temperature", ~degree, "C"))) +
  scale_color_gradient(low = "blue", high = "yellow") +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2)

## Visualizing mean daily discharge with downstream water quality indicators (specific conductance)
ggplot(data = rr.oakcity.join, aes(x= Date, y= mean.daily.discharge, color = DO)) +
  geom_point() +
  ylab("Mean Daily Discharge (cubic ft/sec)") +
  labs(color = expression(paste("Dissolved Oxygen (mg/L)"))) +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2)

## Compiling summary statistics for water quality indicators
preQRR <- rr.oakcity.join %>% filter(qrr == "pre")
postQRR <- rr.oakcity.join %>% filter(qrr == "post")

preQRR.conductance <- summary(preQRR$specific.cond)
postQRR.conductance <- summary(postQRR$specific.cond)
conductance.stats <- rbind(preQRR.conductance, postQRR.conductance)

preQRR.DO <- summary(preQRR$DO)
postQRR.DO <- summary(postQRR$DO)
DO.stats <- rbind(preQRR.DO, postQRR.DO)

preQRR.temp <- summary(preQRR$temperature)
postQRR.temp <- summary(postQRR$temperature)
temp.stats <- rbind(preQRR.temp, postQRR.temp)

## Visualizing mean daily discharge over time at Roanoke Rapids gage station
mdd <- ggplot(data = roanokerapids_discharge, aes(x= Date, y= mean.daily.discharge)) +
  geom_line() +
  ylab("Mean Daily Discharge") +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2) +
  geom_smooth(method = lm, size = 0.5) +
  theme(axis.title.y = element_text(vjust = 2, size = 11))

## Visualizing specific conductance over time at Oak City gage station
sc <- ggplot(data = oakcity_USGS_processed, aes(x=Date, y= specific.cond)) +
  geom_line() +
  ylab("Specific Conductance") +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2) +
  geom_smooth(method = lm, size = 0.5) +
  xlab("") +
  theme(axis.title.y = element_text(vjust = 2, hjust = 0.85, size = 11))

## Visualizing temperature over time at Oak City gage station
temp <- ggplot(data = oakcity_USGS_processed, aes(x= Date, y= temperature)) +
  geom_line() +
  ylab(expression(paste("Temperature"))) +
  geom_vline(data = roanokerapids_discharge, 
             xintercept = as.numeric(roanokerapids_discharge$Date[3196]), color = "red",
             linetype = 2) +
  geom_smooth(method = lm, size = 0.5) + 
  xlab("") +
  theme(axis.title.y = element_text(vjust = -5, size = 11))
temp
## Plot exhibits negative trend in specific conductance before QRR was implemented
ggplot(data = preQRR, aes(x=Date, y= specific.cond)) +
  geom_line() +
  ylab("Specific Conductance") +
  geom_smooth(method = lm)

plot_grid(sc, temp, mdd, nrow = 3, align = 'v', rel_heights = c(1.25, 1))


##############################################################################################
## Mapping gage stations
spatial.data <- here::here("Data", "processedData", "RoanokeRapids_USGS_spatial.csv")
rr.spatial <- sf::read_sf(spatial.data)

spatial.data2 <- here("Data", "processedData", "OakCity_USGS_spatial.csv")
oak.city.spatial <- sf::read_sf(spatial.data2)
class(oak.city.spatial)
oak.city.spatial <- oak.city.spatial[,8:9]
oak.city.spatial$dec_lat_va <- as.numeric(oak.city.spatial$dec_lat_va)
oak.city.spatial$dec_long_va <- as.numeric(oak.city.spatial$dec_long_va)
coordinates(oak.city.spatial) <- ~dec_long_va + dec_lat_va
proj4string(oak.city.spatial)
proj4string(oak.city.spatial) = CRS(my_proj4)
oak.city.spatial <- st_as_sf(oak.city.spatial)
class(oak.city.spatial)
oak.city.geometry <- oak.city.spatial %>% st_geometry()

bas_nonref_SEPlains <- st_read(here("Data", "processedData", "Spatial", "bas_nonref_SEPlains.shp"))
southeast.state.bounds <- st_read(here("Data", "processedData", "Spatial", "southeast_state_bounds.shp"))
gages.spatial <- st_read(here("Data", "processedData", "Spatial", "gagesII_9322_sept30_2011.shp"))

roanoke.river.gage <- gages.spatial %>% filter(STAID == "02080500") %>% st_geometry()
st_crs(roanoke.river.gage) <- my_proj4

rr.loc <- bas_nonref_SEPlains %>% filter(GAGE_ID == "02080500")
st_transform(rr.loc, my_epsg)

nc.va.state.bounds <- southeast.state.bounds[4:5,] %>% st_geometry()
class(nc.va.state.bounds)
st_crs(nc.va.state.bounds)

coordinates(rr.location) <-~dec_lat_va+dec_long_va
my_epsg <- 5070 ## NAD83
my_proj4 = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
st_crs(rr.location) <- my_epsg
nc.va.state.bounds <- st_transform(nc.va.state.bounds, my_proj4)

## Setting projection of roanoke river gage station
class(rr.spatial)
str(rr.spatial)
rr.spatial$dec_lat_va <- as.numeric(rr.spatial$dec_lat_va)
rr.spatial$dec_long_va <- as.numeric(rr.spatial$dec_long_va)
coordinates(rr.spatial) <- ~dec_long_va + dec_lat_va
proj4string(rr.spatial)
proj4string(rr.spatial) = CRS(my_proj4)
rr.spatial <- as.data.frame(rr.spatial)

get_map(maptype = "terrain")

pdf(here("outputs", "check.pdf"), width = 11, height = 8.5)
ggplot() +
  geom_sf(data = rr.loc) +
  geom_sf(data = nc.va.state.bounds, fill = NA) +
  geom_sf(data = oak.city.gage, color = "dark green") +
  geom_sf(data = oak.city.geometry, color = "purple") +
  theme_bw()
dev.off()
