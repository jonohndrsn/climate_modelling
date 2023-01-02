# Climate modelling
# Jono Henderson, 1 Jan 2023

library(raster)
library(tidyverse)

#---------------------------

### LOAD AND ORGANISE DATA ###

# data downloaded from WorldClim on 1 Jan 2023
# https://www.worldclim.org/data/worldclim21.html

#...note...#
# the data is not included in my GitHub repo but it can be downloaded separately for free
# and this script will run successfully if the data is organised
# into the directory structure that is implied by the below code
#..........#

# load climate data into raster stacks/layers
temps <- 
  list.files(path = "worldclim_data/temp_2.5m/", pattern = ".tif", full.names = TRUE) %>%
  stack()

precs <-
  list.files(path = "worldclim_data/prec_2.5m/", pattern = ".tif", full.names = TRUE) %>%
  stack()

elev <-
  raster("worldclim_data/elevation/wc2.1_2.5m_elev.tif")

# name the layers of raster stacks
months <-
  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(temps) <- months
names(precs) <- months

# create a color palette
pal <-
  colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))

# plot rasters
plot(temps, col = pal(100), zlim = c(-80, 50))
plot(precs, col = pal(100), zlim = c(0, 1400))
plot(elev, col = pal(100))

# split precs into hemispheres
nh <-
  as(extent(-180, 180, 0, 90), 'SpatialPolygons')
crs(nh) <- crs(precs)
sh <-
  as(extent(-180, 180, -90, 0), 'SpatialPolygons')
crs(sh) <- crs(precs)

nh_precs <-
  crop(precs, nh)
sh_precs <-
  crop(precs, sh)

# get summer precipitation for both hemispheres
summer_months <-
  c("Apr_Oct", "May_Nov", "Jun_Dec", "Jul_Jan", "Aug_Feb", "Sep_Mar")

nh_summer_precs <-
  nh_precs %>%
  subset(4:9)
names(nh_summer_precs) <- summer_months

sh_summer_precs <-
  sh_precs %>%
  subset(c(10:12, 1:3))
names(sh_summer_precs) <- summer_months

summer_precs <-
  merge(nh_summer_precs, sh_summer_precs) %>%
  stack()
names(summer_precs) <- summer_months

plot(summer_precs, col = pal(100), zlim = c(0, 1400))

#---------------------------

### CITIES ###

# create a data frame with sample site coordinates
cities <- 
  data.frame(
    city = c("Manchester", "Liverpool", "Oxford", "London"),
    lon = c(-2.24, -2.98, -1.25, -0.11),
    lat = c(53.47, 53.4, 51.74, 51.49),
    row.names = "city"
  )

# extract data from raster stack
cities_df <-
  cities %>%
  as_tibble(rownames = "city") %>%
  bind_cols(raster::extract(elev, cities) %>% as_tibble() %>% rename(elev=value)) %>%
  bind_cols(raster::extract(temps, cities)) %>%
  bind_cols(raster::extract(precs, cities))

#---------------------------

### MINMAX ###

max_temp <- 
  max(temps)
plot(max_temp, col = pal(100), zlim = c(-80, 50))

min_temp <-
  min(temps)
plot(min_temp, col = pal(100), zlim = c(-80, 50))

mean_temp <-
  mean(temps)
plot(mean_temp, col = pal(100), zlim = c(-80, 50))

plot(stack(max_temp, min_temp, mean_temp), col = pal(100), zlim = c(-80, 50))

total_precs <- 
  sum(precs)

total_summer_precs <- 
  sum(summer_precs)

#---------------------------

### INITIAL CLASSIFICATION LOGIC ###

summer_prec_percent <-
  overlay(x = total_precs, y = total_summer_precs, fun = function(x, y){
    result <- y/x
    result[x == 0] <- 0
    return(result)
  })
plot(summer_prec_percent)

# using the Koppen dryness threshold
# http://hanschen.org/koppen
dryness_threshold <-
  (2*mean_temp + 28)*(summer_prec_percent >= 0.667) + (2*mean_temp + 14)*(summer_prec_percent < 0.667 & summer_prec_percent >= 0.333) + (2*mean_temp)*(summer_prec_percent < 0.333)
plot(dryness_threshold)
aridity <-
  total_precs/dryness_threshold
plot(aridity, zlim = c(-1000, 1000))

warm_months <-
  sum(temps >= 10)
plot(warm_months)

arid <-
  aridity < 10 & warm_months >= 4
plot(arid)

tropical <-
  min(temps) >= 18 & !arid
plot(tropical)

subtropical <-
  warm_months >= 8 & !arid & !tropical
plot(subtropical)

temperate <-
  warm_months >= 4 & warm_months <= 7 & !arid
plot(temperate)

boreal <-
  warm_months >= 1 & warm_months <= 3
plot(boreal)

polar <-
  warm_months == 0
plot(polar)

trewartha_initial <-
  1*tropical + 2*arid + 3*subtropical + 4*temperate + 5*boreal + 6*polar
plot(trewartha_initial, col = c("red", "yellow", "limegreen", "skyblue", "blue", "cyan"), zlim = c(1, 6))

#---------------------------

### ALTITUDE CORRECTION ###

sea_level_temps <-
  temps + 5.5*(elev/1000)
names(sea_level_temps) <- months
plot(sea_level_temps, col = pal(100), zlim = c(-80, 50))

sl_mean_temp <-
  mean(sea_level_temps)

# using the Koppen dryness threshold
# http://hanschen.org/koppen
sl_dryness_threshold <-
  (2*sl_mean_temp + 28)*(summer_prec_percent >= 0.667) + (2*sl_mean_temp + 14)*(summer_prec_percent < 0.667 & summer_prec_percent >= 0.333) + (2*sl_mean_temp)*(summer_prec_percent < 0.333)
sl_aridity <-
  total_precs/sl_dryness_threshold

sl_warm_months <-
  sum(sea_level_temps >= 10)

sl_arid <-
  sl_aridity < 10 & sl_warm_months >= 4

sl_tropical <-
  min(sea_level_temps) >= 18 & !sl_arid

sl_subtropical <-
  sl_warm_months >= 8 & !sl_arid & !sl_tropical

sl_temperate <-
  sl_warm_months >= 4 & sl_warm_months <= 7 & !sl_arid

sl_boreal <-
  sl_warm_months >= 1 & sl_warm_months <= 3

sl_polar <-
  sl_warm_months == 0

highland <-
  (elev >= 1500) * ((sl_arid & !arid) | (sl_tropical & !tropical) | (sl_subtropical & !subtropical) | (sl_temperate & !temperate) | (sl_boreal & !boreal) | (sl_polar & !polar))
plot(highland)

rm(sea_level_temps, sl_mean_temp, sl_aridity, sl_warm_months, sl_arid, sl_tropical, sl_subtropical, sl_temperate, sl_boreal, sl_polar)
gc()

#---------------------------

### SPECIFIC CLASSIFICATION LOGIC ###

dry_months <-
  sum(precs <= 60)
plot(dry_months)

tropical_rainy <-
  tropical & !highland & dry_months <= 2
plot(tropical_rainy)

tropical_wet_and_dry <-
  tropical & !highland & dry_months > 2
plot(tropical_wet_and_dry)

arid_desert <-
  arid & !highland & aridity < 5
plot(arid_desert)

arid_steppe <-
  arid & !highland & aridity >= 5
plot(arid_steppe)

subtropical_dry <-
  subtropical & !highland & summer_prec_percent < 0.4
plot(subtropical_dry)

subtropical_humid <-
  subtropical & !highland & summer_prec_percent >= 0.4
plot(subtropical_humid)

temperate_oceanic <-
  temperate & !highland & min_temp >= 0
plot(temperate_oceanic)

temperate_continental <-
  temperate & !highland & min_temp < 0
plot(temperate_continental)

boreal_zone <-
  boreal & !highland
plot(boreal_zone)

polar_tundra <-
  polar & !highland & max_temp >= 0
plot(polar_tundra)

polar_icecap <-
  polar & !highland & max_temp < 0
plot(polar_icecap)

#---------------------------

### COMBINING INTO TREWARTHA MAP ###

trewartha_stack <-
  stack(
    tropical_rainy, tropical_wet_and_dry, arid_desert, arid_steppe, 
    subtropical_dry, subtropical_humid, temperate_oceanic, temperate_continental, 
    boreal_zone, polar_tundra, polar_icecap, highland
  )
names(trewartha_stack) <- 
  c(
    "tropical_rainy", "tropical_wet_and_dry", "arid_desert", "arid_steppe",
    "subtropical_dry", "subtropical_humid", "temperate_oceanic", "temperate_continental",
    "boreal_zone", "polar_tundra", "polar_icecap", "highland"
    )
plot(trewartha_stack)

trewartha <-
  1*tropical_rainy + 2*tropical_wet_and_dry + 3*arid_desert + 4*arid_steppe +
  5*subtropical_dry + 6*subtropical_humid + 7*temperate_oceanic + 8*temperate_continental +
  9*boreal_zone + 10*polar_tundra + 11*polar_icecap + 12*highland
# plot(trewartha, col = c("darkred", "red", "yellow", "orange", "limegreen", "darkgreen", "aquamarine", "skyblue", "blue", "grey", "cyan", "pink"), zlim = c(1, 12))
plot(trewartha, col = c("#990100", "#ff3300", "#ffff33", "#ff9934", "#669900", "#336601", "#0ff59b", "#0099ff", "#0066cb", "#b9b9b9", "#99ffff", "#feccff"), zlim = c(1, 12))
