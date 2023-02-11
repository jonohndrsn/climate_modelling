# General exploratory script
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
  raster("worldclim_data/elevation/wc2.1_30s_elev.tif")

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

#---------------------------

### OCEANIA ###

# split precs into hemispheres
oceania <-
  as(extent(100, 180, -50, -10), 'SpatialPolygons')

oceania_elev <-
  crop(elev, oceania)

oceania_elev
