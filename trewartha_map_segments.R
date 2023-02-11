# Producing a world map of Trewartha climate zones
# This version of the script splits the large raster data files into smaller segments
# And recombines them at the end
# Jono Henderson, 1 Jan 2023

library(raster)
library(glue)
library(tidyverse)
library(svglite)

#---------------------------

### CREATE SEGMENTS ###


segments <- list()

for (long in c(-180, -90, 0, 90)) {
  for (lat in c(-90, -45, 0, 45)) {
    segment <-
      as(extent(long, long+90, lat, lat+45), 'SpatialPolygons')
    crs(segment) <- CRS('+init=EPSG:4326')
    
    segments <-
      append(segments, segment)
  }
}

#---------------------------

### REPEAT CODE FOR EACH SEGMENT ###

LOOP <- 0
# set resolution
RESOLUTION <- "30s"

for (segment in segments) {
  
  LOOP <- LOOP + 1
  
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
    list.files(path = glue("worldclim_data/temp_{RESOLUTION}/"), pattern = ".tif", full.names = TRUE) %>%
    stack()
  
  precs <-
    list.files(path = glue("worldclim_data/prec_{RESOLUTION}/"), pattern = ".tif", full.names = TRUE) %>%
    stack()
  
  elev <-
    raster(glue("worldclim_data/elevation/wc2.1_{RESOLUTION}_elev.tif"))
  
  # name the layers of raster stacks
  months <-
    c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  names(temps) <- months
  names(precs) <- months
  
  # create a color palette
  pal <-
    colorRampPalette(c("purple", "blue", "skyblue", "green", "lightgreen", "yellow", "orange", "red", "darkred"))
  
  ### CROP RASTERS TO SEGMENT ###
  
  seg_temps <-
    crop(temps, segment) %>%
    stack()
  seg_precs <-
    crop(precs, segment) %>%
    stack()
  seg_elev <-
    crop(elev, segment)
  
  #---------------------------
  
  ### SUMMARISE RASTER STACKS ###
  
  max_temp <- 
    max(seg_temps)
  
  min_temp <-
    min(seg_temps)
  
  mean_temp <-
    mean(seg_temps)
  
  total_precs <- 
    sum(seg_precs)
  
  if (ymin(segment) >= 0) {
    seg_summer_precs <-
      seg_precs %>%
      subset(4:9)
  } else {
    seg_summer_precs <-
      seg_precs %>%
      subset(c(10:12, 1:3))
  }
  
  total_summer_precs <- 
    sum(seg_summer_precs)
  
  #---------------------------
  
  ### INITIAL CLASSIFICATION LOGIC ###
  
  summer_prec_percent <-
    overlay(x = total_precs, y = total_summer_precs, fun = function(x, y){
      result <- y/x
      result[x == 0] <- 0
      return(result)
    })
  
  # using the Koppen dryness threshold
  # http://hanschen.org/koppen
  dryness_threshold <-
    (2*mean_temp + 28)*(summer_prec_percent >= 0.667) + (2*mean_temp + 14)*(summer_prec_percent < 0.667 & summer_prec_percent >= 0.333) + (2*mean_temp)*(summer_prec_percent < 0.333)
  
  aridity <-
    total_precs/dryness_threshold
  
  warm_months <-
    sum(seg_temps >= 10)
  
  arid <-
    aridity < 10 & warm_months >= 4
  
  tropical <-
    min(seg_temps) >= 18 & !arid
  
  subtropical <-
    warm_months >= 8 & !arid & !tropical
  
  temperate <-
    warm_months >= 4 & warm_months <= 7 & !arid
  
  boreal <-
    warm_months >= 1 & warm_months <= 3
  
  polar <-
    warm_months == 0
  
  #---------------------------
  
  ### ALTITUDE CORRECTION ###
  
  sea_level_temps <-
    seg_temps + 5.5*(seg_elev/1000)
  names(sea_level_temps) <- months
  
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
    (seg_elev >= 1500) * ((sl_arid & !arid) | (sl_tropical & !tropical) | (sl_subtropical & !subtropical) | (sl_temperate & !temperate) | (sl_boreal & !boreal) | (sl_polar & !polar))
  
  rm(sea_level_temps, sl_mean_temp, sl_dryness_threshold, sl_aridity, sl_warm_months, sl_arid, sl_tropical, sl_subtropical, sl_temperate, sl_boreal, sl_polar)
  gc()
  
  #---------------------------
  
  ### SPECIFIC CLASSIFICATION LOGIC ###
  
  dry_months <-
    sum(seg_precs <= 60)
  
  tropical_rainy <-
    tropical & !highland & dry_months <= 2
  
  tropical_wet_and_dry <-
    tropical & !highland & dry_months > 2
  
  arid_desert <-
    arid & !highland & aridity < 5
  
  arid_steppe <-
    arid & !highland & aridity >= 5
  
  subtropical_dry <-
    subtropical & !highland & summer_prec_percent < 0.4
  
  subtropical_humid <-
    subtropical & !highland & summer_prec_percent >= 0.4
  
  temperate_oceanic <-
    temperate & !highland & min_temp >= 0
  
  temperate_continental <-
    temperate & !highland & min_temp < 0
  
  boreal_zone <-
    boreal & !highland
  
  polar_tundra <-
    polar & !highland & max_temp >= 0
  
  polar_icecap <-
    polar & !highland & max_temp < 0
  
  ### COMBINING INTO TREWARTHA MAP ###
  
  trewartha <-
    1*tropical_rainy + 2*tropical_wet_and_dry + 3*arid_desert + 4*arid_steppe +
    5*subtropical_dry + 6*subtropical_humid + 7*temperate_oceanic + 8*temperate_continental +
    9*boreal_zone + 10*polar_tundra + 11*polar_icecap + 12*highland
  
  plot(trewartha, col = c("#990100", "#ff3300", "#ffff33", "#ff9934", "#669900", "#336601", "#0ff59b", "#0099ff", "#0066cb", "#b9b9b9", "#99ffff", "#feccff"), zlim = c(1, 12))
  
  trewartha %>%
    writeRaster(filename = glue("segments/trewartha_map_{LOOP}.tif"))
  
  png(glue("segments/trewartha_map_{LOOP}.png"), height = nrow(trewartha), width = ncol(trewartha))
  plot(trewartha, col = c("#990100", "#ff3300", "#ffff33", "#ff9934", "#669900", "#336601", "#0ff59b", "#0099ff", "#0066cb", "#b9b9b9", "#99ffff", "#feccff"), zlim = c(1, 12), maxpixels = ncell(trewartha))
  dev.off()
  
  # clear variables from memory
  temporary_rasters <-
    c(
      "seg_temps", "seg_precs", "seg_elev", "seg_summer_precs",
      "max_temp", "min_temp", "mean_temp", "total_precs", "total_summer_precs",
      "summer_prec_percent", "dryness_threshold", "aridity", "warm_months", "dry_months",
      "arid", "tropical", "subtropical", "temperate", "boreal", "polar",
      "tropical_rainy", "tropical_wet_and_dry", "arid_desert", "arid_steppe",
      "subtropical_dry", "subtropical_humid", "temperate_oceanic", "temperate_continental",
      "boreal_zone", "polar_tundra", "polar_icecap", "highland"
    )

  rm(list = temporary_rasters)
  gc()
  removeTmpFiles(h=0)
  
}

#---------------------------

### EXPORT TREWARTHA MAP ###

# load segments back in to memory

for (i in 1:16) {
  
  if (i == 1) {
    global_trewartha <-
      raster(glue("segments/trewartha_map_{i}.tif"))
  
    } else {
      
    global_trewartha <-
      raster("segments/global_trewartha.tif")      
    trewartha <-
      raster(glue("segments/trewartha_map_{i}.tif"))
    global_trewartha <-
      merge(global_trewartha, trewartha)  
  
    }
  
  plot(global_trewartha, col = c("#990100", "#ff3300", "#ffff33", "#ff9934", "#669900", "#336601", "#0ff59b", "#0099ff", "#0066cb", "#b9b9b9", "#99ffff", "#feccff"), zlim = c(1, 12))
  global_trewartha %>%
    writeRaster(filename = glue("segments/global_trewartha.tif"), overwrite = TRUE)
  
  rm(trewartha, global_trewartha)
  gc()
  removeTmpFiles(h=0)
}

global_trewartha <- raster("segments/global_trewartha.tif")
global_trewartha %>%
  writeRaster(filename = glue("output/global_trewartha.tif"), overwrite = TRUE)
global_trewartha %>%
  writeRaster(filename = glue("output/global_trewartha_8bit.tif"), datatype='INT1U', overwrite = TRUE)

png(glue("output/trewartha_map_{RESOLUTION}.png"), height = nrow(global_trewartha)/2, width = ncol(global_trewartha)/2)
plot(global_trewartha, col = c("#990100", "#ff3300", "#ffff33", "#ff9934", "#669900", "#336601", "#0ff59b", "#0099ff", "#0066cb", "#b9b9b9", "#99ffff", "#feccff"), zlim = c(1, 12), maxpixels = ncell(global_trewartha))
dev.off()
