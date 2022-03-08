#===============================================================================
# Extract covariates within a 20 meter buffer zone for the habitat 
# classification.
# Load in packages -------------------------------------------------------------
library(tidyverse)
library(sf)
library(raster)
library(terra)
library(here)
# Read in data -----------------------------------------------------------------
# DEM data (stored on external drives)
dem <- rast("D:/data/dem/tcma_dem_dm.tif") #Stored in decimeters
slope <- rast("D:/data/dem/tcma_slope.tif")
aspect <- rast("D:/data/dem/tcma_aspect.tif")
# Movement data
if (exists("movement")) {
  message("Pre-processed biologicals data found. Nice.")
  gps <- movement %>% filter(complete.cases(movement[, c("gps_utm_easting", 
                                                         "gps_utm_northing", 
                                                         "gps_fix_time")]))
  drops <- c("activity_fix_time", "temperature_fix_time", "geometry",
             "gps_altitude", "activity_count", "temperature", "gps_longitude", 
             "gps_latitude", "acquisition_start_time")
  gps <- gps[, !(names(gps) %in% drops)]
  rm(drops)
} else {
  message(
    "Pre-processed movement data has not been found. Damn. Processing now."
  )
  # Load in data
  gps <- read.csv(here("data/processed_data/gps_data.csv"))
  # Coerce all variables into the right format
  gps <- gps %>%
    mutate_at(vars(animal_id, species), funs(as.factor)) %>%
    mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, 
                   gps_utm_easting, gps_altitude), 
              funs(as.numeric)) %>%
    mutate_at(vars(gps_fix_time), funs(as.POSIXct))
}
gps$uid <- paste(gps$animal_id, "_", rownames(gps))
gps_sf <- gps %>% filter(complete.cases(gps[, c("gps_utm_easting", 
                                                "gps_utm_northing", 
                                                "gps_fix_time")])) 
gps_sf<- st_as_sf(gps, coords = c("gps_utm_easting", "gps_utm_northing"))
# Add average elevation w/in 20 m ----------------------------------------------
# Start loop -------------------------------------------------------------------
for (i in 1:nrow(gps_sf)) {
  # Create 20 m buffer
  buff_20 <- st_buffer(gps_sf$geometry[i], dist = 20)
  # Extract raster values within that buffer
  buff_rast_dem <- terra::extract(dem, vect(buff_20))
  # Exclude any NA values introduced
  buff_rast_dem <- buff_rast_dem[!is.na(buff_rast_dem$tcma_dem_dm), ]
  # Average the DEM values
  ave_dem <- mean(buff_rast_dem$tcma_dem_dm)
  # Append to columns of the movement data as a proportion
  gps_sf[i, "dem"] <- ave_dem
}
rm(buff_rast_dem, buff_20, i, ave_dem, dem)
# End loop ---------------------------------------------------------------------
# Convert the DEM from decimeters to meters, rounded to 1 decimal point
gps_sf$dem <- as.numeric(format(round((gps_sf$dem / 10), 1), nsmall = 1))
# Add average slope w/in 20 m --------------------------------------------------
# Start loop -------------------------------------------------------------------
for (i in 1:nrow(gps_sf)) {
  # Create 20 m buffer
  buff_20 <- st_buffer(gps_sf$geometry[i], dist = 20)
  # Extract raster values within that buffer
  buff_rast_slope <- terra::extract(slope, vect(buff_20))
  # Exclude any NA values introduced
  buff_rast_slope <- buff_rast_slope[!is.na(buff_rast_slope$tcma_slope), ]
  # Average the DEM values
  ave_slope <- mean(buff_rast_slope$tcma_slope)
  # Append to columns of the movement data as a proportion
  gps_sf[i, "slope"] <- ave_slope
}
rm(buff_rast_slope, buff_20, i, ave_slope, slope)
# End loop ---------------------------------------------------------------------
# Add average aspect w/in 20 m -------------------------------------------------
# Start loop -------------------------------------------------------------------
for (i in 1:nrow(gps_sf)) {
  # Create 20 m buffer
  buff_20 <- st_buffer(gps_sf$geometry[i], dist = 20)
  # Extract raster values within that buffer
  buff_rast_aspect <- terra::extract(aspect, vect(buff_20))
  # Exclude any NA values introduced
  buff_rast_aspect <- buff_rast_aspect[!is.na(buff_rast_aspect$tcma_aspect), ]
  # Average the DEM values
  ave_aspect <- mean(buff_rast_aspect$tcma_aspect)
  # Append to columns of the movement data as a proportion
  gps_sf[i, "aspect"] <- ave_aspect
}
rm(buff_rast_aspect, buff_20, i, ave_aspect, aspect)
# End loop ---------------------------------------------------------------------
# Clean and join data before exporting -----------------------------------------
drops <- c("animal_id", "species", "gps_fix_time", "geometry")
# Filter out rows
gps_sf <- gps_sf[, !(names(gps_sf) %in% drops)]
rm(drops)
# Join back into gps data file
gps <- gps %>%
  full_join(gps_sf, by = "uid")
gps$uid <- NULL
rm(gps_sf)
# Export as csv
gps %>%
  write.csv(here("data/processed_data/dem_etc_appended.csv"), row.names = FALSE)
rm(gps)
#===============================================================================