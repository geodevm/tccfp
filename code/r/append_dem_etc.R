#===============================================================================
### Extract covariates within a 20 meter buffer zone for the elevation related
### variables.
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Load in packages -------------------------------------------------------------
library(here)
library(lubridate)
library(parallel)
library(sf)
library(terra)
library(tidyverse)
# Set the buffer distance, and storage for parameters --------------------------
params <- tibble(
  buff_dist = 20,
  cores = as.numeric(detectCores())
)
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
  message("Pre-processed movement data has not been found. Processing now.")
  # Load in data
  gps <- read.csv(here("data/processed_data/gps_data.csv"))
  # Coerce all variables into the right format
  gps <- gps %>%
    mutate_at(vars(animal_id, species), funs(as.factor)) %>%
    mutate_at(vars(gps_utm_northing, gps_utm_easting), 
              funs(as.numeric)) %>%
    mutate_at(vars(gps_fix_time), funs(as_datetime)) %>%
    select(animal_id, species, gps_utm_northing, gps_utm_easting, gps_fix_time)
}
gps$uid <- paste0(gps$animal_id, "_", rownames(gps))
gps_sf <- gps %>% filter(complete.cases(gps[, c("gps_utm_easting", 
                                                "gps_utm_northing", 
                                                "gps_fix_time")])) 
gps_sf <- st_as_sf(gps, coords = c("gps_utm_easting", "gps_utm_northing"))
st_crs(gps_sf) <- 26915
# TCMA outline
outline <- st_read(here("data/gis_layers/metro_outline/metro_outline.shp"), 
                   quiet = TRUE) %>%
  # Get rid of buffer edge cases
  st_buffer(dist = -params$buff_dist) 
# Pre-iteration processing -----------------------------------------------------
# Remove edge cases by doing a negative buffer distance of the outline. We want 
# to clip the gps data by the extent of the TCMA since that is our data extent 
# for this spatial layer. We'll do a clip.
st_agr(gps_sf) = "constant"
st_agr(outline) = "constant"
gps_sf <- st_intersection(gps_sf, outline$geometry)
# Buffer for extraction
buff <- st_buffer(gps_sf, dist = params$buff_dist)
rm(outline)
# Define functions -------------------------------------------------------------
# The aspect will be a circular average, so we'll to calculate it as such
# Extract sine and cosine of a value
sin_cos <- function(x) {
  c(sin(x), cos(x))
}
# Calculate the mean of a list of angles, entered as degrees
mean_rads <- function(x) {
  # Convert to radians
  rads_in <- x * (pi / 180)
  # Extract sine/cosine
  coords <- lapply(rads_in, sin_cos)
  # Average the sines and cosines
  ave_coords <- Reduce(`+`, coords)/length(coords)
  # Take the arctangent
  round(atan2(ave_coords[1], ave_coords[2]), 2)
}
# Define extraction function for typical weighted average
extractor <- function (i, x) {
  # Extract raster values within that buffer
  buff_rast <- x %>%
    terra::extract(vect(buff[i, "geometry"]))
  # Exclude any NA values introduced
  buff_rast <- buff_rast[complete.cases(buff_rast), ]
  # Average the DEM values
  extracted <- mean(buff_rast[, 2])
  # Append to columns of the movement data as a proportion
  return(extracted)
}
# Define extraction function for an angular weighted average
circle_extractor <- function (i, x) {
  # Extract raster values within that buffer
  buff_rast <- x %>%
    terra::extract(vect(buff[i, "geometry"]))
  # Exclude any NA values introduced
  buff_rast <- buff_rast[complete.cases(buff_rast), ]
  # Average the DEM values
  circle_extracted <- mean_rads(buff_rast[, 2])
  # Append to columns of the movement data as a proportion
  return(circle_extracted)
}
# Iteration --------------------------------------------------------------------
i <- seq(1:nrow(gps_sf))
# Extract DEM values within 20 m:
# Collect processing start time
start_time <- proc.time()
# Create a column in the GPS data set and extract the covariates to that column
gps_sf$dem <- mclapply(i, extractor, x = dem, mc.cores = params$cores) %>%
  # Convert to vector
  unlist() %>%
  # Round to the nearest decimeter
  round(0) %>%
  # Convert to meters
  `/` (10)
# Collect runtime
end_time <- proc.time() - start_time
# Print message about runtime
message("The following time elapsed for DEM processing: \n", 
       names(end_time[1]), ": ", end_time[[1]], "\n",
       names(end_time[2]), ":  ", end_time[[2]], "\n",
       names(end_time[3]), ":   ", end_time[[3]])
# Extract the slope within 20 m:
start_time <- proc.time()
gps_sf$slope <- mclapply(i, extractor, x = slope, mc.cores = params$cores) %>%
  unlist() %>%
  # Convert to a proportion rather than percentage
  `*` (0.01) %>%
  # Round to three decimal places
  round(3)
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract the aspect within 20 m:
start_time <- proc.time()
gps_sf$aspect <- mclapply(i, circle_extractor, x = aspect, 
                          mc.cores = params$cores) %>%
  unlist()
end_time <- proc.time() - start_time
message("The following time elapsed for aspect processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
rm(aspect, buff, circle_extractor, dem, end_time, extractor, i, mean_rads, 
   params, sin_cos, slope, start_time)
# Clean and join data before exporting -----------------------------------------
drops <- c("animal_id", "species", "gps_fix_time", "geometry")
# Filter out rows
gps_sf <- gps_sf[, !(names(gps_sf) %in% drops)]
rm(drops)
# Join back into gps data file
gps <- gps %>%
  full_join(gps_sf, by = "uid") %>%
  select(!geometry)
gps$uid <- NULL
rm(gps_sf)
# Export -----------------------------------------------------------------------
# Export as csv
gps %>%
  write.csv(here("data/processed_data/dem_etc_appended.csv"), row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================