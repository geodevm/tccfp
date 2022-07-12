#===============================================================================
### Extract covariates within a 20 meter buffer zone for the cumulative index
### of activity.
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Read in packages -------------------------------------------------------------
library(here)
library(lubridate)
library(parallel)
library(sf)
library(terra)
library(tidyverse)
# Set the buffer distance, and storage for parameters --------------------------
params <- tibble(
  buff_dist = 20,
  buff_dist_b = NA,
  total_area = NA,
  cores = as.numeric(detectCores())
)
# Data read and management -----------------------------------------------------
# Movement data
if (exists("movement")) {
  message("Pre-processed movement data found. Nice.")
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
    "Pre-processed movement data has not been found. Processing now."
  )
  # Load in data
  gps <- read.csv(here("data/processed_data/gps_data.csv"))
  # Coerce all variables into the right format
  gps <- gps %>%
    mutate_at(vars(animal_id, species), funs(as.factor)) %>%
    mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, 
                   gps_utm_easting, gps_altitude), 
              funs(as.numeric)) %>%
    mutate_at(vars(gps_fix_time), funs(as_datetime))
  message("Warnings about dplyr deprecated functions are ok.")
}
gps$uid <- paste0(gps$animal_id, "_", rownames(gps))
gps_sf <- gps %>% 
  filter(complete.cases(gps[, c("gps_utm_easting", 
                                "gps_utm_northing", 
                                "gps_fix_time")])) 
gps_sf<- st_as_sf(gps, coords = c("gps_utm_easting", "gps_utm_northing"))
st_crs(gps_sf) <- 26915
# Read in COI layer
coi <- rast(here("data/gis_layers/coi/coi.tif"))
# Data extent outline for the COI data
outline <- st_read(here("data/gis_layers/coi/metadata/coi_layer_coverage.shp"), 
                   quiet = TRUE) %>%
  # Get rid of buffer edge cases
  st_buffer(dist = -params$buff_dist) 
# Pre-iteration processing -----------------------------------------------------
# Remove edge cases by doing a negative buffer distance of the outline. We want 
# to clip the gps data by the extent of the data since that is our data extent 
# for this spatial layer. We'll do a clip.
st_agr(gps_sf) = "constant"
st_agr(outline) = "constant"
gps_sf <- st_intersection(gps_sf, outline$geometry)
# To get the centroid of the pixels (which is the only way they can be extracted
# with polygons), you have to take the distance from the corner of the pixel to
# the center of the pixel and add it to the baseline buffer dimension. Using the
# Pythagorean theorem, this is sqrt((pixel l /2)^2 *2).
params$buff_dist_b <- sqrt((((res(coi)[1] / 2)^2) * 2)) + params$buff_dist
# We also want to store the total area of our buffer, pi * r ^ 2
params$total_area <- pi * (params$buff_dist^2)
# Create the buffer which we'll use to mask the raster layer. Since the 
# raster centroid is what's intersected as explained above, this is where
# that value is used.
buff_b <- st_buffer(gps_sf, dist = params$buff_dist_b)
# Create the buffer which will be used to clip the mask layer.
buff <- st_buffer(gps_sf, dist = params$buff_dist)
st_agr(buff) <- "constant"
rm(outline)
# Define functions -------------------------------------------------------------
# This function collects the values weighted by area so they can be summed
weight_collector <- function (x) {
  weight_value <- as.numeric(st_area(x[, 2]) / 
                               as.numeric(params[1, "total_area"]))
  x <- x %>% st_set_geometry(NULL)
  value <- round((x[, 1] * weight_value), 5)
  return(value)
}
# This creates an area-weighted average for the dataset
average_weighted_area <- function (i) {
  # Clip the raster using the larger buffer.
  coi_clip_rast <- coi %>%
    terra::crop(vect(buff_b[i, "geometry"]))
  # Polygonize the clipped raster.
  coi_clip_vec_int <- coi_clip_rast %>%
    as.polygons() %>%
    st_as_sf()
  st_agr(coi_clip_vec_int) <- "constant"
  # Clip the polygonized raster to the main buffer
  coi_clip_int <- coi_clip_vec_int %>%
    st_intersection(buff[i, "geometry"])
  # Collect the weighted values
  weights <- weight_collector(coi_clip_int)
  # Sum for an area weighted average
  area_weighted_average <- sum(weights)
  return(area_weighted_average)
}
# Iteration --------------------------------------------------------------------
# Collect runtime start for the mclapply call
start_time <- proc.time()
# Run the functions
i <- seq(1:nrow(gps_sf))
area_weighted_average <- mclapply(i, average_weighted_area, 
                                  mc.cores = params$cores) %>% 
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Bind column
gps_sf$coi <- area_weighted_average
rm(area_weighted_average, average_weighted_area, buff, buff_b, coi, end_time, i, 
   params, start_time, weight_collector)
# Data management --------------------------------------------------------------
# Join to original dataset
gps_sf <- gps_sf %>%
  st_set_geometry(NULL) %>%
  select(uid, coi)
gps <- gps %>%
  left_join(gps_sf, by = "uid")
gps$uid <- NULL
rm(gps_sf)
# Export -----------------------------------------------------------------------
# Export the data as a csv
gps <- gps %>%
  write.csv(here("data/processed_data/coi_appended.csv"), 
            row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================