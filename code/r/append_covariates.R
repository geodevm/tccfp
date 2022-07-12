#===============================================================================
### Extract covariates within a 20 meter buffer zone for the cumulative index
### of activity.
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Read in packages -------------------------------------------------------------
library(here)
library(lubridate)
#library(parallel)
library(sf)
library(terra)
library(tidyverse)
# Set the buffer distance, and storage for parameters --------------------------
params <- tibble(
  buff_dist = 20,
  buff_dist_imp = NA,
  buff_dist_coi = NA,
  buff_dist_roads = NA,
  buff_dist_lot = NA,
  total_area = NA,
#  cores = as.numeric(detectCores())
)
# Data read and management -----------------------------------------------------
gps <- read.csv("D:/data/processed_data/rsf_locations.csv")
# Coerce all variables into the right format
gps <- gps %>%
  mutate_at(vars(animal_id, species, season, behavior, tod_, burst_11_h, 
                 burst_5_h, case), 
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, gps_longitude, gps_latitude,
                 gps_altitude, sl_11_h, dir_11_h, ta_11_h, sl_5_h, dir_5_h, 
                 ta_5_h), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), funs(as_datetime))
message("Warnings about dplyr deprecated functions are ok.")
gps$uid <- paste0(gps$animal_id, "_", rownames(gps))
gps_sf <- st_as_sf(gps, coords = c("gps_utm_easting", "gps_utm_northing"))
st_crs(gps_sf) <- 26915
#############################
gps_sf <- gps_sf[1:200, ]
gps <- gps[1:200, ]
#############################

# Read in COI layer
coi <- rast("D:/data/gis_layers/coi/coi.tif")
dem <- rast("D:/data/dem/tcma_dem_dm.tif") #Stored in decimeters
slope <- rast("D:/data/dem/tcma_slope.tif")
aspect <- rast("D:/data/dem/tcma_aspect.tif")
landcover <- rast("D:/data/gis_layers/landcover/tcma_lc_finalv1.tif")
imp <- rast("D:/data/gis_layers/impervious/TCMA_Impervious_2000.tif")
roads <- rast("D:/data/gis_layers/roads_km/kmkm2.tif")
patch_size <- rast("D:/data/gis_layers/patch_size/veg_size_msq.tif")
lot_size <- rast("D:/data/gis_layers/lot_size/lot_size.tif")
# Data extent outline for the COI data
outline_coi <- st_read(here("data/gis_layers/coi/metadata/coi_layer_coverage.shp"), 
                       quiet = TRUE) %>%
  # Get rid of buffer edge cases
  st_buffer(dist = -params$buff_dist) 
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
st_agr(outline_coi) = "constant"
gps_coi <- st_intersection(gps_sf, outline_coi$geometry)
gps_sf <- st_intersection(gps_sf, outline$geometry)
# To get the centroid of the pixels (which is the only way they can be extracted
# with polygons), you have to take the distance from the corner of the pixel to
# the center of the pixel and add it to the baseline buffer dimension. Using the
# Pythagorean theorem, this is sqrt((pixel l /2)^2 *2).
params$buff_dist_imp <- sqrt((((res(imp)[1] / 2)^2) * 2)) + params$buff_dist
params$buff_dist_coi <- sqrt((((res(coi)[1] / 2)^2) * 2)) + params$buff_dist
params$buff_dist_roads <- sqrt((((res(roads)[1] / 2)^2) * 2)) + params$buff_dist
params$buff_dist_lot <- sqrt((((res(lot_size)[1] / 2)^2) * 2)) + 
  params$buff_dist
# We also want to store the total area of our buffer, pi * r ^ 2
params$total_area <- pi * (params$buff_dist^2)
# Create the buffer which we'll use to mask the raster layer. Since the 
# raster centroid is what's intersected as explained above, this is where
# that value is used.
buff_imp <- st_buffer(gps_sf, dist = params$buff_dist_imp)
buff_coi_b <- st_buffer(gps_coi, dist = params$buff_dist_coi)
buff_roads <- st_buffer(gps_sf, dist = params$buff_dist_roads)
buff_lot <- st_buffer(gps_sf, dist = params$buff_dist_lot)
# Create the buffer which will be used to clip the mask layer.
buff <- st_buffer(gps_sf, dist = params$buff_dist)
buff_coi <- st_buffer(gps_coi, dist = params$buff_dist)
rm(outline, outline_coi)
# Define functions -------------------------------------------------------------
# This function collects the values weighted by area so they can be summed
weight_collector <- function (x) {
  weight_value <- as.numeric(st_area(x[, 2]) / 
                               as.numeric(params[1, "total_area"]))
  x <- x %>% st_set_geometry(NULL)
  value <- round((x[, 1] * weight_value), 5)
  return(value)
}
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
habitat_extractor <- function (i, x) {
  # Extract raster values within that buffer
  buff_rast <- x %>%
    terra::extract(vect(buff[i, "geometry"]))
  # Exclude any NA values introduced
  buff_rast <- buff_rast[complete.cases(buff_rast), ] %>%
    # Recode to the correct values
    mutate(tcma_lc_finalv1 = recode(tcma_lc_finalv1,
                                    "1" = "grass_shrub",
                                    "2" = "bare_soil",
                                    "3" = "buildings",
                                    "4" = "roads_paved_surfaces",
                                    "5" = "lakes_ponds",
                                    "6" = "deciduous",
                                    "7" = "coniferous",
                                    "8" = "agriculture",
                                    "9" = "emergent_wetland",
                                    "10" = "forested_shrub_wetland",
                                    "11" = "river",
                                    "12" = "extraction")
    )
  # Count observations of each habitat type
  ct_table <- buff_rast %>%
    group_by(ID, tcma_lc_finalv1) %>%
    count()
  # Divide by number of total habitat observations
  ct_table$prop <- ct_table$n / nrow(buff_rast)
  
  # Append to columns of the movement data as a proportion
  table_out <- setNames(data.frame(matrix(ncol = nrow(ct_table),
                                          nrow = 0)), 
                        ct_table$tcma_lc_finalv1)
  table_out[1, ] <- ct_table$prop
  # Output is a dataframe
  return(table_out)
}
# This creates an area-weighted average for the dataset
imp_weighted_area <- function (i) {
  # Clip the raster using the larger buffer.
  imp_clip_rast <- imp %>%
    terra::crop(vect(buff_imp[i, "geometry"]))
  # Polygonize the clipped raster.
  imp_clip_vec_int <- imp_clip_rast %>%
    as.polygons() %>%
    st_as_sf()
  st_agr(imp_clip_vec_int) <- "constant"
  # Clip the polygonized raster to the main buffer
  imp_clip_int <- imp_clip_vec_int %>%
    st_intersection(buff[i, "geometry"])
  # Collect the weighted values
  weights <- weight_collector(imp_clip_int)
  # Sum for an area weighted average
  area_weighted_average <- sum(weights)
  return(area_weighted_average)
}
roads_weighted_area <- function (i) {
  # Clip the raster using the larger buffer.
  roads_clip_rast <- roads %>%
    terra::crop(vect(buff_roads[i, "geometry"]))
  # Polygonize the clipped raster.
  roads_clip_vec_int <- roads_clip_rast %>%
    as.polygons() %>%
    st_as_sf()
  st_agr(roads_clip_vec_int) <- "constant"
  # Clip the polygonized raster to the main buffer
  roads_clip_int <- roads_clip_vec_int %>%
    st_intersection(buff[i, "geometry"])
  # Collect the weighted values
  weights <- weight_collector(roads_clip_int)
  # Sum for an area weighted average
  area_weighted_average <- sum(weights)
  return(area_weighted_average)
}
# This creates an area-weighted average for the dataset
coi_weighted_area <- function (i) {
  # Clip the raster using the larger buffer.
  coi_clip_rast <- coi %>%
    terra::crop(vect(buff_coi_b[i, "geometry"]))
  # Polygonize the clipped raster.
  coi_clip_vec_int <- coi_clip_rast %>%
    as.polygons() %>%
    st_as_sf()
  st_agr(coi_clip_vec_int) <- "constant"
  # Clip the polygonized raster to the main buffer
  coi_clip_int <- coi_clip_vec_int %>%
    st_intersection(buff_coi[i, "geometry"])
  # Collect the weighted values
  weights <- weight_collector(coi_clip_int)
  # Sum for an area weighted average
  area_weighted_average <- sum(weights)
  return(area_weighted_average)
}
# This creates an area-weighted average for the dataset
lot_weighted_area <- function (i) {
  # Clip the raster using the larger buffer.
  lot_clip_rast <- lot_size %>%
    terra::crop(vect(buff_lot[i, "geometry"]))
  # Polygonize the clipped raster.
  lot_clip_vec_int <- lot_clip_rast %>%
    as.polygons() %>%
    st_as_sf()
  st_agr(lot_clip_vec_int) <- "constant"
  # Clip the polygonized raster to the main buffer
  lot_clip_int <- lot_clip_vec_int %>%
    st_intersection(buff[i, "geometry"])
  # Collect the weighted values
  weights <- weight_collector(lot_clip_int)
  # Sum for an area weighted average
  area_weighted_average <- sum(weights)
  return(area_weighted_average)
}
max_pixel <- function (i, x) {
  # Extract raster values within that buffer
  buff_rast <- x %>%
    terra::extract(vect(buff[i, "geometry"]))
  # Exclude any NA values introduced
  buff_rast <- buff_rast[complete.cases(buff_rast), ]
  # Retrieve the max pixel
  max_pix <- max(buff_rast[, 2])
  # Output is a dataframe
  return(max_pix)
}
# ------------------------------------------------------------------------------
# Iteration --------------------------------------------------------------------
# ------------------------------------------------------------------------------
iteration_start <- proc.time()
# Extract COI within 20 m: -----------------------------------------------------
# Collect runtime start for the mclapply call
start_time <- proc.time()
# Run the functions
i <- seq(1:nrow(gps_coi))
coi_weighted_average <- lapply(i, coi_weighted_area) %>% 
  unlist()
#coi_weighted_average <- mclapply(i, coi_weighted_area, 
#                                 mc.cores = params$cores) %>% 
#  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Bind column
gps_coi$coi <- coi_weighted_average
# Extract DEM values within 20 m: ----------------------------------------------
# Collect processing start time
start_time <- proc.time()
# Get iterator
i <- seq(1:nrow(gps_sf))
# Create a column in the GPS data set and extract the covariates to that column
gps_sf$dem <- lapply(i, extractor, x = dem) %>%
  # Convert to vector
  unlist() %>%
  # Round to the nearest decimeter
  round(0) %>%
  # Convert to meters
  `/` (10)
# Create a column in the GPS data set and extract the covariates to that column
#gps_sf$dem <- mclapply(i, extractor, x = dem, mc.cores = params$cores) %>%
#  # Convert to vector
#  unlist() %>%
#  # Round to the nearest decimeter
#  round(0) %>%
#  # Convert to meters
#  `/` (10)
# Collect runtime
end_time <- proc.time() - start_time
# Print message about runtime
message("The following time elapsed for DEM processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract the slope within 20 m: -----------------------------------------------
start_time <- proc.time()
gps_sf$slope <- lapply(i, extractor, x = slope) %>%
  unlist() %>%
  # Convert to a proportion rather than percentage
  `*` (0.01) %>%
  # Round to three decimal places
  round(3)
#gps_sf$slope <- mclapply(i, extractor, x = slope, mc.cores = params$cores) %>%
#  unlist() %>%
#  # Convert to a proportion rather than percentage
#  `*` (0.01) %>%
#  # Round to three decimal places
#  round(3)
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract the aspect within 20 m: ----------------------------------------------
start_time <- proc.time()
gps_sf$aspect <- lapply(i, circle_extractor, x = aspect) %>%
  unlist()
#gps_sf$aspect <- mclapply(i, circle_extractor, x = aspect, 
#                          mc.cores = params$cores) %>%
#  unlist()
end_time <- proc.time() - start_time
message("The following time elapsed for aspect processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract impervious surface within 20 m: --------------------------------------
start_time <- proc.time()
imp_weighted_average <- lapply(i, imp_weighted_area) %>% 
  unlist()
#imp_weighted_average <- mclapply(i, imp_weighted_area, 
#                                 mc.cores = params$cores) %>% 
#  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Bind column
gps_sf$imp <- imp_weighted_average
# Extract road density within 20 m: --------------------------------------------
start_time <- proc.time()
roads_weighted_average <- lapply(i, roads_weighted_area) %>% 
  unlist()
#roads_weighted_average <- mclapply(i, roads_weighted_area, 
#                                 mc.cores = params$cores) %>% 
#  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for road density processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Bind column
gps_sf$roads_km <- roads_weighted_average
# Extract lot size within 20 m: ------------------------------------------------
start_time <- proc.time()
lot_weighted_average <- lapply(i, lot_weighted_area) %>% 
  unlist() %>%
  # Round to the nearest meter
  round(0) %>%
  # Convert to hectares
  `/` (10000)
#lot_weighted_average <- mclapply(i, lot_weighted_area, 
#                                 mc.cores = params$cores) %>% 
#  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Bind column
gps_sf$lot_size <- lot_weighted_average
# Extract landcover proportion within 20: --------------------------------------
start_time <- proc.time()
# Create a column in the GPS data set and extract the covariates to that column
extracted_habitat <- lapply(i, habitat_extractor, x = landcover) %>%
  bind_rows() %>%
  round(3) %>%
  as_tibble()
#extracted_habitat <- mclapply(i, habitat_extractor, x = landcover, 
#                              mc.cores = params$cores) %>%
#  unlist() %>%
#  bind_rows() %>%
#  round(3) %>%
#  as_tibble()
gps_sf <- gps_sf %>%
  bind_cols(extracted_habitat)
# Collect runtime
end_time <- proc.time() - start_time
# Print message about runtime
message("The following time elapsed for landcover processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract maximum patch size within 20 m: -------------------------------------
start_time <- proc.time()
gps_sf$patch_size <- lapply(i, max_pixel, x = patch_size) %>%
  # Convert to vector
  unlist() %>%
  # Round to the nearest meter
  round(0) %>%
  # Convert to hectares
  `/` (10000)
# If there are missing values, that indicates a 0, not infinity
gps_sf[(gps_sf$patch_size == -Inf | gps_sf$patch_size == Inf), ]$patch_size <- 0
#gps_sf$patch_size <- mclapply(i, max_pixel, x = patch_size, 
#                              mc.cores = params$cores) %>%
#  # Convert to vector
#  unlist() %>%
#  # Round to the nearest meter
#  round(0) %>%
#  # Convert to hectares
#  `/` (10000)
# Collect runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Collect all iteration run time -----------------------------------------------
end_time <- proc.time() - iteration_start
message("The following time elapsed iteration: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
rm(end_time, coi, dem, slope, aspect, landcover, imp, roads, lot_size, 
   patch_size, i, params, buff, buff_coi, buff_imp, buff_coi_b, buff_roads,
   buff_lot, weight_collector, sin_cos, mean_rads, extractor, circle_extractor, 
   habitat_extractor, iteration_start, imp_weighted_area, roads_weighted_area, 
   coi_weighted_area, lot_weighted_area, max_pixel, start_time, 
   coi_weighted_average, imp_weighted_average, lot_weighted_average, 
   roads_weighted_average, extracted_habitat)
# ------------------------------------------------------------------------------
# End iteration ----------------------------------------------------------------
# ------------------------------------------------------------------------------
# Clean and join data before exporting -----------------------------------------
drops <- c("animal_id", "species", "gps_fix_time", "geometry")
# Filter out rows
gps_sf <- gps_sf[, !(names(gps_sf) %in% drops)] %>%
  select(-contains("...")) %>%
  st_drop_geometry() %>%
  select(uid, dem, slope, aspect, imp, roads_km, lot_size, deciduous, 
         grass_shrub, roads_paved_surfaces, lakes_ponds, buildings, 
         emergent_wetland, patch_size)
gps_coi <- gps_coi[, !(names(gps_coi) %in% drops)] %>%
  st_drop_geometry() %>%
  select(uid, coi)
rm(drops)
# Join back into gps data file
gps <- gps %>%
  left_join(gps_sf, by = "uid") %>%
  left_join(gps_coi, by = "uid")
gps$uid <- NULL
rm(gps_sf, gps_coi)
# Export -----------------------------------------------------------------------
# Export as csv
gps %>%
  write.csv(here("data/processed_data/covariates_appended.csv"), 
            row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================