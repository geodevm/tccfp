#===============================================================================
### Extract covariates within a 20 meter buffer zone for the cumulative index
### of activity.
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Read in packages -------------------------------------------------------------
library(here)
library(lubridate)
library(sf)
library(terra)
library(tidyverse)
# Data read and management -----------------------------------------------------
# Read in ranges
ranges <- st_read(here("data/processed_data/shps/random_ranges.shp"), 
                       quiet = TRUE) %>%
  filter(case == "case" & anlyss_ == "hybrid_ellipse") %>%
  rename(animal_id = animl_d, analysis_type = anlyss_) %>%
  mutate_at(vars(animal_id, species, season, case, analysis_type), 
            funs(as.factor))
message("Warnings about dplyr deprecated functions are ok.")
# Read in rasters
coi <- rast("E:/data/resample/mask_30/coi_30.tif")
canopy <- rast("E:/data/resample/mask_30/can_30.tif")
buildings <- rast("E:/data/resample/mask_30/bldng_30.tif")
roads <- (rast("E:/data/resample/mask_30/km_km2_30.tif") * 1000) # Convert m/m2 to km/km2
open <- rast("E:/data/resample/mask_30/open_30.tif")
residential <- rast("E:/data/resample/mask_30/res_30.tif")
slope <- (rast("E:/data/resample/mask_30/slope_30.tif") / 100) # Proportion
veg <- rast("E:/data/resample/mask_30/veg_30.tif")
wetland <- rast("E:/data/resample/mask_30/wet_30.tif")
imp <- (rast("E:/data/gis_layers/impervious/TCMA_Impervious_2000.tif") / 100)
# Pre-iteration processing -----------------------------------------------------
# Define functions -------------------------------------------------------------
# Define extraction function for typical weighted average
extractor <- function (i, x) {
  # Extract raster values within that buffer
  rast <- x %>%
    terra::extract(vect(ranges[i, "geometry"]))
  # Exclude any NA values introduced
  rast <- rast[complete.cases(rast), ]
  # Average the values
  extracted <- mean(rast[, 2])
  # Append to columns of the movement data as a proportion
  return(extracted)
}
max_pixel <- function (i, x) {
  # Extract raster values within that buffer
  rast <- x %>%
    terra::extract(vect(ranges[i, "geometry"]))
  # Exclude any NA values introduced
  rast <- rast[complete.cases(rast), ]
  # Retrieve the max pixel
  max_pix <- max(rast[, 2])
  # Output is a dataframe
  return(max_pix)
}
# ------------------------------------------------------------------------------
# Iteration --------------------------------------------------------------------
# ------------------------------------------------------------------------------
iteration_start <- proc.time()
# Extract COI within home range: -----------------------------------------------
# Collect runtime start for the mclapply call
start_time <- proc.time()
# Run the functions
i <- seq(1:nrow(ranges))
# Extract average COI
ranges$coi <- lapply(i, extractor, x = coi) %>%
  unlist()
# Extract slope within home range: ---------------------------------------------
start_time <- proc.time()
ranges$slope <- lapply(i, extractor, x = slope) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract impervious surface within home range: --------------------------------
start_time <- proc.time()
ranges$imp <- lapply(i, extractor, x = imp) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract lot size within home range: ------------------------------------------
#start_time <- proc.time()
#ranges$lot_size <- lapply(i, extractor, x = lot) %>%
#  unlist()
# Print runtime
#end_time <- proc.time() - start_time
#message("The following time elapsed for impervious surface processing: \n", 
#        names(end_time[1]), ": ", end_time[[1]], "\n",
#        names(end_time[2]), ":  ", end_time[[2]], "\n",
#        names(end_time[3]), ":   ", end_time[[3]])
# Extract residential within home range: ---------------------------------------
start_time <- proc.time()
ranges$residential <- lapply(i, extractor, x = residential) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for impervious surface processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract canopy within home range: --------------------------------------------
start_time <- proc.time()
ranges$canopy <- lapply(i, extractor, x = canopy) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract buildings within home range: -----------------------------------------
start_time <- proc.time()
ranges$buildings <- lapply(i, extractor, x = buildings) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract open space within home range: ----------------------------------------
start_time <- proc.time()
ranges$open <- lapply(i, extractor, x = open) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract road density within home range: --------------------------------------
start_time <- proc.time()
ranges$road_density <- lapply(i, extractor, x = roads) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract wetland within home range: -------------------------------------------
start_time <- proc.time()
ranges$wetland <- lapply(i, extractor, x = wetland) %>%
  unlist()
# Print runtime
end_time <- proc.time() - start_time
message("The following time elapsed for slope processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
# Extract maximum patch size within home range: --------------------------------
start_time <- proc.time()
ranges$patch_size <- lapply(i, max_pixel, x = veg) %>%
  # Convert to vector
  unlist() %>%
  # Round to the nearest meter
  round(0) %>%
  # Convert to hectares
  `/` (10000)
# If there are missing values, that indicates a 0, not infinity
ranges[(ranges$patch_size == -Inf | ranges$patch_size == Inf), ]$patch_size <- 0
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
rm(buildings, canopy, coi, end_time, extractor, i, imp, iteration_start, lot, 
   max_pixel, open, pop, residential, slope, start_time, veg, wetland)
# ------------------------------------------------------------------------------
# End iteration ----------------------------------------------------------------
# ------------------------------------------------------------------------------
# Create strata for analysis ---------------------------------------------------
ranges$strata <- paste0(ranges$animal_id, "_", ranges$season)
# Create a variable that pools home ranges seasonally rather than by year and 
# season
for (i in 1:nrow(ranges)) {
  ranges$season_pooled[i] <-  strsplit(as.character(ranges$season[i]), "[_]")[[1]][1]
}
# Export -----------------------------------------------------------------------
# Export as csv
ranges %>%
  st_drop_geometry() %>%
  select(!c(analysis_type, case, strata)) %>%
  write.csv("C:/Users/mill8849/Desktop/rsf_2nd_order.csv", 
            row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================