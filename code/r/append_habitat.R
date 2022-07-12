#===============================================================================
### Extract covariates within a 20 meter buffer zone for the habitat 
### classification.
#-------------------------------------------------------------------------------
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Load in packages -------------------------------------------------------------
library(here)
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
# Landcover data
landcover <- rast(here("data/gis_layers/landcover/tcma_lc_finalv1.tif"))
# Movement data
if (exists("movement")) {
  message("Pre-processed biologicals data found. Nice.")
  gps <- movement %>% filter(complete.cases(movement[, c("gps_utm_easting", 
                                                         "gps_utm_northing", 
                                                         "gps_fix_time")]))
  drops <- c("activity_fix_time", "temperature_fix_time", "gps_altitude", 
             "activity_count", "temperature", "gps_longitude", "gps_latitude", 
             "acquisition_start_time")
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
    mutate_at(vars(gps_fix_time), funs(as_datetime)) %>%
    select(animal_id, species, gps_utm_northing, gps_utm_easting, gps_fix_time)
  }
gps$uid <- paste0(gps$animal_id, "_", rownames(gps))
gps_sf <- gps %>% filter(complete.cases(gps[, c("gps_utm_easting", 
                                                "gps_utm_northing", 
                                                "gps_fix_time")])) 
gps_sf<- st_as_sf(gps, coords = c("gps_utm_easting", "gps_utm_northing")) 
st_crs(gps_sf) <- 26915
# TCMA outline
outline <- st_read(here("data/gis_layers/metro_outline/metro_outline.shp"), 
                   quiet = TRUE) %>%
  st_buffer(dist = -params$buff_dist) # Get rid of buffer edge cases
# Pre-iteration processing -----------------------------------------------------
# Remove edge cases by doing a negative buffer distance of the outline. We want 
# to clip the gps data by the extent of the TCMA since that is our data extent 
# for this spatial layer. We'll do a clip.
st_agr(gps_sf) = "constant"
st_agr(outline) = "constant"
gps_sf <- st_intersection(gps_sf, outline$geometry)
rm(outline)
# Processing -------------------------------------------------------------------
# Buffer for extraction
buff <- st_buffer(gps_sf, dist = params$buff_dist)
# Create empty columns for extracting
# Add proportion each habitat w/in 20 m ----------------------------------------
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
  table_out
}
# Iteration --------------------------------------------------------------------
# Collect processing start time
start_time <- proc.time()
# Extract habitat values within 20 m
i <- seq(1:nrow(gps_sf))
# Create a column in the GPS data set and extract the covariates to that column
extracted_habitat <- mclapply(i, habitat_extractor, x = landcover, 
                              mc.cores = params$cores) %>%
  unlist() %>%
  bind_rows() %>%
  round(3) %>%
  as_tibble()
gps_sf <- gps_sf %>%
  bind_cols(extracted_habitat)
# Collect runtime
end_time <- proc.time() - start_time
# Print message about runtime
message("The following time elapsed for landcover processing: \n", 
        names(end_time[1]), ": ", end_time[[1]], "\n",
        names(end_time[2]), ":  ", end_time[[2]], "\n",
        names(end_time[3]), ":   ", end_time[[3]])
rm(buff, end_time, extracted_habitat, habitat_extractor, i, landcover, params, 
   start_time)
# Clean and join data before exporting -----------------------------------------
drops <- c("animal_id", "species", "gps_fix_time", "geometry")
# Filter out columns
gps_sf <- gps_sf[, !(names(gps_sf) %in% drops)] %>%
  select(-contains("...")) %>%
  st_drop_geometry()
rm(drops)
# Join back into gps data file
gps <- gps %>%
  full_join(gps_sf, by = "uid")
gps$uid <- NULL
rm(gps_sf)
# Export -----------------------------------------------------------------------
# Export the data as a csv
gps %>%
  write.csv(here("data/processed_data/habitat_appended.csv"), row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================