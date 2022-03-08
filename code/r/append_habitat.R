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
# Add proportion each habitat w/in 20 m ----------------------------------------
# Start loop -------------------------------------------------------------------
for (i in 1:nrow(gps_sf)) {
  # Create 20 m buffer
  buff_20 <- st_buffer(gps_sf$geometry[i], dist = 20)
  # Extract raster values within that buffer
  buff_rast <- terra::extract(landcover, vect(buff_20))
  # Recode extracted raster values
  buff_rast <- buff_rast %>% mutate(tcma_lc_finalv1 =
                                    recode(tcma_lc_finalv1,
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
  # Exclude any NA values introduced
  buff_rast <- buff_rast[!is.na(buff_rast$tcma_lc_finalv1), ]
  # Count observations of each habitat type
  ct_table <- buff_rast %>%
    group_by(ID, tcma_lc_finalv1) %>%
    count()
  # Divide by number of total habitat observations
  ct_table$prop <- ct_table$n / nrow(buff_rast)
  # Append to columns of the movement data as a proportion
  lvls <- ct_table$tcma_lc_finalv1 %>%
    as.factor() %>%
    levels()
  for (j in lvls) {
    gps_sf[i, j] <- ct_table[ct_table$tcma_lc_finalv1 == j, ]$prop
  }
}
rm(buff_rast, buff_20, ct_table, lvls, j, landcover, i)
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
  write.csv(here("data/processed_data/habitat_appended.csv"), row.names = FALSE)
rm(gps)
#===============================================================================