#===============================================================================
### Joining all of the data into a final dataset
# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)
# Load in data -----------------------------------------------------------------
# Using .csv format, load in all data
# Import movement data
movement <- tibble(read.csv(here("data/processed_data/movement_data.csv"), 
                            header = T, sep = ",", na.strings = NA))
# Import steps data
steps <- tibble(read.csv(here("data/processed_data/step_intervals_appended.csv"), 
                         header = T, sep = ",", na.strings = NA))
# Import DEM data
dem <- tibble(read.csv(here("data/processed_data/dem_etc_appended.csv"), 
                       header = T, sep = ",", na.strings = NA))
# Import habitat data
habitat <- tibble(read.csv(here("data/processed_data/habitat_appended.csv"), 
                           header = T, sep = ",", na.strings = NA))
# Import population data
population <- tibble(read.csv(here("data/processed_data/pop_appended.csv"), 
                              header = T, sep = ",", na.strings = NA))
# Import impervious data
impervious <- tibble(read.csv(here("data/processed_data/impervious_appended.csv"), 
                              header = T, sep = ",", na.strings = NA))
# Import COI data
coi <- tibble(read.csv(here("data/processed_data/coi_appended.csv"), 
                       header = T, sep = ",", na.strings = NA))
# Coerce all variables into the right format. This could take a bit. -----------
# Coerce movement data
movement <- movement %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(temperature, gps_latitude, gps_longitude, gps_utm_northing, 
                 gps_utm_easting, gps_altitude, activity_count), 
            funs(as.numeric)) %>%
  mutate_at(vars(activity_fix_time, gps_fix_time, temperature_fix_time, 
                 acquisition_start_time), 
            funs(as_datetime))
message("Warnings about dplyr deprecated functions are ok.")
message("Movement dataset should be >1.6 million obs. of 13 variables")
# Coerce steps data
steps <- steps %>%
  mutate_at(vars(animal_id, species, tod_, burst_11_h, burst_5_h, burst_10_m),
            funs(as.factor)) %>%
  mutate_at(vars(sl_, dir_, ta_, gps_utm_northing, gps_utm_easting, sl_11_h, 
                 dir_11_h, ta_11_h, sl_5_h, dir_5_h, ta_5_h, sl_10_m, dir_10_m,
                 ta_10_m), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_datetime))
message("Steps dataset should be >56000 obs. of 21 variables")
# Coerce dem data
dem <- dem %>%
  mutate_at(vars(animal_id, species),
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, dem, slope, aspect), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_datetime))
message("DEM dataset should be >56000 obs. of 8 variables")
# Coerce habitat data
habitat <- habitat %>%
  mutate_at(vars(animal_id, species),
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, buildings, deciduous, 
                 grass_shrub, roads_paved_surfaces, coniferous, 
                 emergent_wetland, lakes_ponds, river, forested_shrub_wetland,
                 bare_soil, agriculture), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_datetime))
message("Habitat dataset should be >56000 obs. of 16 variables")
# Filter and coerce population data
drops <- c("gps_altitude", "gps_latitude", "gps_longitude")
population <- population[, !(names(population) %in% drops)]
rm(drops)
population <- population %>%
  mutate_at(vars(animal_id, species),
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, pop_den), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_date))
message("Habitat dataset should be >56000 obs. of 6 variables")
# Filter and coerce impervious data
drops <- c("gps_altitude", "gps_latitude", "gps_longitude")
impervious <- impervious[, !(names(impervious) %in% drops)]
rm(drops)
impervious <- impervious %>%
  mutate_at(vars(animal_id, species),
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, imp), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_date))
message("Habitat dataset should be >56000 obs. of 6 variables")
# Filter and coerce coi data
drops <- c("gps_altitude", "gps_latitude", "gps_longitude")
coi <- coi[, !(names(coi) %in% drops)]
rm(drops)
coi <- coi %>%
  mutate_at(vars(animal_id, species),
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting, coi), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), 
            funs(as_date))
message("Habitat dataset should be >56000 obs. of 6 variables")
# Join data with wrong date format ---------------------------------------------
# Create UID for steps dataset
steps$uid <- paste(steps$animal_id, "_", rownames(steps))
# Create a separate dataset with a date for joining
date_steps <- steps %>% mutate_at(vars(gps_fix_time), funs(as_date))
# Join impervious data
date_steps <- date_steps %>%
  full_join(impervious, by = c("animal_id", "species", "gps_utm_easting", 
                               "gps_utm_northing", "gps_fix_time"))
rm(impervious)
date_steps <- date_steps %>%
  dplyr::group_by(animal_id, gps_utm_easting, gps_fix_time, species, tod_, 
                  gps_utm_northing, sl_, dir_, ta_, burst_11_h, sl_11_h, 
                  dir_11_h, ta_11_h, burst_5_h, sl_5_h, dir_5_h, ta_5_h, 
                  burst_10_m, sl_10_m, dir_10_m, ta_10_m, uid) %>%
  summarise_each(funs(mean)) %>%
  as_tibble()
# Join population data
date_steps <- date_steps %>%
  full_join(population, by = c("animal_id", "species", "gps_utm_easting", 
                               "gps_utm_northing", "gps_fix_time"))
rm(population)
date_steps <- date_steps %>%
  dplyr::group_by(animal_id, gps_utm_easting, gps_fix_time, species, tod_, 
                  gps_utm_northing, sl_, dir_, ta_, burst_11_h, sl_11_h, 
                  dir_11_h, ta_11_h, burst_5_h, sl_5_h, dir_5_h, ta_5_h, 
                  burst_10_m, sl_10_m, dir_10_m, ta_10_m, uid) %>%
  summarise_each(funs(mean)) %>%
  as_tibble()
# Join coi data
date_steps <- date_steps %>%
  full_join(coi, by = c("animal_id", "species", "gps_utm_easting", 
                        "gps_utm_northing", "gps_fix_time"))
rm(coi)
date_steps <- date_steps %>%
  dplyr::group_by(animal_id, gps_utm_easting, gps_fix_time, species, tod_, 
                  gps_utm_northing, sl_, dir_, ta_, burst_11_h, sl_11_h, 
                  dir_11_h, ta_11_h, burst_5_h, sl_5_h, dir_5_h, ta_5_h, 
                  burst_10_m, sl_10_m, dir_10_m, ta_10_m, uid) %>%
  summarise_each(funs(mean)) %>%
  as_tibble()
# Rejoin the full data to the steps dataset by UID
steps <- steps %>%
  full_join(date_steps, by = c("animal_id", "gps_utm_easting", "species", 
                               "tod_", "gps_utm_northing", "sl_", "dir_", "ta_", 
                               "burst_11_h", "sl_11_h", "dir_11_h", "ta_11_h", 
                               "burst_5_h", "sl_5_h", "dir_5_h", "ta_5_h", 
                               "burst_10_m", "sl_10_m", "dir_10_m", "ta_10_m", 
                               "uid"))
steps$uid <- NULL
steps$gps_fix_time.y <- NULL
names(steps)[names(steps) == "gps_fix_time.x"] <- "gps_fix_time"
rm(date_steps)
# Join all spatial variables ---------------------------------------------------
# Join DEM data
steps <- steps %>%
  full_join(dem, by = c("animal_id", "gps_utm_easting", "species", 
                               "gps_utm_northing", "gps_fix_time"))
rm(dem)
# Join habitat data
steps <- steps %>%
  full_join(habitat, by = c("animal_id", "gps_utm_easting", "species", 
                        "gps_utm_northing", "gps_fix_time"))
rm(habitat)
# Join all to the movement dataset ---------------------------------------------
# Join the datasets
movement$uid <- paste(movement$animal_id, "_", rownames(movement))
movement <- movement %>%
  full_join(steps, by = c("animal_id", "gps_utm_easting", "species", 
                          "gps_utm_northing", "gps_fix_time"))
rm(steps)
# Export data ------------------------------------------------------------------
# Export the entire data set
movement %>% 
  write.csv(here("data/processed_data/full_data.csv"), row.names = FALSE)
# Export the entire locational data set
movement[!is.na(movement$sl_), ] %>%
  write.csv(here("data/processed_data/full_data_locations.csv"), row.names = FALSE)
#===============================================================================