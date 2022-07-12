#===============================================================================
### Create the random points dataset
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Loading data -----------------------------------------------------------------
# Load Packages
library(here)
library(lubridate)
library(sf)
library(tidyverse)
# Read in the data with step intervals and range behavioral data
gps <- tibble(read.csv(here("data/processed_data/step_intervals_appended.csv"))) %>%
  filter((!is.na(burst_11_h) & !is.na(burst_5_h)) |
           !is.na(burst_11_h) |
           !is.na(burst_5_h)) %>%
  filter(!is.na(season)) %>%
  filter(behavior == "resident")
# Water dataset, dissolved
water <- read_sf(here("data/gis_layers/shp_water_lakes_rivers/LakesAndRivers.shp")) %>%
  st_union()
# Coerce all variables into the right format. This could take a bit.
gps <- gps %>%
  mutate_at(vars(animal_id, species, season, behavior, tod_, burst_11_h, 
                 burst_5_h, burst_10_m), 
            funs(as.factor)) %>%
  mutate_at(vars(gps_utm_northing, gps_utm_easting), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), funs(as_datetime))
message("GPS dataset should be >16000 obs. of 8 variables.")
# Create season pooled

# Read in ranges, filtering for the 95% anisotropic KDEs
ranges <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(method == "kde_ani_95") %>%
  filter(season != "annual")
# Simplify previous stratifications of ranges and season years
ranges$animal_id[ranges$animal_id == "F7_1" | 
                 ranges$animal_id == "F7_2"] <- "F7"
ranges$animal_id[ranges$animal_id == "C9_1" | 
                 ranges$animal_id == "C9_2"] <- "C9"
ranges$season[ranges$season == "mate_20" | 
              ranges$season == "mate_21" | 
              ranges$season == "mate_22"] <- "mate"
ranges$season[ranges$season == "other_20" | 
                ranges$season == "other_21"] <- "other"
ranges$season[ranges$season == "pup_20" | 
                ranges$season == "pup_21"] <- "pup"
# If there are multiple seasons observed for an individual, union those into one
# seasonal home range for analysis.
ids <- levels(as.factor(ranges$animal_id))
season <- levels(as.factor(ranges$season))
for (i in ids) {
  for (j in season) {
    if (nrow(ranges[ranges$animal_id == i & ranges$season == j, ]) > 1) {
      ranges[ranges$animal_id == i & 
             ranges$season == j, ]$geometry <- st_union(ranges[ranges$animal_id == i & 
                                                               ranges$season == j, ])
    }
  }
}
# Remove repeats
ranges <- distinct(ranges)
# Create an ice-off subset for seasonal home ranges, based on Bde Maka Ska 
# median values.
ice_offs <- ranges
ranges <- ranges[ranges$season != "other", ]
ranges$ice <- 1
for (i in 1:nrow(ice_offs)) {
  ice_offs$geometry[i] <- st_difference(ice_offs$geometry[i], water)
}
ice_offs$ice <- 0
ranges <- rbind(ranges, ice_offs)
rm(i, ids, j, season)
# We'll generate 100 locations for each observed at greater than 5.5 hr
# First, classify into ice on or off
for (i in 1:nrow(gps)) {
  if ((gps$gps_fix_time[i] <= ymd("2019-12-11")) |
      (gps$gps_fix_time[i] > ymd("2020-04-09") & 
       gps$gps_fix_time[i] <= ymd("2020-12-11")) |
      (gps$gps_fix_time[i] > ymd("2021-04-09") & 
       gps$gps_fix_time[i] <= ymd("2021-12-11")) |
      (gps$gps_fix_time[i] > ymd("2022-04-09") & 
       gps$gps_fix_time[i] <= ymd("2022-12-11"))) {
    gps$ice[i] <- 0
  } else {
    gps$ice[i] <- 1
  }
}
# Create a sample number column, 100 per observation
# First get GPS data into the right format
gps$season <- as.character(gps$season)
gps$animal_id <- as.character(gps$animal_id)
gps$animal_id[gps$animal_id == "F7_1" | gps$animal_id == "F7_2"] <- "F7"
gps$animal_id[gps$animal_id == "C9_1" | gps$animal_id == "C9_2"] <- "C9"
gps$season[gps$season == "mate_20" | 
             gps$season == "mate_21" | 
             gps$season == "mate_22"] <- "mate"
gps$season[gps$season == "other_20" | 
             gps$season == "other_21"] <- "other"
gps$season[gps$season == "pup_20" | 
             gps$season == "pup_21"] <- "pup"
gps$animal_id <- as.factor(gps$animal_id)
gps$season <- as.factor(gps$season)
# If there are multiple seasons observed for an individual, union those into one
# seasonal home range for analysis.
ids <- levels(as.factor(ranges$animal_id))
season <- levels(as.factor(ranges$season))
ranges <- ranges %>%
  select(!area)
for (i in ids) {
  for (j in season) {
    for (k in seq(0, 1)) {
      if (nrow(ranges[ranges$animal_id == i & ranges$season == j & ranges$ice == k, ]) > 1) {
        ranges[ranges$animal_id == i & 
                 ranges$season == j & 
                 ranges$ice == k, ]$geometry <- st_union(ranges[ranges$animal_id == i & 
                                                                  ranges$season == j & 
                                                                  ranges$ice == k, ])
      }
    }
  }
}
# Remove repeats
ranges <- distinct(ranges)
# Now iterate through, calculating the number of points per range type
ranges$sample <- NA
ids <- levels(as.factor(ranges$animal_id))
season <- levels(as.factor(ranges$season))
for (i in ids) {
  for (j in season) {
    if (nrow(ranges[ranges$animal_id == i & ranges$season == j, ]) == 0) {
      next
    } 
    if (nrow(ranges[ranges$animal_id == i & ranges$season == j, ]) == 2) {
      ices <- nrow(gps[gps$animal_id == i & gps$season == j & gps$ice == 1, ])
      noices <- nrow(gps[gps$animal_id == i & gps$season == j & gps$ice == 0, ])
      ranges[ranges$animal_id == i & ranges$season == j &
               ranges$ice == 0, ]$sample <- (10 * noices)
      ranges[ranges$animal_id == i & ranges$season == j &
               ranges$ice == 1, ]$sample <- (10 * ices)
    } else if (ranges[ranges$animal_id == i & ranges$season == j, ]$ice == 0) {
      noices <- nrow(gps[gps$animal_id == i & gps$season == j & gps$ice == 0, ])
      ranges[ranges$animal_id == i & ranges$season == j, ]$sample <- (10 * noices)
    } else if (ranges[ranges$animal_id == i & ranges$season == j, ]$ice == 1) {
      ices <- nrow(gps[gps$animal_id == i & gps$season == j & gps$ice == 1, ])
      ranges[ranges$animal_id == i & ranges$season == j, ]$sample <- (10 * ices)
    }
  }
}
# Create a sample points dataset -----------------------------------------------
# Table for iteration
sample_points <- tibble()
gps$strata <- NA
# Create points
for (i in 1:nrow(ranges)) {
  # Select range
  range <- ranges[i, ]
  id <- ranges[i, ]$animal_id
  ssn <- ranges[i, ]$season
  ice <- ranges[i, ]$ice
  samp <- (ranges[i, ]$sample / 10)
  if (nrow(gps[gps$animal_id == id & 
               gps$season == ssn & 
               gps$ice == ice, ]) == 0) {
    next
  }
  # Generate pts within the range
  points <- st_sample(range, size = range$sample, type = 'random', 
                      exact = TRUE)
  # Translate into a data table
  coords <- do.call(rbind, st_geometry(points)) %>% 
    as_tibble() %>% 
    setNames(c("gps_utm_easting", "gps_utm_northing"))
  # Append useful columns from the ranges dataset
  no_geom <- st_drop_geometry(range)
  coords$animal_id <- range$animal_id
  coords$season <- range$season
  coords$behavior <- range$range_type
  # Create a strata variable
  case_strata <- 1:samp
  cont_strata <- rep(1:samp, each = 10)
  # Group by ice or no
  if (ice == 1) {
    coords$strata <- paste0(cont_strata, "_ice")
    gps[gps$animal_id == id & gps$season == ssn & 
          gps$ice == ice, ]$strata <- paste0(case_strata, "_ice")
  } else if (ice == 0) {
    coords$strata <- paste0(cont_strata, "_no")
    gps[gps$animal_id == id & gps$season == ssn & 
          gps$ice == ice, ]$strata <- paste0(case_strata, "_no")
  }
  # Append to the wider dataset
  sample_points <- rbind(sample_points, coords)
}
rm(coords, i, no_geom, points, range, ranges)
# Add a species column
sample_points$species <- NA
sample_points[grep("C", sample_points$animal_id), ]$species <- "coyote"
sample_points[grep("F", sample_points$animal_id), ]$species <- "red fox"
sample_points[sample_points$animal_id == "F10" | 
              sample_points$animal_id == "F18", ]$species <- "gray fox"
# Join turn columns into factors
sample_points <- sample_points %>%
  mutate_at(vars(animal_id, species, season, behavior), funs(as.factor))
# Add a case column, control for random points
sample_points$case <- "control"
# Format RSF data --------------------------------------------------------------
# Make sure data fields match
gps$season <- as.character(gps$season)
gps$animal_id <- as.character(gps$animal_id)
gps$animal_id[gps$animal_id == "F7_1" | gps$animal_id == "F7_2"] <- "F7"
gps$animal_id[gps$animal_id == "C9_1" | gps$animal_id == "C9_2"] <- "C9"
gps$season[gps$season == "mate_20" | 
           gps$season == "mate_21" | 
           gps$season == "mate_22"] <- "mate"
gps$season[gps$season == "other_20" | 
           gps$season == "other_21"] <- "other"
gps$season[gps$season == "pup_20" | 
           gps$season == "pup_21"] <- "pup"
gps$animal_id <- as.factor(gps$animal_id)
gps$season <- as.factor(gps$season)
# Filter intervals for residents less than 5.5 hours
rsf_gps <- gps %>%
  filter(behavior == "resident")
# Add a case column, case for observed locations
rsf_gps$case <- "case"
# Join case and control points -------------------------------------------------
rsf_gps <- rsf_gps %>%
  bind_rows(sample_points) %>%
  dplyr::select(animal_id, species, gps_fix_time, gps_utm_northing, 
                gps_utm_easting, gps_altitude, gps_longitude, gps_latitude, 
                season, behavior, tod_, burst_11_h, sl_11_h, dir_11_h, ta_11_h, 
                burst_5_h, sl_5_h, dir_5_h, ta_5_h, case, strata)
rsf_gps$case <- as.factor(rsf_gps$case)
rm(sample_points, gps)
# Export -----------------------------------------------------------------------
rsf_gps %>%
  write.csv(here("data/processed_data/rsf_locations.csv"), row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================