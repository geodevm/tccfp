#===============================================================================
### Append step lengths, turn angles, etc. for all intervals
# Loading data -----------------------------------------------------------------
# Load Packages
library(tidyverse)
library(lubridate)
library(amt)
library(here)
# Read in GPS data -------------------------------------------------------------
gps <- tibble(read.csv(here("data/processed_data/gps_data.csv")))
# Coerce all variables into the right format. This could take a bit.
gps <- gps %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, gps_utm_easting, 
                 gps_altitude), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), funs(as_datetime))
message("Warnings about dplyr deprecated functions are ok.")
message("GPS dataset should be >55000 obs. of 8 variables.")
# Make tracks for different time periods ---------------------------------------
# Make a track using amt that will keep animal id and species, adding time of 
# day.
gps_trk <- gps %>%
  make_track(.x = gps_utm_easting,
             .y = gps_utm_northing,
             .t = gps_fix_time,
             id = animal_id,
             sp = species,
             crs = "epsg:26915") %>%
  time_of_day(include.crepuscule = TRUE)
plot(gps_trk)
rm(gps)
# Make an 11-hr track with steps
gps_11_h <- gps_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) x %>%
                          track_resample(rate = hours(11), 
                                         tolerance = minutes(10)) %>%
                          steps_by_burst())) %>% 
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "resample")
# Make a 5.5-hr track with steps
gps_5_h <- gps_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) x %>%
                          track_resample(rate = minutes(330), 
                                         tolerance = minutes(10)) %>%
                          filter_min_n_burst(2) %>%
                          steps_by_burst())) %>%  
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "resample")
# Make a 10-min track with steps
gps_10_m <- gps_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) x %>%
                          track_resample(rate = minutes(10), 
                                         tolerance = minutes(5)) %>%
                          filter_min_n_burst(2) %>%
                          steps_by_burst())) %>%
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "resample")
# Process tracks ---------------------------------------------------------------
# Collect the first steps that do not have step lengths or related attributes
trks <- list(gps_11_h, gps_5_h, gps_10_m)
rm(gps_11_h, gps_5_h, gps_10_m)
firsts <- list()
firsts_i <- tibble::tibble()
for (i in 1:3) {
  for (j in 1:(nrow(trks[[i]]) - 1)) {
    if (j == 1) {
      firsts_i <- rbind(firsts_i, trks[[i]][j, ])
    } else if (trks[[i]]$burst_[j] != trks[[i]]$burst_[j + 1]) {
      firsts_i <- rbind(firsts_i, trks[[i]][j + 1, ])
    }
  }
  firsts[[i]] <- firsts_i
}
rm(firsts_i, i, j)
# Rename columns within the trks
trks_names <- c("gps_11_h", "gps_5_h", "gps_10_m")
for (i in 1:3) {
  tbl <- trks[[i]]
  tbl_name <- trks_names[i]
  sl <- paste0("sl_", tbl_name)
  burst <- paste0("burst_", tbl_name)
  ta <- paste0("ta_", tbl_name)
  dir <- paste0("dir_", tbl_name)
  oldnames <- c("id", "t2_", "y2_", "x2_", "sl_", "burst_", "ta_", 
                "direction_p")
  newnames <- c("animal_id", "gps_fix_time", "gps_utm_northing", 
                "gps_utm_easting", sl, burst, ta, dir)
  trks[[i]] <- tbl %>% rename_with(~ newnames[which(oldnames == .x)], 
                                   .cols = oldnames)
}
rm(burst, dir, i, newnames, oldnames, sl, ta, tbl_name, tbl)
# Remove irrelevant columns from the trks
drops  <- c("x1_", "y1_", "t1_", "dt_")
for (i in 1:3) {
  trks[[i]] <- trks[[i]][, !(names(trks[[i]]) %in% drops)]
}
rm(drops, i)
# Remove irrelevant columns from the first steps, and rename the columns for 
# joining
drops  <- c("x2_", "y2_", "t2_", "dt_")
for (i in 1:3) {
  tbl_name <- trks_names[i]
  firsts[[i]] <- firsts[[i]] %>% select(!7:9)
  firsts[[i]] <- firsts[[i]][, !(names(firsts[[i]]) %in% drops)]
  burst <- paste0("burst_", tbl_name)
  oldnames <- c("id", "t1_", "y1_", "x1_", "burst_")
  newnames <- c("animal_id", "gps_fix_time", "gps_utm_northing", 
                "gps_utm_easting", burst)
  firsts[[i]] <- firsts[[i]] %>% rename_with(~ newnames[which(oldnames == .x)], 
                                             .cols = oldnames)
}
rm(burst, drops, i, newnames, oldnames, tbl_name, trks_names)
# Make a full track data set ---------------------------------------------------
# Make a trk with steps for the entire data set
lvls <- levels(as.factor(gps_trk$id))
gps_steps <-tibble()
for (i in lvls) {
  gps_steps_i <- gps_trk[gps_trk$id == i, ] %>%
    steps(keep_cols = "end")
  gps_steps <- rbind(gps_steps, gps_steps_i)
}
rm(lvls, i, gps_steps_i, gps_trk)
# Collect the first steps that do not have step lengths or related attributes
first_steps <- tibble::tibble()
for (i in 1:(nrow(gps_steps) - 1)) {
  if (i == 1) {
    first_steps <- rbind(first_steps, gps_steps[i, ])
  } else if (gps_steps$id[i] != gps_steps$id[i + 1]) {
    first_steps <- rbind(first_steps, gps_steps[i + 1, ])
  }
}
rm(i)
# Remove irrelevant columns from the first steps
drops  <- c("x2_", "y2_", "t2_", "dt_")
first_steps <- first_steps %>% select(!5:7)
first_steps <- first_steps[, !(names(first_steps) %in% drops)]
rm(drops)
# Rename columns from the first steps
oldnames <- c("id", "sp", "t1_", "y1_", "x1_")
newnames <- c("animal_id", "species", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting")
first_steps <- first_steps %>% rename_with(~ newnames[which(oldnames == .x)], 
                                           .cols = oldnames)
rm(oldnames, newnames)
# Remove irrelevant columns from the full data set trk
drops  <- c("x1_", "y1_", "t1_", "dt_")
gps_steps <- gps_steps[, !(names(gps_steps) %in% drops)]
rm(drops)
# Rename columns from the full data set trk
oldnames <- c("id", "sp", "t2_", "y2_", "x2_")
newnames <- c("animal_id", "species", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting")
gps_steps <- gps_steps %>% rename_with(~ newnames[which(oldnames == .x)], 
                                       .cols = oldnames)
rm(oldnames, newnames)
# Bind the first steps and the full data trk together
stepped_gps <- as_tibble(bind_rows(gps_steps, first_steps))
rm(gps_steps, first_steps)
# Join all time intervals together ---------------------------------------------
# Join all the other trks with the full data set
for (i in 1:3) {
  stepped_gps <- stepped_gps %>%
    full_join(trks[[i]], by = c("animal_id", "gps_utm_easting", 
                                "gps_utm_northing", "gps_fix_time"))
}
rm(i, trks)
# Join all the other first steps with the full data set
for (i in 1:3) {
  stepped_gps <- stepped_gps %>%
    left_join(firsts[[i]], by = c("animal_id", "gps_utm_easting", 
                                "gps_utm_northing", "gps_fix_time"))
}
rm(i, firsts)
# There are duplicate rows, so do this to make sure that I'm not losing any
# data from duplicated columns
for (i in 1:nrow(stepped_gps)) {
  if(is.na(stepped_gps$burst_gps_11_h.x[i])) {
    stepped_gps$burst_gps_11_h.x[i] <- stepped_gps$burst_gps_11_h.y[i]
  }
  if(is.na(stepped_gps$burst_gps_5_h.x[i])) {
    stepped_gps$burst_gps_5_h.x[i] <- stepped_gps$burst_gps_5_h.y[i]
  }
  if(is.na(stepped_gps$burst_gps_10_m.x[i])) {
    stepped_gps$burst_gps_10_m.x[i] <- stepped_gps$burst_gps_10_m.y[i]
  }
}
rm(i)
# Remove the irrelevant columns
drops  <- c("burst_gps_11_h.y", "burst_gps_5_h.y", "burst_gps_10_m.y")
stepped_gps <- stepped_gps[, !(names(stepped_gps) %in% drops)]
rm(drops)
# Here I need to get rid of duplicate rows. Using the mean function on numeric
# values is just a stand in. All values should be equal except NAs that were
# introduced, and the mean is just a stand in because it will keep the number
# when compared to a NA value.
stepped_gps <- stepped_gps %>%
  dplyr::group_by(animal_id, gps_utm_easting, gps_fix_time, species, tod_, 
                  gps_utm_northing) %>%
  summarise_each(funs(mean)) %>%
  as_tibble()
# Export -----------------------------------------------------------------------
# Rename columns for exporting
oldnames <- c("direction_p", "burst_gps_11_h.x", "burst_gps_5_h.x", 
              "burst_gps_10_m.x", "sl_gps_11_h", "dir_gps_11_h", "ta_gps_11_h",
              "sl_gps_5_h", "dir_gps_5_h", "ta_gps_5_h", "sl_gps_10_m", 
              "dir_gps_10_m", "ta_gps_10_m")
newnames <- c("dir_", "burst_11_h", "burst_5_h", "burst_10_m", "sl_11_h", 
              "dir_11_h", "ta_11_h", "sl_5_h", "dir_5_h", "ta_5_h", "sl_10_m", 
              "dir_10_m", "ta_10_m")
stepped_gps <- stepped_gps %>% rename_with(~ newnames[which(oldnames == .x)], 
                                           .cols = oldnames)
rm(oldnames, newnames)
# Export
stepped_gps %>%
  write.csv(here("data/processed_data/step_intervals_appended.csv"), 
            row.names = FALSE)
#===============================================================================