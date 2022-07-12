#===============================================================================
### Data cleaning protocol for movement data
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Loading data -----------------------------------------------------------------
# If you have not yet used my new package "trackpack" designed for working with
# Telonics reports datasets, execute the following code:
# If you haven't used devtools before:
# install.packages("devtools") 
# devtools::install_github("geodevm/trackpack")
# Load packages (you may need to install some of these)
library(trackpack)
library(tidyverse)
library(lubridate)
library(amt)
library(sf)
library(rgdal)
library(here)
# Install leaflet if there will be manual data examination
#library(leaflet)
# Read in data -----------------------------------------------------------------
# Read in the Telonics data from from the reports directory
collars <- wrangle_telonics(here("data/telonics_reports"))
# Read in the biologicals data. If cleaned data from environment exists, read
# that in instead.
if (exists("biologicals")) {
  bios <- biologicals
  message("Pre-processed biologicals data found. Nice.")
} else if (exists("bios")) {
  message("Pre-processed biologicals data found. Nice.")
} else {
  message(
    "Pre-processed biologicals data has not been found. Processing now."
    )
  # Load in data
  bios <- read.csv(here("data/processed_data/biologicals_data.csv"))
  # Coerce all variables into the right format
  bios <- bios %>%
    mutate_at(vars(animal_id, species, teeth_color, teeth_wear, sagital_crest,
                   sex, age_determination, cod_proximate, fate), 
              funs(as.factor)) %>%
    mutate_at(vars(ketamine, xylazine, additional_ketamine, additional_xylazine, 
                   body_length, tail_length, blood_collected, atipamezole, weight, 
                   hair_sample_g, na, mg, al, p, k, ca, v, cr, mn, fe, co, ni, cu, 
                   zn, as, se, cd, pb, neck_circumference, temp_1, temp_2, temp_3, 
                   temp_4), 
              funs(as.numeric)) %>%
    mutate_at(vars(heartworm_antigen, ehrlichia_antibody, lyme_disease_antibody,
                   anaplasmosis_antibody, l_autumn, l_brat, l_can, l_grip, l_hard, 
                   l_ict, l_pom, t_gondii_igg, t_gondii_igm, parvo_canine,
                   canine_distemper, mange, redeploy, fleas, fecal, hair, scat, 
                   teeth_cond, covid_antigen, covid_oral, covid_rectal, 
                   covid_nasal, retrieved, redepcollar, t_c_s, t_c_s_ct, t_l_s, 
                   t_l_s_ct, s_s, s_s_ct, c_s, c_s_ct, g_z, g_z_ct, s_z, s_z_ct, 
                   u_s, u_s_ct, u_z, u_z_ct, t_g_z, t_g_z_ct, t_s_l_s, t_s_l_s_ct,
                   e_s, e_s_ct, t_g_s, t_g_s_ct, m_s, m_s_ct, c_z, c_z_ct, cr_s,
                   cr_s_ct, ca_s, ca_s_ct, i_s, i_s_ct, t_v_s, t_v_s_ct, d_l_s, 
                   d_l_s_ct, i_z, i_z_ct, a_c_z, a_c_z_ct, t_v_z, t_v_z_ct, 
                   cod_mange, cod_canine_trauma, retrieved, dropped, dead_battery,
                   unknown_fate, mortality), 
              funs(as.integer)) %>%
    mutate_at(vars(date_processed, date_inactive), 
              funs(as_date)) %>%
    mutate_at(vars(injection_time, additional_injection_time, induction_time,
                   reversal_time, time_alert, temp_1_time, temp_2_time,
                   temp_3_time, temp_4_time, release_time), 
              funs(as_datetime))
}
# Compile and reformat collars data  -------------------------------------------
# I like underscores and all lowercase in headers
names(collars) <- names(collars) %>% tolower()
names(collars) <- gsub("\\.", "_", names(collars))
#Create a collar identification number column
collars$collar_id[collars$ctn == "712851A"] <- "F1"
collars$collar_id[collars$ctn == "712852A"] <- "F2"
collars$collar_id[collars$ctn == "712853A"] <- "F3"
collars$collar_id[collars$ctn == "712854A"] <- "F4"
collars$collar_id[collars$ctn == "721909A"] <- "F5"
collars$collar_id[collars$ctn == "712856A"] <- "F6"
collars$collar_id[collars$ctn == "712857A"] <- "F7"
collars$collar_id[collars$ctn == "712859A"] <- "F8"
collars$collar_id[collars$ctn == "712860A"] <- "F9"
collars$collar_id[collars$ctn == "712861A"] <- "F10"
collars$collar_id[collars$ctn == "712862A"] <- "F11"
collars$collar_id[collars$ctn == "712863A"] <- "F12"
collars$collar_id[collars$ctn == "712864A"] <- "F13"
collars$collar_id[collars$ctn == "713584A"] <- "F14"
collars$collar_id[collars$ctn == "712866A"] <- "F15"
collars$collar_id[collars$ctn == "712867A"] <- "F16"
collars$collar_id[collars$ctn == "712868A"] <- "F17"
collars$collar_id[collars$ctn == "712869A"] <- "F18"
collars$collar_id[collars$ctn == "712879A"] <- "C1"
collars$collar_id[collars$ctn == "712880A"] <- "C2"
collars$collar_id[collars$ctn == "712881A"] <- "C3"
collars$collar_id[collars$ctn == "712882A"] <- "C4"
collars$collar_id[collars$ctn == "712883A"] <- "C5"
collars$collar_id[collars$ctn == "712884A"] <- "C6"
collars$collar_id[collars$ctn == "712885A"] <- "C7"
collars$collar_id[collars$ctn == "712886A"] <- "C8"
collars$collar_id[collars$ctn == "712887A"] <- "C9"
collars$collar_id[collars$ctn == "712888A"] <- "C10"
collars$collar_id[collars$ctn == "719839A"] <- "C11"
collars$collar_id[collars$ctn == "719840A"] <- "C12"
collars$collar_id[collars$ctn == "719841A"] <- "C13"
collars$collar_id[collars$ctn == "719842B"] <- "C14"
collars$collar_id[collars$ctn == "719843A"] <- "C15"
# Create a species identification column
collars$species[collars$ctn == "712851A"] <- "red fox"
collars$species[collars$ctn == "712852A"] <- "red fox"
collars$species[collars$ctn == "712853A"] <- "red fox"
collars$species[collars$ctn == "712854A"] <- "red fox"
collars$species[collars$ctn == "721909A"] <- "red fox"
collars$species[collars$ctn == "712856A"] <- "red fox"
collars$species[collars$ctn == "712857A"] <- "red fox"
collars$species[collars$ctn == "712859A"] <- "red fox"
collars$species[collars$ctn == "712860A"] <- "red fox"
collars$species[collars$ctn == "712862A"] <- "red fox"
collars$species[collars$ctn == "712863A"] <- "red fox"
collars$species[collars$ctn == "712864A"] <- "red fox"
collars$species[collars$ctn == "713584A"] <- "red fox"
collars$species[collars$ctn == "712866A"] <- "red fox"
collars$species[collars$ctn == "712867A"] <- "red fox"
collars$species[collars$ctn == "712868A"] <- "red fox"
collars$species[collars$ctn == "712861A"] <- "gray fox"
collars$species[collars$ctn == "712869A"] <- "gray fox"
collars$species[collars$ctn == "712879A"] <- "coyote"
collars$species[collars$ctn == "712880A"] <- "coyote"
collars$species[collars$ctn == "712881A"] <- "coyote"
collars$species[collars$ctn == "712882A"] <- "coyote"
collars$species[collars$ctn == "712883A"] <- "coyote"
collars$species[collars$ctn == "712884A"] <- "coyote"
collars$species[collars$ctn == "712885A"] <- "coyote"
collars$species[collars$ctn == "712886A"] <- "coyote"
collars$species[collars$ctn == "712887A"] <- "coyote"
collars$species[collars$ctn == "712888A"] <- "coyote"
collars$species[collars$ctn == "719839A"] <- "coyote"
collars$species[collars$ctn == "719840A"] <- "coyote"
collars$species[collars$ctn == "719841A"] <- "coyote"
collars$species[collars$ctn == "719842B"] <- "coyote"
collars$species[collars$ctn == "719843A"] <- "coyote"
# Make an animal id column
collars$animal_id <- collars$collar_id
# Datetimes using lubridate
collars$acquisition_time <- ymd_hms(collars$acquisition_time)
collars$gps_fix_time <- ymd_hms(collars$gps_fix_time)
collars$acquisition_start_time <- ymd_hms(collars$acquisition_start_time)
collars$receive_time <- ymd_hms(collars$receive_time)
# Deal with redeployment of collars
collars$animal_id[(collars$collar_id == "C1" & collars$acquisition_time >= 
                  ymd_hms(bios$release_time[bios$animal_id == "C1.1"]))] <- "C1.1"
collars$animal_id[(collars$collar_id == "C5" & collars$acquisition_time >= 
                  ymd_hms(bios$release_time[bios$animal_id == "C5.1"]))] <- "C5.1"
# Also deal with the gps points fixed when moved after death in C1
collars <- collars[!(collars$animal_id == "C1" & collars$acquisition_time >= 
                    ymd(bios$date_inactive[bios$animal_id == "C1"])), ]
# Delete non-datalog data for C5
collars <- collars[!collars$animal_id == "C5", ]
# Retrieve the datalog for C5
wrangle_712883A <- function (path, tidy = TRUE) {
  old <- getwd()
  setwd(path)
  datalogs <- subset(list.files(), grepl("Datalog", list.files()))
  files <- subset(datalogs, grepl("712883A", datalogs))
                  new_files <- c()
                  for (i in levels(as.factor(gsub("([0-9]+).*$", "\\1", files)))) {
                    collar <- files[gsub("([0-9]+).*$", "\\1", files) == i]
                    collar <- collar[as.numeric(gsub("([0-9]+).*$", "\\1", 
                                                     substr(collar, 9, 13))) ==
                                       max(as.numeric(gsub("([0-9]+).*$", "\\1", 
                                                           substr(collar, 9, 
                                                                  13))))]
                    message(paste0("Data from file ", collar, 
                                   " was retrieved."))
                    new_files <- c(new_files, collar)
                  }
                  csv_list <- list()
                  for (i in 1:length(new_files)) {
                    csv_list[[i]] <- read.csv(new_files[i], header = TRUE, 
                                              skip = 22, na.strings = "", 
                                              stringsAsFactors = FALSE)
                    csv_list[[i]]$ctn <- gsub("([0-9]+).*$", "\\1", 
                                              new_files[i])
                  }
                  new_file <- do.call(dplyr::bind_rows, csv_list)
                  if (tidy) {
                   new_file <- tibble::tibble(new_file)
                   message(paste0("\nOutput is a tibble of size ",
                                  as.character(dim(new_file)[1]),
                                  " x ",
                                  as.character(dim(new_file)[2]),
                                  " from ",
                                  as.character(length(csv_list)),
                                  " collars."))
                   } else {
                    message(paste0("\nOutput is a data.frame of size ",
                                   as.character(dim(new_file)[1]),
                                   " x ",
                                   as.character(dim(new_file)[2]),
                                   " from ",
                                   as.character(length(csv_list)),
                                   " collar."))
                  }
                  setwd(old)
                  new_file
                  }
collars_C5 <- wrangle_712883A(here("data/telonics_reports"))
rm(wrangle_712883A)
# I like underscores and all lowercase in headers
names(collars_C5) <- names(collars_C5) %>% tolower()
names(collars_C5) <- gsub("\\.", "_", names(collars_C5))
#Create a collar identification number column
collars_C5$collar_id <- "C5"
# Create a species identification column
collars_C5$species <- "coyote"
# Make an animal id column
collars_C5$animal_id <- collars_C5$collar_id
# Datetimes
collars_C5$acquisition_time <- ymd_hms(collars_C5$acquisition_time)
collars_C5$gps_fix_time <- ymd_hms(collars_C5$gps_fix_time)
collars_C5$acquisition_start_time <- ymd_hms(collars_C5$acquisition_start_time)
# Rbind C5 with the rest of the data.
collars <- bind_rows(collars, collars_C5)
collars <- collars[!(collars$animal_id == "C5" & collars$acquisition_time >= 
                       bios$date_inactive[bios$animal_id == "C5"]), ]
rm(collars_C5)
# Remove dates post-morality or after a drop for all collars
ids <- levels(as.factor(collars$animal_id))
for (i in 1:length(ids)) {
  if (is.na(bios$date_inactive[bios$animal_id == ids[i]])) {
  } else {
    collars <- collars[!(collars$animal_id == ids[i] & 
                         collars$acquisition_time >= 
                           bios$date_inactive[bios$animal_id == ids[i]]), ] 
  }
}
rm(i)
# Remove all pre-deployment data
for (i in 1:length(ids)) {
  collars <- collars[!(collars$animal_id == ids[i] & collars$acquisition_time <= 
                         (ymd_hms(bios$release_time[bios$animal_id == 
                                                    ids[i]]))), ]
}
rm(i)
# Retrieve activity and temperature data ---------------------------------------
# Retrieve a separate activity count data set
activity <- collars[!is.na(collars$activity_count), ]
# Remove 2 days after capture
for (i in 1:length(ids)) {
  activity <- activity[!(activity$animal_id == ids[i] & activity$acquisition_time <= 
                         (ymd_hms(bios$release_time[bios$animal_id == ids[i]]) + 
                            hours(48))), ]
}
rm(i)
# Retrieve a separate temperature data set
temps <- collars[!is.na(collars$temperature), ]
# Remove 2 days after capture
for (i in 1:length(ids)) {
  temps <- temps[!(temps$animal_id == ids[i] & temps$acquisition_time <= 
                           (ymd_hms(bios$release_time[bios$animal_id == ids[i]]) + 
                              hours(48))), ]
}
rm(i)
# GPS data cleaning ------------------------------------------------------------
# Get rid of unsuccessful GPS fixes
collars <- collars[!is.na(collars$gps_fix_attempt) | collars$gps_fix_attempt == 
                   "Failed", ]
# Only observations with complete cases
collars <- collars %>% filter(complete.cases(collars[, c("gps_longitude", 
                                                         "gps_latitude", 
                                                         "gps_fix_time")]))
# Store values for later summary
coy_ct <- count(collars[collars$species == "coyote", ])
red_ct <- count(collars[collars$species == "red fox", ])
gray_ct <- count(collars[collars$species == "gray fox", ])
# For first real filter, remove those 2 days from capture
for (i in 1:length(ids)) {
  collars <- collars[!(collars$animal_id == ids[i] & collars$acquisition_time <= 
                       (ymd_hms(bios$release_time[bios$animal_id == ids[i]]) + 
                        hours(48))), ]
}
rm(bios, ids, i)
# We want 3-dimensional fixes, which are locations with >= 4 satellites.
collars <- collars %>% filter(gps_satellite_count >= 4)
# HDOP cutoff determination
# First, figure out how many we're excluding
hist(collars$gps_horizontal_dilution[(collars$species == "red fox" | 
                                        collars$species == "gray fox")])
hist(collars$gps_horizontal_dilution[collars$species == "coyote"])
# We're going to use those with HDOP <=10 for now for all that have a recorded
# HDOP value.
collars <- collars %>% filter(gps_horizontal_dilution <= 10)
# Top biological speeds for each species
# Create UIDs
collars$uid <- paste0(collars$animal_id, "_", rownames(collars))
# Converting tracks for each species
coyotes_trk <- collars[collars$species == "coyote", ] %>%
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_time,
             id = animal_id,
             sp = species,
             uid = uid,
             lon = gps_longitude,
             lat = gps_latitude,
             crs = "epsg:26915")
reds_trk <- collars[collars$species == "red fox", ] %>%
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_time,
             id = animal_id,
             sp = species,
             uid = uid,
             lon = gps_longitude,
             lat = gps_latitude,
             crs = "epsg:26915")
grays_trk <- collars[collars$species == "gray fox", ] %>%
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_time,
             id = animal_id,
             sp = species,
             uid = uid,
             lon = gps_longitude,
             lat = gps_latitude,
             crs = "epsg:26915")
# Add a speed and step length column
coyotes_trk <- coyotes_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, amt::speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
reds_trk <- reds_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, amt::speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
grays_trk <- grays_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, amt::speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
# Convert velocity to km/h
coyotes_trk$v_ <- coyotes_trk$v_ * 3.6
reds_trk$v_ <- reds_trk$v_ * 3.6
grays_trk$v_ <- grays_trk$v_ * 3.6
# Coyote travel speeds > 69 km/h
coyotes_trk$fast <- NA
for (i in 2:nrow(coyotes_trk)) {
  if (i == 2) {
    if (is.na(coyotes_trk[i, ]$v_)) {
      coyotes_trk[i, ]$fast <- "no"
      coyotes_trk[(i - 1), ]$fast <- "no"
    } else if (coyotes_trk[i, ]$v_ >= 69 & coyotes_trk[(i - 1),]$v_ >= 69) {
      coyotes_trk[i, ]$fast <- "yes"
      coyotes_trk[(i - 1), ]$fast <- "no"
    } else {
      coyotes_trk[i, ]$fast <- "no"
      coyotes_trk[(i - 1), ]$fast <- "no"
    }
  } else if (is.na(coyotes_trk[i, ]$v_)) {
    coyotes_trk[i, ]$fast <- "no"
  } else if (coyotes_trk[i, ]$v_ >= 69 & coyotes_trk[(i - 1),]$v_ >= 69) {
    coyotes_trk[i, ]$fast <- "yes"
  } else {
    coyotes_trk[i, ]$fast <- "no"
  }
}
rm(i)
message(paste(count(coyotes_trk[coyotes_trk$fast == "yes", ]), 
              "fast observation(s) detected in coyote dataset."))
# Red fox travel speeds > 50 km/h
reds_trk$fast <- NA
for (i in 2:nrow(reds_trk)) {
  if (i == 2) {
    if (is.na(reds_trk[i, ]$v_)) {
      reds_trk[i, ]$fast <- "no"
      reds_trk[(i - 1), ]$fast <- "no"
    } else if (reds_trk[i, ]$v_ >= 50 & reds_trk[(i - 1), ]$v_ >= 50) {
      reds_trk[i, ]$fast <- "yes"
      reds_trk[(i - 1), ]$fast <- "no"
    } else {
      reds_trk[i, ]$fast <- "no"
      reds_trk[(i - 1), ]$fast <- "no"
    }
  } else if (is.na(reds_trk[i, ]$v_)) {
    reds_trk[i, ]$fast <- "no"
  } else if (reds_trk[i, ]$v_ >= 50 & reds_trk[(i - 1), ]$v_ >= 50) {
    reds_trk[i, ]$fast <- "yes"
  } else {
    reds_trk[i, ]$fast <- "no"
  }
}
rm(i)
message(paste(count(reds_trk[reds_trk$fast == "yes", ]), 
              "fast observation(s) detected in red fox dataset."))
# Gray fox max speed 45 km/h
grays_trk$fast <- NA
for (i in 2:nrow(grays_trk)) {
  if (i == 2) {
    if (is.na(grays_trk[i, ]$v_)) {
      grays_trk[i, ]$fast <- "no"
      grays_trk[(i - 1), ]$fast <- "no"
    } else if (grays_trk[i, ]$v_ >= 45 & grays_trk[(i - 1), ]$v_ >= 45) {
      grays_trk[i, ]$fast <- "yes"
      grays_trk[(i - 1), ]$fast <- "no"
    } else {
      grays_trk[i, ]$fast <- "no"
      grays_trk[(i - 1), ]$fast <- "no"
    }
  } else if (is.na(grays_trk[i, ]$v_)) {
    grays_trk[i, ]$fast <- "no"
  } else if (grays_trk[i, ]$v_ >= 45 & grays_trk[(i - 1),]$v_ >= 45) {
    grays_trk[i, ]$fast <- "yes"
  } else {
    grays_trk[i, ]$fast <- "no"
  }
}
rm(i)
message(paste(count(grays_trk[grays_trk$fast == "yes", ]), 
              "fast observation(s) detected in gray fox dataset."))
# Bind back together into one data frame
collars_trk <- rbind(coyotes_trk, reds_trk, grays_trk)
# There are some duplicate locations, which all have 0 step lengths, you can
# see that they are duplicates by further examination of timestamps and UIDs.
message(paste(count(collars_trk[(!is.na(collars_trk$sl_) & 
                                   collars_trk$sl_ == 0), ]), 
              "duplicate observations were detected in the full dataset. All duplicates will be removed."))
collars_trk <- collars_trk[!(!is.na(collars_trk$sl_) & collars_trk$sl_ == 0), ]
# Remove fast == 'yes' locations
collars_trk <- collars_trk %>% filter(!fast == "yes")
# Examine manually to see if anything was missed by algorithms
coyotes_trk[!is.na(coyotes_trk$v_) & coyotes_trk$v_ > 69, ]
reds_trk[!is.na(reds_trk$v_) & reds_trk$v_ > 50, ]
grays_trk[!is.na(grays_trk$v_) & grays_trk$v_ > 45, ]
rm(coyotes_trk, reds_trk, grays_trk)
# Remove those missed by algorithm
collars_trk <- collars_trk %>% filter(!uid == "F18_13570")
message("Manually removed the following observation(s) that were missed by the algorithm:
-Gray fox, F18, observation >64kmh.")
# Reclassify the track with the newly filtered data
collars_trk <- collars_trk %>%
  make_track(x_, y_, t_, id = id, sp = sp, uid = uid, lon = lon, lat = lat,
             crs = "epsg:26915")
# Add a speed and step length column
collars_trk <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, amt::speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
# Adjust velocity column to be km/hr
collars_trk$v_ <- collars_trk$v_ * 3.6
# Also classify those >5km/h for further investigation 
collars_5_plus <- collars_trk %>%
  filter(v_ > 5)
# I want to see how realistic these are, so I'll compare to the preceding 3
# observations and the following 3 observations
# A characteristic of unrealistic movements is two movements of abnormal velocity
# in a row. In this case it should be the one in question and the following one.
# I'm going to find a ratio to determine whether movements are realistic or not.
collars_trk$index <- 1:nrow(collars_trk)
collars_investigate <- tibble()
for (i in collars_5_plus$uid) {
  if (!any(collars_investigate$uid == i)) {
    row_i <- collars_trk$index[collars_trk$uid == i]
    # This algorithm doesn't work if where at the start or end of the dataset,
    # so these for loops look complicated but they generalize the formula to be 
    # compatible with those different data structures.
    for (j in 0:3) {
      if (collars_trk[row_i, ]$id == collars_trk[(row_i - j), ]$id) {
        row_befr <- as.numeric(j)
      }
    }
    for (j in 1:4) {
      if (collars_trk[row_i, ]$id == collars_trk[(row_i + j), ]$id) {
        row_aftr <- as.numeric(j)
      }
    }
    row_init <- row_i - row_befr
    row_term <- row_i + row_aftr
    collars_investigate_i <- collars_trk[row_init:row_term, ]
    row_ct <- nrow(collars_investigate_i)
    row_fast <- row_befr + 1
    collars_investigate_i$speedy <- "no"
    collars_investigate_i[row_fast, ]$speedy <- "yes"
    collars_investigate_i$burst <- i
    v_ave <- sum(collars_investigate_i$v_[row_fast:(row_fast + 1)]) / 2
    burst_ave <- ((ifelse(row_fast == 1, 0, 
                          sum(collars_investigate_i$v_[1:(row_fast - 1)])) + 
                   ifelse(row_ct == (row_fast + 1), 0,
                          sum(collars_investigate_i$v_[(row_fast + 2):row_ct])))) / 
                    (row_ct - 2)
    steps <- sum(collars_investigate_i$sl_[row_fast:(row_fast + 1)])
    final_dist <- sqrt(((collars_investigate_i[(row_fast + 2), ]$x_ - 
                           collars_investigate_i[row_fast, ]$x_)^2) +
                       ((collars_investigate_i[(row_fast + 2), ]$y_ - 
                           collars_investigate_i[row_fast, ]$y_)^2))
    dist_ratio <- steps / final_dist
    v_ratio <- v_ave / burst_ave
    if (dist_ratio <= 5 & v_ratio <= 5) {
      collars_investigate_i$good_ratio <- "yes"
    } else if (dist_ratio <= 5 & v_ratio > 5) {
      collars_investigate_i$good_ratio <- "maybe"
    } else if (dist_ratio > 5 & v_ratio <= 5) {
      collars_investigate_i$good_ratio <- "maybe"
    } else {
      collars_investigate_i$good_ratio <- "no"
    }
    collars_investigate_i$d_ratio <- dist_ratio
    collars_investigate_i$ve_ratio <- v_ratio
    collars_investigate_i$d <- final_dist
    collars_investigate <- rbind(collars_investigate, collars_investigate_i)
  }
}
rm(v_ratio, v_ave, steps, row_term, row_init, row_i, i, final_dist, dist_ratio, 
   burst_ave, collars_investigate_i, collars_5_plus, row_aftr, row_befr, j,
   row_ct, row_fast)
# Visualize for manual filtering
#pal <- colorFactor(c("navy", "red"), domain = c("yes", "no"))
#leaflet(collars_investigate[41:48,]) %>% 
#  addTiles()%>%
#  addCircleMarkers(~lon, ~lat,
#                   color = ~pal(speedy),
#                   label = ~uid)
# Confirm that the right values are being filtered
#view(collars_investigate[collars_investigate$good_ratio == "no", ])
# If all check out, we can run the following:
out <- levels(as.factor(collars_investigate[collars_investigate$good_ratio == 
                                              "no", ]$burst))
collars_trk <- collars_trk %>% filter(!uid %in% out)
# Check those labelled "maybe" to see if any were missed by algorithm
collars_trk <- collars_trk %>% filter(!uid == "C9_38446")
message("Manually removed the following observation(s) that were missed by the algorithm:
-Coyote, C9, observation >19kmh.")
collars <- collars %>% filter(uid %in% collars_trk$uid)
rm(collars_investigate, collars_trk, out)
# Make sure there are no more duplicate values
sum <- collars %>%
  select(acquisition_time, gps_longitude, gps_latitude, animal_id) %>%
  duplicated %>%
  sum()
if (sum == 0) {
  message("SUCCESS")
} else { 
  message("FAILURE")
  quit(save="ask")
}
rm(sum)
# Filtering summary
coy_ct_2 <- count(collars[collars$species == "coyote", ])
red_ct_2 <- count(collars[collars$species == "red fox", ])
gray_ct_2 <- count(collars[collars$species == "gray fox", ])
coy_loss <- coy_ct - coy_ct_2
red_loss <- red_ct - red_ct_2
gray_loss <- gray_ct - gray_ct_2
coy_per <- (1 - (coy_ct_2 / coy_ct)) * 100
red_per <- (1 - (red_ct_2 / red_ct)) * 100
gray_per <- (1 - (gray_ct_2 / gray_ct)) * 100
summary <- tibble(species = c("coyote", "red fox", "gray fox"),
                  count = as.numeric(c(coy_ct_2, red_ct_2, gray_ct_2)),
                  lost = as.numeric(c(coy_loss, red_loss, gray_loss)),
                  percent_lost = as.numeric(c(coy_per, red_per, gray_per)))
message(summary)
rm(summary, coy_ct_2, red_ct_2, gray_ct_2, coy_loss, red_loss, gray_loss, 
   coy_per, red_per, gray_per, coy_ct, red_ct, gray_ct)
# GPS data exporting
# Columns to drop that are not useful for analysis
drops <- c("acquisition_time", "iridium_cep_radius", "iridium_latitude", 
           "iridium_longitude", "gps_fix_attempt", "gps_utm_zone", 
           "gps_horizontal_error", "gps_horizontal_dilution", 
           "gps_satellite_bitmap", "gps_satellite_count", 
           "gps_navigation_count", "gps_navigation_time", "activity_count", 
           "satellite_uplink", "receive_time", "repetition_count", 
           "low_voltage", "mortality", "iridium_command", "predeployment_data", 
           "error", "gps_speed", "gps_heading", "index", "uid", "collar_id", 
           "temperature", "ctn")
# Drop columns
collars <- collars[, !(names(collars) %in% drops)]
rm(drops)
# Coerce variables into the proper formats
collars <- collars %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, gps_utm_easting,
                 gps_altitude), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time, acquisition_start_time), funs(as.POSIXct))
message("Warnings about dplyr deprecated functions are ok.")
# Export GPS data alone
collars %>%
  dplyr::select(animal_id, species, gps_fix_time, gps_latitude, gps_longitude, 
         gps_utm_northing, gps_utm_easting, gps_altitude) %>%
  relocate(c("animal_id", "species", "gps_fix_time", "gps_utm_northing", 
             "gps_utm_easting", "gps_altitude")) %>%
  write.csv(here("data/processed_data/gps_data.csv"), row.names = FALSE)
# Activity counts data cleaning ------------------------------------------------
# Columns to drop that are not useful for analysis
drops <- c("iridium_cep_radius", "iridium_latitude", "iridium_longitude", 
           "gps_fix_time", "gps_fix_attempt", "gps_utm_zone", 
           "gps_horizontal_error", "gps_horizontal_dilution",
           "gps_satellite_bitmap", "gps_satellite_count", 
           "gps_navigation_count", "gps_navigation_time", "satellite_uplink",
           "receive_time", "repetition_count", "low_voltage", "mortality",
           "iridium_command", "predeployment_data", "error", "gps_speed",
           "gps_heading", "gps_longitude", "gps_latitude", "collar_id",
           "gps_utm_easting", "gps_utm_northing", "temperature", "gps_altitude",
           "ctn")
# Drop columns
activity <- activity[, !(names(activity) %in% drops)]
rm(drops)
# Coerce variables into the proper formats 
activity <- activity %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(activity_count), funs(as.numeric)) %>%
  mutate_at(vars(acquisition_time, acquisition_start_time), funs(as.POSIXct))
message("Warnings about dplyr deprecated functions are ok.")
# Rename acquisition time variable for joining data later
activity <- activity %>% rename(activity_fix_time = acquisition_time)
# Export activity data alone
activity %>%
  dplyr::select(animal_id, species, activity_fix_time, activity_count) %>%
  relocate(c("animal_id", "species", "activity_fix_time", "activity_count")) %>%
  write.csv(here("data/processed_data/activity_data.csv"), row.names = FALSE)
# Temperature data cleaning ----------------------------------------------------
# Columns to drop that are not useful for analysis
drops <- c("iridium_cep_radius", "iridium_latitude", "iridium_longitude", 
           "gps_fix_time", "gps_fix_attempt", "gps_utm_zone", 
           "gps_horizontal_error", "gps_horizontal_dilution",
           "gps_satellite_bitmap", "gps_satellite_count", 
           "gps_navigation_count", "gps_navigation_time", "satellite_uplink",
           "receive_time", "repetition_count", "low_voltage", "mortality",
           "iridium_command", "predeployment_data", "error", "gps_speed",
           "gps_heading", "gps_longitude", "gps_latitude", "collar_id",
           "gps_utm_easting", "gps_utm_northing", "activity_count", 
           "gps_altitude", "ctn")
# Drop columns
temps <- temps[, !(names(temps) %in% drops)]
rm(drops)
# Coerce variables into the proper formats
temps <- temps %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(temperature), funs(as.numeric)) %>%
  mutate_at(vars(acquisition_time, acquisition_start_time), funs(as.POSIXct))
message("Warnings about dplyr deprecated functions are ok.")
# Rename acquisition time variable for joining data later
temps <- temps %>% rename(temperature_fix_time = acquisition_time)
# Export temperature data alone
temps %>%
  dplyr::select(animal_id, species, temperature_fix_time, temperature) %>%
  relocate(c("animal_id", "species", "temperature_fix_time", "temperature")) %>%
  write.csv(here("data/processed_data/temperature_data.csv"), row.names = FALSE)
# Full movement data exporting -------------------------------------------------
# Join a full dataset based on common acquisition start times
movement <- temps %>%
  full_join(activity, by = c("acquisition_start_time", "animal_id")) %>%
  full_join(collars, by = c("acquisition_start_time", "animal_id"))
movement[is.na(movement$species), ]$species <- movement[is.na(movement$species), ]$species.y
movement[is.na(movement$species), ]$species <- movement[is.na(movement$species), ]$species.x
drops <- c("species.y", "species.x")
movement <- movement[, !(names(movement) %in% drops)]
rm(activity, collars, temps, drops)
# Reorder columns
movement <- movement %>%
  relocate(c("animal_id", "species", "gps_fix_time", "activity_fix_time", 
             "temperature_fix_time", "gps_utm_northing", "gps_utm_easting", 
             "gps_altitude", "activity_count", "temperature", "gps_longitude", 
             "gps_latitude"))
# Coerce variables into the proper formats
movement <- movement %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(temperature, gps_latitude, gps_longitude, gps_utm_northing, 
                 gps_utm_easting, gps_altitude, activity_count), 
            funs(as.numeric)) %>%
  mutate_at(vars(activity_fix_time, gps_fix_time, temperature_fix_time, 
                 acquisition_start_time), 
            funs(as.POSIXct))
message("Warnings about dplyr deprecated functions are ok.")
# Check for any duplicates
sum <- movement %>%
  dplyr::select(acquisition_start_time, animal_id) %>%
  duplicated %>%
  sum()
if (sum == 0) {
  message("SUCCESS")
} else { 
  message("FAILURE")
  quit(save="ask")
}
rm(sum)
# Export data
movement %>%
  write.csv(here("data/processed_data/movement_data.csv"), row.names = FALSE)
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================