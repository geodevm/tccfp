#===============================================================================
### Data cleaning protocol for movement data
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Loading data -----------------------------------------------------------------
# If you have not yet used my new package "trackpack" designed for working with
# Telonics reports datasets, execute the following code:
# If you haven't used devtools before:
#install.packages("devtools") 
#devtools::install_github("geodevm/trackpack")
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
                   sex, age_determination, cod_simple), 
              funs(as.factor)) %>%
    mutate_at(vars(ketamine, xylazine, additional_ketamine, additional_xylazine, 
                   body_length, tail_length, blood_collected, atipamezole, 
                   weight, hair_sample_g, na, mg, al, p, k, ca, v, cr, mn, fe, 
                   co, ni, cu, zn, as, se, cd, pb, neck_circumference, temp_1, 
                   temp_2, temp_3, temp_4), 
              funs(as.numeric)) %>%
    mutate_at(vars(heartworm_antigen, ehrlichia_antibody, lyme_disease_antibody,
                   anaplasmosis_antibody, l_autumn, l_brat, l_can, l_grip, 
                   l_hard, l_ict, l_pom, t_gondii_igg, t_gondii_igm, 
                   parvo_canine, canine_distemper, mange, redeploy, fleas, 
                   fecal, hair, scat, teeth_cond, covid_antigen, covid_oral, 
                   covid_rectal, covid_nasal, dropped, redepcollar, t_c_s, 
                   t_c_s_ct, t_l_s, t_l_s_ct, s_s, s_s_ct, c_s, c_s_ct, g_z, 
                   g_z_ct, s_z, s_z_ct, u_s, u_s_ct, u_z, u_z_ct, t_g_z, 
                   t_g_z_ct, t_s_l_s, t_s_l_s_ct, e_s, e_s_ct, t_g_s, t_g_s_ct, 
                   m_s, m_s_ct, c_z, c_z_ct, cr_s, cr_s_ct, ca_s, ca_s_ct, i_s, 
                   i_s_ct, t_v_s, t_v_s_ct, d_l_s, d_l_s_ct, i_z, i_z_ct, a_c_z, 
                   a_c_z_ct, t_v_z, t_v_z_ct), 
              funs(as.integer)) %>%
    mutate_at(vars(date_processed, date_inactive), 
              funs(as_date)) %>%
    mutate_at(vars(injection_time, additional_injection_time, induction_time,
                   reversal_time, time_alert, temp_1_time, temp_2_time,
                   temp_3_time, temp_4_time, release_time), 
              funs(as_datetime))
}
# Compile and reformat collars data  -------------------------------------------
# I like underscores and all lowercase in headers, so why not fix that?
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
# Datetimes to POSIXct using lubridate
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
# I like underscores and all lowercase in headers, so why not fix that?
names(collars_C5) <- names(collars_C5) %>% tolower()
names(collars_C5) <- gsub("\\.", "_", names(collars_C5))
#Create a collar identification number column
collars_C5$collar_id[collars_C5$ctn == "712883A"] <- "C5"
# Create a species identification column
collars_C5$species[collars_C5$ctn == "712883A"] <- "coyote"
# Make an animal id column
collars_C5$animal_id <- collars_C5$collar_id
# Datetimes to POSIXct
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
# GPS data cleaning ------------------------------------------------------------
# Get rid of unsuccessful GPS fixes
collars <- collars[!is.na(collars$gps_fix_attempt) | collars$gps_fix_attempt == 
                     "Failed", ]
# Only observations with complete cases
collars <- collars %>% filter(complete.cases(collars[, c("gps_longitude", 
                                                         "gps_latitude", 
                                                         "gps_fix_time")]))
# For first real filter, remove those 2 days from capture
for (i in 1:length(ids)) {
  collars <- collars[!(collars$animal_id == ids[i] & collars$acquisition_time <= 
                         (ymd_hms(bios$release_time[bios$animal_id == ids[i]]) + 
                            hours(48))), ]
}
rm(bios, ids, i)
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
# Add a step length column
coyotes_trk <- coyotes_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths)) %>%
  dplyr::select(id, sl_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, data))
reds_trk <- reds_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths)) %>%
  dplyr::select(id, sl_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, data))
grays_trk <- grays_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths)) %>%
  dplyr::select(id, sl_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, data))
# There are some duplicate locations, which all have 0 step lengths, you can
# see that they are duplicates by further examination of timestamps and UIDs.
# Coyotes
message(paste(count(coyotes_trk[(!is.na(coyotes_trk$sl_) & 
                                   coyotes_trk$sl_ == 0), ]), 
              "duplicate observations were detected in the coyote dataset. All duplicates will be removed."))
coyotes_trk <- coyotes_trk[!(!is.na(coyotes_trk$sl_) & coyotes_trk$sl_ == 0), ]
# Red foxes
message(paste(count(reds_trk[(!is.na(reds_trk$sl_) & 
                                   reds_trk$sl_ == 0), ]), 
              "duplicate observations were detected in the red fox dataset. All duplicates will be removed."))
reds_trk <- reds_trk[!(!is.na(reds_trk$sl_) & reds_trk$sl_ == 0), ]
# Red foxes
message(paste(count(grays_trk[(!is.na(grays_trk$sl_) & 
                                 grays_trk$sl_ == 0), ]), 
              "duplicate observations were detected in the gray fox dataset. All duplicates will be removed."))
grays_trk <- grays_trk[!(!is.na(grays_trk$sl_) & grays_trk$sl_ == 0), ]
# Re-track all collars
collars_trk <- rbind(coyotes_trk, reds_trk, grays_trk)
collars_trk <- collars_trk %>%
  make_track(x_, y_, t_,
             id = id,
             sp = sp,
             uid = uid,
             lon = lon,
             lat = lat,
             crs = "epsg:26915")
rm(coyotes_trk, reds_trk, grays_trk)
# Make tracks for different time periods ---------------------------------------
# Make a track using amt that will keep animal id and species, adding time of 
# day.
# Make an 11-hr track with steps
collars_11_h <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) x %>%
                          track_resample(rate = hours(11), 
                                         tolerance = minutes(10)) %>%
                          steps_by_burst())) %>% 
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(cols = "resample")
# Make a 5.5-hr track with steps
collars_5_h <- collars_trk %>%
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
collars_10_m <- collars_trk %>%
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
trks <- list(collars_11_h, collars_5_h, collars_10_m)
firsts <- list()
firsts_i <- tibble()
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
# Rename columns within the trks
trks_names <- c("collars_11_h", "collars_5_h", "collars_10_m")
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
message("Messages about external vectors are fine.")
# Remove irrelevant columns from the trks
drops  <- c("x1_", "y1_", "t1_", "dt_")
for (i in 1:3) {
  trks[[i]] <- trks[[i]][, !(names(trks[[i]]) %in% drops)]
}
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
rm(burst, dir, drops, firsts_i, collars_11_h, collars_5_h, collars_10_m, i, j, 
   newnames, oldnames, sl, ta, tbl, tbl_name, trks_names)
# Make a full track data set ---------------------------------------------------
# Make a trk with steps for the entire data set
lvls <- levels(as.factor(collars_trk$id))
collars_steps <- tibble()
for (i in lvls) {
  collars_steps_i <- collars_trk[collars_trk$id == i, ] %>%
    steps(keep_cols = "end")
  collars_steps <- rbind(collars_steps, collars_steps_i)
}
# Collect the first steps that do not have step lengths or related attributes
first_steps <- tibble()
for (i in 1:(nrow(collars_steps) - 1)) {
  if (i == 1) {
    first_steps <- rbind(first_steps, collars_steps[i, ])
  } else if (collars_steps$id[i] != collars_steps$id[i + 1]) {
    first_steps <- rbind(first_steps, collars_steps[i + 1, ])
  }
}
# Remove irrelevant columns from the first steps
drops  <- c("x2_", "y2_", "t2_", "dt_")
first_steps <- first_steps %>% select(!5:7)
first_steps <- first_steps[, !(names(first_steps) %in% drops)]
# Rename columns from the first steps
oldnames <- c("id", "sp", "t1_", "y1_", "x1_")
newnames <- c("animal_id", "species", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting")
first_steps <- first_steps %>% rename_with(~ newnames[which(oldnames == .x)], 
                                           .cols = oldnames)
# Remove irrelevant columns from the full data set trk
drops  <- c("x1_", "y1_", "t1_", "dt_")
collars_steps <- collars_steps[, !(names(collars_steps) %in% drops)]
# Rename columns from the full data set trk
oldnames <- c("id", "sp", "t2_", "y2_", "x2_")
newnames <- c("animal_id", "species", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting")
collars_steps <- collars_steps %>% rename_with(~ newnames[which(oldnames == .x)], 
                                               .cols = oldnames)
# Bind the first steps and the full data trk together
stepped_collars <- as_tibble(bind_rows(collars_steps, first_steps))
rm(drops, first_steps, collars_steps, collars_steps_i, i, lvls,
   newnames, oldnames)
# Join all time intervals together ---------------------------------------------
# Join all the other trks with the full data set
for (i in 1:3) {
  stepped_collars <- stepped_collars %>%
    full_join(trks[[i]], by = c("animal_id", "gps_utm_easting", 
                                "gps_utm_northing", "gps_fix_time"))
}
# Join all the other first steps with the full data set
for (i in 1:3) {
  stepped_collars <- stepped_collars %>%
    left_join(firsts[[i]], by = c("animal_id", "gps_utm_easting", 
                                  "gps_utm_northing", "gps_fix_time"))
}
# There are duplicate rows, so do this to make sure that I'm not losing any
# data from duplicated columns
for (i in 1:nrow(stepped_collars)) {
  if (is.na(stepped_collars$burst_collars_11_h.x[i])) {
    stepped_collars$burst_collars_11_h.x[i] <- stepped_collars$burst_collars_11_h.y[i]
  }
  if (is.na(stepped_collars$burst_collars_5_h.x[i])) {
    stepped_collars$burst_collars_5_h.x[i] <- stepped_collars$burst_collars_5_h.y[i]
  }
  if (is.na(stepped_collars$burst_collars_10_m.x[i])) {
    stepped_collars$burst_collars_10_m.x[i] <- stepped_collars$burst_collars_10_m.y[i]
  }
}
# Remove the irrelevant columns
drops  <- c("burst_collars_11_h.y", "burst_collars_5_h.y", 
            "burst_collars_10_m.y", "uid")
stepped_collars <- stepped_collars[, !(names(stepped_collars) %in% drops)]
# Here I need to get rid of duplicate rows. Using the mean function on numeric
# values is just a stand in. All values should be equal except NAs that were
# introduced, and the mean is just a stand in because it will keep the number
# when compared to a NA value.
stepped_collars <- stepped_collars %>%
  dplyr::group_by(animal_id, gps_utm_easting, gps_fix_time, species, 
                  gps_utm_northing) %>%
  summarise_each(funs(mean)) %>%
  as_tibble()
message("Warnings about deprecated dplyr functions are fine.")
rm(drops, firsts, i, trks)
# Clean and join data ----------------------------------------------------------
# Rename columns for exporting
oldnames <- c("direction_p", "burst_collars_11_h.x", "burst_collars_5_h.x", 
              "burst_collars_10_m.x", "sl_collars_11_h", "dir_collars_11_h", 
              "ta_collars_11_h", "sl_collars_5_h", "dir_collars_5_h", 
              "ta_collars_5_h", "sl_collars_10_m", "dir_collars_10_m", 
              "ta_collars_10_m")
newnames <- c("dir_", "burst_11_h", "burst_5_h", "burst_10_m", "sl_11_h", 
              "dir_11_h", "ta_11_h", "sl_5_h", "dir_5_h", "ta_5_h", "sl_10_m", 
              "dir_10_m", "ta_10_m")
stepped_collars <- stepped_collars %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)
# Join with original trk
oldnames <- c("x_", "y_", "t_", "id", "sp")
newnames <- c("gps_utm_easting", "gps_utm_northing", "gps_fix_time", 
              "animal_id", "species")
collars_trk <- collars_trk %>% 
  rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames)
collars_trk <- collars_trk %>%
  left_join(stepped_collars, by = c("animal_id", "species", "gps_utm_easting", 
                                    "lon", "gps_utm_northing", "gps_fix_time", 
                                    "lat"))
rm(oldnames, newnames)
# Behavioral processing --------------------------------------------------------
# Add 11-hr velocity
collars_11_h <- collars_trk[!is.na(collars_trk$burst_11_h), ] %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
# Add 5-hr velocity
collars_5_h <- collars_5_h %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
# Add 10-m velocity
collars_10_m <- collars_10_m %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))









# Append turn angle to a correctly formatted trk's
# Columns to keep
keeps <- c("uid", "ta_")
# Drop columns
stepped_collars <- stepped_collars[, (names(stepped_collars) %in% keeps)]
rm(keeps)
# Join
collars_trk <- collars_trk %>%
  full_join(stepped_collars, by = "uid")
rm(stepped_collars)
# Make all turn angles positive 0-pi
collars_trk$ta_ <- sqrt(collars_trk$ta_^2)




collars_trk$index <- rownames(collars_trk)
for (i in 1:(nrow(collars_trk) - 2)) {
  # This algorithm doesn't work if where at the start or end of the dataset,
  # so these for loops look complicated but they generalize the formula to be 
  # compatible with those different data structures.
  steps <- sum(collars_trk$sl_[i:(i + 1)])
  final_dist <- sqrt(((collars_trk[(i + 2), ]$x_ - 
                      collars_trk[i, ]$x_)^2) +
                     ((collars_trk[(i + 2), ]$y_ - 
                      collars_trk[i, ]$y_)^2))
  dist_ratio <- steps / final_dist
  collars_trk$dr_[i] <- dist_ratio
}
rm(dist_ratio, final_dist, i, steps)
# Create a suspicious points dataset
collars_trk$index <- rownames(collars_trk)
# Create a high distance ratios dataset
collars_dr <- collars_trk[collars_trk$dr_ >= 5, ]
# Create an investigate dataset
collars_dr <- collars_dr[!is.na(collars_dr$id), ]
collars_investigate <- tibble()
for (i in 1:nrow(collars_dr)) {
  row_i <- as.numeric(collars_trk[collars_trk$uid == collars_dr$uid[i], ]$index)
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
  collars_investigate_i[row_fast + 1, ]$speedy <- "yes"
  collars_investigate_i$burst <- collars_trk$uid[row_i + 1]
  v_ave <- sum(collars_investigate_i$v_[row_fast:(row_fast + 1)]) / 2
  burst_ave <- ((ifelse(row_fast == 1, 0, 
                        sum(collars_investigate_i$v_[1:(row_fast - 1)])) + 
                 ifelse(row_ct == (row_fast + 1), 0,
                        sum(collars_investigate_i$v_[(row_fast + 2):row_ct])))) / 
    (row_ct - 2)
  v_ratio <- v_ave / burst_ave
  if (is.na(v_ratio)) {
  } else if (v_ratio >= 5) {
    collars_investigate_i$vr_<- v_ratio
    collars_investigate <- rbind(collars_investigate, collars_investigate_i)
  }
}
rm(v_ratio, v_ave, row_term, row_init, row_i, i, burst_ave, collars_dr,
   collars_investigate_i, row_aftr, row_befr, j, row_ct, row_fast)
collars_out <- tibble()
for (i in 1:nrow(collars_investigate)) {
  if (collars_investigate$speedy[i] == "yes" #& 
      #collars_investigate$ta_[i + 1] > 3
      ) {
    collars_out <- rbind(collars_out, collars_investigate[i + 1, ])
  }
}
#rm(collars_investigate)
out <- levels(as.factor(collars_out$uid))
collars_out_fr_2 <- collars %>% filter(uid %in% 
                                       levels(as.factor(collars_investigate$uid)))

# Visualize for manual filtering
pal <- colorFactor(c("navy", "red"), domain = c("yes", "no"))
leaflet(collars_investigate[73:77, ]) %>% 
  addTiles()%>%
  addCircleMarkers(~lon, ~lat,
                   color = ~pal(speedy),
                   label = ~uid)



exclusion_summary <- tibble()
for (i in 1:nrow(collars_investigate)) {
  if (collars_investigate$speedy[i] == "no") {
  } else if (collars_investigate$speedy[i] == "yes") {
    exclusion_summary_i <- tibble()
    exclusion_summary_i[1, "ta_"] <- collars_investigate$ta_[i]
    exclusion_summary_i[1, "dr_"] <- collars_investigate$dr_[i - 1]
    exclusion_summary_i[1, "vr_"] <- collars_investigate$vr_[i - 1]
    exclusion_summary_i[1, "uid"] <- collars_investigate$uid[i - 1]
    exclusion_summary_i[1, "v_"] <- collars_investigate$v_[i - 1]
    exclusion_summary_i[1, "sl_"] <- collars_investigate$sl_[i - 1]
    exclusion_summary_i[1, "id"] <- collars_investigate$id[i - 1]
    diff_r_v_ <- abs((collars_investigate$v_[i - 1] - collars_investigate$v_[i]) / 
      collars_investigate$v_[i - 1])
    diff_r_sl_ <- abs((collars_investigate$sl_[i - 1] - collars_investigate$sl_[i]) / 
      collars_investigate$sl_[i - 1])
    exclusion_summary_i[1, "diff_r_v_"] <- diff_r_v_ 
    exclusion_summary_i[1, "diff_r_sl_"] <- diff_r_sl_
    exclusion_summary <- rbind(exclusion_summary, exclusion_summary_i)
  }
}













# Confirm that the right values are being filtered
view(collars_investigate[collars_investigate$good_ratio == "no", ])

















if (v_ratio >= 10) {
  collars_investigate_i$good_ratio <- "yes"
} else if (dist_ratio <= 5 & v_ratio > 5) {
  collars_investigate_i$good_ratio <- "maybe"
} else if (dist_ratio > 5 & v_ratio <= 5) {
  collars_investigate_i$good_ratio <- "maybe"
} else {
  collars_investigate_i$good_ratio <- "no"
}




















# Reclassify the track with the newly filtered data
collars_trk <- collars_trk %>%
  make_track(x_, y_, t_, id = id, sp = sp, uid = uid, lon = lon, lat = lat,
             crs = "epsg:26915")
# Add a speed and step length column
collars_trk <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(sl_ = map(data, step_lengths), v_ = map(data, speed)) %>%
  dplyr::select(id, sl_, v_, data) %>%
  as_tibble() %>%
  tidyr::unnest(cols = c(sl_, v_, data))
# Adjust velocity column to be km/hr
collars_trk$v_ <- collars_trk$v_ * 3.6
# Also classify those >10km/h for further investigation 
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
  message("SUCCESS BITCHES")
} else { 
  message("FAILURE BITCH")
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
summary
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
  select(animal_id, species, gps_fix_time, gps_latitude, gps_longitude, 
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
  select(animal_id, species, activity_fix_time, activity_count) %>%
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
  select(animal_id, species, temperature_fix_time, temperature) %>%
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
  select(acquisition_start_time, animal_id) %>%
  duplicated %>%
  sum()
if (sum == 0) {
  message("SUCCESS BITCHES")
} else { 
  message("FAILURE BITCH")
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