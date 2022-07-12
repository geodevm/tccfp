#===============================================================================
### Fit home ranges for all individuals possible, seasonally
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Load in packages -------------------------------------------------------------
library(adehabitatHR)
library(amt)
library(ctmm)
library(here)
library(lubridate)
library(sf)
library(tidyverse)
# Read in GPS data -------------------------------------------------------------
gps_ranges <- tibble(read.csv(here("data/processed_data/gps_data.csv")))
# Coerce all variables into the right format. This could take a bit.
gps_ranges <- gps_ranges %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, gps_utm_easting, 
                 gps_altitude), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), funs(as_datetime))
message("GPS dataset should be >56000 obs. of 8 variables.")
gps_ranges <- gps_ranges[!(gps_ranges$gps_fix_time > ymd("2021-03-05") &
                             gps_ranges$animal_id == "F16"), ]
# Create season variable -------------------------------------------------------
gps_ranges$season <- NA
# For coyotes:
gps_ranges_c <- gps_ranges %>%
  filter(grepl("C", animal_id))
# Filter seasonally based on coyote denning/breeding behavior
gps_ranges_c[gps_ranges_c$gps_fix_time < 
             ymd_hms("2019-12-01 05:00:00"), ]$season <- "other_19"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2019-12-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2020-04-01 05:00:00")), ]$season <- "mate_20"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2020-04-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2020-08-01 05:00:00")), ]$season <- "pup_20"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2020-08-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2020-12-01 05:00:00")), ]$season <- "other_20"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2020-12-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2021-04-01 05:00:00")), ]$season <- "mate_21"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2021-04-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2021-08-01 05:00:00")), ]$season <- "pup_21"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2021-08-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2021-12-01 05:00:00")), ]$season <- "other_21"
gps_ranges_c[(gps_ranges_c$gps_fix_time >= ymd_hms("2021-12-01 05:00:00") &
              gps_ranges_c$gps_fix_time < 
              ymd_hms("2022-04-01 05:00:00")), ]$season <- "mate_22"
# For now, we'll exclude anything after mate_22
gps_ranges_c <- gps_ranges_c[!(gps_ranges_c$gps_fix_time >= 
                               ymd_hms("2022-04-01 05:00:00")), ]
# For foxes:
gps_ranges_f <- gps_ranges %>%
  filter(grepl("F", animal_id))
# Filter seasonally based on fox denning/breeding behavior
gps_ranges_f[gps_ranges_f$gps_fix_time < 
             ymd_hms("2019-11-01 05:00:00"), ]$season <- "other_19"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2019-11-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2020-03-01 05:00:00")), ]$season <- "mate_20"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2020-03-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2020-07-01 05:00:00")), ]$season <- "pup_20"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2020-11-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2020-11-01 05:00:00")), ]$season <- "other_20"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2020-11-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2021-03-01 05:00:00")), ]$season <- "mate_21"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2021-03-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2021-07-01 05:00:00")), ]$season <- "pup_21"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2021-07-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2021-11-01 05:00:00")), ]$season <- "other_21"
gps_ranges_f[(gps_ranges_f$gps_fix_time >= ymd_hms("2021-11-01 05:00:00") &
              gps_ranges_f$gps_fix_time < 
              ymd_hms("2022-03-01 05:00:00")), ]$season <- "mate_22"
# For now, we'll exclude anything after mate_22
gps_ranges_f <- gps_ranges_f[!(gps_ranges_f$gps_fix_time >= 
                               ymd_hms("2022-03-01 05:00:00")), ]
# Bind back together
gps_ranges <- bind_rows(gps_ranges_c, gps_ranges_f)
# Reformat season
gps_ranges <- gps_ranges %>%
  mutate_at(vars(season), funs(as.factor))
rm(gps_ranges_c, gps_ranges_f)
# Remove clear outliers
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_northing < 4956000 &
                             gps_ranges$animal_id == "C5"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_northing > 4965700 &
                             gps_ranges$animal_id == "C8"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_easting < 486000 &
                             gps_ranges$animal_id == "C5.1"), ]
gps_ranges <- gps_ranges[!((gps_ranges$gps_utm_northing == 4985389 |
                            gps_ranges$gps_utm_easting > 494000) &
                             gps_ranges$animal_id == "C7"), ]
gps_ranges <- gps_ranges[!((gps_ranges$gps_utm_northing == 5024327 |
                            gps_ranges$gps_utm_northing < 4900000) &
                           gps_ranges$animal_id == "C9"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_easting == 466416 &
                             gps_ranges$animal_id == "F11"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_easting > 490000 &
                             gps_ranges$animal_id == "C2"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_easting > 486000 &
                             gps_ranges$animal_id == "C11"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_northing > 5060000 &
                             gps_ranges$animal_id == "F8"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_northing > 4990000 &
                             gps_ranges$animal_id == "F9"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_utm_northing > 4990000 &
                             gps_ranges$animal_id == "F18"), ]
gps_ranges <- gps_ranges[!(gps_ranges$gps_fix_time > ymd("2021-03-05") &
                             gps_ranges$animal_id == "F16"), ]
# Examine variograms to determine home range fidelity --------------------------
telem <- gps_ranges
telem <- telem %>%
  dplyr::select(animal_id, gps_fix_time, gps_latitude, gps_longitude) %>%
  rename(individual.local.identifier = animal_id,
         timestamp = gps_fix_time,
         location.long = gps_longitude,
         location.lat = gps_latitude)
telem <- as.telemetry(telem)
# Bad, non-asymptotic:
# C1
plot(variogram(telem[[1]]))
# C12
plot(variogram(telem[[5]]))
# C14
plot(variogram(telem[[7]]))
# C9
plot(variogram(telem[[17]]))
# F12
plot(variogram(telem[[21]]))
# F14, linearity without localization suggests constant dispersal
plot(variogram(telem[[23]]))
# F18
plot(variogram(telem[[27]]))
# F4
plot(variogram(telem[[30]]))
# F5, too few to determine
plot(variogram(telem[[31]]))
# F7
plot(variogram(telem[[33]]))
rm(telem)
# Exclude certain periods for animals with non-HR activity ---------------------
# C1 disperses (without return) on Jan 16, 2021
# Create a data set of dispersal locations
dispersing <- gps_ranges[(gps_ranges$animal_id == "C1" & 
                          gps_ranges$gps_fix_time > ymd_hms("2020-01-16 05:00:00")), ]
# Create a data set of home range locations
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "C1" & 
                           gps_ranges$gps_fix_time > ymd_hms("2020-01-16 05:00:00")), ]
# C12 has a transient period before localizing on Apr 6, 2021
# Create a transient data set (with returns, non-directional)
transient <- gps_ranges[(gps_ranges$animal_id == "C12" & 
                         gps_ranges$gps_fix_time < ymd_hms("2021-04-06 05:00:00")), ]
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "C12" & 
                           gps_ranges$gps_fix_time < ymd_hms("2021-04-06 05:00:00")), ]
# C14 disperses on Jan 16, 2022
dispersing <- rbind(dispersing, 
                    gps_ranges[(gps_ranges$animal_id == "C14" & 
                                gps_ranges$gps_fix_time > ymd_hms("2022-01-02 05:00:00")), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "C14" & 
                           gps_ranges$gps_fix_time > ymd_hms("2022-01-02 05:00:00")), ]
# C9 has a stable home range prior to Feb 15, 2021, a period of transiency until
# Apr 1, 2021, and another stable home range after that date.
# Create a dataset for animals with multiple multi-season ranges.
alt_resident <- gps_ranges[(gps_ranges$animal_id == "C9" & 
                            gps_ranges$gps_fix_time < ymd_hms("2021-02-15 05:00:00")), ]
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "C9" & 
                             gps_ranges$gps_fix_time < ymd_hms("2021-02-15 05:00:00")), ]
transient <- rbind(transient,
                   gps_ranges[(gps_ranges$animal_id == "C9" & 
                               gps_ranges$gps_fix_time < ymd_hms("2021-04-01 05:00:00")), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "C9" & 
                           gps_ranges$gps_fix_time < ymd_hms("2021-04-01 05:00:00")), ]
# F12 has a period of transiency until Nov 15, 2020, at which point she 
# establishes a stable home range.
transient <- rbind(transient,
                   gps_ranges[(gps_ranges$animal_id == "F12" & 
                               gps_ranges$gps_fix_time < ymd_hms("2020-11-15 05:00:00")), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "F12" & 
                           gps_ranges$gps_fix_time < ymd_hms("2020-11-15 05:00:00")), ]
# F14 appears to only disperse during the sampling period.
dispersing <- rbind(dispersing, 
                    gps_ranges[gps_ranges$animal_id == "F14", ])
gps_ranges <- gps_ranges[!gps_ranges$animal_id == "F14", ]
# F18 has a period of dispersal before establishing a home range on Dec 21, 
# 2021.
dispersing <- rbind(dispersing,
                    gps_ranges[(gps_ranges$animal_id == "F18" & 
                                gps_ranges$gps_fix_time < ymd_hms("2021-12-21 05:00:00")), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "F18" & 
                           gps_ranges$gps_fix_time < ymd_hms("2021-12-21 05:00:00")), ]
# F4 has a dispersal period prior to Nov 14, 2020, establishing a stable HR 
# afterwards.
dispersing <- rbind(dispersing,
                    gps_ranges[(gps_ranges$animal_id == "F4" & 
                                gps_ranges$gps_fix_time < ymd_hms("2020-11-14 05:00:00")), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "F4" & 
                           gps_ranges$gps_fix_time < ymd_hms("2020-11-14 05:00:00")), ]
# F5 was not tracked long enough for any meaningful interpretation.
# Create an unknown data set.
unknown <- gps_ranges[gps_ranges$animal_id == "F5", ]
gps_ranges <- gps_ranges[!gps_ranges$animal_id == "F5", ]
# F7 appears to be transient prior to Jan 14, 2021, has a stable home range 
# until Apr 15, has another transiency period until Apr 25, and then establishes
# another stable home range.
transient <- rbind(transient,
                   gps_ranges[(gps_ranges$animal_id == "F7" & 
                                 gps_ranges$gps_fix_time < ymd_hms("2021-01-14 05:00:00")), ])
alt_resident <- rbind(alt_resident,
                      gps_ranges[(gps_ranges$animal_id == "F7" & 
                                  (gps_ranges$gps_fix_time > ymd_hms("2021-01-14 05:00:00") &
                                   gps_ranges$gps_fix_time < ymd_hms("2021-04-15 05:00:00"))), ])
transient <- rbind(transient,
                   gps_ranges[(gps_ranges$animal_id == "F7" & 
                               (gps_ranges$gps_fix_time > ymd_hms("2021-04-15 05:00:00") &
                                gps_ranges$gps_fix_time < ymd_hms("2021-04-25 05:00:00"))), ])
gps_ranges <- gps_ranges[!(gps_ranges$animal_id == "F7" & 
                           gps_ranges$gps_fix_time < ymd_hms("2021-04-25 05:00:00")), ]
# Assign identifiers to repeated behavior types --------------------------------
# Resident behaviors
alt_resident$animal_id <- alt_resident$animal_id %>% as.character()
alt_resident$animal_id <- paste0(alt_resident$animal_id, "_2")
alt_resident$animal_id <- alt_resident$animal_id %>% factor()
gps_ranges$animal_id <- as.character(gps_ranges$animal_id)
gps_ranges[gps_ranges$animal_id == "C9", ]$animal_id <- "C9_1"
gps_ranges[gps_ranges$animal_id == "F7", ]$animal_id <- "F7_1"
gps_ranges$animal_id <- gps_ranges$animal_id %>% factor()
# Transient behaviors
transient$animal_id <- transient$animal_id %>% as.character()
transient[(transient$animal_id == "F7" & 
           transient$gps_fix_time < ymd_hms("2021-01-14 05:00:00")), ]$animal_id <- "F7_1"
transient[(transient$animal_id == "F7" & 
           (transient$gps_fix_time > ymd_hms("2021-04-15 05:00:00") &
            transient$gps_fix_time < ymd_hms("2021-04-25 05:00:00"))), ]$animal_id <- "F7_2"
transient$animal_id <- transient$animal_id %>% factor()
#Plot resident variograms -----------------------------------------------------
# Main telemetry data
telem <- gps_ranges %>%
  dplyr::select(animal_id, gps_fix_time, gps_latitude, gps_longitude) %>%
  rename(individual.local.identifier = animal_id,
         timestamp = gps_fix_time,
         location.long = gps_longitude,
         location.lat = gps_latitude)
telem <- as.telemetry(telem)
# Secondary home ranges if they exist
telem_alt <- alt_resident %>%
  dplyr::select(animal_id, gps_fix_time, gps_latitude, gps_longitude) %>%
  rename(individual.local.identifier = animal_id,
         timestamp = gps_fix_time,
         location.long = gps_longitude,
         location.lat = gps_latitude)
telem_alt <- as.telemetry(telem_alt)
# C1
plot(variogram(telem[[1]]))
# C1.1
plot(variogram(telem[[2]]))
# C10
plot(variogram(telem[[3]]))
# C11
plot(variogram(telem[[4]]))
# C12
plot(variogram(telem[[5]]))
# C13
plot(variogram(telem[[6]]))
# C14
plot(variogram(telem[[7]]))
# C15
plot(variogram(telem[[8]]))
# C2
plot(variogram(telem[[9]]))
# C3
plot(variogram(telem[[10]]))
# C4
plot(variogram(telem[[11]]))
# C5
plot(variogram(telem[[12]]))
# C5.1
plot(variogram(telem[[13]]))
# C6
plot(variogram(telem[[14]]))
# C7
plot(variogram(telem[[15]]))
# C8
plot(variogram(telem[[16]]))
# C9
plot(variogram(telem[[17]]))
plot(variogram(telem_alt[[1]]))
# F1
plot(variogram(telem[[18]]))
# F10
plot(variogram(telem[[19]]))
# F11
plot(variogram(telem[[20]]))
# F12
plot(variogram(telem[[21]]))
# F13, all of this is within a very small area, so specific site fidelity in
# this short time period won't show up, making this appear bad. Behaviorally,
# This makes sense as a territory of an old animal with limited mobility (one 
# eye, broken leg, etc.)
plot(variogram(telem[[22]]))
# F15
plot(variogram(telem[[23]]))
# F16
plot(variogram(telem[[24]]))
# F17
plot(variogram(telem[[25]]))
# F18
plot(variogram(telem[[26]]))
# F2
plot(variogram(telem[[27]]))
# F3
plot(variogram(telem[[28]]))
# F4
plot(variogram(telem[[29]]))
# F6
plot(variogram(telem[[30]]))
# F7, 2 stable ranges
plot(variogram(telem[[31]]))
plot(variogram(telem_alt[[2]]))
# F8
plot(variogram(telem[[32]]))
# F9
plot(variogram(telem[[33]]))
rm(telem, telem_alt)
# Plot dispersal variograms ----------------------------------------------------
# Main telemetry data
telem_disp <- dispersing %>%
  dplyr::select(animal_id, gps_fix_time, gps_latitude, gps_longitude) %>%
  rename(individual.local.identifier = animal_id,
         timestamp = gps_fix_time,
         location.long = gps_longitude,
         location.lat = gps_latitude)
telem_disp <- as.telemetry(telem_disp)
# C1
plot(variogram(telem_disp[[1]]))
# C14
plot(variogram(telem_disp[[2]]))
# F14
plot(variogram(telem_disp[[3]]))
# F18
plot(variogram(telem_disp[[4]]))
# F4
plot(variogram(telem_disp[[5]]))
rm(telem_disp)
# Plot transient variograms ----------------------------------------------------
telem_trans <- transient %>%
  dplyr::select(animal_id, gps_fix_time, gps_latitude, gps_longitude) %>%
  rename(individual.local.identifier = animal_id,
         timestamp = gps_fix_time,
         location.long = gps_longitude,
         location.lat = gps_latitude)
telem_trans <- as.telemetry(telem_trans)
# C12
plot(variogram(telem_trans[[1]]))
# C9
plot(variogram(telem_trans[[2]]))
# F12
plot(variogram(telem_trans[[3]]))
# F7, two periods of transiency
plot(variogram(telem_trans[[4]]))
plot(variogram(telem_trans[[5]]))
rm(telem_trans)
# Bind together with identifying columns ---------------------------------------
gps_ranges$behavior <- "resident"
alt_resident$behavior <- "resident"
transient$behavior <- "transient"
dispersing$behavior <- "dispersal"
unknown$behavior <- "unknown"
gps_ranges <- gps_ranges %>%
  bind_rows(alt_resident) %>%
  bind_rows(transient) %>%
  bind_rows(dispersing) %>%
  bind_rows(unknown)
gps_ranges$behavior <- as.factor(gps_ranges$behavior)
# Get an exportable file
behavior_class_export <- gps_ranges
# Get rid of irrelevant columns
gps_ranges <- gps_ranges %>% 
  dplyr::select(animal_id, gps_fix_time, gps_utm_easting, gps_utm_northing, 
                season, behavior)
# How much time has elapsed?
message("The following time has elapsed for pre-processing materials: \n", 
        names(now[1]), ": ", now[[1]], "\n",
        names(now[2]), ":  ", now[[2]], "\n",
        names(now[3]), ":   ", now[[3]])
rm(alt_resident, dispersing, now, transient, unknown)
# Fit study area ---------------------------------------------------------------
study_area <- gps_ranges %>%
  filter(behavior != "dispersal") %>%
  make_track(.x = gps_utm_easting,
             .y = gps_utm_northing,
             .t = gps_fix_time,
             season = season,
             crs = "epsg:26915") %>%
  hr_mcp(levels = 1)
# Fitting annual home ranges using OUF -----------------------------------------
# Get the levels for iteration
#ids <- levels(as.factor(as.character(gps_ranges$animal_id)))
# Exclude those that can't be fit
#ids <- ids[-c(27, 29, 30)]
# Create a list for outputs
#ouf_tibble <- tibble()
# Iterate
#for (i in ids) {
#  animal_i <- gps_ranges[gps_ranges$animal_id == i &
#                         gps_ranges$behavior == "resident", ] %>%
#    drop_na()
#  if (as.numeric(max(animal_i$gps_fix_time) - min(animal_i$gps_fix_time)) < 30) {
#    next
#  }
#  hrs <- animal_i %>%
#    nest(data = -"animal_id") %>%
#    mutate(akde_ouf_95 = map(data, function(x) x %>%
#                               make_track(.x = gps_utm_easting,
#                                          .y = gps_utm_northing,
#                                          .t = gps_fix_time,
#                                          season = season,
#                                          range_type = behavior,
#                                          crs = "epsg:26915") %>%
#                               hr_akde(model = fit_ctmm(., "ouf"),
#                                       levels = 0.95))) %>%
#    mutate(akde_ouf_50 = map(data, function(x) x %>%
#                               make_track(.x = gps_utm_easting,
#                                          .y = gps_utm_northing,
#                                          .t = gps_fix_time,
#                                          season = season,
#                                          range_type = behavior,
#                                          crs = "epsg:26915") %>%
#                               hr_akde(model = fit_ctmm(., "ouf"),
#                                       levels = 0.50)))
#  ouf_tibble <- ouf_tibble %>%
#    bind_rows(hrs)
#}
# How much time has elapsed so far?
#now <- proc.time() - begin
#message("Annual OUF AKDEs finished! \n",
#        "\n",
#        "The following time has elapsed for this script so far: \n", 
#        names(now[1]), ": ", now[[1]], "\n",
#        names(now[2]), ":  ", now[[2]], "\n",
#        names(now[3]), ":   ", now[[3]])
#rm(now)
# Create dispersal and unknown line objects ------------------------------------
# Create line object for dispersers
sf_disp <- st_as_sf(gps_ranges[gps_ranges$behavior == "dispersal", ], 
                    coords = c("gps_utm_easting", "gps_utm_northing"))
ids <- levels(as.factor(as.character(sf_disp$animal_id)))
j <- 1
for (i in ids) {
  points <- sf_disp[sf_disp$animal_id == i, ]
  start_points <- points[1:(nrow(points) - 1), ] %>%
    rename(start_time = gps_fix_time,
           start_point = geometry)
  end_points <- points[2:nrow(points), ] %>%
    dplyr::select(geometry, gps_fix_time) %>%
    rename(end_time = gps_fix_time,
           end_point = geometry)
  disp_points <- cbind(start_points, end_points)
  disp_lines <- st_sfc(mapply(function(a, b) {
    st_cast(st_union(a, b), "LINESTRING")}, 
    disp_points$start_point, disp_points$end_point, SIMPLIFY = FALSE))
  disp_steps <- cbind(disp_points, disp_lines) %>%
    rename(step = geometry)
  if (j == 1) {
    disp_track <- disp_steps
    j <- 0
  } else {
    disp_track <- rbind(disp_track, disp_steps)
  }
}
# Create line object for unknown
sf_unk <- st_as_sf(gps_ranges[gps_ranges$behavior == "unknown", ], 
                   coords = c("gps_utm_easting", "gps_utm_northing"))
ids <- levels(as.factor(as.character(sf_unk$animal_id)))
j <- 1
for (i in ids) {
  points <- sf_unk[sf_unk$animal_id == i, ]
  start_points <- points[1:(nrow(points) - 1), ] %>%
    rename(start_time = gps_fix_time,
           start_point = geometry)
  end_points <- points[2:nrow(points), ] %>%
    dplyr::select(geometry, gps_fix_time) %>%
    rename(end_time = gps_fix_time,
           end_point = geometry)
  unk_points <- cbind(start_points, end_points)
  unk_lines <- st_sfc(mapply(function(a, b) {
    st_cast(st_union(a, b), "LINESTRING")}, 
    unk_points$start_point, unk_points$end_point, SIMPLIFY = FALSE))
  unk_steps <- cbind(unk_points, unk_lines) %>%
    rename(step = geometry)
  if (j == 1) {
    unk_track <- unk_steps
    j <- 0
  } else {
    unk_track <- rbind(unk_track, unk_steps)
  }
}
rm(disp_lines, disp_points, disp_steps, end_points, i, ids, j, points, sf_disp, 
   sf_unk, start_points, unk_lines, unk_points, unk_steps)
# Thinning data management -----------------------------------------------------
# Create a trk at 11 hour intervals
gps_trk <- gps_ranges %>%
  make_track(.x = gps_utm_easting,
             .y = gps_utm_northing,
             .t = gps_fix_time,
             id = animal_id,
             season = season,
             behavior = behavior,
             crs = "epsg:26915")
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
rm(gps_trk)
# Process tracks ---------------------------------------------------------------
# Collect the first steps that do not have step lengths or related attributes
firsts <- tibble()
for (i in 1:(nrow(gps_11_h) - 1)) {
  if (i == 1) {
    firsts <- rbind(firsts, gps_11_h[i, ])
  } else if (gps_11_h$burst_[i] != gps_11_h$burst_[i + 1]) {
    firsts <- rbind(firsts, gps_11_h[i + 1, ])
  }
}
# Remove irrelevant columns from the trk
drops  <- c("x1_", "y1_", "t1_", "dt_")
gps_11_h <- gps_11_h[, !(names(gps_11_h) %in% drops)]
oldnames <- c("id", "t2_", "y2_", "x2_")
newnames <- c("animal_id", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting")
gps_11_h <- gps_11_h %>% rename_with(~ newnames[which(oldnames == .x)], 
                                     .cols = oldnames)
gps_11_h <- gps_11_h %>% 
  dplyr::select(!5:7)
# Remove irrelevant columns from the first steps, and rename the columns for 
# joining
drops  <- c("x2_", "y2_", "t2_", "dt_")
firsts <- firsts[, !(names(firsts) %in% drops)]
oldnames <- c("id", "t1_", "y1_", "x1_", "burst_")
newnames <- c("animal_id", "gps_fix_time", "gps_utm_northing", 
              "gps_utm_easting", "burst_")
firsts <- firsts %>% rename_with(~ newnames[which(oldnames == .x)], 
                                 .cols = oldnames)
firsts <- firsts %>% 
  dplyr::select(!5:7)
# Bind together
tmp_ranges <- bind_rows(firsts, gps_11_h) %>%
  dplyr::select(!burst_)
# Add rows
gps_ranges <- tmp_ranges %>%
  left_join(gps_ranges, by = c("animal_id", "gps_fix_time", "gps_utm_easting", 
                               "gps_utm_northing"))
rm(drops, firsts, gps_11_h, i, newnames, oldnames, tmp_ranges)
# Fitting seasonal home ranges using other methods -----------------------------
# Get the levels for iteration
lvls <- levels(gps_ranges$season)
# Create a list for outputs
range_list <- list()
# Fit for residents
for (i in lvls) {
  season_i <- gps_ranges[gps_ranges$season == i & 
                         gps_ranges$behavior == "resident", ] %>%
    drop_na()
  ids <- levels(as.factor(as.character(season_i$animal_id)))
  for (j in ids) {
    if (as.numeric(max(season_i[season_i$animal_id == j, ]$gps_fix_time) - 
                   min(season_i[season_i$animal_id == j, ]$gps_fix_time)) < 30) {
      season_i <- season_i[!(season_i$animal_id == j), ]
    }
  }
  if (nrow(season_i) == 0) {
    next
  }
  hrs <- season_i %>%
    nest(data = -"animal_id") %>%
    mutate(kde_ani_95 = map(data, function(x) x %>%
                               make_track(.x = gps_utm_easting,
                                          .y = gps_utm_northing,
                                          .t = gps_fix_time,
                                          season = season,
                                          range_type = behavior,
                                          crs = "epsg:26915") %>%
                               hr_akde(model = fit_ctmm(., "iid"),
                                       levels = 0.95))) %>%
    mutate(kde_ani_50 = map(data, function(x) x %>%
                               make_track(.x = gps_utm_easting,
                                          .y = gps_utm_northing,
                                          .t = gps_fix_time,
                                          season = season,
                                          range_type = behavior,
                                          crs = "epsg:26915") %>%
                               hr_akde(model = fit_ctmm(., "iid"),
                                       levels = 0.50))) %>%
    mutate(kde_iso_95 = map(data, function(x) x %>%
                              make_track(.x = gps_utm_easting,
                                         .y = gps_utm_northing,
                                         .t = gps_fix_time,
                                         season = season,
                                         range_type = behavior,
                                         crs = "epsg:26915") %>%
                              hr_kde(levels = 0.95))) %>%
    mutate(kde_iso_50 = map(data, function(x) x %>%
                              make_track(.x = gps_utm_easting,
                                         .y = gps_utm_northing,
                                         .t = gps_fix_time,
                                         season = season,
                                         range_type = behavior,
                                         crs = "epsg:26915") %>%
                              hr_kde(levels = 0.50))) %>%
    mutate(mcp_95 = map(data, function(x) x %>%
                          make_track(.x = gps_utm_easting,
                                     .y = gps_utm_northing,
                                     .t = gps_fix_time,
                                     season = season,
                                     range_type = behavior,
                                     crs = "epsg:26915") %>%
                          hr_mcp(levels = 0.95)))
  add_hrs <- list(hrs)
  names(add_hrs) <- paste(i)
  range_list <- c(range_list, add_hrs)
}
# How much time has elapsed so far?
now <- proc.time() - begin
message("Seasonal anisotropic KDEs, isotropic KDEs, and MCPs finished! \n",
        "\n",
        "The following time has elapsed for this script so far: \n", 
        names(now[1]), ": ", now[[1]], "\n",
        names(now[2]), ":  ", now[[2]], "\n",
        names(now[3]), ":   ", now[[3]])
rm(hrs, i, ids, j, add_hrs, now, season_i)
# Fitting annual home ranges using other methods -------------------------------
# Get the levels for iteration
ids <- levels(as.factor(as.character(gps_ranges[gps_ranges$behavior == "resident", ]$animal_id)))
# Create tibble
annual_tibble <- tibble()
# Iterate
for (i in ids) {
  animal_i <- gps_ranges[gps_ranges$animal_id == i &
                         gps_ranges$behavior == "resident", ] %>%
    drop_na()
  seasons <- levels(as.factor(as.character(animal_i$season)))
  for (j in seasons) {
    if (as.numeric(max(animal_i[animal_i$season == j, ]$gps_fix_time) - 
                   min(animal_i[animal_i$season == j, ]$gps_fix_time)) < 30) {
      seasons <- seasons[seasons != j]
    }
  }
  if (as.numeric(max(animal_i$gps_fix_time) - min(animal_i$gps_fix_time)) < 90 |
      length(levels(as.factor(as.character(seasons)))) < 2) {
    next
  }
  hrs <- animal_i %>%
    nest(data = -"animal_id") %>%
    mutate(kde_ani_95 = map(data, function(x) x %>%
                               make_track(.x = gps_utm_easting,
                                          .y = gps_utm_northing,
                                          .t = gps_fix_time,
                                          season = season,
                                          range_type = behavior,
                                          crs = "epsg:26915") %>%
                               hr_akde(model = fit_ctmm(., "iid"),
                                       levels = 0.95))) %>%
    mutate(kde_ani_50 = map(data, function(x) x %>%
                               make_track(.x = gps_utm_easting,
                                          .y = gps_utm_northing,
                                          .t = gps_fix_time,
                                          season = season,
                                          range_type = behavior,
                                          crs = "epsg:26915") %>%
                               hr_akde(model = fit_ctmm(., "iid"),
                                       levels = 0.50))) %>%
    mutate(kde_iso_95 = map(data, function(x) x %>%
                              make_track(.x = gps_utm_easting,
                                         .y = gps_utm_northing,
                                         .t = gps_fix_time,
                                         season = season,
                                         range_type = behavior,
                                         crs = "epsg:26915") %>%
                              hr_kde(levels = 0.95))) %>%
    mutate(kde_iso_50 = map(data, function(x) x %>%
                              make_track(.x = gps_utm_easting,
                                         .y = gps_utm_northing,
                                         .t = gps_fix_time,
                                         season = season,
                                         range_type = behavior,
                                         crs = "epsg:26915") %>%
                              hr_kde(levels = 0.50))) %>%
    mutate(mcp_95 = map(data, function(x) x %>%
                          make_track(.x = gps_utm_easting,
                                     .y = gps_utm_northing,
                                     .t = gps_fix_time,
                                     season = season,
                                     range_type = behavior,
                                     crs = "epsg:26915") %>%
                          hr_mcp(levels = 0.95)))
  annual_tibble <- annual_tibble %>%
    bind_rows(hrs)
}
annual_list <- list(annual_tibble)
names(annual_list) <- "annual"
# How much time has elapsed so far?
now <- proc.time() - begin
message("Annual anisotropic KDEs, isotropic KDEs, and MCPs finished! \n",
        "\n",
        "The following time has elapsed for this script so far: \n", 
        names(now[1]), ": ", now[[1]], "\n",
        names(now[2]), ":  ", now[[2]], "\n",
        names(now[3]), ":   ", now[[3]])
rm(animal_i, annual_tibble, hrs, i, ids, now)
# Fitting transient MCPs -------------------------------------------------------
# Create a list for outputs
transient_tibble <- tibble()
# Fit for transients
transients_lvls <- levels(as.factor(as.character(gps_ranges[gps_ranges$behavior == "transient", ]$animal_id)))
for (i in transients_lvls) {
  animal_i <- gps_ranges[gps_ranges$behavior == "transient" &
                         gps_ranges$animal_id == i, ] %>%
    drop_na()
  hrs <- animal_i %>%
    nest(data = -"animal_id") %>%
    mutate(mcp_95 = map(data, function(x) x %>%
                          make_track(.x = gps_utm_easting,
                                     .y = gps_utm_northing,
                                     .t = gps_fix_time,
                                     season = season,
                                     range_type = behavior,
                                     crs = "epsg:26915") %>%
                          hr_mcp(levels = 0.95)))
  transient_tibble <- transient_tibble %>%
    bind_rows(hrs)
}
# How much time has elapsed so far?
now <- proc.time() - begin
message("Transient MCPs finished! \n",
        "\n",
        "The following time has elapsed for this script so far: \n", 
        names(now[1]), ": ", now[[1]], "\n",
        names(now[2]), ":  ", now[[2]], "\n",
        names(now[3]), ":   ", now[[3]])
rm(gps_ranges, hrs, now)
# Bind all together based on season --------------------------------------------
range_list <- c(range_list, annual_list)
lvls <- c(lvls, "annual")
j <- 1
for (i in lvls) {
  if (j == 1) {
    ranges <- range_list[[i]]
    ranges$season <- paste(i)
    j <- 0
    next
  }
  tbl <- range_list[[i]]
  tbl$season <- paste(i)
  ranges <- bind_rows(ranges, tbl)
}
ranges$range_type <- "resident"
ranges <- ranges %>%
  filter(!is.na(animal_id))
# Create a full dataset
no_geom <- ranges %>%
  dplyr::select(animal_id, season, range_type)
# Do the same for OUF models to extract all those
kdes_ani_95 <- hr_to_sf(ranges$kde_ani_95) %>%
  filter(what == "estimate") %>%
  dplyr::select(geometry) %>%
  bind_cols(no_geom) %>%
  mutate(method = "kde_ani_95")
kdes_ani_50 <- hr_to_sf(ranges$kde_ani_50) %>%
  filter(what == "estimate") %>%
  dplyr::select(geometry) %>%
  bind_cols(no_geom) %>%
  mutate(method = "kde_ani_50")
kdes_iso_95 <- hr_to_sf(ranges$kde_iso_95) %>%
  dplyr::select(geometry) %>%
  bind_cols(no_geom) %>%
  mutate(method = "kde_iso_95")
kdes_iso_50 <- hr_to_sf(ranges$kde_iso_50) %>%
  dplyr::select(geometry) %>%
  bind_cols(no_geom) %>%
  mutate(method = "kde_iso_50")
# Do the same for transients to extract all MCPs
transient_tibble$season <- NA
transient_tibble$range_type <- "transient"
# Bind together
ranges <- ranges %>%
  bind_rows(transient_tibble) %>%
  filter(!is.na(animal_id))
# Create a full dataset
no_geom <- ranges %>%
  dplyr::select(animal_id, season, range_type)
mcps_95 <- hr_to_sf(ranges$mcp_95) %>%
  dplyr::select(geometry) %>%
  bind_cols(no_geom) %>%
  mutate(method = "mcp_95")
# Do the same for OUF models to extract all those
ouf_tibble$season <- "annual"
ouf_tibble$range_type <- "resident"
# Create a full dataset
no_geom <- ouf_tibble %>%
  dplyr::select(animal_id, season, range_type)
# Extract all kinds
#akdes_ouf_95 <- hr_to_sf(ouf_tibble$akde_ouf_95) %>%
#  filter(what == "estimate") %>%
#  select(geometry) %>%
#  bind_cols(no_geom) %>%
#  mutate(method = "akde_ouf_95")
#akdes_ouf_50 <- hr_to_sf(ouf_tibble$akde_ouf_50) %>%
#  filter(what == "estimate") %>%
#  select(geometry) %>%
#  bind_cols(no_geom) %>%
#  mutate(method = "akde_ouf_50")
home_ranges <- kdes_ani_95 %>%
# home_ranges <- akdes_ouf_95 #%>%
#  bind_rows(akdes_ouf_50) %>%
#  bind_rows(kdes_ani_95) %>%
  bind_rows(kdes_ani_50) %>%
  bind_rows(kdes_iso_95) %>%
  bind_rows(kdes_iso_50) %>%
  bind_rows(mcps_95) %>%
  mutate_at(vars(method, season, range_type), funs(as_factor))
rm(akdes_ouf_95, akdes_ouf_50, animal_i, annual_list, i, j, kdes_ani_95, 
   kdes_ani_50, kdes_iso_95, kdes_iso_50, lvls, mcps_95, no_geom, range_list,
   ranges, tbl, transients_lvls, transient_tibble)
# Collect total runtime --------------------------------------------------------
# Write out the unknown track
unk_track %>%
  dplyr::select(animal_id, start_time, season, behavior, end_time, step) %>%
  write_sf(here("data/processed_data/shps/unknown_track.shp"))
# Write out the dispersal track
disp_track %>%
  dplyr::select(animal_id, start_time, season, behavior, end_time, step) %>%
  st_write(here("data/processed_data/shps/dispersal_track.shp"))
# Write out the home ranges
home_ranges %>%
  st_write(here("data/processed_data/shps/home_ranges.shp")) 
# Write out the home ranges
study_area$mcp[[4]] %>%
  st_write(here("data/processed_data/shps/study_area.shp")) 
# Write out the behavioral classes appended
behavior_class_export %>%
  write.csv(here("data/processed_data/hr_behavior_appended.csv"), 
            row.names = FALSE)
rm(disp_track, behavior_class_export, home_ranges, study_area, unk_track)
# Export here ------------------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================