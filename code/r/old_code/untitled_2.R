# COVID-19 "15 days to slow the spread" began 16 March 2020. I mark 00:00 UTC (19:00 CST) as the start of this.
# 27 March 2020 is stay at home order for Minnesota
# Load Packages
library(RColorBrewer)
library(raster)
library(tidyverse)
library(lubridate)
library(amt)
library(sf)
library(survival)
library(lme4)
library(lmerTest)
library(here)
# Import collars
# collars <- read.csv(here("data/latest_data.csv"))
# Import rasters
impervious <- raster(here("data/impervious/TCMA_Impervious_2000.tif"))
proj4string(impervious) <- CRS("+init=epsg:32615")
landcover <- raster(here("data/landcover/tcma_lc_finalv1.tif"))
proj4string(landcover) <- CRS("+init=epsg:32615")
# Import census shp
pop <- read_sf(here("data/population/joined_census_blocks_2010.shp"))
pop = st_set_crs(pop, 32615)
# Convert to population density
pop$density_m_2 <- as.numeric(pop$pop_total / st_area(pop$geometry))
pop <- pop %>% dplyr::select(density_m_2, geometry)
# Make a track using amt that will keep animal id and species, adding time of day
collars_dif <- collars[collars$animal_id != "F4", ]
collars_dif <- collars_dif[collars_dif$animal_id != "F14", ]
collars_dif <- collars_dif[!(collars_dif$animal_id == "C1" & collars_dif$acquisition_time > ymd_hms("2020-01-16 00:00:00")), ]
collars_trk <- collars_dif %>%
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_time,
             id = animal_id,
             sp = species,
             crs = CRS("+init=epsg:32615")) %>%
  time_of_day()
plot(collars_trk)
# Create a subset of this to be used for RSFs. I am going to assume that resampling to include 5.5 and 11 hour intervals will be sufficient
collars_rsf <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) 
    x %>%
      track_resample(rate = hours(11), tolerance = hours(1)))) %>%
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(col = "resample")

full_set <- tibble()
for (i in levels(as.factor(collars_rsf$id))) {
  pts <- make_track(collars_rsf[collars_rsf$id == i,], x_, y_, t_, id = id, sp = sp, tod_ = tod_, burst_ = burst_)
  hr <- hr_kde(pts, levels = 0.80)
  rp <- random_points(hr, presence = pts, n = nrow(pts) * 10)
  rp$id <- i
  rp$sp <- pts$sp[1]
  full_set <- rbind(full_set, rp)
}

full_set <- full_set %>%
  extract_covariates(landcover) %>%
  extract_covariates(impervious) %>%
  st_as_sf(coords = c("x_", "y_"), crs = 32615) %>%
  st_join(pop["density_m_2"]) %>%
  mutate(lc =
           recode(tcma_lc_finalv1,
                  "1" = "Grass/Shrub",
                  "2" = "Bare Soil",
                  "3" = "Buildings",
                  "4" = "Impervious",
                  "5" = "Lakes/Ponds",
                  "6" = "Deciduous Canopy",
                  "7" = "Coniferous Canopy",
                  "8" = "Agriculture",
                  "9" = "Emergent Wetland",
                  "10" = "Swamp",
                  "11" = "River",
                  "12" = "Extraction"))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

summary_i <- data_summary(full_set, varname="TCMA_Impervious_2000", 
                            groupnames=c("id", "case_", "sp"))
summary_i$id <- as.factor(summary_i$id)

full_sum_imp <- data.frame(id = NA, ratio = NA, sp = NA)
j <- 1
for (i in levels(summary_i$id)) {
  sum <- data.frame(id = NA, ratio = NA)
  sum$sp <- levels(as.factor(summary_i$sp[summary_i$id == i]))
  sum$id <- i
  sum$ratio <- summary_i$TCMA_Impervious_2000[summary_i$id == i & summary_i$case_ == TRUE] /
    summary_i$TCMA_Impervious_2000[summary_i$id == i & summary_i$case_ == FALSE]
  if (j == 1) {
    full_sum_imp <- sum
  } else {
    full_sum_imp <- rbind(full_sum_imp, sum)
  }
  j <- j + 1
}

summary_imp <- data_summary(full_sum_imp, varname="ratio", 
                            groupnames="sp")
summary_imp <- summary_imp[summary_imp$sp != "gray fox",]

summary_p <- data_summary(full_set, varname="density_m_2", 
                          groupnames=c("id", "case_", "sp"))
summary_p$id <- as.factor(summary_p$id)

full_sum_pop <- data.frame(id = NA, ratio = NA, sp = NA)
j <- 1
for (i in levels(summary_p$id)) {
  sum <- data.frame(id = NA, ratio = NA)
  sum$sp <- levels(as.factor(summary_p$sp[summary_p$id == i]))
  sum$id <- i
  sum$ratio <- summary_p$density_m_2[summary_p$id == i & summary_p$case_ == TRUE] /
    summary_p$density_m_2[summary_p$id == i & summary_p$case_ == FALSE]
  if (j == 1) {
    full_sum_pop <- sum
  } else {
    full_sum_pop <- rbind(full_sum_pop, sum)
  }
  j <- j + 1
}

summary_pop <- data_summary(full_sum_pop, varname="ratio", 
                            groupnames="sp")
summary_pop <- summary_pop[summary_pop$sp != "gray fox",]

collars_rsf <- collars_rsf %>% mutate(tcma_lc_finalv1 =
                                        recode(tcma_lc_finalv1,
                                                "1" = "Grass/Shrub",
                                               "2" = "Bare Soil",
                                               "3" = "Buildings",
                                               "4" = "Impervious",
                                               "5" = "Lakes/Ponds",
                                               "6" = "Deciduous Canopy",
                                               "7" = "Coniferous Canopy",
                                               "8" = "Agriculture",
                                               "9" = "Emergent Wetland",
                                               "10" = "Swamp",
                                               "11" = "River",
                                               "12" = "Extraction"))

collars_sf <- st_as_sf(collars_rsf, coords = c("x_", "y_"), crs = 32615)

plot(collars_sf)

collars_sf <- st_join(collars_sf, pop["density_m_2"])

collars_rsf_trk <- make_track(collars_rsf, x_, y_, t_, id = id, sp = sp, tod_ = tod_, burst_ = burst_)

plot(hr_kde(collars_rsf_trk))

for (i in levels(collars_rsf_trk$id)) {
  collars
}

