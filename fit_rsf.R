# Read in packages -------------------------------------------------------------
library(here)
library(terra)
library(sf)
library(tidyverse)
library(survival)
library(ggplot2)
library(lme4)
library(lmerTest)
# Read in data -----------------------------------------------------------------
# Read in rasters
coi <- rast("E:/data/resample/mask_30/coi_30.tif") %>%
  scale()
canopy <- rast("E:/data/resample/mask_30/can_30.tif") %>%
  scale()
buildings <- rast("E:/data/resample/mask_30/bldng_30.tif") %>%
  scale()
roads <- (rast("E:/data/resample/mask_30/km_km2_30.tif") * 1000) %>%
  scale() # Convert m/m2 to km/km2
open <- rast("E:/data/resample/mask_30/open_30.tif") %>%
  scale()
residential <- rast("E:/data/resample/mask_30/res_30.tif") %>%
  scale()
slope <- (rast("E:/data/resample/mask_30/slope_30.tif") / 100) %>%
  scale()# Proportion
veg <- (rast("E:/data/resample/mask_30/veg_30.tif") / 10000) %>%
  log() %>%
  scale()# Hectares
wetland <- rast("E:/data/resample/mask_30/wet_30.tif") %>%
  scale()
# Read in the RSF data with locations 
gps <- read_csv(here("data/processed_data/rsf_locations.csv"))
# Imp averages
imp_averages <- read.csv(here("data/processed_data/imp_averages.csv")) %>%
  rename(imp = dem)
# Data processing --------------------------------------------------------------
# Transform RSF dataset to georeferenced dataset
gps_sf <- gps %>%
  st_as_sf(coords = c("gps_utm_easting", "gps_utm_northing"))
# Extract covariates to the georeferenced datasets using the TERRA package
coi_dat <- coi %>%
  terra::extract(vect(gps_sf$geometry))
can_dat <- canopy %>%
  terra::extract(vect(gps_sf$geometry))
bui_dat <- buildings %>%
  terra::extract(vect(gps_sf$geometry))
rod_dat <- roads %>%
  terra::extract(vect(gps_sf$geometry))
res_dat <- residential %>%
  terra::extract(vect(gps_sf$geometry))
slo_dat <- slope %>%
  terra::extract(vect(gps_sf$geometry))
veg_dat <- veg %>%
  terra::extract(vect(gps_sf$geometry))
wet_dat <- wetland %>%
  terra::extract(vect(gps_sf$geometry))
# Combine these into a dataframe with a column for each
rsf_dat <- coi_dat %>%
  cbind(can_dat) %>%
  cbind(bui_dat) %>%
  cbind(rod_dat) %>%
  cbind(res_dat) %>%
  cbind(slo_dat) %>%
  cbind(veg_dat) %>%
  cbind(wet_dat) %>%
  dplyr::select(!ID)
# Join to the original RSF data
gps <- gps %>%
  cbind(rsf_dat)
# Create lists for iteration
ids <- levels(as.factor(as.character(gps$animal_id)))
season <- levels(as.factor(as.character(gps$season)))
# Sort out any ids/season where there are no control/case data
for (i in ids) {
  for (j in season) {
    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
      next
    } 
    if (nrow(gps[gps$animal_id == i & gps$season == j & 
                 gps$case == "control", ]) == 0) {
      gps <- gps[!(gps$animal_id == i & gps$season == j & 
                     gps$case == "case"), ]
    }
  }
}
# If there are any seasonal datasets with less than 30 days of tracking, remove
for (i in ids) {
  for (j in season) {
    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
      next
    } 
    cases <- gps[1:nrow(gps[gps$animal_id == i & gps$season == j & 
                              gps$case == "case", ]), ]
    if (nrow(gps[gps$animal_id == i & gps$season == j & 
                 gps$case == "control", ]) == 0) {
      next
    }
    if (as.numeric(difftime(max(cases$gps_fix_time),
                            min(cases$gps_fix_time))) <= 30) {
      gps <- gps[!(gps$animal_id == i & gps$season == j), ]
    }
  }
}
# Case/control as zero or 1
gps[gps$case == "case", ]$case <- 1
gps[gps$case == "control", ]$case <- 0
gps$case <- as.integer(gps$case)
# Get strata
gps$strata <- as.factor(gps$strata)
# Set variables as factor
gps$animal_id <- as.factor(gps$animal_id)
gps$species <- as.factor(gps$species)

frsf_full <- function(df) {
  clogit(case ~ km_km2_30 + 
           veg_30 +
           can_30 + 
           bldng_30 + 
           coi_30 + 
           slope_30 + 
           res_30 + 
           wet_30 + 
           strata(strata), 
         data = df)
}

frsf_res <- function(df) {
  clogit(case ~ km_km2_30 + 
           veg_30 +
           can_30 + 
           bldng_30 + 
           coi_30 + 
           slope_30 + 
           wet_30 + 
           strata(strata), 
         data = df)
}

frsf_wet <- function(df) {
  clogit(case ~ km_km2_30 + 
           veg_30 +
           can_30 + 
           bldng_30 + 
           coi_30 + 
           slope_30 + 
           res_30 +
           strata(strata), 
         data = df)
}

frsf_full <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        bldng_30 + 
        coi_30 + 
        slope_30 + 
        res_30 + 
        wet_30, 
      data = df,
      family = "binomial")
}

frsf_res <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        bldng_30 + 
        coi_30 + 
        slope_30 + 
        wet_30, 
      data = df,
      family = "binomial")
}

frsf_wet <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        bldng_30 + 
        coi_30 + 
        slope_30 + 
        res_30, 
      data = df,
      family = "binomial")
}

frsf_bui <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        coi_30 + 
        slope_30 + 
        res_30 + 
        wet_30, 
      data = df,
      family = "binomial")
}

fmfull <- gps %>%
  group_by(animal_id, species, season) %>%
  filter(!(animal_id == "C14" & season == "other"),
         !(animal_id == "C6"),
         !(animal_id == "F7" & season == "mate"),#) %>%#,
         !(animal_id == "C3"),
         !(animal_id == "C12" & season != "pup"),
         !(animal_id == "C5" & season == "mate"),
         !(animal_id == "C13" & season != "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_full)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy, season)) %>%
  mutate(fm = "full")

fm_res <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "C14" & season == "other") |
         (animal_id == "C6")) %>%
  nest() %>%
  mutate(model = map(data, frsf_res)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy, season)) %>%
  mutate(fm = "-res")

fm_wet <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "F7" & season == "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_wet)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy)) %>%
  mutate(fm = "-wet")

fm_bui <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "C3") |
         (animal_id == "C12" & season != "pup") |
         (animal_id == "C5" & season == "mate") |
         (animal_id == "C13" & season != "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_bui)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy)) %>%
  mutate(fm = "-bui")

models <- fmfull %>%
  bind_rows(fm_res) %>%
  bind_rows(fm_wet) %>%
  bind_rows(fm_bui) %>%
  dplyr::select(!data)

coef_summary_individual <- models %>% 
  dplyr::select(c(animal_id, species, season, conf.low, conf.high, term, estimate, std.error)) %>% 
  group_by(animal_id, season, term, species) %>% 
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate),
            std.error = std.error) %>%
  left_join(imp_averages, by = "animal_id")

coef_summary_species <- coef_summary_individual %>%
  dplyr::select(c(species, season, term, conf.low, conf.high, estimate, std.error)) %>%
  group_by(species, season, term) %>%
  summarize(conf.low = sum(conf.low * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
            conf.high = sum(conf.high * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
            estimate = sum(estimate * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))))

ggplot(coef_summary_species[coef_summary_species$season == "mate" &
                              coef_summary_species$term != "(Intercept)", ], 
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Mating season third order")

ggplot(coef_summary_species[coef_summary_species$season == "pup" &
                              coef_summary_species$term != "(Intercept)", ], 
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Pup season third order")

ggplot(coef_summary_species[coef_summary_species$season == "other" &
                              coef_summary_species$term != "(Intercept)", ], 
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Other season third order")

ggplot(coef_summary_individual[coef_summary_individual$season == "other" &
                              coef_summary_individual$term == "res_30", ], 
       aes(color = reorder(animal_id, -imp))) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Other season third order")

ggplot(coef_summary_individual[coef_summary_individual$season == "mate" &
                                 coef_summary_individual$term == "res_30", ], 
       aes(color = reorder(animal_id, -imp))) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Other season third order")

ggplot(coef_summary_individual[coef_summary_individual$season == "pup" &
                                 coef_summary_individual$term == "res_30" &
                                 coef_summary_individual$species == "coyote", ], 
       aes(color = animal_id)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = imp, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  theme_bw() +
  ggtitle("") +
  ggtitle("Coyote residential third order by % impervious (pup)")

ggplot(coef_summary_individual[coef_summary_individual$season == "pup" &
                                 coef_summary_individual$term == "wet_30" &
                                 coef_summary_individual$species == "coyote", ], 
       aes(color = animal_id)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = imp, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  theme_bw() +
  ggtitle("") +
  ggtitle("Coyote wetland third order by % impervious (pup)")



ssns <- levels(as.factor(coef_summary_individual$season))
terms <- levels(as.factor(coef_summary_individual$term))
sps <- c("coyote", "red fox")

corrs <- data.frame(NA)

l <- 1

for(i in ssns) {
  for(j in terms) {
    for(k in sps) {
      corrs[l, "season"] <- i
      corrs[l, "term"] <- j
      corrs[l, "species"] <- k
      corrs[l, "p"] <- cor.test(coef_summary_individual[coef_summary_individual$season == i &
                                         coef_summary_individual$term == j &
                                         coef_summary_individual$species == k, ]$estimate, 
               coef_summary_individual[coef_summary_individual$season == i &
                                         coef_summary_individual$term == j &
                                         coef_summary_individual$species == k, ]$imp)$p.value
      corrs[l, "est"] <- cor.test(coef_summary_individual[coef_summary_individual$season == i &
                                                       coef_summary_individual$term == j &
                                                       coef_summary_individual$species == k, ]$estimate, 
                             coef_summary_individual[coef_summary_individual$season == i &
                                                       coef_summary_individual$term == j &
                                                       coef_summary_individual$species == k, ]$imp)$estimate
      if(corrs[l, "p"] < 0.05) {
        corrs[l, "sig"] <- "Y"
      } else {
        corrs[l, "sig"] <- "N"
      }
      corrs$NA. <- NULL
      l <- l + 1
    }
  }
}





cor.test(coef_summary_individual[coef_summary_individual$season == "pup" &
                              coef_summary_individual$term == "res_30" &
                              coef_summary_individual$species == "red fox", ]$estimate, 
    coef_summary_individual[coef_summary_individual$season == "pup" &
                              coef_summary_individual$term == "res_30" &
                              coef_summary_individual$species == "red fox", ]$imp)


# Read in the study area (100% MCP of all non-dispersal data)
study_area <- st_read(here("data/processed_data/shps/study_area.shp"), 
                      quiet = TRUE)
water <- read_sf(here("data/gis_layers/shp_water_lakes_rivers/LakesAndRivers.shp")) %>%
  st_union()

study_area <- study_area %>%
  st_difference(water)

index <- tibble(read.csv(here("data/processed_data/step_intervals_appended.csv"))) %>%
  filter((!is.na(burst_11_h) & !is.na(burst_5_h)) |
           !is.na(burst_11_h) |
           !is.na(burst_5_h)) %>%
  filter(!is.na(season)) %>%
  filter(behavior == "resident")

ids <- levels(as.factor(as.character(index$animal_id)))
season <- levels(as.factor(as.character(index$season)))

for (i in ids) {
  for (j in season) {
    if (nrow(index[index$animal_id == i & index$season == j, ]) == 0) {
      next
    } 
    cases <- index[index$animal_id == i & index$season == j, ]
    if (as.numeric(difftime(max(cases$gps_fix_time),
                            min(cases$gps_fix_time))) <= 30) {
      index <- index[!(index$animal_id == i & index$season == j), ]
    }
  }
}

range_summary <- tribble(
  ~animal_id, ~species, ~season, ~count, ~season_pooled
)

for (i in ids) {
  for (j in season) {
    if (nrow(index[index$animal_id == i & index$season == j, ]) == 0) {
      next
    } 
    spp <- index[index$animal_id == i & index$season == j, ]$species[1]
    sum_i <- tribble(
      ~animal_id, ~species, ~season, ~count, ~season_pooled,
      i, spp, j, nrow(index[index$animal_id == i & index$season == j, ]), strsplit(j, "[_]")[[1]][1]
    )
    range_summary <- rbind(range_summary, sum_i)
  }
}

range_values <- range_summary %>%
  group_by(species, season_pooled) %>%
  summarize(mean = round(mean(count), 0), n = n())

range_values$mean



ranges <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(method == "kde_ani_95") %>%
  filter(season != "annual") %>%
  filter(range_type == "resident")
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
# Get rid of water
for (i in 1:nrow(ranges)) {
  ranges$geometry[i] <- st_difference(ranges$geometry[i], water)
}
# Create a sample points dataset -----------------------------------------------
# Table for iteration
sample_points <- tibble()
# Create points
for (i in 1:nrow(ranges)) {
  # Select range
  range <- ranges[i, ]
  spp <- ranges[i, ]$species
  ssn <- ranges[i, ]$season
  samp <- range_values[range_values$season_pooled == ssn & range_values$species == spp, ]$mean
  # Generate pts within the range
  points <- st_sample(range, size = samp, type = 'random', exact = TRUE)
  # Translate into a data table
  coords <- do.call(rbind, st_geometry(points)) %>% 
    as_tibble() %>% 
    setNames(c("gps_utm_easting", "gps_utm_northing")) %>%
    mutate(case = "case")
  # Generate control pts within the study area
  cont_points <- st_sample(study_area, size = (samp * 10), type = 'random', exact = TRUE)
  # Translate into a data table
  cont_coords <- do.call(rbind, st_geometry(cont_points)) %>% 
    as_tibble() %>% 
    setNames(c("gps_utm_easting", "gps_utm_northing")) %>%
    mutate(case = "control")
  # Bind together
  coords <- coords %>%
    rbind(cont_coords)
  # Append useful columns from the ranges dataset
  coords$animal_id <- range$animal_id
  coords$season <- range$season
  # Append to the wider dataset
  sample_points <- rbind(sample_points, coords)
}



sample_points$species <- NA
sample_points[grep("C", sample_points$animal_id), ]$species <- "coyote"
sample_points[grep("F", sample_points$animal_id), ]$species <- "red fox"
sample_points[sample_points$animal_id == "F10" | 
                sample_points$animal_id == "F18", ]$species <- "gray fox"
# Join turn columns into factors
sample_points <- sample_points %>%
  mutate_at(vars(animal_id, species, season, case), funs(as.factor))

gps <- sample_points

gps$case_ <- NA

gps[gps$case == "case", ]$case_ <- 1
gps[gps$case == "control", ]$case_ <- 0
gps$case <- gps$case_
gps$case_ <- NULL
gps$case <- as.integer(gps$case)


gps_sf <- gps %>%
  st_as_sf(coords = c("gps_utm_easting", "gps_utm_northing"))

coi_dat <- coi %>%
  terra::extract(vect(gps_sf$geometry))
can_dat <- canopy %>%
  terra::extract(vect(gps_sf$geometry))
bui_dat <- buildings %>%
  terra::extract(vect(gps_sf$geometry))
rod_dat <- roads %>%
  terra::extract(vect(gps_sf$geometry))
#ope_dat <- open %>%
#  terra::extract(vect(gps_sf$geometry))
res_dat <- residential %>%
  terra::extract(vect(gps_sf$geometry))
slo_dat <- slope %>%
  terra::extract(vect(gps_sf$geometry))
veg_dat <- veg %>%
  terra::extract(vect(gps_sf$geometry))
wet_dat <- wetland %>%
  terra::extract(vect(gps_sf$geometry))

rsf_dat <- coi_dat %>%
  cbind(can_dat) %>%
  cbind(bui_dat) %>%
  cbind(rod_dat) %>%
#  cbind(ope_dat) %>%
  cbind(res_dat) %>%
  cbind(slo_dat) %>%
  cbind(veg_dat) %>%
  cbind(wet_dat) %>%
  dplyr::select(!ID)

gps <- gps %>%
  cbind(rsf_dat)







frsf_full <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        bldng_30 + 
        coi_30 + 
        slope_30 + 
        res_30 + 
        wet_30, 
      data = df,
      family = "binomial")
}

frsf_wet <- function(df) {
  glm(case ~ km_km2_30 + 
#        veg_30 +
        can_30 + 
        bldng_30 + 
        coi_30 + 
        #scale(open_30) + 
        slope_30 + 
        res_30,
      data = df,
      family = "binomial")
}

#frsf_veg <- function(df) {
#  glm(case ~ km_km2_30 + 
#        can_30 + 
#        bldng_30 + 
#        coi_30 + 
#        slope_30 + 
#        res_30 + 
#        wet_30, 
#      data = df,
#      family = "binomial")
#}

fmfull <- gps %>%
  group_by(animal_id, species, season) %>%
  filter(!(animal_id == "F8" & season == "mate"),
         !(animal_id == "F7" & season == "mate"),
#         !(animal_id == "F2"),
#         !(animal_id == "F4"),
#         !(animal_id == "F16"),
         !(animal_id == "F12" & season == "mate"),
         !(animal_id == "F10" & season == "other"),
         !(animal_id == "F1" & season == "other"), #,
#         !(animal_id == "C14" & season == "other"),
#####         !(animal_id == "C12" & season != "pup"),
         !(animal_id == "C6" & season == "pup"), #,
         !(animal_id == "C1" & season == "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_full)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy, season)) %>%
  mutate(fm = "full")

fm_wet <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "F8" & season == "mate") |
         (animal_id == "F7" & season == "mate") |
         (animal_id == "F12" & season == "mate")
         (animal_id == "F1" & season == "other") |
         (animal_id == "F10" & season == "other") |
         (animal_id == "C1" & season == "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_wet)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy)) %>%
  mutate(fm = "-wet")

fm_res <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "C6" & season == "pup")) %>%
  nest() %>%
  mutate(model = map(data, frsf_veg)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy, season)) %>%
  mutate(fm = "veg")




models_2nd_order <- fmfull# %>%
  bind_rows(fm_wet) %>%
  bind_rows(fm_veg) %>%
  dplyr::select(!c(data))


imp_averages <- read.csv(here("data/processed_data/imp_averages.csv")) %>%
  rename(imp = dem)


coef_summary_individual_2nd <- models_2nd_order %>% 
  dplyr::select(c(animal_id, species, season, conf.low, conf.high, term, estimate, std.error)) %>% 
  group_by(animal_id, season, term, species) %>% 
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate),
            std.error = std.error)



coef_summary_species_2nd <- coef_summary_individual_2nd %>%
  dplyr::select(c(species, season, term, conf.low, conf.high, estimate, std.error)) %>%
  group_by(species, season, term) %>%
  summarize(conf.low = sum(conf.low * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
            conf.high = sum(conf.high * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
            estimate = sum(estimate * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))))

ggplot(coef_summary_species_2nd[coef_summary_species_2nd$season == "mate" &
                                coef_summary_species_2nd$species != "gray fox" &
                                  coef_summary_species_2nd$term != "(Intercept)",],
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Mating season second order")

ggplot(coef_summary_species_2nd[coef_summary_species_2nd$season == "pup" &
                                  coef_summary_species_2nd$species != "gray fox" &
                                  coef_summary_species_2nd$term != "(Intercept)",],
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Pup season second order")

ggplot(coef_summary_species_2nd[coef_summary_species_2nd$season == "other" &
                                  coef_summary_species_2nd$species != "gray fox" &
                                  coef_summary_species_2nd$term != "(Intercept)",],
       aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species) +
  ggtitle("Other season second order")


kvars <- levels(as.factor(as.character(coef_summary_individual_2nd$term)))

for (i in kvars) {
  plt <- ggplot(coef_summary_individual_2nd[coef_summary_individual_2nd$term == i, ], 
                aes(color = reorder(animal_id, imp))) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                   position = position_dodge(width = 1/2)) +
    coord_flip() + 
    theme_bw() +
    scale_fill_brewer() +
    ggtitle("") +
    facet_grid(~species)
  print(plt)
}

species <- levels(as.factor(as.character(coef_summary_species_2nd$species)))
season <- levels(as.factor(as.character(coef_summary_species_2nd$season)))

rerange <- function(x) {
  (x - min(x)) / (max(x) - min(x))
  }

for (i in species) {
  for (j in season) {
    fm <- coef_summary_species_2nd[coef_summary_species_2nd$season == j & 
                                     coef_summary_species_2nd$species == i, ]
    pred_i <- fm$estimate[1] + (fm$estimate[2] * buildings) + 
      (fm$estimate[3] * canopy) + 
      (fm$estimate[4] * coi) + (fm$estimate[5] * roads) + 
      (fm$estimate[6] * residential) + (fm$estimate[7] * slope) + 
      (fm$estimate[8] * veg) + (fm$estimate[9] * wetland)
    odds <- exp(pred_i)
    prob <- odds / (1 + odds)
    prob <- (prob - prob@ptr$range_min) / (prob@ptr$range_max - prob@ptr$range_min)
    assign(paste0(sub(" ", "_", i), "_", j, "_2nd"), prob)
  }
}

  
for (i in species) {
  for (j in season) {
    fm <- coef_summary_species[coef_summary_species$season == j & 
                                     coef_summary_species$species == i, ]
    pred_i <- fm$estimate[1] + (fm$estimate[2] * buildings) + 
      (fm$estimate[3] * canopy) + 
      (fm$estimate[4] * coi) + (fm$estimate[5] * roads) + 
      (fm$estimate[6] * residential) + (fm$estimate[7] * slope) + 
      (fm$estimate[8] * veg) + (fm$estimate[9] * wetland)
    odds <- exp(pred_i)
    prob <- odds / (1 + odds)
    prob <- (prob - prob@ptr$range_min) / (prob@ptr$range_max - prob@ptr$range_min)
    assign(paste0(sub(" ", "_", i), "_", j, "_3rd"), prob)
  }
}

plot(terra::mask((red_fox_mate_2nd * red_fox_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((red_fox_pup_2nd * red_fox_pup_3rd), vect(water), inverse = TRUE))
plot(terra::mask((red_fox_other_2nd * red_fox_other_3rd), vect(water), inverse = TRUE))

plot(terra::mask((coyote_mate_2nd * coyote_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_pup_2nd * coyote_pup_3rd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_other_2nd * coyote_other_3rd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_mate_2nd * coyote_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((gray_fox_mate_2nd * gray_fox_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((gray_fox_other_2nd * gray_fox_other_3rd), vect(water), inverse = TRUE))
plot(terra::mask((gray_fox_pup_2nd * gray_fox_pup_3rd), vect(water), inverse = TRUE))

plot(terra::mask(((red_fox_pup_2nd / red_fox_pup_2nd@ptr$range_max) * (coyote_pup_2nd / coyote_pup_2nd@ptr$range_max)), vect(water), inverse = TRUE), range = c(0, 1))
     

plot(coyote_mate_2nd)



plot(terra::mask((red_fox_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((red_fox_pup_3rd), vect(water), inverse = TRUE))

plot(terra::mask((red_fox_other_3rd), vect(water), inverse = TRUE))

plot(terra::mask((coyote_mate_3rd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_pup_3rd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_other_3rd), vect(water), inverse = TRUE))



plot(terra::mask((red_fox_mate_2nd), vect(water), inverse = TRUE))
plot(terra::mask((red_fox_pup_2nd), vect(water), inverse = TRUE))
plot(terra::mask((red_fox_other_2nd), vect(water), inverse = TRUE))

plot(terra::mask((coyote_mate_2nd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_pup_2nd), vect(water), inverse = TRUE))
plot(terra::mask((coyote_other_2nd), vect(water), inverse = TRUE))

plot(terra::mask(((red_fox_mate_3rd * red_fox_other_3rd) * red_fox_pup_3rd), vect(water), inverse = TRUE))
plot(terra::mask(((red_fox_mate_2nd * red_fox_other_2nd) * red_fox_pup_2nd), vect(water), inverse = TRUE))
plot(terra::mask(((coyote_mate_3rd * coyote_other_3rd) * coyote_pup_3rd), vect(water), inverse = TRUE))
plot(terra::mask(((coyote_mate_2nd * coyote_other_2nd) * coyote_pup_2nd), vect(water), inverse = TRUE))

plot(terra::mask((((coyote_mate_2nd * coyote_other_2nd) * coyote_pup_2nd) * ((coyote_mate_3rd * coyote_other_3rd) * coyote_pup_3rd)), vect(water), inverse = TRUE))
hist(terra::mask((((red_fox_mate_2nd * red_fox_other_2nd) * red_fox_pup_2nd) * ((red_fox_mate_3rd * red_fox_other_3rd) * red_fox_pup_3rd)), vect(water), inverse = TRUE))
################################################
# Include coyote selection for foxes
#
#gps <- read_csv(here("data/processed_data/rsf_locations.csv"))
#
#gps_sf <- gps %>%
#  st_as_sf(coords = c("gps_utm_easting", "gps_utm_northing"))
#
#
# Fox w/ coyote selection
#cm <- scale(coyote_mate_2nd * coyote_mate_3rd)
#names(cm) <- "cm"
#cm2 <- scale(coyote_mate_2nd)
#names(cm2) <- "cm2"
#cm3 <- scale(coyote_mate_3rd)
#names(cm3) <- "cm3"
#cp <- scale(coyote_pup_2nd * coyote_pup_3rd)
#names(cp) <- "cp"
#cp2 <- scale(coyote_pup_2nd)
#names(cp2) <- "cp2"
#cp3 <- scale(coyote_pup_3rd)
#names(cp3) <- "cp3"
#co <- scale(coyote_other_2nd * coyote_other_3rd)
#names(co) <- "co"
#co2 <- scale(coyote_other_2nd)
#names(co2) <- "co2"
#co3 <- scale(coyote_other_3rd)
#names(co3) <- "co3"
##
#
#
#coi_dat <- coi %>%
#  terra::extract(vect(gps_sf$geometry))
#can_dat <- canopy %>%
#  terra::extract(vect(gps_sf$geometry))
#bui_dat <- buildings %>%
#  terra::extract(vect(gps_sf$geometry))
#rod_dat <- roads %>%
#  terra::extract(vect(gps_sf$geometry))
##ope_dat <- open %>%
##  terra::extract(vect(gps_sf$geometry))
#res_dat <- residential %>%
#  terra::extract(vect(gps_sf$geometry))
#slo_dat <- slope %>%
#  terra::extract(vect(gps_sf$geometry))
#veg_dat <- veg %>%
#  terra::extract(vect(gps_sf$geometry))
#wet_dat <- wetland %>%
#  terra::extract(vect(gps_sf$geometry))
#log_veg_dat <- log_veg %>%
#  terra::extract(vect(gps_sf$geometry))
#cm_dat <- cm %>%
#  terra::extract(vect(gps_sf$geometry))
#cm2_dat <- cm2 %>%
#  terra::extract(vect(gps_sf$geometry))
#cm3_dat <- cm3 %>%
#  terra::extract(vect(gps_sf$geometry))
#cp_dat <- cp %>%
#  terra::extract(vect(gps_sf$geometry))
#cp2_dat <- cp2 %>%
#  terra::extract(vect(gps_sf$geometry))
#cp3_dat <- cp3 %>%
#  terra::extract(vect(gps_sf$geometry))
#co_dat <- co %>%
#  terra::extract(vect(gps_sf$geometry))
#co2_dat <- co2 %>%
#  terra::extract(vect(gps_sf$geometry))
#co3_dat <- co3 %>%
#  terra::extract(vect(gps_sf$geometry))
#rsf_dat <- coi_dat %>%
#  cbind(can_dat) %>%
#  cbind(bui_dat) %>%
#  cbind(rod_dat) %>%
#  cbind(res_dat) %>%
#  cbind(slo_dat) %>%
#  cbind(veg_dat) %>%
#  cbind(wet_dat) %>%
#  cbind(log_veg_dat) %>%
#  cbind(cm_dat) %>%
#  cbind(cm2_dat) %>%
#  cbind(cm3_dat) %>%
#  cbind(cp_dat) %>%
#  cbind(cp2_dat) %>%
#  cbind(cp3_dat) %>%
#  cbind(co_dat) %>%
#  cbind(co2_dat) %>%
#  cbind(co3_dat) %>%
#  dplyr::select(!ID)
#
#
#
#
#
#
#gps <- gps %>%
#  cbind(rsf_dat)
#
#
#
#
#
#ids <- levels(as.factor(as.character(gps$animal_id)))
#season <- levels(as.factor(as.character(gps$season)))
#
#
#for (i in ids) {
#  for (j in season) {
#    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
#      next
#    } 
#    if (nrow(gps[gps$animal_id == i & gps$season == j & 
#                 gps$case == "control", ]) == 0) {
#      gps <- gps[!(gps$animal_id == i & gps$season == j & 
#                     gps$case == "case"), ]
#    }
#  }
#}
#
#for (i in ids) {
#  for (j in season) {
#    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
#      next
#    } 
#    cases <- gps[1:nrow(gps[gps$animal_id == i & gps$season == j & 
#                              gps$case == "case", ]), ]
#    if (nrow(gps[gps$animal_id == i & gps$season == j & 
#                 gps$case == "control", ]) == 0) {
#      next
#    }
#    if (as.numeric(difftime(max(cases$gps_fix_time),
#                            min(cases$gps_fix_time))) <= 30) {
#      gps <- gps[!(gps$animal_id == i & gps$season == j), ]
#    }
#  }
#}
#
#gps[gps$case == "case", ]$case <- 1
#gps[gps$case == "control", ]$case <- 0
#gps$case <- as.integer(gps$case)
#gps$strata <- as.factor(gps$strata)
#gps$animal_id <- as.factor(gps$animal_id)
#gps$species <- as.factor(gps$species)
#gps[is.na(gps$buildings), ]$buildings <- 0
#
#
#
#
#frsf_full_mate <- function(df) {
#  clogit(case ~ km_km2_30 + 
#           veg_30 +
#           can_30 + 
#           #bldng_30 + 
#           coi_30 + 
#           slope_30 + 
#           res_30 + 
#           wet_30 +
#           cm +
#           cm2 +
#           cm3 +
#           strata(strata), 
#         data = df)
#}
#
#frsf_wet_mate <- function(df) {
#  clogit(case ~ km_km2_30 + 
#           veg_30 +
#           can_30 + 
#           #bldng_30 + 
#           coi_30 + 
#           slope_30 + 
#           res_30 + 
#           cm +
#           cm2 +
#           cm3 +
#           strata(strata),
#         data = df)
#}
#
#
#fmfull_mate <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(species == "red fox" & season == "mate") %>%
#  filter(!(animal_id == "F7" & season == "mate")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_full_mate)) %>%
#  mutate(glance = map(model, broom::glance)) %>% 
#  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, conf_tidy, tidy, season)) %>%
#  mutate(fm = "full")
#
#fm_wet_mate <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(species == "red fox" & season == "mate") %>%
#  filter((animal_id == "F7" & season == "mate")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_wet_mate)) %>%
#  mutate(glance = map(model, broom::glance)) %>% 
#  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, conf_tidy, tidy)) %>%
#  mutate(fm = "-wet")
#
#
#foxes_3rd_mate <- fmfull_mate %>%
#  bind_rows(fm_wet_mate) %>%
#  dplyr::select(!c(data))
#
#mean(foxes_3rd_mate$AIC)
#
#
#
#coef_summary_individual <- foxes_3rd_mate %>% 
#  dplyr::select(c(animal_id, species, season, conf.low, conf.high, term, estimate, std.error)) %>% 
#  group_by(animal_id, season, term, species) %>% 
#  summarize(conf.low = mean(conf.low), 
#            conf.high = mean(conf.high), 
#            estimate = mean(estimate),
#            std.error = std.error)
#
#coef_summary_species <- coef_summary_individual %>%
#  dplyr::select(c(species, season, term, conf.low, conf.high, estimate, std.error)) %>%
#  group_by(species, season, term) %>%
#  summarize(conf.low = sum(conf.low * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            conf.high = sum(conf.high * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            estimate = sum(estimate * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))))
#
#ggplot(coef_summary_species, aes(colour = species)) +
#  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
#  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
#                 position = position_dodge(width = 1/2)) +
#  coord_flip() + 
#  theme_bw() +
#  ggtitle("") +
#  facet_grid(~species)
#
#
#
#
# PUP
#
#frsf_full_pup <- function(df) {
#  clogit(case ~ km_km2_30 + 
#           veg_30 +
#           can_30 + 
#           bldng_30 + 
#           coi_30 + 
#           slope_30 + 
#           res_30 + 
#           wet_30 +
##           cp +
#           cp2 +
#           cp3 +
#           strata(strata), 
#         data = df)
#}
#
#fmfull_mate <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(species == "red fox" & season == "pup") %>%
#  nest() %>%
#  mutate(model = map(data, frsf_full_pup)) %>%
#  mutate(glance = map(model, broom::glance)) %>% 
#  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, conf_tidy, tidy, season)) %>%
#  mutate(fm = "full")#
#
#
#foxes_3rd_pup <- fmfull_mate %>%
#  dplyr::select(!c(data))
#
#mean(foxes_3rd_pup$AIC)
#
#car::vif(foxes_3rd_pup$model[[1]])
#
#coef_summary_individual <- foxes_3rd_pup %>% 
#  dplyr::select(c(animal_id, species, season, conf.low, conf.high, term, estimate, std.error)) %>% 
#  group_by(animal_id, season, term, species) %>% 
#  summarize(conf.low = mean(conf.low), 
#            conf.high = mean(conf.high), 
#            estimate = mean(estimate),
#            std.error = std.error)#
#
#coef_summary_species <- coef_summary_individual %>%
#  dplyr::select(c(species, season, term, conf.low, conf.high, estimate, std.error)) %>%
#  group_by(species, season, term) %>%
#  summarize(conf.low = sum(conf.low * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            conf.high = sum(conf.high * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            estimate = sum(estimate * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))))
#
#ggplot(coef_summary_species, aes(colour = species)) +
#  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
#  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
#                 position = position_dodge(width = 1/2)) +
#  coord_flip() + 
#  theme_bw() +
#  ggtitle("") +
#  facet_grid(~species)
#
##
#
#
#
########################## 2nd order
#
#
#gps <- sample_points
#
#gps$case_ <- NA
#
#gps[gps$case == "case", ]$case_ <- 1
#gps[gps$case == "control", ]$case_ <- 0
#gps$case <- gps$case_
#gps$case_ <- NULL
#gps$case <- as.integer(gps$case)
#
#
#gps_sf <- gps %>%
#  st_as_sf(coords = c("gps_utm_easting", "gps_utm_northing"))
#
#coi_dat <- coi %>%
#  terra::extract(vect(gps_sf$geometry))
#can_dat <- canopy %>%
#  terra::extract(vect(gps_sf$geometry))
#bui_dat <- buildings %>%
#  terra::extract(vect(gps_sf$geometry))
#rod_dat <- roads %>%
#  terra::extract(vect(gps_sf$geometry))
#res_dat <- residential %>%
#  terra::extract(vect(gps_sf$geometry))
#slo_dat <- slope %>%
#  terra::extract(vect(gps_sf$geometry))
#veg_dat <- veg %>%
#  terra::extract(vect(gps_sf$geometry))
#wet_dat <- wetland %>%
#  terra::extract(vect(gps_sf$geometry))
#log_veg_dat <- log_veg %>%
#  terra::extract(vect(gps_sf$geometry))
#cm_dat <- cm %>%
#  terra::extract(vect(gps_sf$geometry))
#cm2_dat <- cm2 %>%
#  terra::extract(vect(gps_sf$geometry))
#cm3_dat <- cm3 %>%
#  terra::extract(vect(gps_sf$geometry))
#cp_dat <- cp %>%
#  terra::extract(vect(gps_sf$geometry))
#cp2_dat <- cp2 %>%
#  terra::extract(vect(gps_sf$geometry))
#cp3_dat <- cp3 %>%
#  terra::extract(vect(gps_sf$geometry))
#co_dat <- co %>%
#  terra::extract(vect(gps_sf$geometry))
#co2_dat <- co2 %>%
#  terra::extract(vect(gps_sf$geometry))
#co3_dat <- co3 %>%
#  terra::extract(vect(gps_sf$geometry))
#rsf_dat <- coi_dat %>%
#  cbind(can_dat) %>%
#  cbind(bui_dat) %>%
#  cbind(rod_dat) %>%
#  cbind(res_dat) %>%
#  cbind(slo_dat) %>%
#  cbind(veg_dat) %>%
#  cbind(wet_dat) %>%
#  cbind(log_veg_dat) %>%
#  cbind(cm_dat) %>%
#  cbind(cm2_dat) %>%
#  cbind(cm3_dat) %>%
#  cbind(cp_dat) %>%
#  cbind(cp2_dat) %>%
#  cbind(cp3_dat) %>%
#  cbind(co_dat) %>%
#  cbind(co2_dat) %>%
#  cbind(co3_dat) %>%
#  dplyr::select(!ID)
#
#gps <- gps %>%
#  cbind(rsf_dat)
###
#
#
#
#
#
#
#frsf_full <- function(df) {
#  glm(case ~ #km_km2_30 + 
#        veg_30 +
#        can_30 + 
#        bldng_30 + 
#        coi_30 +
#        cp +
#        cp2 +
#        cp3 +
#        slope_30 + 
#        res_30 + 
#        wet_30 - 
#        - 1, 
#      data = df,
#      family = "binomial")
#}
#
#
#fmfull_mate_2nd <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(species == "red fox" & season == "pup") %>%
#  filter(animal_id != "F1") %>%
#  nest() %>%
#  mutate(model = map(data, frsf_full)) %>%
#  mutate(glance = map(model, broom::glance)) %>% 
#  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, conf_tidy, tidy, season)) %>%
#  mutate(fm = "full")
#
#fm_wet <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter((animal_id == "F7" & season == "mate") |
#         (animal_id == "F16" & season == "mate") |
#         (animal_id == "F10" & season == "pup")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_wet)) %>%
#  mutate(glance = map(model, broom::glance)) %>% 
#  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, conf_tidy, tidy)) %>%
#  mutate(fm = "-wet")
#
#mean(fmfull_mate_2nd$AIC)
#
#car::vif(fmfull_mate_2nd$model[[1]])
#
#
#coef_summary_individual_2nd <- fmfull_mate_2nd %>% 
#  dplyr::select(c(animal_id, species, season, conf.low, conf.high, term, estimate, std.error)) %>% 
#  group_by(animal_id, season, term, species) %>% 
#  summarize(conf.low = mean(conf.low), 
#            conf.high = mean(conf.high), 
#            estimate = mean(estimate),
#            std.error = std.error)
#
#
#
#coef_summary_species_2nd <- coef_summary_individual_2nd %>%
#  dplyr::select(c(species, season, term, conf.low, conf.high, estimate, std.error)) %>%
#  group_by(species, season, term) %>%
#  summarize(conf.low = sum(conf.low * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            conf.high = sum(conf.high * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))), 
#            estimate = sum(estimate * ((1 / (std.error^2)) / (sum(1 / (std.error^2))))))
#
#ggplot(coef_summary_species_2nd[coef_summary_species_2nd$season == "pup", ],
#       aes(colour = species)) +
#  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
#  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
#                 position = position_dodge(width = 1/2)) +
#  coord_flip() + 
#  theme_bw() +
#  ggtitle("") +
#  facet_grid(~species)
#
#
#
#ggplot(data = data.frame(x = terra::mask((coyote_pup_2nd * coyote_pup_3rd), vect(water), inverse = TRUE),
#                         y = terra::mask((red_fox_pup_2nd * red_fox_pup_3rd), vect(water), inverse = TRUE))) +
#  geom_point(aes(x = x, y = y))

cm <- as.points(terra::mask((coyote_mate_2nd * coyote_mate_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(coyote_mate = bldng_30) %>% 
  select(x, y, coyote_mate)

fm <- as.points(terra::mask((red_fox_mate_2nd * red_fox_mate_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(fox_mate = bldng_30) %>% 
  select(x, y, fox_mate)

mate <- full_join(cm, fm, by = c("x", 'y'))

cp <- as.points(terra::mask((coyote_pup_2nd * coyote_pup_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(coyote_pup = bldng_30) %>% 
  select(x, y, coyote_pup)

fp <- as.points(terra::mask((red_fox_pup_2nd * red_fox_pup_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(fox_pup = bldng_30) %>% 
  select(x, y, fox_pup)

pup <- full_join(cp, fp, by = c("x", 'y'))

co <- as.points(terra::mask((coyote_other_2nd * coyote_other_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(coyote_other = bldng_30) %>% 
  select(x, y, coyote_other)

fo <- as.points(terra::mask((red_fox_other_2nd * red_fox_other_3rd), 
                            vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(fox_other = bldng_30) %>% 
  select(x, y, fox_other)

other <- full_join(co, fo, by = c("x", 'y'))


rod_pt <- as.points(terra::mask(roads, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(rod = km_km2_30) %>% 
  select(x, y, rod)

can_pt <- as.points(terra::mask(canopy, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(can = can_30) %>% 
  select(x, y, can)

bui_pt <- as.points(terra::mask(buildings, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(bui = bldng_30) %>% 
  select(x, y, bui)

res_pt <- as.points(terra::mask(residential, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(res = res_30) %>% 
  select(x, y, res)

slo_pt <- as.points(terra::mask(slope, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(slo = slope_30) %>% 
  select(x, y, slo)

veg_pt <- as.points(terra::mask(veg, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(veg = veg_30) %>% 
  select(x, y, veg)

wet_pt <- as.points(terra::mask(wetland, vect(water), inverse = TRUE)) %>% 
  data.frame(values(.), geom(.)) %>% 
  tibble() %>% 
  mutate(wet = wet_30) %>% 
  select(x, y, wet)


vars <- full_join(rod_pt, can_pt, by = c("x", 'y')) %>%
  full_join(bui_pt, by = c("x", 'y')) %>%
  full_join(res_pt, by = c("x", 'y')) %>%
  full_join(slo_pt, by = c("x", 'y')) %>%
  full_join(veg_pt, by = c("x", 'y')) %>%
  full_join(wet_pt, by = c("x", 'y'))

mate <- full_join(mate, vars, by = c("x", "y"))
pup <- full_join(pup, vars, by = c("x", "y"))
other <- full_join(other, vars, by = c("x", "y"))


ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  geom_smooth()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  geom_smooth()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis") +
  geom_smooth()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate, 
                                          color = res)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate,
                                          color = rod)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate,
                                          color = can)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate, 
                                          color = bui)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate,
                                          color = veg)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate, 
                                          color = slo)) +
  geom_point()

ggplot(data = sample_n(mate, 100000), aes(x = coyote_mate, y = fox_mate, 
                                         colour = wet)) +
  geom_point()



ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                          color = res)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = rod)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = can)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = bui)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = veg)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = slo)) +
  geom_point()

ggplot(data = sample_n(pup, 100000), aes(x = coyote_pup, y = fox_pup, 
                                         color = wet)) +
  geom_point()



ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = res)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = rod)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = can)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = bui)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = veg)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = slo)) +
  geom_point()

ggplot(data = sample_n(other, 100000), aes(x = coyote_other, y = fox_other, 
                                         color = wet)) +
  geom_point()

# # spearman correlation test
xspcor <- gps[, 22:29]  # create object containing specific columns of a dataframe (mine had lots of extraneous columns) 
xspcor <- xspcor[complete.cases(xspcor), ]
spcor <- cor(xspcor, method = "spearman") # run test and send output to new object; note there are other tests available but I probably used Spearman because it is nonparametric
spcor <- as.data.frame(spcor)  # convert the new object to dataframe  
# xlsx::write.xlsx(spcor, "C:/GIS/r_output/intermediates/spearmancor041719.xlsx") # output dataframe to XLS for post processing; to flag correlations above a threshold (e.g., r>0.9)