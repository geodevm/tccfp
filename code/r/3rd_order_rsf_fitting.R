#===============================================================================
### Extract covariates within a 20 meter buffer zone for the cumulative index
### of activity.
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Read in packages -------------------------------------------------------------
library(here)
library(lubridate)
library(sf)
library(tidyverse)
library(survival)
library(viridis)
# Load in data -----------------------------------------------------------------
gps <- read.csv(here("data/processed_data/covariates_appended.csv"))
imp_averages <- read.csv(here("data/processed_data/imp_averages.csv")) %>%
  rename(imp = dem)
# Fix the animal_id column
rsfs <- read.csv(here("data/processed_data/rsf_locations.csv"))
gps$animal_id <- rsfs$animal_id
rm(rsfs)
# Make NAs into zeroes
gps[is.na(gps$grass_shrub), ]$grass_shrub <- 0
gps[is.na(gps$lakes_ponds), ]$lakes_ponds <- 0
gps[is.na(gps$deciduous), ]$deciduous <- 0
gps[is.na(gps$coniferous), ]$coniferous <- 0
gps[is.na(gps$agriculture), ]$agriculture <- 0
gps[is.na(gps$emergent_wetland), ]$emergent_wetland <- 0
gps[is.na(gps$forested_shrub_wetland), ]$forested_shrub_wetland <- 0
gps[is.na(gps$river), ]$river <- 0

gps$open <- gps$grass_shrub + gps$agriculture
gps$canopy <- gps$deciduous + gps$coniferous + gps$forested_shrub_wetland

gps <- gps %>%
  filter(season != "other_19")

gps$strata <- NA

ids <- levels(as.factor(as.character(gps$animal_id)))
season <- levels(as.factor(as.character(gps$season)))
for (i in ids) {
  for (j in season) {
    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
      next
    } else {
      gps[gps$animal_id == i & 
            gps$case == "case" &
            gps$season == j, ]$strata <- 1:nrow(gps[gps$animal_id == i & 
                                                      gps$case == "case" &
                                                      gps$season == j, ])
      q <- nrow(gps[gps$animal_id == i & 
                      gps$case == "control" &
                      gps$season == j, ]) %/%
        nrow(gps[gps$animal_id == i & 
                   gps$case == "case" &
                   gps$season == j, ])
      r <- nrow(gps[gps$animal_id == i & 
                      gps$case == "control" &
                      gps$season == j, ]) %%
        nrow(gps[gps$animal_id == i & 
                   gps$case == "case" &
                   gps$season == j, ])
      if (q == 0) {
        next
      }
      repeats <- 1:nrow(gps[gps$animal_id == i & 
                              gps$case == "case" &
                              gps$season == j, ])
      final <- 1:r
      rand_strata <- sample(c(rep(repeats, q), final))
      gps[gps$animal_id == i & 
            gps$case == "control" &
            gps$season == j, ]$strata <- rand_strata
    }
  }
}

gps <- gps[rowSums(is.na(gps)) != ncol(gps), ]

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

for (i in ids) {
  for (j in season) {
    if (nrow(gps[gps$animal_id == i & gps$season == j, ]) == 0) {
      next
    } 
    cases <- gps[1:nrow(gps[gps$animal_id == i & gps$season == j & 
                              gps$case == "case", ]), ]
    if (as.numeric(difftime(max(cases$gps_fix_time),
                            min(cases$gps_fix_time))) <= 30) {
      next
    }
    if (nrow(gps[gps$animal_id == i & gps$season == j & 
                 gps$case == "control", ]) == 0) {
      next
    }
    for (k in 1:nrow(cases)) {
      gps[gps$animal_id == i & gps$season == j & gps$case == "control" &
            gps$strata == k, ]$tod_ <- cases$tod_[k]
      gps[gps$animal_id == i & gps$season == j & gps$case == "control" &
            gps$strata == k, ]$gps_fix_time <- cases$gps_fix_time[k]
    }
  }
}

gps[gps$case == "case", ]$case <- 1
gps[gps$case == "control", ]$case <- 0
gps$case <- as.integer(gps$case)
gps$strata <- as.factor(gps$strata)
gps$animal_id <- as.factor(gps$animal_id)
gps$species <- as.factor(gps$species)
gps[is.na(gps$buildings), ]$buildings <- 0

frsf_full <- function(df) {
  clogit(case ~ scale(imp) + scale(patch_size) + scale(canopy) + scale(open) + 
           scale(coi) + scale(emergent_wetland) + scale(slope) + 
           scale(buildings) + strata(strata), 
         data = df)
}



frsf_partial <- function(df) {
  clogit(case ~ scale(imp) + scale(patch_size) + scale(canopy) + scale(open) + 
           scale(coi) + scale(slope) + scale(buildings) + strata(strata), 
         data = df)
}






frsf_1 <- function(df) {
  clogit(case ~ scale(imp) + strata(strata), 
         data = df)
}
frsf_2 <- function(df) {
  clogit(case ~ scale(aspect) + strata(strata), 
         data = df)
}
frsf_3 <- function(df) {
  clogit(case ~ scale(open) + strata(strata), 
         data = df)
}
frsf_4 <- function(df) {
  clogit(case ~ scale(canopy) + strata(strata), 
         data = df)
}
frsf_5 <- function(df) {
  clogit(case ~ scale(emergent_wetland) + strata(strata), 
         data = df)
}
frsf_6 <- function(df) {
  clogit(case ~ scale(coi) + strata(strata), 
         data = df)
}
frsf_7 <- function(df) {
  clogit(case ~ scale(patch_size) + strata(strata), 
         data = df)
}
frsf_8 <- function(df) {
  clogit(case ~ scale(open) + scale(canopy) + strata(strata), 
         data = df)
}
frsf_9 <- function(df) {
  clogit(case ~ scale(emergent_wetland) + scale(canopy) + strata(strata), 
         data = df)
}
frsf_10 <- function(df) {
  clogit(case ~ scale(patch_size) + scale(canopy) + strata(strata), 
         data = df)
}
frsf_11 <- function(df) {
  clogit(case ~ scale(coi) + scale(canopy) + strata(strata), 
         data = df)
}
frsf_12 <- function(df) {
  clogit(case ~ scale(imp) + scale(canopy) + strata(strata), 
         data = df)
}
frsf_13 <- function(df) {
  clogit(case ~ scale(coi) + tod_ + strata(strata), 
         data = df)
}
frsf_14 <- function(df) {
  clogit(case ~ scale(coi) + scale(open) + strata(strata), 
         data = df)
}
frsf_hab <- function(df) {
  clogit(case ~ scale(patch_size) + scale(canopy) + scale(open) + 
           scale(emergent_wetland) + scale(slope) + strata(strata), 
         data = df)
}





fmfull <- gps %>%
  group_by(animal_id, species, season) %>%
  filter(!(animal_id == "C9" & season == "other"), 
         !(animal_id == "F6" & season == "mate"), 
         !(animal_id == "F7" & season == "mate"),
         !(animal_id == "F16" & season == "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_full)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy)) %>%
  mutate(fm = "full")


fmpartial <- gps %>%
  group_by(animal_id, species, season) %>%
  filter((animal_id == "C9" & season == "other") | 
         (animal_id == "F6" & season == "mate") |
         (animal_id == "F7" & season == "mate") |
         (animal_id == "F16" & season == "mate")) %>%
  nest() %>%
  mutate(model = map(data, frsf_partial)) %>%
  mutate(glance = map(model, broom::glance)) %>% 
  mutate(conf_tidy = map(model, broom::confint_tidy)) %>%
  mutate(tidy = map(model, broom::tidy)) %>%
  unnest(c(glance, conf_tidy, tidy)) %>%
  mutate(fm = "partial")


#fm1 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_1)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy())) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "imp")
#fm2 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_2)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "asp")
#fm3 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_3)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "open")
#fm4 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_4)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wood")
#fm5 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(!(animal_id == "C9" & season == "other"), 
#         !(animal_id == "F6" & season == "mate"), 
#         !(animal_id == "F7" & season == "mate")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_5)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wet")
#fm6 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_6)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "coi")
#fm7 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_7)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "patch")
#fm8 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_8)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wo_o")
#fm9 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(!(animal_id == "C9" & season != "mate"), 
#         !(animal_id == "F6" & season == "mate"), 
#         !(animal_id == "F7" & season == "mate")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_9)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wo_we")
#fm10 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_10)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wo_p")
#fm11 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_11)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wo_c")
#fm12 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_12)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "wo_i")
#fm13 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_13)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "c_t")
#fm14 <- gps %>%
#  group_by(animal_id, species, season) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_14)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "c_o")
#fmhab <- gps %>%
#  group_by(animal_id, species, season) %>%
#  filter(!(animal_id == "C9" & season == "other"), 
#         !(animal_id == "F6" & season == "mate"), 
#         !(animal_id == "F7" & season == "mate")) %>%
#  nest() %>%
#  mutate(model = map(data, frsf_hab)) %>%
#  mutate(glance = map(model, broom::glance)) %>%
#  mutate(tidy = map(model, broom::tidy)) %>%
#  unnest(c(glance, tidy)) %>%
#  mutate(fm = "hab")

#univariate <- fm1 %>%
#  bind_rows(fm2, fm3, fm4, fm5, fm6, fm7)

#models <- fm1 %>%
#  bind_rows(fm2, fm3, fm4, fm5, fm6, fm7, fm8, fm9, fm10, fm11, fm12, fm13, 
#            fm14, fmfull, fmhab)
#models$fm <- as.factor(models$fm)

#mods <- levels(models$fm)
#sps <- levels(as.factor(as.character(models$species)))

#aic_summary <- tibble()

#for (i in sps) {
#  for (j in mods) {
#    aic <- models %>%
#      select(species, fm, AIC) %>%
#      filter(species == i, fm == j)
#    ave_aic <- mean(aic$AIC)
#    aic_i <- tribble(
#      ~species, ~fm, ~AIC,
#      i, j, ave_aic
#    )
#    aic_summary <- aic_summary %>%
#      bind_rows(aic_i)
#  }
#}

#aic_summary_uni <- tibble()

#unis <- levels(as.factor(univariate$fm))
#for (i in sps) {
#  for (j in unis) {
#    aic <- models %>%
#      select(species, fm, AIC) %>%
#      filter(species == i, fm == j)
#    ave_aic <- mean(aic$AIC)
#    aic_i <- tribble(
#      ~species, ~fm, ~AIC,
#      i, j, ave_aic
#    )
#    aic_summary_uni <- aic_summary_uni %>%
#      bind_rows(aic_i)
#  }
#}

#aic_summary_uni[aic_summary_uni$AIC == 
#                min(aic_summary_uni[aic_summary_uni$species == "coyote" & 
#                                    aic_summary_uni$AIC != -Inf, ]$AIC), ]$fm
#aic_summary_uni[aic_summary_uni$AIC == 
#                min(aic_summary_uni[aic_summary_uni$species == "red fox" & 
#                                    aic_summary_uni$AIC != -Inf, ]$AIC), ]$fm
#aic_summary_uni[aic_summary_uni$AIC == 
#                min(aic_summary_uni[aic_summary_uni$species == "gray fox" & 
#                                    aic_summary_uni$AIC != -Inf, ]$AIC), ]$fm

models <- fmfull %>%
#  bind_rows(fmpartial) %>%
  dplyr::select(!c(data, model))


coef_summary_individual <- models %>% 
  dplyr::select(c(animal_id, species, conf.low, conf.high, term, estimate)) %>% 
  group_by(animal_id, term, species) %>% 
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate)) %>%
  left_join(imp_averages, by = "animal_id")
coef_summary_individual$term <- str_sub(coef_summary_individual$term, 7, 
                                        nchar(coef_summary_individual$term) - 1)

coef_summary_species <- coef_summary_individual %>%
  dplyr::select(c(species, term, conf.low, conf.high, estimate)) %>%
  group_by(species, term) %>%
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate))
coef_summary_species$term <- str_sub(coef_summary_species$term, 7, 
                                     nchar(coef_summary_species$term) - 1)



ggplot(gps[gps$species == "coyote", ], aes(y = ))


ggplot(coef_summary_species, aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  
  facet_grid(~species)





vars <- levels(as.factor(as.character(coef_summary_individual$term)))

for (i in vars) {
  plt <- ggplot(coef_summary_individual[coef_summary_individual$term == i, ], 
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








ggplot() +
  geom_bin2d(data = collars_true_steps, aes(x = t2_, y = active, color = reorder(id, -ave_pop))) +
  scale_color_brewer(palette = "PiYG")



ggplot() +
  geom_bin2d(data = collars_true_steps, aes(x = t2_, y = active, color = reorder(id, -ave_pop))) +
  scale_color_brewer(palette = "PiYG")






ggplot(collars_true_steps, aes(x = t2_, y = prop_act, color = id)) +
  geom_point() +
  facet_grid(reorder(id, -ave_pop) ~ tod_end_)












ggplot(fmfull, aes(colour = animal_id)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() +
  ggtitle("") +
  facet_grid(season ~ species)

coyote_models <- models %>%
  filter(species == "coyote")

ggplot(models, aes(x = fm, y = AIC, color = species)) +
  geom_boxplot()


ggplot(fm1, aes(colour = animal)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low,
                     ymax = conf.high),
                 lwd = 1, position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() +
  ggtitle("clogit(case_ ~ landcover + scale(perc_imp) + scale(density_m_2) + strata(step_id_))") +
  facet_grid(period ~ sp)

ggplot(collars_true_steps, aes(x = t2_, y = prop_act, color = id)) +
  geom_point() +
  facet_grid(reorder(id, -ave_pop) ~ tod_end_)

ggplot() +
  geom_bin2d(data = collars_true_steps, aes(x = t2_, y = active, color = reorder(id, -ave_pop))) +
  scale_color_brewer(palette = "PiYG")






# Read in rasters
coi <- rast("D:/data/resample/resample_30/coi_30.tif")
canopy <- rast("D:/data/resample/resample_30/can_30.tif")
buildings <- rast("D:/data/resample/resample_30/bldng_30.tif")
imp <- rast("D:/data/resample/resample_30/imp_30.tif") / 100 # Proportion
lot <- rast("D:/data/resample/resample_30/lot_30.tif") / 10000 # Hectares
open <- rast("D:/data/resample/resample_30/open_30.tif")
pop <- rast("D:/data/resample/resample_30/popden_30.tif")
residential <- rast("D:/data/resample/resample_30/res_30.tif")
slope <- rast("D:/data/resample/resample_30/slope_30.tif") / 100 # Proportion
veg <- rast("D:/data/resample/resample_30/veg_30.tif") / 10000 # Hectares
wetland <- rast("D:/data/resample/resample_30/wet_30.tif")


selection_score <- mean(fmfull[fmfull$species == "coyote" & 
                                 fmfull$season_pooled == "mate", ]$model$[[i]]coefficients[[1]])
  
  
  
  (mean(fmfull[fmfull$species == "coyote" & 
                           fmfull$season_pooled == "mate", ]$model) * coi) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * slope) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * imp) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * lot_size) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * residential) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * canopy) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * buildings) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * open) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * wetland) +
  (mean(fmfull[fmfull$species == "coyote" & 
                 fmfull$season_pooled == "mate", ]$model) * patch_size)