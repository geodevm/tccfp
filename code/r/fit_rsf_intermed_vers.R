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
gps <- read.csv(here("data/processed_data/rsf_2nd_order.csv"))
imp_averages <- read.csv(here("data/processed_data/imp_averages.csv")) %>%
  rename(imp = dem)
# 
gps <- gps %>%
  filter(season != "other_19")

ids <- levels(as.factor(as.character(gps$animal_id)))
season <- levels(as.factor(as.character(gps$season)))

for (i in 1:nrow(gps)) {
  gps$season_pooled[i] <-  strsplit(gps$season[i], "[_]")[[1]][1]
}

gps[gps$case == "case", ]$case <- 1
gps[gps$case == "control", ]$case <- 0
gps$case <- as.integer(gps$case)
gps$strata <- as.factor(gps$strata)
gps$animal_id <- as.factor(gps$animal_id)
gps$species <- as.factor(gps$species)
gps[is.na(gps$buildings), ]$buildings <- 0
gps[is.na(gps$wetland), ]$wetland <- 0
gps[is.na(gps$open), ]$open <- 0
gps[is.na(gps$canopy), ]$canopy <- 0
gps[is.na(gps$residential), ]$residential <- 0


frsf_full <- function(df) {
  glm(case ~ scale(imp) + scale(patch_size) + scale(canopy) + scale(open) + 
           scale(coi) + scale(wetland) + scale(slope) + scale(residential) +
           scale(buildings), 
      data = df,
      family = binomial)
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












fmfull <- gps[gps$analysis_type == "circle_1000" | 
                gps$analysis_type == "circle_circle_1000", ] %>%
  group_by(species, season_pooled) %>%
#  filter(!(animal_id == "C9" & season == "other"), 
#         !(animal_id == "F6" & season == "mate"), 
#         !(animal_id == "F7" & season == "mate"),
#         !(animal_id == "F16" & season == "mate")) %>%
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
  bind_rows(fmpartial) %>%
  select(!c(data, model))


coef_summary_individual <- fmfull %>% 
  dplyr::select(c(species, conf.low, conf.high, term, estimate)) %>% 
  group_by(term, species) %>% 
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate))
coef_summary_individual$term <- str_sub(coef_summary_individual$term, 7, 
                                        nchar(coef_summary_individual$term) - 1)

coef_summary_species <- coef_summary_individual %>%
  select(c(species, term, conf.low, conf.high, estimate)) %>%
  group_by(species, term) %>%
  summarize(conf.low = mean(conf.low), 
            conf.high = mean(conf.high), 
            estimate = mean(estimate))
coef_summary_species$term <- str_sub(coef_summary_species$term, 7, 
                                     nchar(coef_summary_species$term) - 1)



ggplot(coef_summary_individual[coef_summary_individual$species == "coyote", ],
                               aes(colour = species)) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  geom_linerange(aes(x = term, ymin = conf.low, ymax = conf.high), lwd = 1, 
                 position = position_dodge(width = 1/2)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  facet_grid(~species)


ggplot(gps[gps$species == "coyote", ], aes(case, imp)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, lot_size)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, coi)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, slope)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, residential)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, canopy)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, buildings)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, open)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, wetland)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "coyote", ], aes(case, patch_size)) +
  geom_bar(stat = "summary",
           fun = "mean")


ggplot(gps[gps$species == "red fox", ], aes(case, imp)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, lot_size)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, coi)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, slope)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, residential)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, canopy)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, buildings)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, open)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, wetland)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "red fox", ], aes(case, patch_size)) +
  geom_bar(stat = "summary",
           fun = "mean")


ggplot(gps[gps$species == "gray fox", ], aes(case, imp)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, lot_size)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, coi)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, slope)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, residential)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, canopy)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, buildings)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, open)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, wetland)) +
  geom_bar(stat = "summary",
           fun = "mean")
ggplot(gps[gps$species == "gray fox", ], aes(case, patch_size)) +
  geom_bar(stat = "summary",
           fun = "mean")







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