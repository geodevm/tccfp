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
# we're going to filter out C1 because is dead during time period in question
covid_collars <- collars[!(collars$animal_id == 'C1'),]
# For this analysis we will use the period after 00:01 on 2-17 for the start period to have equal intervals
covid_collars <- covid_collars[covid_collars$acquisition_time >= ymd_hms("2020-02-17 00:00:00"),]
covid_collars <- covid_collars[covid_collars$acquisition_time <= ymd_hms("2020-04-17 00:00:00"),]
# Import rasters
impervious <- raster("U:/research/tccfp/tccfp_gis_layers/imp_2000/TCMA_Impervious_2000.tif")
proj4string(impervious) <- CRS("+init=epsg:32615")
landcover <- raster("U:/research/tccfp/tccfp_gis_layers/tcma_lc_finalv1/tcma_lc_finalv1.tif")
proj4string(landcover) <- CRS("+init=epsg:32615")
# Import census shp
pop <- read_sf("U:/research/tccfp/tccfp_gis_layers/joined_pop_blocks2010/joined_census_blocks_2010.shp")
pop = st_set_crs(pop, 32615)
# Imort the time-series human activity data
human_activity <- read.csv("U:/research/tccfp/cuebiq_activity_data.csv")
human_activity$week <- mdy(human_activity$week)
human_activity$week <- as_datetime(human_activity$week)
plot(human_activity$percent ~ human_activity$week, type = "line")
# We just want the population from this
pop$density_m_2 <- as.numeric(pop$pop_total / st_area(pop$geometry))
pop <- pop %>% select(density_m_2, geometry)
# Make a track using amt that will keep animal id and species, adding time of day
covid_collars <- covid_collars %>%
  make_track(.x = gps_utm_easting,
             .y = gps_utm_northing,
             .t = acquisition_time,
             id = animal_id,
             sp = species,
             crs = CRS("+init=epsg:32615")) %>%
  time_of_day()
plot(covid_collars)
# Create a subset of this to be used for RSFs. I am going to assume that resampling to include 5.5 and 11 hour intervals will be sufficient
collars_rsf <- covid_collars %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) x %>%
                          track_resample(rate = hours(11), tolerance = hours(6)) %>%
                          extract_covariates(impervious) %>%
                          extract_covariates(landcover))) %>%
  dplyr::select(id, resample) %>%
  unnest(cols = "resample")
collars_rsf <- collars_rsf %>% mutate(tcma_lc_finalv1 =
                                        recode(tcma_lc_finalv1,
                                               "1"="Natural",
                                               "2"="Human-use",
                                               "3"="Human-use",
                                               "4"="Human-use",
                                               "5"="Natural",
                                               "6"="Natural",
                                               "7"="Natural",
                                               "8"="Human-use",
                                               "9"="Natural",
                                               "10"="Natural",
                                               "11"="Natural",
                                               "12"="Human-use"))
# Filter by species
coyote_collars  <- covid_collars %>%
  filter(substring(id, 1, 1) == "C")
fox_collars  <- covid_collars %>%
  filter(substring(id, 1, 1) == "F")
# Nest coyotes for analysis
coyote_collars_ssf <- coyote_collars %>%
  nest(data = -"id")
# Plot the coyote step length distributions (try to find a good lambda for exponential, gamma will not fit)
coyote_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.01)))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  ggplot(aes(sl_, fill = factor(case_))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ id)
# Plot the coyote turn angle distributions
coyote_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.01)))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  ggplot(aes(ta_, fill = factor(case_))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ id)
# Make random steps for coyote
coyote_collars_ssf <- coyote_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.01)) %>%
                       time_of_day() %>%
                       extract_covariates(impervious) %>%
                       extract_covariates(landcover))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  mutate(sp = "coyote")
# Red foxes
fox_collars_ssf <- fox_collars %>% nest(data = -"id")
# Plot the coyote step length distributions (try to find a good lambda for exponential, gamma will not fit)
fox_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.005)))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  ggplot(aes(sl_, fill = factor(case_))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ id)
# Plot the red turn angles distributions
fox_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.005)))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  ggplot(aes(ta_, fill = factor(case_))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ id)
# Make random steps for red fox
fox_collars_ssf <- fox_collars_ssf %>%
  mutate(steps = map(data, function(x) x %>%
                       track_resample(rate = minutes(10), tolerance = minutes(5)) %>%
                       steps_by_burst() %>%
                       random_steps(sl_distr = make_exp_distr(rate = 0.005)) %>%
                       time_of_day() %>%
                       extract_covariates(impervious) %>%
                       extract_covariates(landcover))) %>%
  dplyr::select(id, steps) %>%
  unnest(cols = "steps") %>%
  mutate(sp = "red fox")
# rbind to get them back together
collars_ssf <- rbind(coyote_collars_ssf, fox_collars_ssf)
# Extract census 2010 population
collars_ssf <- st_as_sf(collars_ssf, coords = c("x2_", "y2_"), crs = 32615, remove = FALSE)
collars_rsf <- st_as_sf(collars_rsf, coords = c("x_", "y_"), crs = 32615, remove = FALSE)
collars_ssf <- st_join(collars_ssf, pop["density_m_2"])
collars_rsf <- st_join(collars_rsf, pop["density_m_2"])
collars_ssf <- collars_ssf %>% mutate(tcma_lc_finalv1 =
                                        recode(tcma_lc_finalv1,
                                               "1"="Natural",
                                               "2"="Non-natural",
                                               "3"="Non-natural",
                                               "4"="Non-natural",
                                               "5"="Natural",
                                               "6"="Natural",
                                               "7"="Natural",
                                               "8"="Non-natural",
                                               "9"="Natural",
                                               "10"="Natural",
                                               "11"="Natural",
                                               "12"="Non-natural"))
collars_ssf <- collars_ssf %>% tibble()
collars_ssf<- collars_ssf %>%
  rename(perc_imp = TCMA_Impervious_2000, landcover = tcma_lc_finalv1)
collars_rsf<- collars_rsf %>%
  rename(perc_imp = TCMA_Impervious_2000, landcover = tcma_lc_finalv1)
collars_ssf$landcover <- factor(collars_ssf$landcover)
collars_rsf$landcover <- factor(collars_rsf$landcover)
collars_rsf <- collars_rsf %>% tibble()
collars_ssf$scale_imp <- scale(collars_ssf$perc_imp)
collars_ssf$scale_pop <- scale(collars_ssf$density_m_2)
urb_ave <- collars_ssf %>%
  group_by(id) %>%
  summarize(
    ave.pop = mean((scale_pop + scale_imp), na.rm = T))
collars_ssf$id <- factor(collars_ssf$id)
urb_ave$id <- factor(urb_ave$id)
for (i in levels(collars_ssf$id)) {
  collars_ssf$ave_pop[collars_ssf$id == i] <- urb_ave$ave.pop[urb_ave$id == i]
}
# Sort pre vs. post covid restrictions
#collars_ssf$period[collars_ssf$t1_ <= ymd_hms("2020-03-17 00:00:00")] <- "pre-Covid"
#collars_ssf$period[collars_ssf$t1_ > ymd_hms("2020-03-17 00:00:00")] <- "Covid"
#collars_rsf$period[collars_rsf$t_ <= ymd_hms("2020-03-17 00:00:00")] <- "pre-Covid"
#collars_rsf$period[collars_rsf$t_ > ymd_hms("2020-03-17 00:00:00")] <- "Covid"
# Make a week column
# <- collars_ssf %>% mutate(week = (year(t1_) - year(min(t1_)))*52 +
#                                        week(t1_) - week(min(t1_)),
#                                     week2 = (as.numeric(t1_) %/% 7) - (as.numeric(min(t1_)) %/% 7)) %>%
#  arrange(t1_)
# Filter out the true steps for further analysis of activity
#collars_true_steps <- collars_ssf %>%
#  filter(case_ == TRUE)
# Make an activity column column for the true steps (if a step is greater than 20 meters, is considered active)
#collars_true_steps$active[collars_true_steps$sl_ >= 20] <- 1
#collars_true_steps$active[collars_true_steps$sl_ < 20] <- 0
# Find the activity level within burst by taking the average of the active
#coyote_true_steps  <- collars_true_steps %>%
#  filter(sp == "coyote")
#fox_true_steps  <- collars_true_steps %>%
#  filter(sp == "red fox")
#
#coyote_ave_active <- coyote_true_steps %>%
#  group_by(burst_) %>%
#  summarize(
#    prop_act = mean(active, na.rm = T))
#fox_ave_active <- fox_true_steps %>%
#  group_by(burst_) %>%
#  summarize(
#    prop_act = mean(active, na.rm = T))
#
coyote_true_steps$burst_ <- factor(coyote_true_steps$burst_)
coyote_ave_active$burst_ <- factor(coyote_ave_active$burst_)
for (i in levels(coyote_true_steps$burst_)) {
  coyote_true_steps$prop_act[coyote_true_steps$burst_ == i] <- coyote_ave_active$prop_act[coyote_ave_active$burst_ == i]
}
fox_true_steps$burst_ <- factor(fox_true_steps$burst_)
fox_ave_active$burst_ <- factor(fox_ave_active$burst_)
for (i in levels(fox_true_steps$burst_)) {
  fox_true_steps$prop_act[fox_true_steps$burst_ == i] <- fox_ave_active$prop_act[fox_ave_active$burst_ == i]
}
# rbind back together
collars_true_steps <- rbind(fox_true_steps, coyote_true_steps)
# Assign bursts as active or not
collars_true_steps$burst_act[collars_true_steps$prop_act >= 0.25] <- "active"
collars_true_steps$burst_act[collars_true_steps$prop_act <= 0.25] <- "inactive"
# Sort pre vs. post covid restrictions
collars_true_steps$period[collars_true_steps$t1_ <= ymd_hms("2020-03-17 00:00:00")] <- "pre-Covid"
collars_true_steps$period[collars_true_steps$t1_ > ymd_hms("2020-03-17 00:00:00")] <- "Covid"
# Pre covid ssfs
fssf_1 <- function(df) {
  clogit(case_ ~ scale(perc_imp) + landcover + scale(density_m_2) + strata(step_id_), data = df)
}
fm1 <- collars_ssf %>%
  group_by(period, id, sp, ave_pop) %>%
  nest() %>%
  mutate(model = map(data, fssf_1)) %>%
  mutate(glance = map(model, broom::tidy)) %>%
  unnest(glance)

ggplot(fm1, aes(colour = reorder(id, -ave_pop))) +
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
  scale_color_brewer(palette = "PiYG") #+
#  geom_line(data = human_activity, aes(x = week, y = percent))
ggplot() +
  geom_smooth(data = collars_true_steps, aes(x = t2_, y = sl_, color = reorder(id, -ave_pop))) +
  scale_color_brewer(palette = "PiYG")


plot(collars_true_steps$prop_act ~ collars_true_steps$t2_)


ggplot(human_activity, aes(x = week + hours(84), y = percent, ymin = -35, ymax = 35)) +
  geom_ribbon(aes(ymin=0, ymax = percent), fill = "Yellow", alpha = 0.5) +
  geom_point() +
  geom_label(aes(label = paste0(percent, "%")), nudge_y = 5) +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_dark()

weeks <- collars_true_steps %>%
  group_by(week, sp) %>%
  x  summarise(activity = mean(active))

ggplot(weeks, aes(x = week, y = activity, color = sp)) +
  geom_line()

ggplot(collars_true_steps, aes(week, active)) +
  geom_boxplot()


fm2 <- lm(active ~ hum_act, data = collars_true_steps)
summary(fm2)

`fm3 <- lmer(log(sl_ + 1) ~ period + tod_end_ + scale_pop + (1|id/sp) + (1|t2_), data = collars_true_steps)
summary(fm3)
plot(fm3)

plot(collars_true_steps$prop_act ~ collars_true_steps$period)

dplot(collars_true_steps$t2_, collars_true_steps$active)


collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-02-16 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-02-23 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-02-17")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-02-23 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-01 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-03-01")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-03-01 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-08 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-03-09")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-03-08 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-15 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-03-16")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-03-15 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-22 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-03-23")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-03-22 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-29 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-03-30")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-03-29 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-04-05 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-04-06")]
collars_true_steps$hum_act[collars_true_steps$t1_ >= ymd_hms("2020-04-05 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-04-12 19:00:00")] <- human_activity$percent[human_activity$week == ymd("2020-04-13")]

collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-02-16 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-02-23 19:00:00")] <- ymd("2020-02-17")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-02-23 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-01 19:00:00")] <- ymd("2020-03-01")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-03-01 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-08 19:00:00")] <- ymd("2020-03-09")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-03-08 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-15 19:00:00")] <- ymd("2020-03-16")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-03-15 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-22 19:00:00")] <- ymd("2020-03-23")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-03-22 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-03-29 19:00:00")] <- ymd("2020-03-30")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-03-29 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-04-05 19:00:00")] <- ymd("2020-04-06")
collars_true_steps$week[collars_true_steps$t1_ >= ymd_hms("2020-04-05 19:00:00") & collars_true_steps$t1_ < ymd_hms("2020-04-12 19:00:00")] <- ymd("2020-04-13")


plot(collars_true_steps$active ~ collars_true_steps$hum_act)

fm2 <- lmer(log(sl_+1) ~ hum_act + tod_end_ + (1|id), data = collars_true_steps)
summary(fm2)
plot(fm2)

#####################################################################################

p1 <- ggplot(both.activity[substr(both.activity$identy, 1, 1) == "F",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Vulpes vulpes total")

p2 <- ggplot(day.activity[substr(day.activity$identy, 1, 1) == "F",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Vulpes vulpes day")

p3 <- ggplot(night.activity[substr(night.activity$identy, 1, 1) == "F",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Vulpes vulpes night")

p4 <- ggplot(both.activity[substr(both.activity$identy, 1, 1) == "C",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Canis latrans total")

p5 <- ggplot(day.activity[substr(day.activity$identy, 1, 1) == "C",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Canis latrans day")

p6 <- ggplot(night.activity[substr(night.activity$identy, 1, 1) == "C",], aes(fill = period, y = prop.actv, x = reorder(identy, urb))) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=paste(actv, tot, sep = "/")), position=position_dodge(width=0.9), vjust=-0.25) +
  ggtitle("Canis latrans night")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)