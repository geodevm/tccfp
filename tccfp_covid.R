# COVID-19 "15 days to slow the spread" began 16 March 2020. I mark 00:00 UTC (19:00 CST) as the start of this.
# 27 March 2020 is stay at home order for Minnesota

packages_needed <- c("knitr", "lubridate", "maptools", "raster", "move", 
                     "amt",  "tibble", "leaflet", "dplyr", "readr", "ggplot2", 
                     "glmmTMB", "lme4", "tidyr", "purrr", "glue", "sf", 
                     "here", "moveVis", "GGally", "devtools", "TwoStepCLogit", 
                     "broom", "tictoc", "ezknitr", "moveVis", "maps", "rgeos", 
                     "maptools")
new_packages <- packages_needed[!(packages_needed %in% 
                                    installed.packages()[,"Package"])]
if(length(new_packages)) 
  install.packages(new_packages, repos = "https://cloud.r-project.org")

library(knitr)
library(lubridate)
library(maptools)
library(raster)
library(move)
library(amt) 
library(tibble)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(glmmTMB)
library(sf)
library(here)
options(width=165,digits.secs = 3)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = FALSE)

covid.collars <- collars[!(collars$animal_id == 'C1'),]
pre.covid.collars <- covid.collars[(covid.collars$acquisition_time <= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
covid.collars <- covid.collars[(covid.collars$acquisition_time >= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
pre.covid.collars <- pre.covid.collars[(pre.covid.collars$acquisition_time >= as.POSIXct("2020-02-17 00:00:00", tz = 'UTC')),]

ind1 <- complete.cases(pre.covid.collars[, c("gps_longitude", "gps_latitude", "acquisition_time")])
ind2 <- complete.cases(covid.collars[, c("gps_longitude", "gps_latitude", "acquisition_time")])

table(ind1)
pre.covid.collars <- pre.covid.collars %>% filter(ind1)
ind1. <- pre.covid.collars %>% 
  select(acquisition_time, gps_longitude, gps_latitude, animal_id) %>%
  duplicated
sum(ind1.)
pre.covid.collars <- pre.covid.collars %>% filter(!ind1.)

table(ind2)
covid.collars <- covid.collars %>% filter(ind2)
ind2. <- covid.collars %>% 
  select(acquisition_time, gps_longitude, gps_latitude, animal_id) %>%
  duplicated
sum(ind2.)
covid.collars <- covid.collars %>% filter(!ind2.)

pre.covid.collars <- pre.covid.collars %>% mutate(acquisition_time = ymd_hms(acquisition_time, tz = "UTC"))
covid.collars <- covid.collars %>% mutate(acquisition_time = ymd_hms(acquisition_time, tz = "UTC"))

leaflet(covid.collars) %>% addTiles()%>%
  addCircles(covid.collars$gps_longitude, covid.collars$gps_latitude)
leaflet(pre.covid.collars) %>% addTiles()%>%
  addCircles(pre.covid.collars$gps_longitude, pre.covid.collars$gps_latitude)

ggplot(covid.collars, 
       aes(x = gps_latitude, y = gps_longitude))+
  geom_point() +
  facet_wrap(~animal_id, scales = "free")
ggplot(pre.covid.collars, 
       aes(x = gps_latitude, y = gps_longitude))+
  geom_point() +
  facet_wrap(~animal_id, scales = "free")

ggplot(covid.collars, 
       aes(gps_longitude, gps_latitude, color = animal_id, 
           group = animal_id))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")
ggplot(pre.covid.collars, 
       aes(gps_longitude, gps_latitude, color = animal_id, 
           group = animal_id))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")

covid.trk <- make_track(covid.collars, .x = gps_longitude, .y = gps_latitude, 
                  .t = acquisition_time, id = animal_id, crs = CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs"))
pre.covid.trk <- make_track(pre.covid.collars, .x = gps_longitude, .y = gps_latitude, 
                                .t = acquisition_time, id = animal_id, crs = CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs"))

covid.trk <- covid.trk %>% time_of_day()
pre.covid.trk <- pre.covid.trk %>% time_of_day()

covid.nesttrk <- covid.trk %>% group_by(id) %>%
  nest()
covid.nesttrk
pre.covid.nesttrk <- pre.covid.trk %>% group_by(id) %>%
  nest()
pre.covid.nesttrk

covid.temp <- direction_rel(covid.nesttrk$data[[1]])
pre.covid.temp <- direction_rel(pre.covid.nesttrk$data[[1]])

covid.trk1 <- covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd),
  ) %>% unnest()
covid.trk1
pre.covid.trk1 <- pre.covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd),
  ) %>% unnest()
pre.covid.trk1

ggplot(covid.trk1, aes(x = dir_abs, y = ..density..)) + 
  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
  scale_x_continuous(limits = c(0, 2 * pi), 
                     breaks = c(0, pi/2, pi, 3 * pi/2), 
                     labels = c("0", "pi/2", "pi", "3pi/2")) +
  facet_wrap( ~ id)
ggplot(pre.covid.trk1, aes(x = dir_abs, y = ..density..)) + 
  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
  scale_x_continuous(limits = c(0, 2 * pi), 
                     breaks = c(0, pi/2, pi, 3 * pi/2), 
                     labels = c("0", "pi/2", "pi", "3pi/2")) +
  facet_wrap( ~ id)

ggplot(covid.trk1, aes(x = dir_rel, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~id)
ggplot(pre.covid.trk1, aes(x = dir_rel, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~id)

ggplot(covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
  facet_wrap(~id, scales = "free")
ggplot(pre.covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
  facet_wrap(~id, scales = "free")

ggplot(covid.trk1, aes(x = tod_, y = log(sl))) + 
  geom_boxplot() + geom_smooth() + facet_wrap(~id)
ggplot(pre.covid.trk1, aes(x = tod_, y = log(sl))) + 
  geom_boxplot() + geom_smooth() + facet_wrap(~id)

covid.trk1 <- track_resample(covid.trk1, rate = min(10), tolerance = min(30)) %>% 
  filter_min_n_burst(min_n = 1)

dat <- track_resample(dat, rate = min(10), tolerance = min(30)) %>% 
  filter_min_n_burst(min_n = 1)

steps <- dat %>% steps_by_burst() 
steps %>% ggplot(aes(sl_)) + geom_histogram()

##################################
c11.covid.dat <- covid.trk %>% filter(id == "C11")
c2.covid.dat <- covid.trk %>% filter(id == "C2")
c3.covid.dat <- covid.trk %>% filter(id == "C3")
c4.covid.dat <- covid.trk %>% filter(id == "C4")
c5.covid.dat <- covid.trk %>% filter(id == "C5")
c6.covid.dat <- covid.trk %>% filter(id == "C6")
c7.covid.dat <- covid.trk %>% filter(id == "C7")
f1.covid.dat <- covid.trk %>% filter(id == "F1")
f2.covid.dat <- covid.trk %>% filter(id == "F2")
f3.covid.dat <- covid.trk %>% filter(id == "F3")
f8.covid.dat <- covid.trk %>% filter(id == "F8")
##################################
c11.pre.covid.dat <- pre.covid.trk %>% filter(id == "C11")
c2.pre.covid.dat <- pre.covid.trk %>% filter(id == "C2")
c3.pre.covid.dat <- pre.covid.trk %>% filter(id == "C3")
c4.pre.covid.dat <- pre.covid.trk %>% filter(id == "C4")
c5.pre.covid.dat <- pre.covid.trk %>% filter(id == "C5")
c6.pre.covid.dat <- pre.covid.trk %>% filter(id == "C6")
c7.pre.covid.dat <- pre.covid.trk %>% filter(id == "C7")
f1.pre.covid.dat <- pre.covid.trk %>% filter(id == "F1")
f2.pre.covid.dat <- pre.covid.trk %>% filter(id == "F2")
f3.pre.covid.dat <- pre.covid.trk %>% filter(id == "F3")
f8.pre.covid.dat <- pre.covid.trk %>% filter(id == "F8")
###################################
c11.covid.dat <- track_resample(c11.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c2.covid.dat <- track_resample(c2.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c3.covid.dat <- track_resample(c3.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c4.covid.dat <- track_resample(c4.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c5.covid.dat <- track_resample(c5.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c6.covid.dat <- track_resample(c6.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c7.covid.dat <- track_resample(c7.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f1.covid.dat <- track_resample(f1.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f2.covid.dat <- track_resample(f2.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f3.covid.dat <- track_resample(f3.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f8.covid.dat <- track_resample(f8.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
###################################
c11.pre.covid.dat <- track_resample(c11.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c2.pre.covid.dat <- track_resample(c2.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c3.pre.covid.dat <- track_resample(c3.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c4.pre.covid.dat <- track_resample(c4.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c5.pre.covid.dat <- track_resample(c5.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c6.pre.covid.dat <- track_resample(c6.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
c7.pre.covid.dat <- track_resample(c7.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f1.pre.covid.dat <- track_resample(f1.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f2.pre.covid.dat <- track_resample(f2.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f3.pre.covid.dat <- track_resample(f3.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
f8.pre.covid.dat <- track_resample(f8.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3)
######################################
f8.pre.covid.dat <- f8.pre.covid.dat %>% extract_covariates(landuse)



