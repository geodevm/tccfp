# COVID-19 "15 days to slow the spread" began 16 March 2020. I mark 00:00 UTC (19:00 CST) as the start of this.
# 27 March 2020 is stay at home order for Minnesota
# Load Packages
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
# DATA CLEANING ############################################################################
covid.collars <- collars[!(collars$animal_id == 'C1'),]
pre.covid.collars <- covid.collars[(covid.collars$acquisition_time <= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
covid.collars <- covid.collars[(covid.collars$acquisition_time >= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
#pre.covid.collars <- pre.covid.collars[(pre.covid.collars$acquisition_time >= as.POSIXct("2020-02-17 00:00:00", tz = 'UTC')),]
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
################################################################################################
pre.covid.collars <- pre.covid.collars %>% mutate(acquisition_time = ymd_hms(acquisition_time, tz = "UTC"))
covid.collars <- covid.collars %>% mutate(acquisition_time = ymd_hms(acquisition_time, tz = "UTC"))
################################################################################################
covid.trk <- make_track(covid.collars, .x = gps_utm_northing, .y = gps_utm_easting, 
                  .t = acquisition_time, id = animal_id, crs = CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs"))
pre.covid.trk <- make_track(pre.covid.collars, .x = gps_utm_northing, .y = gps_utm_easting, 
                                .t = acquisition_time, id = animal_id, crs = CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs"))
#################################################################################################
covid.trk <- covid.trk %>% time_of_day()
pre.covid.trk <- pre.covid.trk %>% time_of_day()
##################################################################################################
#covid.temp <- direction_rel(covid.nesttrk$data[[1]])
#pre.covid.temp <- direction_rel(pre.covid.nesttrk$data[[1]])
#covid.trk1 <- covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
#  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
#         dir_rel = map(data, direction_rel), 
#         sl = map(data, step_lengths),
#         nsd_=map(data, nsd),
#  ) %>% unnest()
#covid.trk1
#pre.covid.trk1 <- pre.covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
#  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
#         dir_rel = map(data, direction_rel), 
#         sl = map(data, step_lengths),
#         nsd_=map(data, nsd),
# ) %>% unnest()
#pre.covid.trk1
#
#ggplot(covid.trk1, aes(x = dir_abs, y = ..density..)) + 
#  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
#  coord_polar(start = 0) + theme_minimal() + 
#  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
#  scale_x_continuous(limits = c(0, 2 * pi), 
#                     breaks = c(0, pi/2, pi, 3 * pi/2), 
#                     labels = c("0", "pi/2", "pi", "3pi/2")) +
#  facet_wrap( ~ id)
#ggplot(pre.covid.trk1, aes(x = dir_abs, y = ..density..)) + 
#  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
#  coord_polar(start = 0) + theme_minimal() + 
#  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
#  scale_x_continuous(limits = c(0, 2 * pi), 
#                     breaks = c(0, pi/2, pi, 3 * pi/2), 
#                     labels = c("0", "pi/2", "pi", "3pi/2")) +
#  facet_wrap( ~ id)

#ggplot(covid.trk1, aes(x = dir_rel, y = ..density..)) + 
#  geom_histogram(breaks = seq(-pi, pi, length = 20))+
#  coord_polar(start = 0) + theme_minimal() +
#  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
#  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
#                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
#  facet_wrap(~id)
#ggplot(pre.covid.trk1, aes(x = dir_rel, y = ..density..)) + 
#  geom_histogram(breaks = seq(-pi, pi, length = 20))+
#  coord_polar(start = 0) + theme_minimal() +
#  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
#  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
#                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
#  facet_wrap(~id)

#ggplot(covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
#  facet_wrap(~id, scales = "free")
#ggplot(pre.covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
#  facet_wrap(~id, scales = "free")

#ggplot(covid.trk1, aes(x = tod_, y = log(sl))) + 
#  geom_boxplot() + geom_smooth() + facet_wrap(~id)
#ggplot(pre.covid.trk1, aes(x = tod_, y = log(sl))) + 
#  geom_boxplot() + geom_smooth() + facet_wrap(~id)

#covid.trk1 <- track_resample(covid.trk1, rate = min(10), tolerance = min(30)) %>% 
#  filter_min_n_burst(min_n = 1)

#dat <- track_resample(dat, rate = min(10), tolerance = min(30)) %>% 
#  filter_min_n_burst(min_n = 1)

#steps <- dat %>% steps_by_burst() 
#steps %>% ggplot(aes(sl_)) + geom_histogram()

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
##################################
c11.covid.dat.1 <- as.data.frame(c11.covid.dat)
c2.covid.dat.1 <- as.data.frame(c2.covid.dat)
c3.covid.dat.1 <- as.data.frame(c3.covid.dat)
c4.covid.dat.1 <- as.data.frame(c4.covid.dat)
c5.covid.dat.1 <- as.data.frame(c5.covid.dat)
c6.covid.dat.1 <- as.data.frame(c6.covid.dat)
c7.covid.dat.1 <- as.data.frame(c7.covid.dat)
f1.covid.dat.1 <- as.data.frame(f1.covid.dat)
f2.covid.dat.1 <- as.data.frame(f2.covid.dat)
f3.covid.dat.1 <- as.data.frame(f3.covid.dat)
f8.covid.dat.1 <- as.data.frame(f8.covid.dat)
##################################
c11.pre.covid.dat.1 <- as.data.frame(c11.pre.covid.dat)
c2.pre.covid.dat.1 <- as.data.frame(c2.pre.covid.dat)
c3.pre.covid.dat.1 <- as.data.frame(c3.pre.covid.dat)
c4.pre.covid.dat.1 <- as.data.frame(c4.pre.covid.dat)
c5.pre.covid.dat.1 <- as.data.frame(c5.pre.covid.dat)
c6.pre.covid.dat.1 <- as.data.frame(c6.pre.covid.dat)
c7.pre.covid.dat.1 <- as.data.frame(c7.pre.covid.dat)
f1.pre.covid.dat.1 <- as.data.frame(f1.pre.covid.dat)
f2.pre.covid.dat.1 <- as.data.frame(f2.pre.covid.dat)
f3.pre.covid.dat.1 <- as.data.frame(f3.pre.covid.dat)
f8.pre.covid.dat.1 <- as.data.frame(f8.pre.covid.dat)
###################################
c11.covid.dat.2 <- as.data.frame(track_resample(c11.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c2.covid.dat.2 <- as.data.frame(track_resample(c2.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c3.covid.dat.2 <- as.data.frame(track_resample(c3.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c4.covid.dat.2 <- as.data.frame(track_resample(c4.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c5.covid.dat.2 <- as.data.frame(track_resample(c5.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c6.covid.dat.2 <- as.data.frame(track_resample(c6.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c7.covid.dat.2 <- as.data.frame(track_resample(c7.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f1.covid.dat.2 <- as.data.frame(track_resample(f1.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f2.covid.dat.2 <- as.data.frame(track_resample(f2.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f3.covid.dat.2 <- as.data.frame(track_resample(f3.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f8.covid.dat.2 <- as.data.frame(track_resample(f8.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
######################################
c11.pre.covid.dat.2 <- as.data.frame(track_resample(c11.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c2.pre.covid.dat.2 <- as.data.frame(track_resample(c2.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c3.pre.covid.dat.2 <- as.data.frame(track_resample(c3.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c4.pre.covid.dat.2 <- as.data.frame(track_resample(c4.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c5.pre.covid.dat.2 <- as.data.frame(track_resample(c5.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c6.pre.covid.dat.2 <- as.data.frame(track_resample(c6.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
c7.pre.covid.dat.2 <- as.data.frame(track_resample(c7.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f1.pre.covid.dat.2 <- as.data.frame(track_resample(f1.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f2.pre.covid.dat.2 <- as.data.frame(track_resample(f2.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f3.pre.covid.dat.2 <- as.data.frame(track_resample(f3.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
f8.pre.covid.dat.2 <- as.data.frame(track_resample(f8.pre.covid.dat, rate = minutes(10), tolerance = minutes(3)) %>% 
  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
########################################
##################################
c11.covid.dat <- merge(c11.covid.dat.1, c11.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c2.covid.dat <- merge(c2.covid.dat.1, c2.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c3.covid.dat <- merge(c3.covid.dat.1, c3.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c4.covid.dat <- merge(c4.covid.dat.1, c4.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c5.covid.dat <- merge(c5.covid.dat.1, c5.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c6.covid.dat <- merge(c6.covid.dat.1, c6.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c7.covid.dat <- merge(c7.covid.dat.1, c7.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f1.covid.dat <- merge(f1.covid.dat.1, f1.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f2.covid.dat <- merge(f2.covid.dat.1, f2.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f3.covid.dat <- merge(f3.covid.dat.1, f3.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f8.covid.dat <- merge(f8.covid.dat.1, f8.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
##################################
c11.pre.covid.dat <- merge(c11.pre.covid.dat.1, c11.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c2.pre.covid.dat <- merge(c2.pre.covid.dat.1, c2.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c3.pre.covid.dat <- merge(c3.pre.covid.dat.1, c3.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c4.pre.covid.dat <- merge(c4.pre.covid.dat.1, c4.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c5.pre.covid.dat <- merge(c5.pre.covid.dat.1, c5.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c6.pre.covid.dat <- merge(c6.pre.covid.dat.1, c6.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
c7.pre.covid.dat <- merge(c7.pre.covid.dat.1, c7.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f1.pre.covid.dat <- merge(f1.pre.covid.dat.1, f1.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f2.pre.covid.dat <- merge(f2.pre.covid.dat.1, f2.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f3.pre.covid.dat <- merge(f3.pre.covid.dat.1, f3.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
f8.pre.covid.dat <- merge(f8.pre.covid.dat.1, f8.pre.covid.dat.2, by.x = "t_", by.y = "t1_", all.y=TRUE)
######################################
pre.covid.resample.c.l <- list(c11.pre.covid.dat,
                            c2.pre.covid.dat,
                            c3.pre.covid.dat,
                            c4.pre.covid.dat,
                            c5.pre.covid.dat,
                            c6.pre.covid.dat,
                            c7.pre.covid.dat)
c.l.pre.covid.dat <- do.call(rbind,pre.covid.resample.c.l)
#######################################
pre.covid.resample.v.v <- list(f1.pre.covid.dat,
                               f2.pre.covid.dat,
                               f3.pre.covid.dat,
                               f8.pre.covid.dat)
v.v.pre.covid.dat <- do.call(rbind,pre.covid.resample.v.v)
########################################
########################################
covid.resample.c.l <- list(c11.covid.dat,
                               c2.covid.dat,
                               c3.covid.dat,
                               c4.covid.dat,
                               c5.covid.dat,
                               c6.covid.dat,
                               c7.covid.dat)
c.l.covid.dat <- do.call(rbind,covid.resample.c.l)
#######################################
covid.resample.v.v <- list(f1.covid.dat,
                               f2.covid.dat,
                               f3.covid.dat,
                               f8.covid.dat)
v.v.covid.dat <- do.call(rbind,covid.resample.v.v)
#################################################################################
c.l.pre.covid.dat$t1_ <- c.l.pre.covid.dat$t_
v.v.pre.covid.dat$t1_ <- v.v.pre.covid.dat$t_
c.l.covid.dat$t1_ <- c.l.covid.dat$t_
v.v.covid.dat$t1_ <- v.v.covid.dat$t_
drop <- c("t_", "x_", "y_")
c.l.pre.covid.dat <- c.l.pre.covid.dat[, !(names(c.l.pre.covid.dat) %in% drop)]
v.v.pre.covid.dat <- v.v.pre.covid.dat[, !(names(v.v.pre.covid.dat) %in% drop)]
c.l.covid.dat <- c.l.covid.dat[, !(names(c.l.covid.dat) %in% drop)]
v.v.covid.dat <- v.v.covid.dat[, !(names(v.v.covid.dat) %in% drop)]
#################################################################################
c.l.pre.covid.dat$burst_id <- paste(c.l.pre.covid.dat$id,".",c.l.pre.covid.dat$burst_)
v.v.pre.covid.dat$burst_id <- paste(v.v.pre.covid.dat$id,".",v.v.pre.covid.dat$burst_)
c.l.covid.dat$burst_id <- paste(c.l.covid.dat$id,".",c.l.covid.dat$burst_)
v.v.covid.dat$burst_id <- paste(v.v.covid.dat$id,".",v.v.covid.dat$burst_)

for (i in 1:dim(c.l.pre.covid.dat)) {
  t0 <- min(c.l.pre.covid.dat$t1_[c.l.pre.covid.dat$burst_id == c.l.pre.covid.dat$burst_id[i]])
  c.l.pre.covid.dat$burst_nsd[i] <- sqrt((c.l.pre.covid.dat$y2_[i] -
                       c.l.pre.covid.dat$y1_[c.l.pre.covid.dat$burst_id == c.l.pre.covid.dat$burst_id[i] & 
                                               c.l.pre.covid.dat$t1_ == t0])^2 + 
                      (c.l.pre.covid.dat$x2_[i] -
                         c.l.pre.covid.dat$x1_[c.l.pre.covid.dat$burst_id == c.l.pre.covid.dat$burst_id[i] & 
                                                 c.l.pre.covid.dat$t1_ == t0])^2)
}
for (i in 1:dim(c.l.covid.dat)) {
  t0 <- min(c.l.covid.dat$t1_[c.l.covid.dat$burst_id == c.l.covid.dat$burst_id[i]])
  c.l.covid.dat$burst_nsd[i] <- sqrt((c.l.covid.dat$y2_[i] -
                                            c.l.covid.dat$y1_[c.l.covid.dat$burst_id == c.l.covid.dat$burst_id[i] & 
                                                                    c.l.covid.dat$t1_ == t0])^2 + 
                                           (c.l.covid.dat$x2_[i] -
                                              c.l.covid.dat$x1_[c.l.covid.dat$burst_id == c.l.covid.dat$burst_id[i] & 
                                                                      c.l.covid.dat$t1_ == t0])^2)
}
for (i in 1:dim(v.v.pre.covid.dat)) {
  t0 <- min(v.v.pre.covid.dat$t1_[v.v.pre.covid.dat$burst_id == v.v.pre.covid.dat$burst_id[i]])
  v.v.pre.covid.dat$burst_nsd[i] <- sqrt((v.v.pre.covid.dat$y2_[i] -
                                            v.v.pre.covid.dat$y1_[v.v.pre.covid.dat$burst_id == v.v.pre.covid.dat$burst_id[i] & 
                                                                    v.v.pre.covid.dat$t1_ == t0])^2 + 
                                           (v.v.pre.covid.dat$x2_[i] -
                                              v.v.pre.covid.dat$x1_[v.v.pre.covid.dat$burst_id == v.v.pre.covid.dat$burst_id[i] & 
                                                                      v.v.pre.covid.dat$t1_ == t0])^2)
}
for (i in 1:dim(v.v.covid.dat)) {
  t0 <- min(v.v.covid.dat$t1_[v.v.covid.dat$burst_id == v.v.covid.dat$burst_id[i]])
  v.v.covid.dat$burst_nsd[i] <- sqrt((v.v.covid.dat$y2_[i] -
                                            v.v.covid.dat$y1_[v.v.covid.dat$burst_id == v.v.covid.dat$burst_id[i] & 
                                                                    v.v.covid.dat$t1_ == t0])^2 + 
                                           (v.v.covid.dat$x2_[i] -
                                              v.v.covid.dat$x1_[v.v.covid.dat$burst_id == v.v.covid.dat$burst_id[i] & 
                                                                  v.v.covid.dat$t1_ == t0])^2)
}


burst.nsd <- function(bid) {
  for (i in 1:dim(bid)) {
    t0 <- min(bid$t1_[bid$burst_id == bid$burst_id[i]])
    bid$burst_nsd[i] <- sqrt((bid$y2_[i] -
                                              bid[bid$burst_id == bid$burst_id[i] & 
                                                                      bid$t1_ == t0])^2 + 
                                             (bid$x2_[i] -
                                                bid$x1_[bid$burst_id == bid$burst_id[i] & 
                                                                        bid$t1_ == t0])^2)
  }
}
burst.nsd(v.v.covid.dat)
