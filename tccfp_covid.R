# COVID-19 "15 days to slow the spread" began 16 March 2020. I mark 00:00 UTC (19:00 CST) as the start of this.
# 27 March 2020 is stay at home order for Minnesota
# Load Packages
#########################################
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
library(purrr)
options(width=165,digits.secs = 3)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = FALSE)
# DATA CLEANING ############################################################################
covid.collars <- collars[!(collars$animal_id == 'C1'),]
pre.covid.collars <- covid.collars[(covid.collars$acquisition_time <= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
covid.collars <- covid.collars[(covid.collars$acquisition_time >= as.POSIXct("2020-03-17 00:00:00", tz = 'UTC')),]
######## pre.covid.collars <- pre.covid.collars[(pre.covid.collars$acquisition_time >= as.POSIXct("2020-02-17 00:00:00", tz = 'UTC')),] #
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
##################################
c11.covid.dat <- covid.trk %>% filter(id == "C11")
c2.covid.dat <- covid.trk %>% filter(id == "C2")
c3.covid.dat <- covid.trk %>% filter(id == "C3")
c4.covid.dat <- covid.trk %>% filter(id == "C4")
c5.covid.dat <- covid.trk %>% filter(id == "C5")
c6.covid.dat <- covid.trk %>% filter(id == "C6")
c7.covid.dat <- covid.trk %>% filter(id == "C7")
c.l.covid.list <- list(c11.covid.dat,
                           c2.covid.dat,
                           c3.covid.dat,
                           c4.covid.dat,
                           c5.covid.dat,
                           c6.covid.dat,
                           c7.covid.dat)
###################################
f1.covid.dat <- covid.trk %>% filter(id == "F1")
f2.covid.dat <- covid.trk %>% filter(id == "F2")
f3.covid.dat <- covid.trk %>% filter(id == "F3")
f8.covid.dat <- covid.trk %>% filter(id == "F8")
pre.covid.resample.v.v <- list(f1.covid.dat,
                               f2.covid.dat,
                               f3.covid.dat,
                               f8.covid.dat)
##################################
c11.pre.covid.dat <- pre.covid.trk %>% filter(id == "C11")
c2.pre.covid.dat <- pre.covid.trk %>% filter(id == "C2")
c3.pre.covid.dat <- pre.covid.trk %>% filter(id == "C3")
c4.pre.covid.dat <- pre.covid.trk %>% filter(id == "C4")
c5.pre.covid.dat <- pre.covid.trk %>% filter(id == "C5")
c6.pre.covid.dat <- pre.covid.trk %>% filter(id == "C6")
c7.pre.covid.dat <- pre.covid.trk %>% filter(id == "C7")
c.l.pre.covid.list <- list(c11.pre.covid.dat,
                           c2.pre.covid.dat,
                           c3.pre.covid.dat,
                           c4.pre.covid.dat,
                           c5.pre.covid.dat,
                           c6.pre.covid.dat,
                           c7.pre.covid.dat)

#####################################
f1.pre.covid.dat <- pre.covid.trk %>% filter(id == "F1")
f2.pre.covid.dat <- pre.covid.trk %>% filter(id == "F2")
f3.pre.covid.dat <- pre.covid.trk %>% filter(id == "F3")
f8.pre.covid.dat <- pre.covid.trk %>% filter(id == "F8")
v.v.pre.covid.list <- list(f1.pre.covid.dat,
                               f2.pre.covid.dat,
                               f3.pre.covid.dat,
                               f8.pre.covid.dat)
##################################
make.d.f <- function (x) {
  x <- as.data.frame(x)
}
c.l.pre.covid.list.1 <- lapply(c.l.pre.covid.list, make.d.f)
v.v.pre.covid.list.1 <- lapply(v.v.pre.covid.list, make.d.f)
c.l.covid.list.list.1 <- lapply(v.v.pre.covid.list, make.d.f)
v.v.covid.list.1 <- lapply(v.v.pre.covid.list, make.d.f)
###################################
make.trk.d.f <- function (x) {
  as.data.frame(track_resample(x, rate = minutes(10), tolerance = minutes(3)) %>% 
                  filter_min_n_burst(min_n = 3) %>% steps_by_burst())
}
c.l.pre.covid.list.2 <- lapply(c.l.pre.covid.list, make.trk.d.f)
v.v.pre.covid.list.2 <- lapply(v.v.pre.covid.list, make.trk.d.f)
c.l.covid.list.list.2 <- lapply(v.v.pre.covid.list, make.trk.d.f)
v.v.covid.list.2 <- lapply(v.v.pre.covid.list, make.trk.d.f)
##################################
join <- function (z, r) {
  merge(z, r, by.x = "t_", by.y = "t1_", all.y=TRUE)
}
c.l.pre.covid.dat <- map2_df(c.l.pre.covid.list.1, c.l.pre.covid.list.2, inner_join, by = c("t_" = "t1_"))
v.v.pre.covid.dat <- map2_df(v.v.pre.covid.list.1, v.v.pre.covid.list.2, inner_join, by = c("t_" = "t1_"))
c.l.covid.dat <- map2_df(c.l.covid.list.1, c.l.covid.list.2, inner_join, by = c("t_" = "t1_"))
v.v.covid.dat <- map2_df(v.v.covid.list.1, v.v.covid.list.2, inner_join, by = c("t_" = "t1_"))
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
c.l.pre.covid.dat$burst_id <- paste(c.l.pre.covid.dat$id,".",c.l.pre.covid.dat$burst_, sep = "")
v.v.pre.covid.dat$burst_id <- paste(v.v.pre.covid.dat$id,".",v.v.pre.covid.dat$burst_, sep = "")
c.l.covid.dat$burst_id <- paste(c.l.covid.dat$id,".",c.l.covid.dat$burst_, sep = "")
v.v.covid.dat$burst_id <- paste(v.v.covid.dat$id,".",v.v.covid.dat$burst_, sep = "")
#################################################################################
# Couldn't get this to work as a function for some odd frustrating reason. Oh well
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
######################################
c.l.pre.covid.dat$burst_id <- paste(c.l.pre.covid.dat$burst_id,".p", sep = "")
v.v.pre.covid.dat$burst_id <- paste(v.v.pre.covid.dat$burst_id,".p", sep = "")
c.l.covid.dat$burst_id <- paste(c.l.covid.dat$burst_id,".c", sep = "")
v.v.covid.dat$burst_id <- paste(v.v.covid.dat$burst_id,".c", sep = "")
############################################################
full.dat <- list(c.l.pre.covid.dat,
                 v.v.pre.covid.dat,
                 c.l.covid.dat,
                 v.v.covid.dat)
full.dat <- do.call(rbind,full.dat)
setwd("C:/Users/Geoffrey/Desktop/")
write.csv(full.dat, "full.dat.10track.csv")
