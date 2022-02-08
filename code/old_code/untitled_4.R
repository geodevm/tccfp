library(tidyverse)
library(amt)
library(sp)

is.data.frame(collars)

canid_trk <- make_track(collars, 
                        gps_utm_easting, 
                        gps_utm_northing, 
                        acquisition_time, 
                        id = animal_id,
                        species = species,
                        crs = sp::CRS("+init=epsg:32615"))

canid_trk <- canid_trk %>%
  time_of_day()

canid_nest <- canid_trk %>%
  nest(data = -"id")

canid_trk_10min <- canid_nest %>%
  mutate(steps = map(data, function(x)
    x %>%
      track_resample(rate = minutes(10), 
                     tolerance = minutes(5)) %>%
      steps_by_burst))

canid_trk_5.5hr <- canid_nest %>%
  mutate(steps = map(data, function(x)
    x %>%
      track_resample(rate = minutes(330),
                     tolerance = minutes(30)
                     filter_min_n_burst(min_n = 3)) %>%
      steps_by_burst))

canid_trk_11hr <- canid_nest %>%
  mutate(steps = map(data, function(x)
    x %>%
      track_resample(rate = hours(11), tolerance = minutes(30)) %>%
      steps_by_burst))

canid_trk_10min %>% dplyr::select(id, steps) %>% unnest(cols = "steps") %>% 
  ggplot(aes(sl_, fill = factor(id))) + geom_density(alpha = 0.4)








canid_trk <- canid_trk %>%
  mutate(sl_ = step_lengths(.))

summary(canid_trk$sl_)

summarize_sampling_rate(canid_trk)

canid_trk_10min <- canid_trk %>%
  track_resample(rate = minutes(10), tolerance = minutes(5))

canid_trk_5.5hr <- canid_trk %>%
  track_resample(rate = hours(6), tolerance = minutes(50))

canid_trk_11hr <- canid_trk %>%
  track_resample(rate = hours(11), tolerance = minutes(30))


