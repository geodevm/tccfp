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

#' Now load all packages.

#+warning=FALSE, message=FALSE
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

#' Record time for running all code
ptm<-proc.time()

#' ### Read in data from movebank
#' 
#' Create a login object for a user account at movebank.org
loginStored <- movebankLogin(username="MovebankWorkshop", password="genericuserforthisexercise")

#' Get overview information about a Movebank study. Be sure to check the citation and license terms if not using your own data.

getMovebankStudy(study="Martes pennanti LaPoint New York", login=loginStored) # see study-level info

#' You may receive a message that you do not have access to data in a study. If you are not a 
#' data manager for a study, the owner may require that you read and accept the license terms 
#' once before you may download it. Currently license terms cannot be accepted directly from 
#' the move package. To view and accept them, go to movebank.org, search for the study and begin a 
#' download, and view and accept the license terms. After that you will be able to load from R.

#' Load data from a study in Movebank and create a MoveStack object. For more details and options see https://cran.r-project.org/web/packages/move/index.html.
#' 
head(collars)

#' Note: If there are duplicate animal-timestamp records in Movebank, you will get a warning. 
#' You can exclude duplicate records on import using `removeDuplicatedTimestamps = TRUE`. If you are 
#' a data manager for a study in Movebank you can also filter them out directly in the study 
#' so they are excluded by default in downloads (see https://www.movebank.org/node/27252).

#' Create a data frame from the MoveStack object
canid.dat <- as(collars, "data.frame")

#' ### Data cleaning

#' Delete observations where missing lat or long or a timestamp.  There are no missing
#' observations in this data set, but it is still good practice to check.
ind <- complete.cases(canid.dat[, c("gps_longitude", "gps_latitude", "acquisition_time")])

#' The number of relocations with missing coordinates or timestamp (if any).
table(ind)
canid.dat <- canid.dat %>% filter(ind)

#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier). There are no duplicate
#' observations in this data set, but it is still good practice to check.
ind2 <- canid.dat %>% 
  select(acquisition_time, gps_longitude, gps_latitude, animal_id) %>%
  duplicated
sum(ind2) # no duplicates
canid.dat <- canid.dat %>% filter(!ind2)

#' Make timestamp a date/time variable
canid.dat <- canid.dat %>% mutate(acquisition_time = ymd_hms(acquisition_time, tz = "UTC"))

#' ### Move package
#' 
#' Look at functions in the move package.
plot(collars)
show(collars)
summary(collars)

#' ## Plots of the data
#' 
#' Note: there are lots of ways to plot data using R. I have included code that 
#' illustrates how to create a map using the `leaflet` package with data for a single individual.
#' This approach can become cumbersome and slow with too many observations.
#' 

#' Lets look at the data from F2. Note: When reading data from Movebank always be 
#' sure to refer to the animal-id and individual-local-identifer
#' and not the tag-id or tag-local-identifier in order to exclude pre- and 
#' post-deployment records and correctly separate out individual animals when the 
#' study includes redeployments.

red_foxF1 <- canid.dat %>% filter(animal_id == "F1")

#' ### leaflet

#' leaflet lets you interact / move the map around which is nice.
leaflet(red_foxF1) %>% addTiles()%>%
  addCircles(red_foxF1$gps_longitude, red_foxF1$gps_latitude)


#' ### Using `ggplot2` to plot the data
#' 
#' Use separate axes for each individual (add scales="free" to facet_wrap)
#+fig.height=12, fig.width=12
ggplot(canid.dat, 
       aes(x = gps_latitude, y = gps_longitude))+
  geom_point() +
  facet_wrap(~animal_id, scales = "free")

#' Now, all on 1 plot
#+fig.height=6, fig.width=12
ggplot(canid.dat, 
       aes(gps_longitude, gps_latitude, color = animal_id, 
           group = animal_id))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")

#' ## Creating a track in amt
#' 
#' Before we can use the `amt` package to calculate step lengths, turn angles, and bearings
#' for fisher data, we need to add a class (track) to the data. Then, we can summarize 
#' the data by individual, month, etc.
#' 
#' If we have a data set with  projected coordinates, we can use:
#' `trk.temp <- make_track(fisher.dat, .x=utm.easting, .y=utm.northing, .t=timestamp, id = individual_local.identifier)`
#'
#' Note: we did not need to explicitly specify x, y and t (but probably good to do so).
#' This would also have worked
#' `trk <- make_track(fisher.dat, utm.easting, utm.northing, timestamp, id = local_identifier).`
#'
#' We can also use lat, long, which will allow us to determine
#' time of day. 
trk <- make_track(canid.dat, .x = gps_longitude, .y = gps_latitude, 
                  .t = acquisition_time, id = animal_id, crs = CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs"))

#' Now it is easy to calculate day/night from the movement track
trk <- trk %>% time_of_day()

#' Now, we can transform the geographic coordinates to projected coordinates (we use NAD83).
#trk <- transform_coords(trk, CRS("+init=epsg:5070"))

#' ## Movement Characteristics
#' 
#' Functions in the amt package:
#' 
#' - dir_abs will calculate absolute angles for steps
#' - dir_rel will calculate turning angles (relative angles)
#' - step_lengths will calculate distances between points
#' - nsd = Net Squared Displacement (distance from first point)
#' 
#' Arguments direction_abs:
#' 
#' - full_circle will calculate between 0 and 2 $\pi$ (rather than $-\pi$ and $\pi$)
#' - zero gives the direction = 0 for absolute angle
#' 
#' Note:  we have to calculate these characteristics separately for each 
#' individual (to avoid calculating a distance between say the last observation
#' of the first individual and the first observation of the second individual).
#' 
#' To do this, we could loop through individuals, calculate these
#' characteristics for each individual, then rbind the data 
#' back together. Or, use nested data frames and the map function
#' in the `purrr` package to do this with very little code. 
#' 
#' To see how nesting works, we can create a nested object by individual
nesttrk <- trk %>% group_by(id) %>% # here we say which variable (column) contains
  # information about groups. Note, this could be more than one.
  nest()  # And now we nest the data frame, creating a list column.
nesttrk


#' Each row contains data from an individual. For example, we can access data
#' from the first individual using:
nesttrk$data[[12]][[4]]

#' We could calculate movement characteristics by individual using:
temp <- direction_rel(nesttrk$data[[1]])
head(temp)

#' or:
temp <- trk %>% filter(id=="F1") %>% direction_rel
head(temp)

#' Or, we can add a columns to each nested column of data using `purrr::map`
trk1 <- trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd),
         ) %>% unnest()
trk1
trk
#' Now, calculate month, year, hour, week of each observation and append these to the dataset.
#' Unlike the movement charactersitics, these calculations can be done all at once, 
#' since they do not utilize successive observations (like step lengths and turn angles do).
trk1 <- trk1 %>% 
  mutate(
    week = week(t_),
    month = month(t_, label=TRUE), 
    year = year(t_),
    hour = hour(t_)
  )


#' ## Some plots of movement characteristics

#' ### Absolute angles (for each movement) relative to North 
#' We could use a rose diagram (below) to depict the distribution of angles. 
#+fig.height=12, fig.width=12
ggplot(trk1, aes(x = dir_abs, y = ..density..)) + 
  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
  scale_x_continuous(limits = c(0, 2 * pi), 
                     breaks = c(0, pi/2, pi, 3 * pi/2), 
                     labels = c("0", "pi/2", "pi", "3pi/2")) +
  facet_wrap( ~ id)

#' ### Turning angles 
#' 
#' Note: a 0 indicates the animal continued to move in a straight line, a $\pi$ 
#' indicates the animal turned around (but note, resting + measurement error often can
#' make it look like the animal turned around).
#+fig.height=12, fig.width=12
ggplot(trk1, aes(x = dir_rel, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~id)

#' ### Net-squared displacement over time for each individual
#+fig.height=12, fig.width=12
ggplot(trk1, aes(x = t_, y = nsd_)) + geom_path()+
  facet_wrap(~id, scales = "free")


#' ## Explore movement characteristics by (day/night, hour, month)
#' 
#' ### step length distribution by day/night
#' 
#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(trk1, aes(x = tod_, y = log(sl))) + 
  geom_boxplot() + geom_smooth() + facet_wrap(~id)

#' ### Space use (MCP or KDE) by week, month, and year
#' 
#' Note:  this code will only work for your critters if you
#' have multiple observations for each combination of
#' (month, year). If you don't have many observations, you could
#' try:  `group_by(id, year)`.
#' 
mcps.year <- trk %>% 
  mutate(year = year(t_)) %>% 
  group_data(id, year) %>% nest() %>% 
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.95)) %>% hr_area)) %>% 
  select(id, year, month, week, mcparea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(mcps.week, aes(x = week, y = area, colour = factor(year))) + 
  geom_point() +
  geom_smooth() + facet_wrap(~id, scales="free")

#' Same for KDE
kde.week <- trk %>% 
  mutate(year = year(t_), month = month(t_), week = week(t_)) %>% 
  group_by(id, year, month, week) %>% nest() %>% 
  mutate(kdearea = map(data, ~ hr_kde(., levels=c(0.95)) %>% hr_area)) %>%
  select(id, year, month, week,  kdearea) %>% unnest()

#+fig.height=12, fig.width=12, warning=FALSE, message=FALSE
ggplot(kde.week, aes(x = week, y = kdearea, colour = factor(year))) + 
  geom_point() +
  geom_smooth() + facet_wrap(~id, scales = "free")

#' Write out files for later analyses
here()
write_rds(trk, path = here("data","trk.Rdata"))
write_rds(fisher.dat, path = here("data","fisher.Rdata"))


#' # Investigate step length and turn angle distributions further
#'
#' We will use only one animal to illustrate these approaches, but it could be
#' easily extended to multiple animals using the same strategies as above.
#'

#' ## Different habitats

dat <- trk %>% filter(id == "C1")

#' We first have to make sure that the sampling rate is constant. Otherwise step
#' length and turning angles can not be compared. We first can check what the
#' current sampling rate is:

summarize_sampling_rate(dat)

#' 10 minutes seems to be appropriate. We can resample our track to a 10 min
#' sampling rate with a tolerance of 1 minute.

dat <- track_resample(dat, rate = hours(11), tolerance = hours(3)) %>% 
  filter_min_n_burst(min_n = 1)

#' `dat` gained a new column: `burst_`. This column indicates for each point to
#' which burst it belongs. A burst is a sequence of relocations with equidstant
#' (in time) relocations.

steps <- dat %>% steps_by_burst() 
steps %>% ggplot(aes(sl_)) + geom_histogram()

#' Let us add to each step in which habitat it started. 

#' It might be good to group some of the categories. 
#' 1 = 21, 22, 23: Developed
#' 4 = 90: Wetland
data("amt_fisher_lu")
lu <- amt_fisher_lu %in% c(41:43, 90)
plot(lu)


steps %>% extract_covariates(lu, where = "start") %>% 
  ggplot(aes(sl_)) + geom_histogram() + facet_wrap(~ layer)


steps %>% extract_covariates(lu, where = "both") %>% 
  mutate(where = case_when(
    layer_start == 1 & layer_end == 1 ~ "in forest", 
    layer_start == 0 & layer_end == 0 ~ "in other", 
    layer_start != layer_end ~ "transistion")) %>% 
  ggplot(aes(sl_)) + geom_density() + 
  facet_wrap(~ where)

#' ## Look at step turning angles for long and short steps
#' 
#' We first need to split steps into long and short steps and therefore determine
#' a threshold. 

steps %>% ggplot(aes(sl_)) + geom_histogram() + 
  geom_vline(xintercept = 120, col = "red")

#' 120 m seems to be reasonabLe.

steps %>% 
  mutate(step_length = case_when(sl_ > 120 ~ "long", TRUE ~ "short")) %>% 
  ggplot(aes(x = ta_, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~ step_length)

#' We can see that longer steps tend to be more directed (with turn angles near 0), and that short steps often have turn angles near -$\pi$ or $\pi$. Some of these angles associated with small steps are likely due to measurement error. 
#' 

#' Here is another plot of turn angles for short and long steps.
steps %>% 
  mutate(step_length = case_when(sl_ > 120 ~ "long", TRUE ~ "short")) %>% 
  ggplot(aes(x = ta_, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~ step_length)

#' ## Document Footer	
#' 	
#' Session Information:	
#' 	
sessionInfo()	  
proc.time() - ptm

