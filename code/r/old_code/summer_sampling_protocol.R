# Load Packages
library(RColorBrewer)
library(adehabitatHR)
library(raster)
library(tidyverse)
library(lubridate)
library(amt)
library(sf)
library(survival)
library(lme4)
library(lmerTest)
library(here)
library(rgdal)
library(geosphere)
library(leaflet)
# Import collars
# collars <- read.csv(here("data/latest_data.csv"))
# Import rasters in case needed
impervious <- raster(here("data/impervious/TCMA_Impervious_2000.tif"))
proj4string(impervious) <- CRS("+init=epsg:32615")
landcover <- raster(here("data/landcover/tcma_lc_finalv1.tif"))
proj4string(landcover) <- CRS("+init=epsg:32615")
# Import census shp in case needed
pop <- read_sf(here("data/population/joined_census_blocks_2010.shp"))
pop = st_set_crs(pop, 32615)
# Convert to population density in case needed
pop$density_m_2 <- as.numeric(pop$pop_total / st_area(pop$geometry))
pop <- pop %>% dplyr::select(density_m_2, geometry)
# Make a track using amt that will keep animal id, starting in the month of may and excluding foxes
collars <- collars[!(collars$acquisition_time <= ymd_hms("2021-05-01 00:00:00")), ] # Starting in May
collars_trk <- collars[collars$species == "coyote",] %>% # Subset to exclude foxes
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_time,
             id = animal_id,
             sp = species,
             crs = CRS("+init=epsg:32615")) %>%
  time_of_day()
plot(collars_trk)
# Subset to the 10 minute interval, with a 150 minute tolerance so we acquire the entire burst
collars_10min <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) 
    x %>%
      track_resample(rate = minutes(10), tolerance = minutes(150)) %>% # 150 min tolerance
      filter_min_n_burst(min_n = 3) %>% # At least 3 in a burst
      extract_covariates(impervious) %>%
      extract_covariates(landcover))) %>%
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(col = "resample")
# Create a unique identifier for each burst
collars_10min <- transform(collars_10min, burst_uid = interaction(id, burst_, sep = "_"))
# Create a spatial object from the track
sp_collars <- SpatialPointsDataFrame(collars_10min[c("x_", "y_")], collars_10min)
# Project to UTM zone 15 N
proj4string(sp_collars) <- CRS("+init=epsg:32615")
# Reproject to epsg:4326 for plotting
sp_collars <- spTransform(sp_collars, CRS("+init=epsg:4326"))
# Create bursts using hierarchical methods, capturing bursts within a 20m radius with 5 or more points
d=20 # The radius within which it will be counted as a cluster
sp_collars$burst_uid <- as.factor(as.character(sp_collars$burst_uid))
sp_collars$clust <- NA # Empty column for cluster UID
for (i in levels(sp_collars$burst_uid)) {
  mdist <- distm(sp_collars[sp_collars$burst_uid == i, ]) # Create a distance matrix
  hc <- hclust(as.dist(mdist), method="complete") # Hierarchical cluster
  sp_collars$clust[sp_collars$burst_uid == i] <- cutree(hc, h=d) # Cut decision tree at 20m
  for (j in unique(sp_collars$clust[sp_collars$burst_uid == i])) { # This loop prunes the clusters with less than 5 observations
    if (nrow(sp_collars[(sp_collars$burst_uid == i & sp_collars$clust == j), ]) < 5) {
      sp_collars <- sp_collars[!(sp_collars$burst_uid == i & sp_collars$clust == j), ]
    }
  }
}
# Visualize the clusters
leaflet(sp_collars) %>% addTiles()%>%
  addCircles()
# Transform back into UTM 15N for further analysis
sp_collars <- spTransform(sp_collars, CRS("+init=epsg:32615"))
# Create a list of burst UIDs to be used for iteration
id <- as.list(levels(as.factor(as.character(sp_collars$burst_uid))))
# Create a 100% MCP for every cluster
mcps <- list()
for (i in id) {
  i.mcp <- mcp(sp_collars[sp_collars$burst_uid == i, ], percent = 100, unin = "m", unout = "m2")
  mcps <- append(mcps, i.mcp)
}
# Add UID to the list
names(mcps) <- id
# Create a dataframe of the mcps
for (i in id) {
  if (names(mcps[i]) == names(mcps[1])) {
    mcp <- mcps[[i]]
  } else {
    mcp <- rbind(mcp, mcps[[i]])
  }
}
# Convert to an sf object
mcp <- st_as_sf(mcp)
# Add burst UID as a column
mcp$burst_id <- as.vector(id)
# Calculate the centroids and append UID
centroids <- st_centroid(mcp$geometry)
names(centroids) <- id
# Create the 100m and 50m buffers, with UID appended
big_buff <- st_buffer(centroids, dist = 100)
lil_buff <- st_buffer(centroids, dist = 50)
names(big_buff) <- id
names(lil_buff) <- id
# Create a list of the differences (leaving a "donut" from 50-100m)
full_buff <- list(NA)
j <- 1
for (i in id) {
  buff.i <- st_difference(big_buff[[i]], lil_buff[[i]])
  full_buff[[j]] <- st_sfc(st_polygon(buff.i))
  j <- j + 1
}
names(full_buff) <- id
# Create the random points within the "donut", randomly generated
rand_samps <- list(NA)
k <- 1
l <- 111
for (i in 1:length(full_buff)) {
  set.seed(l) # Set seed so repeatable
  pts <- st_sample(full_buff[[i]], size = 500, type = "random")
  pts <- st_as_sf(pts)
  j <- 1 # iterator start
  buffer_size <- 50 # minimal distance to be enforced (in units of your CRS)
  repeat({
    #  create buffer around i-th point
    buffer <- st_buffer(pts[j, ], buffer_size ) 
    
    offending <- pts %>%  # start with the intersection of master points... 
      st_intersects(buffer, sparse = F) # ... and the buffer, as a vector
    
    # i-th point is not really offending - it is the origin (not to be excluded)
    offending[j] <- FALSE
    
    # if there are any offending points left - re-assign the master points, 
    # with the offending ones excluded / this is the main pruning part :)
    pts <- pts[!offending, ] 
    
    if (j >= nrow(pts)) {
      # the end was reached; no more points to process
      break 
    } else {
      # rinse & repeat
      j <- j + 1 
    }
  })
  rand_samps[[k]] <- pts
  k <- k + 1
  l <- l + 111
}

names(rand_samps) <- id
# Combine into a table
for (i in id) {
  if (i == id[1]) {
    rand_sf <- st_as_sf(rand_samps[[i]])
    rand_sf$id <- NA
    rand_sf$id <- i
  } else {
    rand_2 <- st_as_sf(rand_samps[[i]])
    rand_2$id <- i
    rand_sf <- rbind(rand_sf, rand_2)
  }
}
# Create a sf object
rand_samps <- st_as_sf(rand_sf)
# Make sure it is projected
st_crs(rand_samps) <- CRS("+init=epsg:32615")
# Check to see if any of the points are intersecting a previously sampled mcp
nrow(st_intersection(rand_samps, mcp))
# Extract landcover to see if any of the sampling points are on pavement, in water, or in a building
rand_samps$offending <- raster::extract(landcover, rand_samps)
# Recode the column with 0 representing sampleable and 1 unsampleable
rand_samps <- rand_samps %>% mutate(offending =
                                        recode(offending,
                                               "1" = 0,
                                               "2" = 0,
                                               "3" = 1,
                                               "4" = 1,
                                               "5" = 1,
                                               "6" = 0,
                                               "7" = 0,
                                               "8" = 0,
                                               "9" = 0,
                                               "10" = 0,
                                               "11" = 1,
                                               "12" = 0))
# Remove offending observations
rand_samps <- rand_samps[!rand_samps$offending == 1, ]
# Make UID a factor
rand_samps$id <- as.factor(rand_samps$id)
# Subset the random samples randomly to create samples of 3
full_samples <- data.frame()
j = 111
for (i in levels(rand_samps$id)) {
  set.seed(j) # Set seed so repeatable
  subset <- sample_n(rand_samps[rand_samps$id == i, ], 2)
  full_samples <- rbind(full_samples, subset)
  j <- j + 111
}
# Create an sf object from these
full_samples <- st_as_sf(full_samples)
# Remove and add necessary columns
full_samples$column_label <- NULL
full_samples$offending <- NULL
full_samples$case <- "control"
# Create a table of the "true" cases and add necessary columns
for (i in id) {
  if (i == id[1]) {
    cent_sf <- st_as_sf(centroids[i])
  } else {
    cent_sf <- rbind(cent_sf, st_as_sf(centroids[i]))
  }
}
cent_sf$id <- id
cent_sf$case <- "true"
# Append both case and control observations
sampling_protocol <- rbind(full_samples, cent_sf)
# Transform to EPSG:4326
sampling_protocol <- sampling_protocol %>% 
  st_transform(crs = st_crs("EPSG:4326"))
# Filter out previously sampled or unsampleable bursts
sampling_protocol <- sampling_protcol[sampling_protcol$id != "C10_16" &
                                        sampling_protcol$id != "C10_20" &
                                        sampling_protcol$id != "C10_6" &
                                        sampling_protcol$id != "C10_9" &
                                        sampling_protcol$id != "C11_14" &
                                        sampling_protcol$id != "C11_15" &
                                        sampling_protcol$id != "C11_18" &
                                        sampling_protcol$id != "C11_9" &
                                        sampling_protcol$id != "C12_11" &
                                        sampling_protcol$id != "C12_7" &
                                        sampling_protcol$id != "C13_11" &
                                        sampling_protcol$id != "C13_18" &
                                        sampling_protcol$id != "C13_22" &
                                        sampling_protcol$id != "C13_7" &
                                        sampling_protcol$id != "C8_3" &
                                        sampling_protcol$id != "C9_10",]
# Visualize
leaflet(sampling_protocol) %>% addTiles()%>%
  addCircles()
# Print output for sampling
# Remove already sampled bursts
# Remove all the following IDs from the next sample:
# C10_6    C10_9    C11_9    C12_11    C12_7    C13_11    C13_7    C8_3    C9_10
sampling_protocol <- as.data.frame(sampling_protocol)
sampling_protocol <- sampling_protocol[sampling_protocol$id != "C10_16" &
                                        sampling_protocol$id != "C10_20" &
                                        sampling_protocol$id != "C10_27" &
                                        sampling_protocol$id != "C10_41" &
                                        sampling_protocol$id != "C10_46" &
                                        sampling_protocol$id != "C10_51" &
                                        sampling_protocol$id != "C10_6" &
                                        sampling_protocol$id != "C10_66" &
                                        sampling_protocol$id != "C10_9" &
                                        sampling_protocol$id != "C11_14" &
                                        sampling_protocol$id != "C11_15" &
                                        sampling_protocol$id != "C11_18" &
                                        sampling_protocol$id != "C11_29" &
                                        sampling_protocol$id != "C11_33" &
                                        sampling_protocol$id != "C11_34" &
                                        sampling_protocol$id != "C11_42" &
                                        sampling_protocol$id != "C11_45" &
                                        sampling_protocol$id != "C11_59" &
                                        sampling_protocol$id != "C11_9" &
                                        sampling_protocol$id != "C12_11" &
                                        sampling_protocol$id != "C12_17" &
                                        sampling_protocol$id != "C12_7" &
                                        sampling_protocol$id != "C13_11" &
                                        sampling_protocol$id != "C13_18" &
                                        sampling_protocol$id != "C13_22" &
                                        sampling_protocol$id != "C13_28" &
                                        sampling_protocol$id != "C13_29" &
                                        sampling_protocol$id != "C13_33" &
                                        sampling_protocol$id != "C13_39" &
                                        sampling_protocol$id != "C13_40" &
                                        sampling_protocol$id != "C13_44" &
                                        sampling_protocol$id != "C13_50" &
                                        sampling_protocol$id != "C13_54" &
                                        sampling_protocol$id != "C13_60" &
                                        sampling_protocol$id != "C13_7" &
                                        sampling_protocol$id != "C13_81" &
                                        sampling_protocol$id != "C13_82" &
                                        sampling_protocol$id != "C8_12" &
                                        sampling_protocol$id != "C8_17" &
                                        sampling_protocol$id != "C8_27" &
                                        sampling_protocol$id != "C8_3" &
                                        sampling_protocol$id != "C8_31" &
                                        sampling_protocol$id != "C8_37" &
                                        sampling_protocol$id != "C8_42" &
                                        sampling_protocol$id != "C8_52" &
                                        sampling_protocol$id != "C9_10" &
                                        sampling_protocol$id != "C9_26" &
                                        sampling_protocol$id != "C9_53" &
                                        sampling_protocol$id != "C9_64" &
                                        sampling_protocol$id != "C10_86" &
                                        sampling_protocol$id != "C11_69" &
                                        sampling_protocol$id != "C11_80" &
                                        sampling_protocol$id != "C11_89" &
                                        sampling_protocol$id != "C11_92" &
                                        sampling_protocol$id != "C12_53" &
                                        sampling_protocol$id != "C12_59" &
                                        sampling_protocol$id != "C12_60" &
                                        sampling_protocol$id != "C12_68" &
                                        sampling_protocol$id != "C12_78" &
                                        sampling_protocol$id != "C13_108" &
                                        sampling_protocol$id != "C13_113" &
                                        sampling_protocol$id != "C13_114" &
                                        sampling_protocol$id != "C13_86" &
                                        sampling_protocol$id != "C13_93" &
                                        sampling_protocol$id != "C13_97" &
                                        sampling_protocol$id != "C9_68" &
                                        sampling_protocol$id != "C9_89" &
                                        sampling_protocol$id != "C10_101" &
                                         sampling_protocol$id != "C10_111" &
                                         sampling_protocol$id != "C10_112" &
                                         sampling_protocol$id != "C10_115" &
                                         sampling_protocol$id != "C11_100" &
                                         sampling_protocol$id != "C11_106" &
                                         sampling_protocol$id != "C11_107" &
                                         sampling_protocol$id != "C11_110" &
                                         sampling_protocol$id != "C11_115" &
                                         sampling_protocol$id != "C12_82" &
                                         sampling_protocol$id != "C12_89" &
                                         sampling_protocol$id != "C12_92" &
                                         sampling_protocol$id != "C12_98" &
                                         sampling_protocol$id != "C13_118" &
                                         sampling_protocol$id != "C13_124" &
                                         sampling_protocol$id != "C13_125" &
                                         sampling_protocol$id != "C13_134" &
                                         sampling_protocol$id != "C13_135" &
                                         sampling_protocol$id != "C13_139" &
                                         sampling_protocol$id != "C9_96",]

as.data.frame(sampling_protocol)
