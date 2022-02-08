# Here are the packages I would used to map home ranges.
library(adehabitatHR)
library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(lubridate)

# First thing we'll do is run some commands just to get a look at the data. I'm going to run through examples of 
# different types of home range mapping and easy commands for generating them. I'll be using the entire data set
# for this, not specific animals.

# First, summarize the data as per usual.
summary(collars)

# Convert the longitude and latitude columns to spatial points (think of as x = longitude, y = latitude for plotting)
sp_collars <- SpatialPoints(collars[c("gps_longitude", "gps_latitude")])

# Plot these spatial points to get a look
plot(sp_collars)

# And summarize these spatial points to see if anything is missing.
summary(sp_collars)
# Note that there is no "proj4string", and "Is projected" is NA. THIS IS A PROBLEM! These for sure need to be projected
# once you start working with other layers. From the dataset you can see that the gps_utm_zone is 15T. This is within
# zone 15N, so Google UTM zone 15N to get to spatialreference.org, which will give you a Proj4 link to click which
# will give what I put in the CRS command below.

# Here is where we project the spatial points.
proj4string(sp_collars) <- CRS("+init=epsg:32615")
# If you wanted to transform this data to be in another projection, use the command below filling in the projection
#sp_collars <- spTransform(sp_collars, CRS(""))

# The most common way of mapping home ranges is through MCPs. Typically, a 95% Minimum Convex Polygon is used to map
# a home range, while something like a 50% MCP is used to map a "core area".
#collars_mcp95 <- mcp(sp_collars, percent = 95, unin = "m", unout = "km2")

# This shows what this is actually doing, with 95% of the points being within the area with the most points, excluding
# the animal that had dispersed
#plot(sp_collars)
#plot(collars_mcp95, add = TRUE)
#axis(1, tcl = 0.3)
#axis(2, las = 1, tcl = 0.3)
#axis(3, labels = NA, tcl = 0.3)
#axis(4, labels = NA, tcl = 0.3)
#box()

# Here's a 50% MCP
#collars_mcp50 <- mcp(sp_collars, percent = 50, unin = "m", unout = "km2")

#And another visualization
#plot(sp_collars)
#plot(collars_mcp50, add = TRUE)
#axis(1, tcl = 0.3)
#axis(2, las = 1, tcl = 0.3)
#axis(3, labels = NA, tcl = 0.3)
#axis(4, labels = NA, tcl = 0.3)
#box()

# Kernel Density Estimates
# Here's what we're actually going to do. Kernels estimate density rather than doing something like a convex
# polygon, which really tells you nothing about use within that polygon, or if there are patterns at the edge of 
# the polygon which are not picked up because of the "convex" property of MCPs.

# Here's what a KDE really does.
#collars_kde <- kernelUD(sp_collars, h = "href")
#image(collars_kde)
#collars_rast <- raster(as(collars_kde, "SpatialPixelsDataFrame"))
#plot(collars_rast)
#plot(sp_collars, add = T)

# Export as a raster if you want
# writeRaster(collars.all.rast, "________kde.tif")

#Here is where we actually do the 95% KDE
#collars_kde95 <- getverticeshr(collars_kde, percent = 95, unin = "m", unout = "km2")
#plot(sp_collars)
#plot(collars_kde95, add = TRUE)
#axis(1, tcl = 0.3)
#axis(2, las = 1, tcl = 0.3)
#axis(3, labels = NA, tcl = 0.3)
#axis(4, labels = NA, tcl = 0.3)
#box()

# In this case the KDE looks pretty gnarly because of the dispersed coyoted and because this is a conglomeration
# really densely distributed home ranges relative to the overall scale. So in this case (mapping all) the KDE was
# not a very representative choice. However, it will likely be pretty good as a core areas estimator.
#Here is where we actually do the 95% KDE
#collars.all.kde95 <- getverticeshr(collars.all.kde, percent = 50, unin = "m", unout = "km2")

# and visualize
#plot(collars.all)
#plot(collars.all.kde95, add = TRUE)
#axis(1, tcl = 0.3)
#axis(2, las = 1, tcl = 0.3)
#axis(3, labels = NA, tcl = 0.3)
#axis(4, labels = NA, tcl = 0.3)
#box()

# Not very easily visualized with this plot, but you get the gist. 50% lie within these estimates and it is based
# on density rather than distance.

# To export as shapefiles - Make sure to set the working directory beforehand or it will save to where you have the
# data right now and will cause mayhem.
# setwd()
# writeOGR(F1.kde95, dsn = ".", layer = "F1.kde95", driver="ESRI Shapefile")

# So far I've done these examples with the entire dataset. To do this on an individual basis in an efficient way
# we'll have to get a little creative (aka write a for loop). I have a good idea of something that might work, but
# I'll let you have the fun. If you get stuck, let me know.

#### I'll go over some of the components of what you'll need for the loop.

# One thing you'll definitely need in this loop is accessing individuals. For that I would recommend this.
#F1.sp <- collars[collars$animal_id == "F1",]
#F1.sp <- SpatialPoints(F1.sp[c("gps_longitude", "gps_latitude")])

# From there you can just do what we did with 95% KDEs. For example, I'll go through what I would do for F1,
# commenting next to everything I wouldn't do during a for loop.
#F1.kde <- kernelUD(F1.sp, h = "href") 

#image(F1.kde) # Nope
#F1.rast <- raster(as(F1.kde, "SpatialPixelsDataFrame")) # Nope
#plot(F1.rast) # Nope
#plot(F1.sp, add = T) # Nope
#writeRaster(F1.rast, "F1.kde.tif") # Nope

#F1.kde95 <- getverticeshr(F1.kde, percent = 95, unin = "m", unout = "km2")

#plot(F1.rast) # Nope
#plot(F1.sp, add = T) # Nope
#plot(F1.kde95, add = T) # Nope
# As I comment also above, make sure your directory is set somewhere besides where you are storing the data before
# running this or else you'll run into problems.

#writeOGR(F1.kde95, dsn = ".", layer = "F1.kde95", driver="ESRI Shapefile")

# Overdispersed individuals without home ranges.
collars_kde <- collars[collars$animal_id != "F14", ]
collars_kde <- collars_kde[collars_kde$animal_id != "F4", ]
collars_kde <- collars_kde[!(collars_kde$animal_id == "C1" & collars_kde$acquisition_time > ymd_hms("2020-01-16 00:00:00")), ]


id <- as.list(levels(as.factor(collars_kde$animal_id)))
kdes_95 <- list()
for (i in id) {
  i.a <- collars_kde[collars_kde$animal_id == i,]
  i.sp <- SpatialPoints(i.a[c("gps_utm_easting", "gps_utm_northing")])
  proj4string(i.sp) <- CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs")
  i.kde <- kernelUD(i.sp, h = "href") 
  i.kde95 <- getverticeshr(i.kde, percent = 95, unin = "m", unout = "km2")
  #writeOGR(i.kde95, dsn = ".", layer = paste(i, ".kde95", sep = ""), driver="ESRI Shapefile")
  kdes_95 <- append(kdes_95, i.kde95)
  names(kdes_95) <- i
}

#mcps_95 <- list()

#for (i in id) {
#  i.a <- collars_kde[collars_kde$animal_id == i,]
#  i.sp <- SpatialPoints(i.a[c("gps_utm_easting", "gps_utm_northing")])
#  proj4string(i.sp) <- CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs")
#  i.mcp95 <- mcp(i.sp, percent = 95, unin = "m", unout = "km2")
#  mcps_95 <- append(mcps_95, i.mcp95)
#  names(mcps_95) <- i
#}

#names(mcps_95) <- id

names(kdes_95) <- id

for (i in id) {
  if (names(kdes_95[i]) == names(kdes_95[1])) {
    kde_95 <- kdes_95[[i]]
  } else {
    kde_95 <- rbind(kde_95, kdes_95[[i]])
  }
}

plot(kde_95)

kde_95 <- st_as_sf(kde_95)

kde_95$animal_id <- as.vector(id)


kde_buffs <- st_buffer(kde_95, dist = 1)
kde_buffs$animal_id <- as.character(kde_buffs$animal_id)

for (i in 1:nrow(kde_buffs)) {
  if (substr(kde_buffs$animal_id[i], 1, 1) == "C") {
    kde_buffs$species[i] <- "coyote"
  } else if (kde_buffs$animal_id[i] == "F10") {
    kde_buffs$species[i] <- "gray fox"
  } else {
    kde_buffs$species[i] <- "red fox"
  }
}

plot(kde_buffs)

for (i in id) {
  # kde buff i start gets min date kde 95 i 
  kde_buffs$start[kde_buffs$animal_id == i] <- min(collars_kde$acquisition_time[collars_kde$animal_id == i])
  kde_buffs$end[kde_buffs$animal_id == i] <- max(collars_kde$acquisition_time[collars_kde$animal_id == i])
 # kde buff i end gets max date kde 95 i
}

kde_buffs$start <- as_datetime(kde_buffs$start)
kde_buffs$end <- as_datetime(kde_buffs$end)

st_write(kde_buffs, dsn = ".", layer = "kde_buffs", driver="ESRI Shapefile")


#for (i in id) {
#  if (names(mcps_95[i]) == names(mcps_95[1])) {
#    mcp_95 <- mcps_95[[i]]
#  } else {
#    mcp_95 <- rbind(mcp_95, mcps_95[[i]])
#  }
#}

#plot(mcp_95)

#mcp_95 <- st_as_sf(mcp_95)

#mcp_95$animal_id <- as.vector(id)


#mcp_buffs <- st_buffer(mcp_95, dist = 1)
#mcp_buffs$animal_id <- as.character(mcp_buffs$animal_id)

#for (i in 1:nrow(mcp_buffs)) {
#  if (substr(mcp_buffs$animal_id[i], 1, 1) == "C") {
#    mcp_buffs$species[i] <- "coyote"
#  } else if (mcp_buffs$animal_id[i] == "F10") {
#    mcp_buffs$species[i] <- "gray fox"
#  } else {
#    mcp_buffs$species[i] <- "red fox"
#  }
#}

#plot(mcp_buffs)

#for (i in id) {
  # kde buff i start gets min date kde 95 i 
#  kde_buffs$start[kde_buffs$animal_id == i] <- min(collars_kde$acquisition_time[collars_kde$animal_id == i])
#  kde_buffs$end[kde_buffs$animal_id == i] <- max(collars_kde$acquisition_time[collars_kde$animal_id == i])
  # kde buff i end gets max date kde 95 i
#}

#kde_buffs$start <- as_datetime(kde_buffs$start)
#kde_buffs$end <- as_datetime(kde_buffs$end)

#st_write(kde_buffs, dsn = ".", layer = "kde_buffs", driver="ESRI Shapefile")





#collars$used <- 1
#for (i in id) {
#  i.a <- collars[collars$animal_id == i,]
#  write.csv(i.a, paste(i, ".csv", sep = ""))
#}
