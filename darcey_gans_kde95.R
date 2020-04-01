# Here are the packages I would used to map home ranges.
library(adehabitatHR)
library(rgdal)
library(rgeos)
library(raster)

# First thing we'll do is run some commands just to get a look at the data. I'm going to run through examples of 
# different types of home range mapping and easy commands for generating them. I'll be using the entire data set
# for this, not specific animals.

# First, summarize the data as per usual.
summary(collars)

# Convert the longitude and latitude columns to spatial points (think of as x = longitude, y = latitude for plotting)
collars.all <- SpatialPoints(collars[c("gps_longitude", "gps_latitude")])

# Plot these spatial points to get a look
plot(collars.all)

# And summarize these spatial points to see if anything is missing.
summary(collars.all)
# Note that there is no "proj4string", and "Is projected" is NA. THIS IS A PROBLEM! These for sure need to be projected
# once you start working with other layers. From the dataset you can see that the gps_utm_zone is 15T. This is within
# zone 15N, so Google UTM zone 15N to get to spatialreference.org, which will give you a Proj4 link to click which
# will give what I put in the CRS command below.

# Here is where we project the spatial points.
proj4string(collars.all) <- CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs")
# If you wanted to transform this data to be in another projection, use the command below filling in the projection
#collars.all <- spTransform(collars.all, CRS(""))

# The most common way of mapping home ranges is through MCPs. Typically, a 95% Minimum Convex Polygon is used to map
# a home range, while something like a 50% MCP is used to map a "core area".
collars.all.mcp95 <- mcp(collars.all, percent = 95, unin = "m", unout = "km2")

# This shows what this is actually doing, with 95% of the points being within the area with the most points, excluding
# the animal that had dispersed
plot(collars.all)
plot(collars.all.mcp95, add = TRUE)
axis(1, tcl = 0.3)
axis(2, las = 1, tcl = 0.3)
axis(3, labels = NA, tcl = 0.3)
axis(4, labels = NA, tcl = 0.3)
box()

# Here's a 50% MCP
collars.all.mcp50 <- mcp(collars.all, percent = 50, unin = "m", unout = "km2")

#And another visualization
plot(collars.all)
plot(collars.all.mcp50, add = TRUE)
axis(1, tcl = 0.3)
axis(2, las = 1, tcl = 0.3)
axis(3, labels = NA, tcl = 0.3)
axis(4, labels = NA, tcl = 0.3)
box()

# Kernel Density Estimates
# Here's what we're actually going to do. Kernels estimate density rather than doing something like a convex
# polygon, which really tells you nothing about use within that polygon, or if there are patterns at the edge of 
# the polygon which are not picked up because of the "convex" property of MCPs.

# Here's what a KDE really does.
collars.all.kde <- kernelUD(collars.all, h = "href")
image(collars.all.kde)
collars.all.rast <- raster(as(collars.all.kde, "SpatialPixelsDataFrame"))
plot(collars.all.rast)
plot(collars.all, add = T)

# Export as a raster if you want
# writeRaster(collars.all.rast, "________kde.tif")

#Here is where we actually do the 95% KDE
collars.all.kde95 <- getverticeshr(collars.all.kde, percent = 95, unin = "m", unout = "km2")
plot(collars.all)
plot(collars.all.kde95, add = TRUE)
axis(1, tcl = 0.3)
axis(2, las = 1, tcl = 0.3)
axis(3, labels = NA, tcl = 0.3)
axis(4, labels = NA, tcl = 0.3)
box()

# In this case the KDE looks pretty gnarly because of the dispersed coyoted and because this is a conglomeration
# really densely distributed home ranges relative to the overall scale. So in this case (mapping all) the KDE was
# not a very representative choice. However, it will likely be pretty good as a core areas estimator.
#Here is where we actually do the 95% KDE
collars.all.kde95 <- getverticeshr(collars.all.kde, percent = 50, unin = "m", unout = "km2")

# and visualize
plot(collars.all)
plot(collars.all.kde95, add = TRUE)
axis(1, tcl = 0.3)
axis(2, las = 1, tcl = 0.3)
axis(3, labels = NA, tcl = 0.3)
axis(4, labels = NA, tcl = 0.3)
box()

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
F1.sp <- collars[collars$animal_id == "F1",]
F1.sp <- SpatialPoints(F1.sp[c("gps_longitude", "gps_latitude")])

# From there you can just do what we did with 95% KDEs. For example, I'll go through what I would do for F1,
# commenting next to everything I wouldn't do during a for loop.
F1.kde <- kernelUD(F1.sp, h = "href") 

image(F1.kde) # Nope
F1.rast <- raster(as(F1.kde, "SpatialPixelsDataFrame")) # Nope
plot(F1.rast) # Nope
plot(F1.sp, add = T) # Nope
#writeRaster(F1.rast, "F1.kde.tif") # Nope

F1.kde95 <- getverticeshr(F1.kde, percent = 95, unin = "m", unout = "km2")

plot(F1.rast) # Nope
plot(F1.sp, add = T) # Nope
plot(F1.kde95, add = T) # Nope
# As I comment also above, make sure your directory is set somewhere besides where you are storing the data before
# running this or else you'll run into problems.

#writeOGR(F1.kde95, dsn = ".", layer = "F1.kde95", driver="ESRI Shapefile")

# That should be all you need as components of a pretty efficient for loop. Work flow will look like this:
# 1. Filter for just that animal
# 2. Convert the filtered data to spatial points.
# 3. Do a kernal density estimate.
# 4. Make the 95% KDE boundary.
# 5. Write out the file (IN THE RIGHT DIRECTORY!)

# Also, this might be a useful command in the loop

levels(collars$animal_id)

# The advice I can give you for writing a for loop is to look at what is common throughout all of the commands that you call to do this
# task, and make that the iterator. If you get totally stuck, that's understandable. Give it a try though!