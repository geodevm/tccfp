# lme4 includes the glm function that will be used for fitting models
library(lme4)
# Set the working directory here to wherever you keep the files generated in QGIS
setwd()
# This is the list of animal identification numbers that will be used for iterations in the for loops below
ids <- list("C11", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "F1", "F2", "F3", "F8")
# Make an empty data frame to be appended to in the iterations below
rsf.data <- read.csv(text="X,Y,tcma_lc_fi,distance,used,id")  
# This loop reads through the directory and reads in the files
for (i in ids) {
  used <- read.csv(paste(i, "use.csv", sep = "")) # Get the used points generated in QGIS
  used <- used[, c(1,2,3,68)] # Select the columns useful for analysis
  used$used <- 1 # Since these are "used" points in the binary analysis, they all get assigned a 1
  available <- read.csv(paste(i, "ava.csv", sep = "")) # Get the available points generated in QGIS
  available <- available[, c(1,2,3,68)] # Select the columns useful for analysis
  available$used <- 0 # Available points get assigned 0
  full <- rbind(used, available) # Join the used and available
  full$id <- i # Make a column with the animal id
  rsf.data <- rbind(rsf.data, full) # Add to a larger data table which will include all of the animals after the loop is done
}
# This loop just assigns values to the numbers from the landcover raster
for (i in 1:dim(rsf.data)) {
  if (is.na(rsf.data$tcma_lc_fi[i])){
    rsf.data$tcma_lc_fi[i] <- NA
  } else if (rsf.data$tcma_lc_fi[i] == "1") {
    rsf.data$tcma_lc_fi[i] <- "Grass/Shrub"
  } else if (rsf.data$tcma_lc_fi[i] == "2") {
    rsf.data$tcma_lc_fi[i] <- "Bare Soil"
  } else if (rsf.data$tcma_lc_fi[i] == "3") {
    rsf.data$tcma_lc_fi[i] <- "Buildings"
  } else if (rsf.data$tcma_lc_fi[i] == "4") {
    rsf.data$tcma_lc_fi[i] <- "Roads/Paved Surfaces"
  } else if (rsf.data$tcma_lc_fi[i] == "5") {
    rsf.data$tcma_lc_fi[i] <- "Lakes/Ponds"
  } else if (rsf.data$tcma_lc_fi[i] == "6") {
    rsf.data$tcma_lc_fi[i] <- "Deciduous Tree Canopy"
  } else if (rsf.data$tcma_lc_fi[i] == "7") {
    rsf.data$tcma_lc_fi[i] <- "Coniferous Tree Canopy"
  } else if (rsf.data$tcma_lc_fi[i] == "8") {
    rsf.data$tcma_lc_fi[i] <- "Agriculture"
  } else if (rsf.data$tcma_lc_fi[i] == "9") {
    rsf.data$tcma_lc_fi[i] <- "Emergent Wetland"
  } else if (rsf.data$tcma_lc_fi[i] == "10") {
    rsf.data$tcma_lc_fi[i] <- "Forested/Shrub Wetland"
  } else if (rsf.data$tcma_lc_fi[i] == "11") {
    rsf.data$tcma_lc_fi[i] <- "River"
  } else if (rsf.data$tcma_lc_fi[i] == "12") {
    rsf.data$tcma_lc_fi[i] <- "Extraction"
  }
}
# Convert the landcover to a factor
rsf.data$tcma_lc_fi <- as.factor(rsf.data$tcma_lc_fi)
# Make an empty list to put the fitted models in
fits <- list()
# This loop fits a model for each animal
# This is logistic regression comparing used versus available points, seeing how it varies with habitat and distance to trails
for (i in ids) {
  fdat <- rsf.data[rsf.data$id == i,]
  fm <- glm(used ~ tcma_lc_fi + scale(distance), family = "binomial", data = fdat)
  fits[[i]] <- fm
}
# This is the command you would use to check the fit for certain models (in this case for C2)
summary(fits[["C2"]])
# We can talk about interpretation of models after you get to this point