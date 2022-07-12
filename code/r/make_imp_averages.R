library(here)
library(tidyverse)
library(terra)


home_ranges <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(season != "annual" & method == "kde_ani_95")
home_ranges[(home_ranges$animal_id == "F7_1" | 
             home_ranges$animal_id == "F7_2"), ]$animal_id <- "F7"
home_ranges[(home_ranges$animal_id == "C9_1" | 
               home_ranges$animal_id == "C9_2"), ]$animal_id <- "C9"

imp <- rast(here("data/gis_layers/impervious/TCMA_Impervious_2000.tif"))

annual_ranges <- home_ranges[0, ] %>%
  select(animal_id, geometry)

ids <- levels(as.factor(as.character(home_ranges$animal_id)))

for (i in ids) {
  indivs <- home_ranges[home_ranges$animal_id == i, ]
  range <- st_union(indivs)
  annual_range <- indivs[1, ] %>%
    select(animal_id, geometry)
  annual_range$geometry <- range
  annual_ranges <- annual_ranges %>%
    bind_rows(annual_range)
}

# Define extraction function for typical weighted average
extractor <- function (i, x) {
  # Extract raster values within that buffer
  rast <- x %>%
    terra::extract(vect(annual_ranges[annual_ranges$animal_id == i, "geometry"]))
  # Exclude any NA values introduced
  rast <- rast[complete.cases(rast), ]
  # Average the DEM values
  extracted <- mean(rast[, 2])
  # Append to columns of the movement data as a proportion
  return(extracted)
}


annual_ranges$dem <- lapply(ids, extractor, x = imp) %>%
  # Convert to vector
  unlist()

annual_ranges <- annual_ranges %>%
  st_drop_geometry()

annual_ranges %>%
  write.csv(here("data/processed_data/imp_averages.csv"), 
            row.names = FALSE)
