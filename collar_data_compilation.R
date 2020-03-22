# Set the directory to wherever you are keeping all of the downloaded reports data from Telonics
setwd('C:/Users/Geoffrey/Documents/TCCFP/Telonics Data Converter/Reports/')

# Store the names of the files in "files"
files <- list.files()

# Filter out the .kmls and the .csv's that just contain summary statistics
files <- subset(files, !grepl("kml", files))
files <- subset(files, !grepl("Stat", files))

# Create an empty vector
new.files <- c()

# Loop through to get the first round of alphanumeric groupings, is not intuitive so will need to do second loop.
l <- as.numeric(length(files))
for (i in 2:l) {
  if ((as.numeric(substr(files[i], 9, 10)) > as.numeric(substr(files[i-1], 9, 10)) &
      substr(files[i], 1, 7) == substr(files[i-1], 1, 7) & 
      as.numeric(substr(files[i], 9, 10)) > as.numeric(substr(files[i+1], 9, 10)) &
      substr(files[i], 1, 7) == substr(files[i+1], 1, 7)) |
      (files[i] == files[l]) |
      substr(files[i], 1, 7) != substr(files[i+1], 1, 7)) {
    new.files <- c(new.files, files[i])
  }
}

# Create a new empty vector
new_files <- c()

# This loop will sort out the newest broadcasts from Iridium.
l <- as.numeric(length(new.files))
for (i in 2:l) {
  if ((substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
       substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
       as.numeric(substr(new.files[i], 9, 10)) > as.numeric(substr(new.files[i-1], 9, 10))) |
      (substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
       substr(new.files[i], 1, 7) != substr(new.files[i-1], 1, 7)) |
      (new.files[i] == new.files[l] &
       substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
       as.numeric(substr(new.files[i], 9, 10)) > as.numeric(substr(new.files[i-1], 9, 10))) |
      (new.files[i] == new.files[l] &
       substr(new.files[i], 1, 7) != substr(new.files[i-1], 1, 7))) {
    new_files <- c(new_files, new.files[i])
  } else if ((substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
              substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
              as.numeric(substr(new.files[i], 9, 10)) < as.numeric(substr(new.files[i-1], 9, 10))) |
             (new.files[i] == new.files[l] &
              substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
              as.numeric(substr(new.files[i], 9, 10)) < as.numeric(substr(new.files[i-1], 9, 10)))) {
    new_files <- c(new_files, new.files[i-1])
  }
}

# Create a list for storing the files that we are going to read in.
csvs <- list()

# Loop thorugh to compile all of the data files into a list with a column corresponding to the collar identity (ctn).
l <- as.numeric(length(new_files))
for (i in 1:l) {
  csvs[[i]] <- read.csv(new_files[i], header = TRUE, skip = 23, na.strings = "")
  csvs[[i]]$ctn <- substr(new_files[i], 1, 7)
}

# Combine into one table
collars <- do.call(rbind,csvs)