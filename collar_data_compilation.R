# Set the directory to wherever you are keeping all of the downloaded reports data from Telonics
setwd('U:/research/tccfp/Telonics Data Converter/Reports/')

# Store the names of the files in "files"
f <- list.files()

# Filter out the .kmls and the .csv's that just contain summary statistics
f <- subset(f, !grepl("kml", f))
f <- subset(f, !grepl("Stat", f))

# Create an empty vector
n.f <- c()

# Loop through to get the first round of alphanumeric groupings, is not intuitive so will need to do second loop.
for (i in 2:length(f)) {
  if ((as.numeric(substr(f[i], 9, 10)) > as.numeric(substr(f[i-1], 9, 10)) &
      substr(f[i], 1, 7) == substr(f[i-1], 1, 7) & 
      as.numeric(substr(f[i], 9, 10)) > as.numeric(substr(f[i+1], 9, 10)) &
      substr(f[i], 1, 7) == substr(f[i+1], 1, 7)) |
      (f[i] == f[length(f)]) |
      substr(f[i], 1, 7) != substr(f[i+1], 1, 7)) {
    n.f <- c(n.f, f[i])
  }
}

# Create a new empty vector
f.f <- c()

# This loop will sort out the newest broadcasts from Iridium.
for (i in 2:length(n.f)) {
  if ((substr(n.f[i], 1, 7) != substr(n.f[i+1], 1, 7) &
       substr(n.f[i], 1, 7) == substr(n.f[i-1], 1, 7) &
       as.numeric(substr(n.f[i], 9, 10)) > as.numeric(substr(n.f[i-1], 9, 10))) |
      (substr(n.f[i], 1, 7) != substr(n.f[i+1], 1, 7) &
       substr(n.f[i], 1, 7) != substr(n.f[i-1], 1, 7)) |
      (n.f[i] == n.f[length(n.f)] &
       substr(n.f[i], 1, 7) == substr(n.f[i-1], 1, 7) &
       as.numeric(substr(n.f[i], 9, 10)) > as.numeric(substr(n.f[i-1], 9, 10))) |
      (n.f[i] == n.f[length(n.f)] &
       substr(n.f[i], 1, 7) != substr(n.f[i-1], 1, 7))) {
    f.f <- c(f.f, n.f[i])
  } else if ((substr(n.f[i], 1, 7) != substr(n.f[i+1], 1, 7) &
              substr(n.f[i], 1, 7) == substr(n.f[i-1], 1, 7) &
              as.numeric(substr(n.f[i], 9, 10)) < as.numeric(substr(n.f[i-1], 9, 10))) |
             (n.f[i] == n.f[length(n.f)] &
              substr(n.f[i], 1, 7) == substr(n.f[i-1], 1, 7) &
              as.numeric(substr(n.f[i], 9, 10)) < as.numeric(substr(n.f[i-1], 9, 10)))) {
    f.f <- c(f.f, n.f[i-1])
  }
}

# Create a list for storing the files that we are going to read in.
csvs <- list()

# Loop thorugh to compile all of the data files into a list with a column corresponding to the collar identity (ctn).
for (i in 1:length(f.f)) {
  csvs[[i]] <- read.csv(f.f[i], header = TRUE, skip = 23, na.strings = "")
  csvs[[i]]$ctn <- substr(f.f[i], 1, 7)
}

# Combine into one table
collars <- do.call(rbind,csvs)

