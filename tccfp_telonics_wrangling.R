# Set the directory to wherever you are keeping all of the downloaded reports data from Telonics
setwd()

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
collars <- do.call(rbind, csvs)

# I like underscores and all lowercase in headers, so why not fix that?
names(collars) <- lapply(names(collars), function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
})

names(collars) <- gsub("\\.", "_", names(collars))

# Get just Resolved QFP data
collars <- collars[collars$gps_fix_attempt == 'Resolved QFP',]

# Get rid of predeployment data
collars <- collars[!collars$predeployment_data == "Yes",]

# Get rid of the NA rows.
collars <- collars[!is.na(collars$acquisition_time),]

#Create a collar identification number column
collars$collar_id <- 0
for (i in 1:dim(collars)[1]) {
  if (is.na(collars$ctn[i])) {
    collars$collar_id[i] <- NA
  } else if (collars$ctn[i] == "712851A") {
    collars$collar_id[i] <- "F1"
  } else if (collars$ctn[i] == "712852A") {
    collars$collar_id[i] <- "F2"
  } else if (collars$ctn[i] == "712853A") {
    collars$collar_id[i] <- "F3"
  } else if (collars$ctn[i] == "712857A") {
    collars$collar_id[i] <- "F7"
  } else if (collars$ctn[i] == "712859A") {
    collars$collar_id[i] <- "F8"
  } else if (collars$ctn[i] == "712879A") {
    collars$collar_id[i] <- "C1"
  } else if (collars$ctn[i] == "712880A") {
    collars$collar_id[i] <- "C2"
  } else if (collars$ctn[i] == "712881A") {
    collars$collar_id[i] <- "C3"
  } else if (collars$ctn[i] == "712882A") {
    collars$collar_id[i] <- "C4"
  } else if (collars$ctn[i] == "712883A") {
    collars$collar_id[i] <- "C5"
  } else if (collars$ctn[i] == "712884A") {
    collars$collar_id[i] <- "C6"
  } else if (collars$ctn[i] == "712885A") {
    collars$collar_id[i] <- "C7"
  }
}

# Datetimes to POSIXct
collars$acquisition_time <- as.POSIXct(collars$acquisition_time, format = "%Y.%m.%d %H:%M:%S")
collars$gps_fix_time <- as.POSIXct(collars$gps_fix_time, format = "%Y.%m.%d %H:%M:%S")
collars$acquisition_start_time <- as.POSIXct(collars$acquisition_start_time, format = "%Y.%m.%d %H:%M:%S")
collars$receive_time <- as.POSIXct(collars$receive_time, format = "%Y.%m.%d %H:%M:%S")

# Deal with redeployment of collars
collars$animal_id <- 0
for (i in 1:dim(collars)[1]) {
  if (is.na(collars$collar_id[i])) {
    collars$animal_id[i] <- NA
  } else if (!is.na(collars$collar_id[i]) & is.na(collars$gps_fix_time[i])) {
    collars$animal_id[i] <- collars$animal_id[i - 1]
  }  else if (collars$collar_id[i] == "C1" & collars$gps_fix_time[i] > as.POSIXct("2020-02-03 00:00:00")) {
    collars$animal_id[i] <- "C11"
  } else {
    collars$animal_id[i] <- collars$collar_id[i]
  }
}

# If this is direct from satellite, filter out useless columns as well as predeployment and others used for filtering.
collars <- subset(collars, select = c(acquisition_time,
                                      acquisition_start_time,
                                      gps_fix_time,
                                      gps_fix_attempt,
                                      gps_latitude,
                                      gps_longitude,
                                      gps_utm_zone,
                                      gps_utm_northing,
                                      gps_utm_easting,
                                      gps_altitude,
                                      gps_horizontal_error,
                                      gps_horizontal_dilution,
                                      gps_satellite_bitmap,
                                      gps_satellite_count,
                                      gps_navigation_time,
                                      activity_count,
                                      temperature,
                                      satellite_uplink,
                                      receive_time,
                                      repetition_count,
                                      low_voltage,
                                      mortality,
                                      iridium_command,
                                      error,
                                      ctn,
                                      collar_id,
                                      animal_id
))

###### MAKE SURE TO SET THE DIRECTORY AS A NEW ONE or else you'll have problems when
###### you rerun this.
path_out = # Your directory here
write.csv(dt, paste(path_out, 'compiled_collar_data.csv', sep = ''))

