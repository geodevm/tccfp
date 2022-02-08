library(here)

# Store the names of the files in "files"
files <- list.files(here("data/reports"))

# Filter out the .kmls and the .csv's that just contain summary statistics
datalogs <- subset(files, grepl("Datalog", files))
files <- subset(files, grepl("Complete", files))

# Create an empty vector
new.files <- c()

# Loop through to get the first round of alphanumeric groupings, is not intuitive so will need to do second loop.
l <- as.numeric(length(files))
for (i in 2:l) {
  if ((as.numeric(gsub("[^\\d]+", "", substr(files[i], 9, 11), perl=TRUE)) > 
       as.numeric(gsub("[^\\d]+", "", substr(files[i-1], 9, 11), perl=TRUE)) &
       substr(files[i], 1, 7) == substr(files[i-1], 1, 7) & 
       as.numeric(gsub("[^\\d]+", "", substr(files[i], 9, 11), perl=TRUE)) > 
       as.numeric(gsub("[^\\d]+", "", substr(files[i+1], 9, 11), perl=TRUE)) &
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
for (i in 2:as.numeric(length(new.files))) {
  if ((substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
       substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
       as.numeric(gsub("[^\\d]+", "", substr(new.files[i], 9, 11), perl=TRUE)) >
       as.numeric(gsub("[^\\d]+", "", substr(new.files[i-1], 9, 11), perl=TRUE))) |
      (substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
       substr(new.files[i], 1, 7) != substr(new.files[i-1], 1, 7)) |
      (new.files[i] == new.files[l] &
       substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
       as.numeric(gsub("[^\\d]+", "", substr(new.files[i], 9, 11), perl=TRUE)) >
       as.numeric(gsub("[^\\d]+", "", substr(new.files[i-1], 9, 11), perl=TRUE))) |
      (new.files[i] == new.files[l] &
       substr(new.files[i], 1, 7) != substr(new.files[i-1], 1, 7))) {
    new_files <- c(new_files, new.files[i])
  } else if ((substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) &
              substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
              as.numeric(gsub("[^\\d]+", "", substr(new.files[i], 9, 11), perl=TRUE)) < 
              as.numeric(gsub("[^\\d]+", "", substr(new.files[i-1], 9, 11), perl=TRUE))) |
             (new.files[i] == new.files[l] &
              substr(new.files[i], 1, 7) == substr(new.files[i-1], 1, 7) &
              as.numeric(gsub("[^\\d]+", "", substr(new.files[i], 9, 11), perl=TRUE)) < 
              as.numeric(gsub("[^\\d]+", "", substr(new.files[i-1], 9, 11), perl=TRUE)))) {
    new_files <- c(new_files, new.files[i-1])
  }
}

# Subset the new_files to only include broadcasts that we do not have a datalog for.
#
log_nums <- c()
# Here we get a vector of the CTN numbers we have datalog files for.
for (i in 1:length(datalogs)) {
  log_nums <- c(log_nums, substr(datalogs[i], 1, 7))
}
# Exclude files that also have datalogs
for (i in 1:length(log_nums)) {
  new_files <- subset(new_files, !grepl(log_nums[i], new_files))
}

# Loop thorugh to compile all of the data files into a list with a column corresponding to the collar identity (ctn).
csvs <- list()
l <- as.numeric(length(new_files))
for (i in 1:l) {
  csvs[[i]] <- read.csv(here(paste0("data/reports/", new_files[i]), sep = ""), header = TRUE, skip = 22, na.strings = "")
  csvs[[i]]$ctn <- substr(new_files[i], 1, 7)
}
csvs_d <- list()
for (i in 1:l) {
  csvs_d[[i]] <- read.csv(here(paste0("data/reports/", datalogs[i]), sep = ""), header = TRUE, skip = 22, na.strings = "")
  csvs_d[[i]]$ctn <- substr(datalogs[i], 1, 7)
}
# Combine into one table
broadcast_data <- do.call(rbind, csvs)
# Get rid of useless columns for working with the datalogs
broadcast_data <- subset(broadcast_data, select = -c(Iridium.CEP.Radius,
                                                     Iridium.Latitude,
                                                     Iridium.Longitude,
                                                     Receive.Time,
                                                     Repetition.Count,
                                                     Low.Voltage))

# Add the columns that are included in the datalogs but not in the broadcasts
broadcast_data$GPS.Speed <- NA
broadcast_data$GPS.Heading <- NA

# Bind the datalog csvs
datalog_data <- do.call(rbind, csvs_d)

# Bind the full dataset
collars <- rbind(datalog_data, broadcast_data)

# I like underscores and all lowercase in headers, so why not fix that?
names(collars) <- lapply(names(collars), function(v) {
  if (is.character(v)) return(tolower(v))
  else return(v)
})

names(collars) <- gsub("\\.", "_", names(collars))

# Get just Resolved QFP data ?
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
  } else if (collars$ctn[i] == "712854A") {
    collars$collar_id[i] <- "F4"
  } else if (collars$ctn[i] == "712856A") {
    collars$collar_id[i] <- "F6"
  } else if (collars$ctn[i] == "712857A") {
    collars$collar_id[i] <- "F7"
  } else if (collars$ctn[i] == "712859A") {
    collars$collar_id[i] <- "F8"
  } else if (collars$ctn[i] == "712860A") {
    collars$collar_id[i] <- "F9"
  } else if (collars$ctn[i] == "712861A") {
    collars$collar_id[i] <- "F10"
  } else if (collars$ctn[i] == "712862A") {
    collars$collar_id[i] <- "F11"
  } else if (collars$ctn[i] == "712863A") {
    collars$collar_id[i] <- "F12"
  } else if (collars$ctn[i] == "712864A") {
    collars$collar_id[i] <- "F13"
  } else if (collars$ctn[i] == "713584A") {
    collars$collar_id[i] <- "F14"
  } else if (collars$ctn[i] == "712866A") {
    collars$collar_id[i] <- "F15"
  } else if (collars$ctn[i] == "712867A") {
    collars$collar_id[i] <- "F16"
  } else if (collars$ctn[i] == "712868A") {
    collars$collar_id[i] <- "F17"
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
  } else if (collars$ctn[i] == "712886A") {
    collars$collar_id[i] <- "C8"
  } else if (collars$ctn[i] == "712887A") {
    collars$collar_id[i] <- "C9"
  } else if (collars$ctn[i] == "712888A") {
    collars$collar_id[i] <- "C10"
  } else if (collars$ctn[i] == "719839A") {
    collars$collar_id[i] <- "C11"
  } else if (collars$ctn[i] == "719840A") {
    collars$collar_id[i] <- "C12"
  } else if (collars$ctn[i] == "719841A") {
    collars$collar_id[i] <- "C13"
  }
}

collars <- collars[!collars$collar_id == 0, ]

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
    collars$animal_id[i] <- "C1.1"
  } else {
    collars$animal_id[i] <- collars$collar_id[i]
  }
}

# Animal ID should be a factor
collars$animal_id <- as.factor(collars$animal_id)

###### MAKE SURE TO SET THE DIRECTORY AS A NEW ONE or else you'll have problems when
###### you rerun this.
setwd()
write.csv(collars, 'compiled_collar_data.csv')

