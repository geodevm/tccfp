library(lubridate)
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
for (i in 1:nrow(collars)[1]) {
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
collars$acquisition_time <- ymd_hms(collars$acquisition_time)
collars$gps_fix_time <- ymd_hms(collars$gps_fix_time)
collars$acquisition_start_time <- ymd_hms(collars$acquisition_start_time)
collars$receive_time <- ymd_hms(collars$receive_time)
# Deal with redeployment of collars
collars[(collars$collar_id == "C1" & collars$acquisition_time > ymd_hms("2020-02-06 00:00:00")),]$collar_id <- "C11"

# Also deal with the gps points fixed when moved after death in C1
collars <- collars[-(collars$animal_id == 'C1' & collars$acquisition_time >= ymd_hms("2020-02-02 00:00:00")),]

# Remove this observation, when coyote was transferred to vet clinic overnight.
collars <- collars[-(collars$gps_latitude == 45.015218 & collars$animal_id == 'C2'),]

# Animal ID should be a factor
collars$animal_id <- as.factor(collars$animal_id)

# If this is direct from satellite, filter out useless columns as well as predeployment and others used for filtering.
collars <- subset(collars, select = !c(acquisition_time,
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

#setwd("C:/Users/Geoffrey/Desktop/")
#write.csv(collars, "full.dat.csv")

