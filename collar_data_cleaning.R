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
collars$collar_id[collars$ctn == "712851A"] <- "F1"
collars$collar_id[collars$ctn == "712852A"] <- "F2"
collars$collar_id[collars$ctn == "712853A"] <- "F3"
collars$collar_id[collars$ctn == "712857A"] <- "F7"
collars$collar_id[collars$ctn == "712859A"] <- "F8"
collars$collar_id[collars$ctn == "712879A"] <- "C1"
collars$collar_id[collars$ctn == "712880A"] <- "C2"
collars$collar_id[collars$ctn == "712881A"] <- "C3"
collars$collar_id[collars$ctn == "712882A"] <- "C4"
collars$collar_id[collars$ctn == "712883A"] <- "C5"
collars$collar_id[collars$ctn == "712884A"] <- "C6"
collars$collar_id[collars$ctn == "712885A"] <- "C7"

# Make an animal id column
collars$animal_id <- collars$collar_id
# Datetimes to POSIXct
collars$acquisition_time <- ymd_hms(collars$acquisition_time)
collars$gps_fix_time <- ymd_hms(collars$gps_fix_time)
collars$acquisition_start_time <- ymd_hms(collars$acquisition_start_time)
collars$receive_time <- ymd_hms(collars$receive_time)
# Deal with redeployment of collars
collars[(collars$collar_id == "C1" & collars$acquisition_time > ymd_hms("2020-02-06 00:00:00")),]$animal_id <- "C11"

# Also deal with the gps points fixed when moved after death in C1
collars <- collars[!(collars$animal_id == 'C1' & collars$acquisition_time >= ymd_hms("2020-02-02 00:00:00")), ]

# Remove this observation, when coyote was transferred to vet clinic overnight.
collars <- collars[!(collars$gps_latitude == 45.015218 & collars$animal_id == 'C2'), ]

# Animal ID should be a factor
collars$animal_id <- as.factor(collars$animal_id)

# Exporting data
#setwd("C:/Users/Geoffrey/Desktop/")
#write.csv(collars, "full.dat.csv")

