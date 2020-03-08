# Using .csv format, be sure to do 
biologicals <- read.csv("C:/Users/Geoffrey/Desktop/biologicals.csv", header = T, sep = ",", na.strings = NA)

# Formating weight so that all are in kilograms
# If weight is in lbs, multiply by 0.453592 to get kilograms
for (i in 1:dim(biologicals)[1]) {
  if (biologicals$weight_unit[i] == "lbs") {
    biologicals$weight[i] <- biologicals$weight[i]*0.453592
  }
}

# Change the lbs to kg label in that column
for (i in 1:dim(biologicals)[1]) {
  if (biologicals$weight_unit[i] == "lbs") {
    biologicals$weight_unit <- "kg"
  }
}

# Convert bag weight units to kilos, and then change lbs label to kg
for (i in 1:dim(biologicals)[1]) {
  if (is.na(biologicals$bag_weight_unit[i])) {
    biologicals$bag_weight[i] <- biologicals$bag_weight[i]
  } else if (biologicals$bag_weight_unit[i] == "lbs") {
    biologicals$bag_weight[i] <- biologicals$bag_weight[i]*0.453592
  }
}

for (i in 1:dim(biologicals)[1]) {
  if (is.na(biologicals$bag_weight_unit[i])) {
    biologicals$bag_weight_unit[i] <- biologicals$bag_weight_unit[i]
  } else if (biologicals$bag_weight_unit[i] == "lbs") {
    biologicals$bag_weight_unit[i] <- "kg"
  }
}

# Subtract collar weight if needed
for (i in 1:dim(biologicals)[1]) {
  if (is.na(biologicals$collar_type[i])) {
    biologicals$weight[i] <- biologicals$weight[i]
  } else if (biologicals$collar_type[i] == "c") {
    biologicals$weight[i] <- (biologicals$weight[i] - "INSERT COYOTE COLLAR WEIGHT KG")
  } else if (biologicals$collar_type[i] == "f") {
    biologicals$weight[i] <- (biologicals$weight[i] - "INSERT FOX COLLAR WEIGHT KG")
  }
}

# Subtract bag weight if needed
for (i in 1:dim(biologicals)[1]) {
  if (biologicals$with_bag[i] == "Y") {
    biologicals$weight[i] <- biologicals$weight[i] - biologicals$bag_weight[i]
  }
}

# Round weights to 2 digits
biologicals$weight <- format(round(biologicals$weight, 2), nsmall = 2)

# Since ml = cc, I'm going to reformat these columns to display only cc for ketamine_units, 
# xylazine_units, additional_ketamine_units, additional_xylazine_units, blood_collected_units, and
# atipamezole_units

for (i in seq_along(biologicals)) {
  biologicals[[i]][biologicals[[i]] %in% "ml"] <- "cc"
}

# Change the date format
biologicals$date <- format(as.Date(biologicals$date, format = "%Y%m%d"), "%Y-%m-%d")
biologicals$date_released <- format(as.Date(biologicals$date_released, format = "%Y%m%d"), "%Y-%m-%d")

# Change times to POSIXct datetimes
biologicals$injection_time <- as.POSIXct(paste(biologicals$date, biologicals$injection_time), format = "%Y-%m-%d %H:%M")
biologicals$additional_injection_time <- as.POSIXct(paste(biologicals$date, biologicals$additional_injection_time), format = "%Y-%m-%d %H:%M")
biologicals$induction_time <- as.POSIXct(paste(biologicals$date, biologicals$induction_time), format = "%Y-%m-%d %H:%M")
biologicals$collar_time <- as.POSIXct(paste(biologicals$date, biologicals$collar_time), format = "%Y-%m-%d %H:%M")
biologicals$reversal_time <- as.POSIXct(paste(biologicals$date, biologicals$reversal_time), format = "%Y-%m-%d %H:%M")
biologicals$time_alert <- as.POSIXct(paste(biologicals$date, biologicals$time_alert), format = "%Y-%m-%d %H:%M")
biologicals$time_released <- as.POSIXct(paste(biologicals$date_released, biologicals$time_released), format = "%Y-%m-%d %H:%M")
biologicals$temp_1_time <- as.POSIXct(paste(biologicals$date, biologicals$temp_1_time), format = "%Y-%m-%d %H:%M")
biologicals$temp_2_time <- as.POSIXct(paste(biologicals$date, biologicals$temp_2_time), format = "%Y-%m-%d %H:%M")
biologicals$temp_3_time <- as.POSIXct(paste(biologicals$date, biologicals$temp_3_time), format = "%Y-%m-%d %H:%M")
biologicals$temp_4_time <- as.POSIXct(paste(biologicals$date, biologicals$temp_4_time), format = "%Y-%m-%d %H:%M")

# Convert Fahrenheit to Celsius
# (F-32)/1.8

for (i in 1:dim(biologicals)[1]) {
  if (biologicals$temp_unit_all[i] == "Fahr") {
    biologicals$temp_1[i] <- (as.numeric(biologicals$temp_1[i]) - 32)/1.8
  }
}

for (i in 1:dim(biologicals)[1]) {
  if (biologicals$temp_unit_all[i] == "Fahr") {
    biologicals$temp_2[i] <- (as.numeric(biologicals$temp_2[i]) - 32)/1.8
  }
}

for (i in 1:dim(biologicals)[1]) {
  if (biologicals$temp_unit_all[i] == "Fahr") {
    biologicals$temp_3[i] <- (as.numeric(biologicals$temp_3[i]) - 32)/1.8
  }
}

for (i in 1:dim(biologicals)[1]) {
  if (biologicals$temp_unit_all[i] == "Fahr") {
    biologicals$temp_4[i] <- (as.numeric(biologicals$temp_4[i]) - 32)/1.8
  }
}

# One decimal
biologicals$temp_1 <- format(round(as.numeric(biologicals$temp_1), 1), nsmall = 1)
biologicals$temp_2 <- format(round(as.numeric(biologicals$temp_2), 1), nsmall = 1)
biologicals$temp_3 <- format(round(as.numeric(biologicals$temp_3), 1), nsmall = 1)
biologicals$temp_4 <- format(round(as.numeric(biologicals$temp_4), 1), nsmall = 1)

# Convert Y/N to 1/0 for analyses

biologicals$injuries <- as.character(biologicals$injuries)
biologicals$injuries[biologicals$injuries == "N"] <- "0"
biologicals$injuries[biologicals$injuries == "Y"] <- "1"

biologicals$teeth_cond <- as.character(biologicals$teeth_cond)
biologicals$teeth_cond[biologicals$teeth_cond == "N"] <- "0"
biologicals$teeth_cond[biologicals$teeth_cond == "Y"] <- "1"

biologicals$parasites <- as.character(biologicals$parasites)
biologicals$parasites[biologicals$parasites == "N"] <- "0"
biologicals$parasites[biologicals$parasites == "Y"] <- "1"

biologicals$mange <- as.character(biologicals$mange)
biologicals$mange[biologicals$mange == "N"] <- "0"
biologicals$mange[biologicals$mange == "Y"] <- "1"

biologicals$fecal <- as.character(biologicals$fecal)
biologicals$fecal[biologicals$fecal == "N"] <- "0"
biologicals$fecal[biologicals$fecal == "Y"] <- "1"

biologicals$hair <- as.character(biologicals$hair)
biologicals$hair[biologicals$hair == "N"] <- "0"
biologicals$hair[biologicals$hair == "Y"] <- "1"

biologicals$scat <- as.character(biologicals$scat)
biologicals$scat[biologicals$scat == "N"] <- "0"
biologicals$scat[biologicals$scat == "Y"] <- "1"

biologicals$redeploy <- as.character(biologicals$redeploy)
biologicals$redeploy[biologicals$redeploy == "N"] <- "0"
biologicals$redeploy[biologicals$redeploy == "Y"] <- "1"

# Filter out columns no longer useful
biologicals <- subset(biologicals, select = c(date,
                                              identification,
                                              location,
                                              species,
                                              injuries,
                                              ketamine,
                                              xylazine,
                                              injection_time,
                                              additional_injection_time,
                                              additional_ketamine,
                                              additional_xylazine,
                                              induction_time,
                                              teeth_color,
                                              teeth_wear,
                                              teeth_cond,
                                              sagital_crest,
                                              sex,
                                              weight,
                                              age_determination,
                                              parasites,
                                              parasites_comments,
                                              mange,
                                              blood_collected,
                                              fecal,
                                              hair,
                                              scat,
                                              atipamezole,
                                              reversal_time,
                                              time_alert,
                                              date_released,
                                              time_released,
                                              temp_1,
                                              temp_1_time,
                                              temp_2,
                                              temp_2_time,
                                              temp_3,
                                              temp_3_time,
                                              temp_4,
                                              temp_4_time,
                                              redeploy,
                                              redepcollar
                                              ))

# Convert all to the correct class, they're all messed up
biologicals$ketamine <- as.numeric(biologicals$ketamine)
biologicals$xylazine <- as.numeric(biologicals$xylazine)
biologicals$additional_ketamine <- as.numeric(biologicals$additional_ketamine)
biologicals$additional_xylazine <- as.numeric(biologicals$additional_xylazine)
biologicals$weight <- as.numeric(biologicals$weight)
biologicals$blood_collected <- as.numeric(biologicals$blood_collected)
biologicals$atipamezole <- as.numeric(biologicals$atipamezole)
biologicals$temp_1 <- as.numeric(biologicals$temp_1)
biologicals$temp_2 <- as.numeric(biologicals$temp_2)
biologicals$temp_3 <- as.numeric(biologicals$temp_3)
biologicals$temp_4 <- as.numeric(biologicals$temp_4)
biologicals$injuries <- as.numeric(biologicals$injuries)
biologicals$teeth_cond <- as.numeric(biologicals$teeth_cond)
biologicals$parasites <- as.numeric(biologicals$parasites)
biologicals$mange <- as.numeric(biologicals$mange)
biologicals$fecal <- as.numeric(biologicals$fecal)
biologicals$hair <- as.numeric(biologicals$hair)
biologicals$scat <- as.numeric(biologicals$scat)
biologicals$redeploy <- as.numeric(biologicals$redeploy)

# Load in the metadata for this file

biologicals_metadata <- read.csv("C:/Users/Geoffrey/Desktop/biologicals_metadata.csv", header = T, sep = ",", na.strings = NA)

