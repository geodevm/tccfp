# Using .csv format, be sure to do 
biologicals <- read.csv("C:/Users/Geoffrey/Desktop/biologicals.csv", header = T, na.strings = NA)

# Formating weight so that all are in kilograms
# If weight is in lbs, multiply by 0.453592 to get kilograms
for (i in 1:dim(biologicals)[1]) {
  if (biologicals$weight_unit[i] == "lbs") {
    biologicals$weight[i] <- biologicals$weight[i]*0.453592
    } else {
    biologicals$weight[i] <- biologicals$weight[i]
  }
}

# Change the lbs to kg label in that column
for (i in 1:dim(biologicals)[1]) {
  if (biologicals$weight_unit[i] == "lbs") {
    biologicals$weight_unit <- "kg"
  } else {
    biologicals$weight_unit[i] <- biologicals$weight_unit[i]
  }
}

biologicals$weight <- format(round(biologicals$weight, 2), nsmall = 2)

# Since ml = cc, I'm going to reformat these columns to display only cc for ketamine_units, 
# xylazine_units, additional_ketamine_units, additional_xylazine_units, blood_collected_units, and
# atipamezole_units

for (i in seq_along(biologicals)) {
  biologicals[[i]][biologicals[[i]] %in% "ml"] <- "cc"
}

# Change the date format to POSIXct