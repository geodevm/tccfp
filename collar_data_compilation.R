setwd('C:/Users/Geoffrey/Documents/TCCFP/Telonics Data Converter/Reports/')
files <- list.files()
files <- subset(files, !grepl("kml", files))
files <- subset(files, !grepl("Stat", files))
new.files <- c()
l <- as.numeric(length(files))
for (i in 2:l) {
  if ((as.numeric(substr(files[i], 9, 10)) > as.numeric(substr(files[i-1], 9, 10)) 
      & substr(files[i], 1, 7) == substr(files[i-1], 1, 7) 
      & as.numeric(substr(files[i], 9, 10)) > as.numeric(substr(files[i+1], 9, 10)) 
      & substr(files[i], 1, 7) == substr(files[i+1], 1, 7))
      | (files[i] == files[l])) {
    new.files <- c(new.files, files[i])
  }
}
new_files <- c()
l <- as.numeric(length(new.files))
for (i in 1:l) {
  if (substr(new.files[i], 1, 7) != substr(new.files[i+1], 1, 7) | new.files[i] == new.files[l]) {
    new_files <- c(new_files, new.files[i])
  }
}
csvs <- list()
l <- as.numeric(length(new_files))
for (i in 1:l) {
  name <- substr(new_files[i], 1, 7)
  csvs[[i]] <- read.csv(new_files[i], header = TRUE, skip = 23, na.strings = "")
  csvs[[i]]$ctn <- name
}
collars <- do.call(rbind,csvs)

