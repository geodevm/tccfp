collars_kde <- collars_kde[collars_kde$animal_id != "F4", ]
collars_kde <- collars_kde[!(collars_kde$animal_id == "C1" & collars_kde$acquisition_time > ymd_hms("2020-01-16 00:00:00")), ]


id <- as.list(levels(as.factor(collars_kde$animal_id)))
kdes_95 <- list()
for (i in id) {
  i.a <- collars_kde[collars_kde$animal_id == i,]
  i.sp <- SpatialPoints(i.a[c("gps_utm_easting", "gps_utm_northing")])
  proj4string(i.sp) <- CRS("+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +no_defs")
  i.kde <- kernelUD(i.sp, h = "href") 
  i.kde95 <- getverticeshr(i.kde, percent = 95, unin = "m", unout = "km2")
  #writeOGR(i.kde95, dsn = ".", layer = paste(i, ".kde95", sep = ""), driver="ESRI Shapefile")
  kdes_95 <- append(kdes_95, i.kde95)
  names(kdes_95) <- i
}

names(kdes_95) <- id

for (i in id) {
  if (names(kdes_95[i]) == names(kdes_95[1])) {
    kde_95 <- kdes_95[[i]]
  } else {
    kde_95 <- rbind(kde_95, kdes_95[[i]])
  }
}

plot(kde_95)