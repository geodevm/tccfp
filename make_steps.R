turn_ang <- function(xlen, ylen, comp =TRUE, as.deg = FALSE) {
  u = 1
  if (as.deg) {
    u = 180/pi
  }
  s <- sign(xlen)
  s[s == 0] <- 1
  ang = s * (ylen < 0) * pi + atan(xlen / ylen)
  if (comp) {
    ang[ang < 0] <- ang[ang < 0] + 2 * pi
  }
  return(ang * u)
}

bearing.ta <- function(xy1, xy2, xy3, as.deg = FALSE) {
  if (length(xy1) != 2 | length(xy2) != 2 | length(xy3) != 2) {
    return(NaN)
  }
  u = 1
  if (as.deg) {
    u = 180 / pi
  }
  xylen1 <- xy2 - xy1
  xylen2 <- xy3 - xy2
  comp1 <- turn_ang(xylen1[1], xylen1[2], comp = F)
  comp2 <- turn_ang(xylen2[1], xylen2[2], comp = F)
  if(is.data.frame(xylen1)) {
    sl1 <- sqrt(rowSums(xylen1^2))
    sl2 <- sqrt(rowSums(xylen2^2))
  } else {
    sl1 <- sqrt(sum(xylen1^2))
    sl2 <- sqrt(sum(xylen2^2))
  }
  ta = (comp2 - comp1)
  ta[ta < -pi] = ta[ta < -pi] + 2 * pi
  ta[ta > pi] = ta[ta > pi] - 2 * pi
  return(list(comp1 = unlist(comp1 * u), comp2 = unlist(comp2 * u),
              ta = unlist(ta * u), sl1 = unlist(sl1), sl2 = unlist(sl2)))
}


################# This is a generalized make track function ###################################################################
track <- function (x, y, bid, df, as.deg = FALSE, comp = FALSE, rm.missing = FALSE) {
  # x is UTM easting
  # y is UTM northing
  # bid is the column containing burst id numbers
  # data is the data being used for the calculation
  ############ Columns to be created ########################
  # ta is turn angle in radians (+/- pi)
  # sl is step length
  # bear is the bearing, defaulted to radians (+/- pi)
  # as.deg = FALSE is default, keeping the units in radians
  # comp = FALSE is default, which keeps the radians on a (+/- pi) scale
  for (i in 1:dim(df)) {
    if (i == 1) { 
      df[i, "ta"] <- NA
      df[i, "sl"] <- NA
      df[i, "bear"] <- NA
    } else if (i == 2) { 
      # If you have only the first two points in a burst (this case second of the table), you can't calculate the turn angle
      xy2 <- c(df[i, x], df[i, y]) # XY coords of current point
      xy1 <- c(df[i - 1, x], df[i - 1, y]) # XY coords of previous point
      xylen1 <- xy2 - xy1 # Distance between them
      s <- sign(xylen1[1]) # This will find the sign of the x difference
      s[s == 0] <- 1  # And correct for when the sign is 0
      ang = s * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])  # Calculate the compass bearing
      if (comp) {
        #This is defaulted to comp = FALSE, keeps it on +/- pi turn angles
        ang[ang < 0] <- ang[ang < 0] + 2 * pi
      }
      if(is.data.frame(xylen1)) {
        sl_ <- sqrt(rowSums(xylen1^2)) # Calculate the step length (net squared displacement from previous)
      } else {
        sl_ <- sqrt(sum(xylen1^2))
      }
      df[i, "ta"] <- NA
      df[i, "sl"] <- sl_
      df[i, "bear"] <- ang # Bearing of current step
    } else if (df[i, bid] != df[i - 1, bid]) { 
      # If you only have the first point of the burst, you can't calculate any of the 3 categories
      df[i, "ta"] <- NA
      df[i, "sl"] <- NA
      df[i, "bear"] <- NA
    } else if (df[i - 2, bid] != df[i, bid] & df[i - 1, bid] == df[i, bid]) { 
      # If you have only the first two points in a burst, you can't calculate the turn angle
      xy2 <- c(df[i, x], df[i, y])
      xy1 <- c(df[i - 1, x], df[i - 1, y])
      xylen1 <- xy2 - xy1
      s <- sign(xylen1[1])
      s[s == 0] <- 1
      ang = s * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])
      if (comp) {
        ang[ang < 0] <- ang[ang < 0] + 2 * pi
      }
      if(is.data.frame(xylen1)) {
        sl_ <- sqrt(rowSums(xylen1^2))
      } else {
        sl_ <- sqrt(sum(xylen1^2))
      }
      df[i, "ta"] <- NA
      df[i, "sl"] <- sl_
      df[i, "bear"] <- ang
    } else if (df[i - 2, bid] == df[i, bid] & df[i - 1, bid] == df[i, bid]) { 
      # If the previous two observations are both from the same burst, you can calculate everything
      xy3 <- c(df[i, x], df[i, y]) # XY coords of current point
      xy2 <- c(df[i - 1, x], df[i - 1, y]) # XY coords of previous point
      xy1 <- c(df[i - 2, x], df[i - 2, y]) # XY coords of point two previous
      xylen1 <- xy2 - xy1 # Distance between points previous and two previous
      xylen2 <- xy3 - xy2 # Distance between points current and previous
      s1 <- sign(xylen1[1])
      s1[s1 == 0] <- 1
      ang1 = s1 * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])
      if (comp) {
        ang1[ang1 < 0] <- ang1[ang1 < 0] + 2 * pi
      }
      s2 <- sign(xylen2[1])
      s2[s2 == 0] <- 1
      ang2 = s * (xylen2[2] < 0) * pi + atan(xylen2[1] / xylen2[2])
      if (comp) {
        ang2[ang2 < 0] <- ang2[ang2 < 0] + 2 * pi
      }
      if(is.data.frame(xylen1)) {
        sl_ <- sqrt(rowSums(xylen2^2)) # Calculate the current step length (net squared displacement from previous)
      } else {
        sl_ <- sqrt(sum(xylen2^2))
      }
      ta_ = (ang2 - ang1) # Calculate turn angle
      ta_[ta_ < -pi] = ta_[ta_ < -pi] + 2 * pi # Corrections to make it +/- pi
      ta_[ta_ > pi] = ta_[ta_ > pi] - 2 * pi
      df[i, "ta"] <- ta_
      df[i, "sl"] <- sl_
      df[i, "bear"] <- ang2 # Bearing of current step
    }
  }
  for (i in 1:dim(df)) {
    if (is.na(df[i, "ta"])) {
    } else if (df[i, "ta"] < -pi) {
      df[i, "ta"] <- df[i, "ta"] + 2 * pi
    } else if (df[i, "ta"] > pi) {
      df[i, "ta"] <- df[i, "ta"] - 2 * pi
    }
    if (is.na(df[i, "bear"])) {
    } else if (df[i, "bear"] < -pi) {
      df[i, "bear"] <- df[i, "bear"] + 2 * pi
    } else if (df[i, "bear"] > pi) {
      df[i, "bear"] <- df[i, "bear"] - 2 * pi
    }
  }
  if (rm.missing) {
    df <- df[complete.cases(df[ , "ta"]),]
  }
  u = 1
  if(as.deg) { # This will convert the angles to degrees if the user so wishes
    u = 180/pi
    df[, "ta"] <- df[, "ta"] * u
    df[, "bear"] <- df[, "bear"] * u
  }
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

track("gps_utm_easting", "gps_utm_northing", "burst_id", test)

test <- test[-(is.na(test$ta)), ]
