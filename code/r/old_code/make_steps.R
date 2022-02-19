####################################################################################################################
############## TRACKER #############################################################################################
####################################################################################################################
tracker <- function (df, track.name, x, y, bid, as.deg = FALSE, as.comp = FALSE, rm.missing = FALSE, too.few = 1) {#
  ##################################################################################################################
  ############ Function specifications #############################################################################
  # Variable 'df' is the data being used for the calculation.
  # Variable 'track.name' is the name to be specified for the new table that is generated from running this 
  #   function
  # Variable 'x' is UTM easting.
  # Variable 'y' is UTM northing.
  # Variable 'bid' is the column containing burst identification column.
  # Option 'as.deg = FALSE' is default, keeping the units in radians rather than degrees.
  # Option 'as.comp = FALSE' is default, which keeps the radians on a (+/- pi) scale rather than a 2pi scale.
  # Option 'rm.missing = FALSE' is the default, which keeps all data. rm.missing = TRUE will remove values 
  #   without turn angles.
  # Variable 'too.few' will specify the fewest number of bursts that the user wants out of the function. For 
  #   example, 'too.few = 3' when 'rm.missing = FALSE' would return all bursts with more than three points. 
  #   The same specification with 'rm.missing = TRUE' would return all bursts with 3 or more recorded turn angles.
  #   Default value is 'too.few = 3'.
  ##################################################################################################################
  ############ Columns to be created ###############################################################################
  # Column 'ta' is turn angle in (default) radians (+/- pi).
  # Column 'sl' is step length.
  # Column 'bear' is the bearing in (default) radians (+/- pi)
  ##################################################################################################################
  ############ FUNCTION ############################################################################################
  if(class(df)[1] == "tbl_df") {
    # If a tibble was entered, convert to a data.frame
    df <- as.data.frame(df)
    tbl = TRUE
  } else {
    tbl = FALSE
  }
  for (i in 1:nrow(df)) {
    if (i == 1) {
      # The first row in the table will clearly be NA for all
      df[i, "ta"] <- NA
      df[i, "sl"] <- NA
      df[i, "bear"] <- NA
    } else if (i == 2) { 
      # If you have only the first two points in a burst (this case second of the table), you can't calculate
      #   the turn angle
      xy2 <- c(df[i, x], df[i, y]) # XY coords of current point
      xy1 <- c(df[i - 1, x], df[i - 1, y]) # XY coords of previous point
      xylen1 <- xy2 - xy1 # Distance between them
      s <- sign(xylen1[1]) # This will find the sign of the x difference
      s[s == 0] <- 1  # And correct for when the sign is 0
      ang = s * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])  # Calculate the compass bearing
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
      s2 <- sign(xylen2[1])
      s2[s2 == 0] <- 1
      ang2 = s * (xylen2[2] < 0) * pi + atan(xylen2[1] / xylen2[2])
      if(is.data.frame(xylen2)) {
        # Calculate the current step length (net squared displacement from previous)
        sl_ <- sqrt(rowSums(xylen2^2))
      } else {
        sl_ <- sqrt(sum(xylen2^2))
      }
      ta_ = (ang2 - ang1) # Calculate turn angle
      df[i, "ta"] <- ta_
      df[i, "sl"] <- sl_
      df[i, "bear"] <- ang2 # Bearing of current step
    }
  }
  if (as.comp) {
    # Corrections for if there are negative pi values and a 2pi scale is desired
    for (i in 1:nrow(df)) {
      if (is.na(df[i, "ta"])) {
      } else if (df[i, "ta"] < 0) {
        df[i, "ta"] <- df[i, "ta"] + (2 * pi)
      }
      if (is.na(df[i, "bear"])) {
      } else if (df[i, "bear"] < 0) {
        df[i, "bear"] <- df[i, "bear"] + 2 * pi
      }
    }
  } else {
    # Corrections for if a +/- pi scale is desired
    for (i in 1:nrow(df)) {
      if (is.na(df[i, "ta"])) {
      } else if (df[i, "ta"] < -pi) {
        df[i, "ta"] <- -(df[i, "ta"] + 2 * pi)
      } else if (df[i, "ta"] > pi) {
        df[i, "ta"] <- -(df[i, "ta"] - 2 * pi)
      }
      if (is.na(df[i, "bear"])) {
      } else if (df[i, "bear"] < -pi) {
        df[i, "bear"] <- -(df[i, "bear"] + 2 * pi)
      } else if (df[i, "bear"] > pi) {
        df[i, "bear"] <- -(df[i, "bear"] - 2 * pi)
      }
    }
  }
  if (rm.missing) {
    # Removes any observations without a turn angle
    df <- df[complete.cases(df[ , "ta"]),]
  }
  u = 1
  if(as.deg) { 
    # This will convert the angles to degrees if the user so wishes
    u = 180/pi
    df[, "ta"] <- df[, "ta"] * u
    df[, "bear"] <- df[, "bear"] * u
  }
  df[, bid] <- as.factor(df[, bid]) # Make 'burst_id' a factor
  for (i in levels(df[, bid])) {
    # Remove bursts with less than too.few observations
    if (sum(as.numeric(df[, bid] == i)) < too.few) {
      df <- df[!df[, bid] == i,]
    }
  }
  # I made a mistake that shifts the output ahead one iteration number, so this
  # code is a fill in to shift it back one cell so I don't have to change the
  # whole algorithm..
  for (i in 1:(nrow(df) - 1)) {
    df[i, "ta"] <- df[i + 1, "ta"]
    df[i, "sl"] <- df[i + 1, "sl"]
    df[i, "bear"] <- df[i + 1, "bear"]
  }
  df[nrow(df), "ta"] <- NA
  df[nrow(df), "sl"] <- NA
  df[nrow(df), "bear"] <- NA
  if(tbl) {
    # Return a tibble if a tibble was entered
    df <- as_tibble(df)
  }
  newfile <- df
  return(assign(track.name, newfile, envir = .GlobalEnv))
} ##################################################################################################################
############## END FUNCTION ########################################################################################
####################################################################################################################
