# Filter out bursts
function(x) {
for (i in 1:as.numeric(dim(collars))) {
  if (collars$animal_id[i + 1] != collars$animal_id[i] | as.numeric(dim(collars)) == i) {
    collars$difftime[i] <- NA
  } else if (collars$animal_id[i + 1] == collars$animal_id[i]) {
    collars$difftime[i] <- difftime(collars$acquisition_time[i + 1], collars$acquisition_time[i], units = "hours")
  }
}
for (i in 1:as.numeric(dim(collars))) {
  if (collars$difftime[i] < 4) {
    j = i
    while (collars$difftime[j+1] < 1 | is.na(collars$difftime[j+1])) {
      collars <- collars[-(j+1),]
    }
  }
}
if (collars$difftime[1] < 4) {
  collars <- collars[!1,]
}
collars$difftime <- NULL

# Filter in bursts
for (i in 1:as.numeric(dim(collars))) {
  if (collars$animal_id[i + 1] != collars$animal_id[i] | as.numeric(dim(collars)) == i) {
    collars$difftime[i] <- NA
  } else if (collars$animal_id[i + 1] == collars$animal_id[i]) {
    collars$difftime[i] <- difftime(collars$acquisition_time[i + 1], collars$acquisition_time[i], units = "hours")
  }
}
k = 0
for (i in 1:dim(collars)) {
  if (is.na(collars$difftime[i])) {
    if (is.null(collars$difftime[i + 1])) {
      collars$burst_id[i] <- k
      break
    } else if (collars$difftime[i+1] > 1) {
      j = i  
      while (collars$difftime[j+1] > 1 | is.na(collars$difftime[j+1])) {
        collars <- collars[-(j+1),]
      }
      collars$burst_id[j] <- k
      k = k + 1
    } else {
      collars$burst_id[i] <- k
    }
  } else if (collars$difftime[i] > 1) {
    j = i
    while (collars$difftime[j+1] > 1 | is.na(collars$difftime[j+1])) {
      collars <- collars[-(j+1),]
    }
    collars$burst_id[j] <- k
    k = k + 1
  } else {
    collars$burst_id[i] <- k
  }
}
if (collars$difftime[1] > 1) {
  collars <- collars[-1,]
}
collars$difftime <- NULL


####################################################################################

regular.bursts <- function(anid, df, t, unit, lower, upper) {
  # Variable anid is the unique animal identification number
  # Variable df is the data frame that is being used for the analysis
  # Variable u is the units that will be used in the difftime function
  # Variable t is the fix time
  for (i in 1:dim(df)) {
    if (df[i + 1, anid] != df[i, anid] | dim(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  k = 0
  for (i in 1:dim(df)) {
    if (is.na(df[i, "dt"])) {
      if (is.null(df[i + 1, "dt"]) | is.na(df[i + 1, "dt"])) {
        df[i, "burst_id"] <- k
      } else if (df[i + 1, "dt"] > upper | df[i + 1, "dt"] < lower) {
        j = i  
        while (df[j + 1, "dt"] > upper | df[j + 1, "dt"] < lower | is.na(df[j + 1, "dt"])) {
          df <- df[-(j+1),]
        }
        df[j, "burst_id"] <- k
        k = k + 1
      } else {
        df[i, "burst_id"] <- k
      }
    } else if (df[i, "dt"] > upper | df[i, "dt"] < lower) {
      j = i
      while (df[j + 1, "dt"] > upper | df[j + 1, "dt"] < lower | is.na(df[j + 1, "dt"])) {
        df <- df[-(j+1),]
      }
      df[j, "burst_id"] <- k
      k = k + 1
    } else {
      df[i, "burst_id"] <- k
    }
  }
  if (df[1, "dt"] > upper | df[1, "dt"] < lower) {
    df <- df[-1,]
  }
  df[, "dt"] <- NULL
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

fine.bursts <- function(anid, df, t, unit, up) {
  # Variable anid is the unique animal identification number
  # Variable df is the data frame that is being used for the analysis
  # Variable u is the units that will be used in the difftime function
  # Variable t is the fix time
  for (i in 1:nrow(df)) {
    if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  k = 0
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])) {
      if (is.null(df[i + 1, "dt"]) | is.na(df[i + 1, "dt"])) {
        df[i, "burst_id"] <- k
        break
      } else if (df[i + 1, "dt"] > up) {
        j = i  
        while (df[j + 1, "dt"] > up | is.na(df[j + 1, "dt"])) {
          df <- df[-(j+1),]
        }
        df[j, "burst_id"] <- k
        k = k + 1
      } else {
        df[i, "burst_id"] <- k
      }
    } else if (df[i, "dt"] > up) {
      j = i
      while (df[j + 1, "dt"] > up | is.na(df[j + 1, "dt"])) {
        df <- df[-(j+1),]
      }
      df[j, "burst_id"] <- k
      k = k + 1
    } else {
      df[i, "burst_id"] <- k
    }
  }
  if (df[1, "dt"] > up) {
    df <- df[-1,]
  }
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

fine.bursts("animal_id", collars, "acquisition_time", "hours", 7)

large.bursts <- function(anid, df, t, unit, low) {
  # Variable anid is the unique animal identification number
  # Variable df is the data frame that is being used for the analysis
  # Variable u is the units that will be used in the difftime function
  # Variable t is the fix time
  for (i in 1:nrow(df)) {
    if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  k = 0
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])) {
      if (is.null(df[i + 1, "dt"]) | is.na(df[i + 1, "dt"])) {
        df[i, "burst_id"] <- k
        break
      } else if (df[i + 1, "dt"] < low) {
        j = i  
        while (df[j + 1, "dt"] < low | is.na(df[j + 1, "dt"])) {
          df <- df[-(j+1),]
        }
        df[j, "burst_id"] <- k
        k = k + 1
      } else {
        df[i, "burst_id"] <- k
      }
    } else if (df[i, "dt"] < low) {
      j = i
      while (df[j + 1, "dt"] < low | is.na(df[j + 1, "dt"])) {
        df <- df[-(j+1),]
      }
      df[j, "burst_id"] <- k
      k = k + 1
    } else {
      df[i, "burst_id"] <- k
    }
  }
  if (df[1, "dt"] < low) {
    df <- df[-1,]
  }
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

large.bursts("animal_id", collars, "acquisition_time", "hours", 5)

#######################


bursts <- function(anid, df, t, unit, low, up) {
  # Variable anid is the unique animal identification number
  # Variable df is the data frame that is being used for the analysis
  # Variable u is the units that will be used in the difftime function
  # Variable t is the fix time
  for (i in 1:nrow(df)) {
    if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])){
    } else if (df[i, "dt"] < low) {
      j = i
      while (df[j + 1, "dt"] < low | is.na(df[j + 1, "dt"])) {
        df <- df[-(j + 1),]
      }
    }
  }
  k = 0
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])) {
      if (is.null(df[i + 1, "dt"]) | is.na(df[i + 1, "dt"])) {
        df[i, "burst_id"] <- k
        break
      } else if (df[i + 1, "dt"] > up) {
        j = i  
        while (df[j + 1, "dt"] > up | is.na(df[j + 1, "dt"])) {
          df <- df[-(j+1),]
        }
        df[j, "burst_id"] <- k
        k = k + 1
      } else {
        df[i, "burst_id"] <- k
      }
    } else if (df[i, "dt"] > up) {
      j = i
      while (df[j + 1, "dt"] > up | is.na(df[j + 1, "dt"])) {
        df <- df[-(j+1),]
      }
      df[j, "burst_id"] <- k
      k = k + 1
    } else {
      df[i, "burst_id"] <- k
    }
  }
  if (df[1, "dt"] > up) {
    df <- df[-1,]
  }
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

bursts("animal_id", test, "acquisition_time", "hours", 4, 10)

allalallal

bursts <- function(anid, df, t, unit, low) {
  # Variable anid is the unique animal identification number
  # Variable df is the data frame that is being used for the analysis
  # Variable u is the units that will be used in the difftime function
  # Variable t is the fix time
  for (i in 1:nrow(df)) {
    if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])){
    } else if (df[i, "dt"] < low) {
      j = i
      while (df[j + 1, "dt"] < 1 | is.na(df[j + 1, "dt"])) {
        df <- df[-(j + 1),]
      }
    }
    if (df[1, "dt"] < low) {
      df <- df[-1,]
    }
  }
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}

bursts("animal_id", test, "acquisition_time", "hours", 4)


for (i in nrow(df)) {
  if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
    df[i, "dt"] <- NA
  } else if (df[i + 1, anid] == df[i, anid]) {
    df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
  }
}
for (i in 1:nrow(df)) {
  if (df[i, "dt"] < low) {
    j = i
    while (df[j+1, "dt"] < low | is.na(df[j+1, "dt"])) {
      df <- df[-(j+1),]
    }
  }
}
if (df[1, "dt"] < low) {
  df <- df[!1,]
}
df[, "dt"] <- NULL

#########
bursts <- function(anid, df, t, unit, low) {
  for (i in 1:nrow(df)) {
    if (df[i + 1, anid] != df[i, anid] | nrow(df) == i) {
      df[i, "dt"] <- NA
    } else if (df[i + 1, anid] == df[i, anid]) {
      df[i, "dt"] <- difftime(df[i + 1, t], df[i, t], units = unit)
    }
  }
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])) {
    } else if (df[i, "dt"] < low) {
      j = i
      while (df[j + 1, "dt"] < low | is.na(df[j+1, "dt"])) {
        df <- df[-(j+1),]
      }
    }
  }
  df[, "dt"] <- NULL
  newfile <- df
  return(assign("test", newfile, envir = .GlobalEnv))
}


bursts("animal_id", collars, "acquisition_time", "hours", 1)


