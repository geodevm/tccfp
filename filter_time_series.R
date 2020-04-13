##############INTERVALIZER######################################################################################
################################################################################################################
intervalizer <- function(df, name, an.id, t.col, lower, upper, unit) { #############################################
  ##############################################################################################################
  ############ Function specifications #########################################################################
  # Function 'intervalizer' will take time series data and make it into equal interval 
  # bursts by animal
  # Variable 'df' is the data frame that is being used for the analysis
  # Variable 'name' is the name that will be specified for the new table
  #   defaults to name of input 'df', which will replace input table
  # Variable 'an.id' is the column containing unique animal identification number
  # Variable 't.col' is the column containing the raw time sequence data
  # Variable 'upper' is the upper limit of acceptable values for the interval
  # Variable 'lower' is the lower limit of acceptable values for the interval
  # Variable u is the units that will be used in the difftime function ("hours", "mins")
  ##############################################################################################################
  ############ Columns to be created ###########################################################################
  # Column 'burst_id' will contain unique identifiers for bursts for later analyses
  ##############################################################################################################
  ############ Function ########################################################################################
  if(class(df)[2] == "tbl") {
    df <- as.data.frame(df)
    tbl = TRUE
  }
  for (i in 1:nrow(df)) {
    if (df[i + 1, an.id] != df[i, an.id] | nrow(df) == i) { 
      # Inputs an NA where there is a transition between animals
      df[i, "dt"] <- NA
    } else if (df[i + 1, an.id] == df[i, an.id]) { 
      # Calculates the time difference between the current point and the next observation
      df[i, "dt"] <- difftime(df[i + 1, t.col], df[i, t.col], units = unit)
    }
  }
  for (i in 1:nrow(df)) {
    if (is.na(df[i, "dt"])) {
      # Do nothing if there is an NA
    } else if (df[i, "dt"] < lower) {
      # Remove values that are lower than the specified interval
      j = i
      while (df[j+1, "dt"] < lower | is.na(df[j+1, "dt"])) { 
        # This will delete all values lower than 'lower' until something higher than 'lower'
        # is encountered
        if (is.na(df[j+2, "dt"])) { 
          # This takes care of when the end of the table is reached so there is not an 
          # eternal while
          df <- df[-(j+2),] # Remove the row two ahead
          df <- df[-(j+1),] # Remove next row
          break # Break while
        } else {
          # This will be the condition for every situation except the end of the table,
          # delete following row while condition is met
          df <- df[-(j+1),]
        }
      }
    }
  }
  k = 0 # Set k to 0 for numbering burst id rows
  for (i in 1:nrow(df)) {
    # This will filter out higher values while naming bursts
    if (is.na(df[i, "dt"])) {
      # NA can signify multiple things in this case
      if (is.null(df[i + 1, "dt"]) | is.na(df[i + 1, "dt"])) {
        # Condition for the end of the table so there is no possibility of an eternal while
        df[i, "burst_id"] <- k
        break
      } else if (df[i + 1, "dt"] > upper) {
        # Remove values higher than upper until condition is no longer met
        j = i  
        while (df[j + 1, "dt"] > upper | is.na(df[j + 1, "dt"])) {
          df <- df[-(j+1),]
        }
        df[j, "burst_id"] <- k # Assign burst id number
        k = k + 1 # Increase for next assignment
      } else {
        # Assign burst id for all values within range
        df[i, "burst_id"] <- k
      }
    } else if (df[i, "dt"] > upper) {
      # Remove values higher than upper until condition is no longer met
      j = i
      while (df[j + 1, "dt"] > upper | is.na(df[j + 1, "dt"])) {
        df <- df[-(j+1),]
      }
      df[j, "burst_id"] <- k
      k = k + 1
    } else {
      # Assign burst id for all values within range
      df[i, "burst_id"] <- k
    }
  }
  if (df[1, "dt"] > upper | df[1, "dt"] < lower) {
    # If the first value in the table is not within the interval, remove it
    df <- df[-1,]
  }
  # Remove the row of time differences
  df[, "dt"] <- NULL
  # Assign a file to be returned
  if(tbl) {
    df <- as_tibble(df)
  }
  newfile <- df
  # Make relevant assignments
  return(assign(name, newfile, envir = .GlobalEnv))
} ##############################################################################################################
################################################################################################################