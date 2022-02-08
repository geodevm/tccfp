####################################################################################################################
############## TELONICS.WRANGLER ###################################################################################
####################################################################################################################
telonics.wrangler <- function (path, name, as.tbl = FALSE) { #######################################################
  ##################################################################################################################
  ############ Function specifications #############################################################################
  # Function 'telonics.wrangler' sorts through a directory of Telonics reports, finds the most recent
  #   report for each animal, creates a unique identifier for each collar (ctn), and compiles the data into one
  #   full dataset containing all animals.
  # Variable 'path' is the path to the directory containing all telonics reports.
  # Variable 'name' is the name that will be assigned to the data.frame that is produced by the function.
  # Option 'as.tbl' is whether the user wants a tibble returned rather than data.frame. Defualts to 'as.tbl = 
  # `FALSE', which returns a data.frame. User must have loaded tidyverse/tibble to run 'as.tbl = TRUE'.
  ##################################################################################################################
  ############ Table to be created #################################################################################
  # Table will be compiled data from Telonics reports
  ##################################################################################################################
  ############ FUNCTION ############################################################################################
  f <- list.files(path) # List the files in the Telonics reports directory
  f <- subset(f, !grepl("kml", f)) # Remove the .kml files
  f <- subset(f, !grepl("Stat", f)) # Remove the summary statistic .csv files
  f.f <- c() # Empty vector to concatenate newest files into
  for (i in levels(as.factor(gsub("([0-9]+).*$", "\\1", f)))) {
    # Levels are the numeric portions of the CTN numbers
    r <- f[gsub("([0-9]+).*$", "\\1", f) == i] # Subset files to a single CTN number
    r <- r[as.numeric(gsub("([0-9]+).*$", "\\1", substr(r, 9, 11))) == max(as.numeric(gsub("([0-9]+).*$", "\\1", substr(r, 9, 11))))]
    # Above command gets the maximum broadcast number in the list of files
    f.f <- c(f.f, r) # Concatenate to vector
  }
  csvs <- list() # Empty vector to concatenate data.frames into
  for (i in 1:length(f.f)) {
    csvs[[i]] <- read.csv(f.f[i], header = TRUE, skip = 23, na.strings = "")
    # The 'skip == 23' in read.csv deals with the fact that there are non-R-friendly characters embedded in
    #   Telonics .csv reports
    csvs[[i]]$ctn <- substr(f.f[i], 1, 7) # Create column of CTN numbers as unique identifiers for collars
  }
  newfile <- do.call(rbind,csvs) # Compile all files into one larger data.frame
  # Assign name and create data.frame
  if(as.tbl) {
    newfile <- as_tibble(newfile)
  }
  return(assign(name, newfile, envir = .GlobalEnv))
} ##################################################################################################################
############## END FUNCTION ########################################################################################
####################################################################################################################

telonics.wrangler(path = "U:/research/tccfp/Telonics Data Converter/Reports/", name = "collars")
