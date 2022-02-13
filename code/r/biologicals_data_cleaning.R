#===============================================================================
### Data cleaning protocol for biological data
# Load packages ----------------------------------------------------------------
library(here)
library(tidyverse)
library(lubridate)
# Load in data -----------------------------------------------------------------
# Using .csv format, load in all biologicals data
biologicals <- tibble(read.csv(here("data/raw_data/processing_raw.csv"), 
                               header = T, sep = ",", na.strings = NA))
metals <- tibble(read.csv(here("data/raw_data/metals_raw.csv"), header = T, 
                          sep = ",", na.strings = NA))
serology <- tibble(read.csv(here("data/raw_data/serology_raw.csv"), header = T, 
                            sep = ",", na.strings = NA))
fecal <- tibble(read.csv(here("data/raw_data/fecal_raw.csv"), header = T, 
                         sep = ",", na.strings = NA))
#isotope <- tibble(read.csv(here("data/raw_data/isotope_raw.csv"), header = T, 
#                           sep = ",", na.strings = NA))
# Cleaning processing data -----------------------------------------------------
# Formatting weight so that all are in kilograms
# If weight is in lbs, multiply by 0.453592 to get kilograms
biologicals$weight <- ifelse(biologicals$weight_unit == "lbs", 
                             biologicals$weight * 0.453592, 
                             biologicals$weight)
# Subtract collar weight if needed
if (biologicals$with_collar == "Y") {
  ifelse(biologicals$collar_type == "c", biologicals$weight[i] - 0.31,
         biologicals$weight[i] - 0.13)
}
# Round weights to 2 digits
biologicals$weight <- format(round(biologicals$weight, 2), nsmall = 2)
# Change the date and time format
biologicals$date_processed <- mdy(biologicals$date_processed)
biologicals$date_released <- mdy(biologicals$date_released)
biologicals$date_inactive <- mdy(biologicals$date_inactive)
# Create datetime for release
for (i in 1:nrow(biologicals)) {
  if (is.na(biologicals$date_released[i]) & 
      is.na(biologicals$time_released[i])) {
    biologicals$release_time[i] <- NA
  } else if (!is.na(biologicals$date_released[i]) & 
             is.na(biologicals$time_released[i])) {
    biologicals$release_time[i] <- paste((biologicals$date_released[i] + 
                                          days(1)), "00:00:00")
  } else {
    biologicals$release_time[i] <- paste(biologicals$date_released[i], " ", 
                                         biologicals$time_released[i], ":00", 
                                         sep = "")
  } 
}
rm(i)
biologicals$release_time <- biologicals$release_time %>%
  ymd_hms(tz = "America/Chicago") %>%
  with_tz(tzone = "UTC")
# Convert other times that will be retained into datetimes
datetimes <- c("injection_time", "additional_injection_time", "induction_time",
               "reversal_time", "time_alert", "temp_1_time", "temp_2_time",
               "temp_3_time", "temp_4_time")
# One NA observation was not entered in correct format
biologicals$time_alert[29] <- NA
# Loop over columns, converting to datetime
for (i in datetimes) {
  biologicals$ref <- NA
  biologicals[, "ref"] <- biologicals[, i]
  biologicals[, i] <- ifelse(is.na(biologicals[, i]), NA, 
                             paste(biologicals$date_processed, " ",
                                   biologicals$ref, ":00", sep = ""))
  biologicals$ref <- NA
  biologicals[, "ref"] <- biologicals[, i]
  biologicals[, i] <- biologicals$ref %>%
    ymd_hms(tz = "America/Chicago") %>%
    with_tz(tzone = "UTC")
  biologicals$ref <- NULL
}
rm(datetimes, i)
# Convert Fahrenheit to Celsius
# (F-32)/1.8
# Since all were measured in Fahr, all can be converted, one decimal
biologicals$temp_1 <- format(round((as.numeric(biologicals$temp_1) - 32) / 1.8), 
                             1, nsmall = 1)
biologicals$temp_2 <- format(round((as.numeric(biologicals$temp_2) - 32) / 1.8), 
                             1, nsmall = 1)
biologicals$temp_3 <- format(round((as.numeric(biologicals$temp_3) - 32) / 1.8),
                             1, nsmall = 1)
biologicals$temp_4 <- format(round((as.numeric(biologicals$temp_4) - 32) / 1.8), 
                             1, nsmall = 1)
# Convert Y/N to 1/0 for analyses
biologicals$injuries <- ifelse(biologicals$injuries == "N", 0, 1)
biologicals$teeth_cond <- ifelse(biologicals$teeth_cond == "N", 0, 1)
biologicals$mange <- ifelse(biologicals$mange == "N", 0, 1)
biologicals$fecal <- ifelse(biologicals$fecal == "N", 0, 1)
biologicals$hair <- ifelse(biologicals$hair == "N", 0, 1)
biologicals$scat <- ifelse(biologicals$scat == "N", 0, 1)
biologicals$redeploy <- ifelse(biologicals$redeploy == "N", 0, 1)
biologicals$dropped <- ifelse(biologicals$dropped == "N", 0, 1)
# Add a fleas column
biologicals$fleas <- ifelse(biologicals$parasites == "N", 0, 1)
# All covid tests were negative, so we can just fill in 0's for any observation
# since there was no covid antigen y/n variable, it can be based on the nasal
# swabs, which were collected for each individual.
biologicals$covid_oral <- ifelse(!is.na(biologicals$oral_covid_id), 0, NA)
biologicals$covid_rectal <- ifelse(!is.na(biologicals$rectal_covid_id), 0, NA)
biologicals$covid_nasal <- ifelse(!is.na(biologicals$nasal_covid_id), 0, NA)
biologicals$covid_antigen <- ifelse(!is.na(biologicals$nasal_covid_id), 0, NA)
# Filter out columns no longer useful for analyses or that will be redundant 
# when additional data is joined.
drops <- c("observer_init", "location", "injuries", "injuries_comments", 
           "gps_init", "ketamine_units", "xylazine_units", 
           "additional_ketamine_units", "additional_xylazine_units",
           "immobilization_comments", "weight_unit", "with_bag", "bag_weight",
           "bag_weight_unit", "collar_type", "with_collar",
           "neck_circumference_units", "body_length_units", "tail_length_units",
           "parasites", "parasites_comments", "mange_comments", 
           "condition_comments", "collar_time", "collar_comments", 
           "blood_collected_units", "photos", "photos_comments", 
           "samples_comments", "atipamezole_units", "date_released", 
           "cause_of_death", "time_released", "reversal_comments", 
           "temp_unit_all", "general_comments", "nasal_covid_id", 
           "oral_covid_id", "rectal_covid_id", "human_1_init", "human_1_id", 
           "human_2_init", "human_2_id", "human_3_init", "human_3_id")
biologicals <- biologicals[, !(names(biologicals) %in% drops)]
rm(drops)
# change column name for id to be common between data sets
names(biologicals)[names(biologicals) == "identification"] <- "animal_id"
# Export just the cleaned processing data
biologicals %>%
  relocate(c("animal_id", "species")) %>%
  write.csv(here("data/processed_data/processing_data.csv"), row.names = FALSE)
# Cleaning the metals data -----------------------------------------------------
# We don't need all of this data to be appended, so exclude some of the columns.
# Also drop the species variable as it will be redundant when joined.
drops <- c("empty_tube_g", "plus_hair_g", "hno3_ul", "h2o2_ul", 
           "h2o_ml", "plus_h2o_g", "icp_sol_g")
metals <- metals[, !(names(metals) %in% drops)]
rm(drops)
# Export just the cleaned metals data
metals %>%
  relocate(c("animal_id", "species")) %>%
  write.csv(here("data/processed_data/metals_data.csv"), row.names = FALSE)
# Also drop the species variable as it will be redundant when joined.
metals <- metals[, !(names(metals) == "species")]
# Cleaning the serology data ---------------------------------------------------
# change column name for id to be common between data sets
names(serology)[names(serology) == "id"] <- "animal_id"
# We don't need all of this data to be appended, so exclude some of the columns.
# Also drop the species variable as it will be redundant when joined.
drops <- c("l_autumn_titer", "l_brat_titer", "l_can_titer", 
           "l_grip_titer", "l_hard_titer", "l_ict_titer", "l_pom_titer",
           "t_gondii_igg_titer", "t_gondii_igm_titer", "parvo_canine_titer",
           "parvo_canine_comment", "canine_distemper_titer", 
           "canine_distemper_comment")
serology <- serology[, !(names(serology) %in% drops)]
rm(drops)
# Export just the cleaned serology data
serology %>%
  relocate(c("animal_id", "species")) %>%
  write.csv(here("data/processed_data/serology_data.csv"), row.names = FALSE)
# Also drop the species variable as it will be redundant when joined.
serology <- serology[, !(names(serology) == "species")]
# Cleaning the fecal data ------------------------------------------------------
# We don't need all of this data to be appended, so exclude some of the columns.
# Also drop the species variable as it will be redundant when joined.
drops <- c("date_fecal_processed", "observers", "total", "zinc", "sucrose", 
           "freezer", "comments")
fecal <- fecal[, !(names(fecal) %in% drops)]
rm(drops)
# Remove rows where tests are all NA due to being stored for further processing.
fecal <- fecal[-which(is.na(fecal[,4:46])),]
# Export just the cleaned fecal data
fecal %>%
  relocate(c("animal_id", "species")) %>%
  write.csv(here("data/processed_data/fecal_data.csv"), row.names = FALSE)
# Also drop the species variable as it will be redundant when joined.
fecal <- fecal[, !(names(fecal) == "species")]
# Export the full biologicals dataset ------------------------------------------
# Join the metals and serology data to the processing data
biologicals <- biologicals %>%
  left_join(metals, by = "animal_id") %>%
  left_join(serology, by = "animal_id") %>%
  left_join(fecal, by = "animal_id") #%>%
#  left_join(isotope, by = "animal_id")
rm(metals)
rm(serology)
rm(fecal)
#rm(isotope)
# Coerce everything into the right format
biologicals <- biologicals %>%
  mutate_at(vars(animal_id, species, teeth_color, teeth_wear, sagital_crest,
                 sex, age_determination, cod_simple), 
            funs(as.factor)) %>%
  mutate_at(vars(ketamine, xylazine, additional_ketamine, additional_xylazine, 
                 body_length, tail_length, blood_collected, atipamezole, weight, 
                 hair_sample_g, na, mg, al, p, k, ca, v, cr, mn, fe, co, ni, cu, 
                 zn, as, se, cd, pb, neck_circumference, temp_1, temp_2, temp_3, 
                 temp_4), 
            funs(as.numeric)) %>%
  mutate_at(vars(heartworm_antigen, ehrlichia_antibody, lyme_disease_antibody,
                 anaplasmosis_antibody, l_autumn, l_brat, l_can, l_grip, l_hard, 
                 l_ict, l_pom, t_gondii_igg, t_gondii_igm, parvo_canine,
                 canine_distemper, mange, redeploy, fleas, fecal, hair, scat, 
                 teeth_cond, covid_antigen, covid_oral, covid_rectal, 
                 covid_nasal, dropped, redepcollar, t_c_s, t_c_s_ct, t_l_s, 
                 t_l_s_ct, s_s, s_s_ct, c_s, c_s_ct, g_z, g_z_ct, s_z, s_z_ct, 
                 u_s, u_s_ct, u_z, u_z_ct, t_g_z, t_g_z_ct, t_s_l_s, t_s_l_s_ct,
                 e_s, e_s_ct, t_g_s, t_g_s_ct, m_s, m_s_ct, c_z, c_z_ct, cr_s,
                 cr_s_ct, ca_s, ca_s_ct, i_s, i_s_ct, t_v_s, t_v_s_ct, d_l_s, 
                 d_l_s_ct, i_z, i_z_ct, a_c_z, a_c_z_ct, t_v_z, t_v_z_ct), 
            funs(as.integer)) %>%
  mutate_at(vars(date_processed, date_inactive), 
            funs(as.Date)) %>%
  mutate_at(vars(injection_time, additional_injection_time, induction_time,
                 reversal_time, time_alert, temp_1_time, temp_2_time,
                 temp_3_time, temp_4_time, release_time), 
            funs(as.POSIXct))
message(
  "The warning messages for the 'temp_*' and 'redpocollar' columns are fine. Warnings about dplyr deprecated functions are also ok."
)
# Reorder the columns to be more intuitive
biologicals <- biologicals %>%
  relocate(c("animal_id", "species"))
# Export the csv
biologicals %>%
  write.csv(here("data/processed_data/biologicals_data.csv"), row.names = FALSE)
#===============================================================================