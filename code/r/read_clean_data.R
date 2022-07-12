#===============================================================================
### Initialize data from .csv files
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
# Load in packages -------------------------------------------------------------
library(tidyverse)
library(here)
library(lubridate)
# Read in full datasets ========================================================
# This dataset includes all of the additional datasets (metals, fecal, serology)
# appended to the processing data. If you read in this, there's no need to read
# in the more specific datasets unless you want to work with them specifically 
# for some reason. Just run the following two sections of code to get all of our
# data.
# 1. Read in full movement data ------------------------------------------------
movement <- tibble(read.csv(here("data/processed_data/movement_data.csv")))
# Coerce all variables into the right format. This could take a bit.
movement <- movement %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(temperature, gps_latitude, gps_longitude, gps_utm_northing, 
                 gps_utm_easting, gps_altitude, activity_count), 
            funs(as.numeric)) %>%
  mutate_at(vars(activity_fix_time, gps_fix_time, temperature_fix_time, 
                 acquisition_start_time), funs(as_datetime))
message("Warnings about dplyr deprecated functions are ok.")
message("Movement dataset should be >1.6 million obs. of 13 variables")
# 2. Read in full biologicals data ---------------------------------------------
# Get the data from path
biologicals <- tibble(read.csv(here("data/processed_data/biologicals_data.csv")))
# Coerce all variables into the right format
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
                 temp_3_time, temp_4_time, release_time), funs(as_datetime))
message("Biologicals dataset should be 37 obs. of 125 variables")
#===============================================================================
### Read in partial datasets 
#-------------------------------------------------------------------------------
# Read in GPS data -------------------------------------------------------------
gps <- tibble(read.csv(here("data/processed_data/gps_data.csv")))
# Coerce all variables into the right format. This could take a bit.
gps <- gps %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(gps_latitude, gps_longitude, gps_utm_northing, gps_utm_easting, 
                 gps_altitude), 
            funs(as.numeric)) %>%
  mutate_at(vars(gps_fix_time), funs(as_datetime))
message("GPS dataset should be >55000 obs. of 8 variables.")
# Read in activity data --------------------------------------------------------
activity <- tibble(read.csv(here("data/processed_data/activity_data.csv")))
# Coerce all variables into the right format. This could take a bit.
activity <- activity %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(activity_count), funs(as.numeric)) %>%
  mutate_at(vars(activity_fix_time), funs(as_datetime))
message("Activity dataset should be >1.6 million obs. of 4 variables.")
# Read in temperature data -----------------------------------------------------
temperature <- tibble(read.csv(here("data/processed_data/temperature_data.csv")))
# Coerce all variables into the right format. This could take a bit.
temperature <- temperature %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(temperature), funs(as.numeric)) %>%
  mutate_at(vars(temperature_fix_time), funs(as_datetime))
message("Temperature dataset should be >1.6 million obs. of 4 variables.")
# Read in processing data ------------------------------------------------------
processing <- tibble(read.csv(here("data/processed_data/processing_data.csv")))
# Coerce all variables into the right format.
processing <- processing %>%
  mutate_at(vars(animal_id, species, teeth_color, teeth_wear, sagital_crest,
                 sex, age_determination, cod_simple), 
            funs(as.factor)) %>%
  mutate_at(vars(ketamine, xylazine, additional_ketamine, additional_xylazine, 
                 body_length, tail_length, blood_collected, atipamezole, weight,
                 neck_circumference, temp_1, temp_2, temp_3, temp_4), 
            funs(as.numeric)) %>%
  mutate_at(vars(mange, redeploy, fleas, fecal, hair, scat, teeth_cond, 
                 covid_antigen, covid_oral, covid_rectal, covid_nasal, dropped, 
                 redepcollar), 
            funs(as.integer)) %>%
  mutate_at(vars(date_processed, date_inactive), 
            funs(as_date)) %>%
  mutate_at(vars(injection_time, additional_injection_time, induction_time,
                 reversal_time, time_alert, temp_1_time, temp_2_time,
                 temp_3_time, temp_4_time, release_time), 
            funs(as_datetime))
message("Warnings about NA coercion are ok.")
message("Processing dataset should be 37 obs. of 47 variables")
# Read in serology data --------------------------------------------------------
serology <- tibble(read.csv(here("data/processed_data/serology_data.csv")))
# Coerce all variables into the right format.
serology <- serology %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(heartworm_antigen, ehrlichia_antibody, lyme_disease_antibody,
                 anaplasmosis_antibody, l_autumn, l_brat, l_can, l_grip, l_hard, 
                 l_ict, l_pom, t_gondii_igg, t_gondii_igm, parvo_canine,
                 canine_distemper), 
            funs(as.integer))
message("Serology dataset should be 30 obs. of 17 variables")
# Read in metals data ----------------------------------------------------------
metals <- tibble(read.csv(here("data/processed_data/metals_data.csv")))
# Coerce all variables into the right format.
metals <- metals %>%
  mutate_at(vars(animal_id, species), funs(as.factor)) %>%
  mutate_at(vars(hair_sample_g, na, mg, al, p, k, ca, v, cr, mn, fe, co, ni, cu, 
                 zn, as, se, cd, pb), 
            funs(as.numeric))
message("Metals dataset should be 31 obs. of 21 variables")
# Read in fecal data -----------------------------------------------------------
fecal <- tibble(read.csv(here("data/processed_data/fecal_data.csv")))
# Coerce all variables into the right format.
fecal <- fecal %>%
  mutate_at(vars(animal_id, species), 
            funs(as.factor)) %>%
  mutate_at(vars(t_c_s, t_c_s_ct, t_l_s, t_l_s_ct, s_s, s_s_ct, c_s, c_s_ct, 
                 g_z, g_z_ct, s_z, s_z_ct, u_s, u_s_ct, u_z, u_z_ct, t_g_z, 
                 t_g_z_ct, t_s_l_s, t_s_l_s_ct, e_s, e_s_ct, t_g_s, t_g_s_ct, 
                 m_s, m_s_ct, c_z, c_z_ct, cr_s, cr_s_ct, ca_s, ca_s_ct, i_s, 
                 i_s_ct, t_v_s, t_v_s_ct, d_l_s, d_l_s_ct, i_z, i_z_ct, a_c_z, 
                 a_c_z_ct, t_v_z, t_v_z_ct), 
            funs(as.integer))
message("Fecal dataset should be 8 obs. of 46 variables")
# Collect total runtime --------------------------------------------------------
end <- proc.time() - begin
message("The following time elapsed for this script: \n", 
        names(end[1]), ": ", end[[1]], "\n",
        names(end[2]), ":  ", end[[2]], "\n",
        names(end[3]), ":   ", end[[3]])
rm(begin, end)
#===============================================================================