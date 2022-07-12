# Get the data from path
biologicals <- tibble(read.csv(here("data/processed_data/biologicals_data.csv")))
# Coerce all variables into the right format
biologicals <- biologicals %>%
  mutate_at(vars(animal_id, species, teeth_color, teeth_wear, sagital_crest,
                 sex, age_determination), 
            funs(as.factor)) %>%
  mutate_at(vars(ketamine, xylazine, additional_ketamine, additional_xylazine, 
                 body_length, tail_length, blood_collected, atipamezole, weight, 
                 hair_sample_g, na, mg, al, p, k, ca, v, cr, mn, fe, co, ni, cu, 
                 zn, as, se, cd, pb, neck_circumference, temp_1, temp_2, temp_3, 
                 temp_4, weight_mg, n_percent, n15_v_at_air, c_percent, 
                 c13_v_vpdb), 
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
message("Biologicals dataset should be 37 obs. of 138 variables")
# 
imp_averages <- read.csv(here("data/processed_data/imp_averages.csv")) %>%
  rename(imp = dem)

biologicals <- biologicals %>%
  left_join(imp_averages, by = "animal_id")

ggplot(biologicals, aes(x = n15_v_at_air, y = c13_v_vpdb)) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis() +
  facet_grid(~species)

mtls <- 57:74
for (i in mtls) {
  plt_dat <- biologicals[, c(1, 2, i, 139)]
  plt <- ggplot(plt_dat, aes_string(x = names(plt_dat)[3], y = "imp")) +
    geom_point(aes(size = imp, color = imp)) +
    coord_flip() + 
    theme_bw() +
    ggtitle("") +
    scale_color_viridis() +
    facet_grid(~species)
  print(plt)
}


plt_dat <- biologicals[, c(1, 2, i, 139)]
plt <- ggplot(plt_dat, aes_string(x = names(plt_dat)[3], y = "imp")) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis() +
  facet_grid(~species)
print(plt)

biologicals$lepto <- NA
biologicals$toxo <- NA

for (i in 1:nrow(biologicals)) {
  if (is.na(biologicals$l_autumn[i])) {
    next
  } else if (sum(biologicals[i, 79:85]) >= 1) {
    biologicals$lepto[i] <- 1
  } else {
    biologicals$lepto[i] <- 0
  }
  if (is.na(biologicals$t_gondii_igg[i]) & is.na(biologicals$t_gondii_igm[i])) {
    next
  } else if (is.na(biologicals$t_gondii_igg[i]) & 
             !is.na(biologicals$t_gondii_igm[i]) &
             biologicals$t_gondii_igm[i] == 1) {
    biologicals$toxo[i] <- 1
  } else if (!is.na(biologicals$t_gondii_igg[i]) & 
             is.na(biologicals$t_gondii_igm[i]) &
             biologicals$t_gondii_igg[i] == 1) {
    biologicals$toxo[i] <- 1
  } else if (sum(biologicals[i, 86:87]) >= 1) {
    biologicals$toxo[i] <- 1
  } else {
    biologicals$toxo[i] <- 0
  }
}
serology <- biologicals %>% 
  select(animal_id, species, heartworm_antigen, ehrlichia_antibody, 
         lyme_disease_antibody, anaplasmosis_antibody, lepto, toxo, 
         parvo_canine, canine_distemper, imp) %>%
  na.omit()
for (i in 1:nrow(serology)) {
  serology$richness[i] <- sum(serology[i, 3:10])
}
ggplot(serology, aes(x = richness, y = imp)) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis() +
  facet_grid(~species)


for (i in 3:10) {
  plt <- ggplot(serology, aes_string(x = names(serology)[i], y = "imp")) +
    geom_point(aes(size = imp, color = imp)) +
    coord_flip() + 
    theme_bw() +
    ggtitle("") +
    scale_color_viridis() +
    facet_grid(~species)
  print(plt)
}


ggplot(biologicals, aes(x = mange, y = imp)) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis() +
  facet_grid(~species)

ggplot(biologicals, aes(x = fleas, y = imp)) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis() +
  facet_grid(~species)

fecals <- biologicals %>%
  filter(!is.na(t_l_s))

fcl <- seq(90, 132, 2)

for (i in 1:nrow(fecals)) {
  fecals$fecal_richness[i] <- sum(fecals[i ,fcl], na.rm = TRUE)
}

ggplot(fecals[fecals$species == "coyote", ], aes(x = fecal_richness, y = imp)) +
  geom_point(aes(size = imp, color = imp)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("") +
  scale_color_viridis()

river <- c(0, 0, 0, 1, 1, 1, NA, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1, 
           NA, NA, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0)
biologicals <- biologicals %>%
  bind_cols(as.vector(river))
names(biologicals)[142] <- "river"


mtls <- 57:74
for (i in mtls) {
  plt_dat <- biologicals[, c(1, 2, i, 139)]
  plt <- ggplot(plt_dat, aes_string(x = names(plt_dat)[3], y = "river")) +
    geom_point(aes(size = imp, color = imp)) +
    coord_flip() + 
    theme_bw() +
    ggtitle("") +
    scale_color_viridis() +
    facet_grid(~species)
  print(plt)
}



