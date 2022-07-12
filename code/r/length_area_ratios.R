library(sf)
library(nngeo)

study_area <- st_read(here("data/processed_data/shps/study_area.shp"), 
                       quiet = TRUE)
home_ranges <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(season != "annual" & method == "kde_ani_95") %>%
  select(!area)
home_ranges[(home_ranges$animal_id == "F7_1" | 
               home_ranges$animal_id == "F7_2"), ]$animal_id <- "F7"
home_ranges[(home_ranges$animal_id == "C9_1" | 
               home_ranges$animal_id == "C9_2"), ]$animal_id <- "C9"

ids <- levels(as.factor(as.character(home_ranges$animal_id)))
seasons <- levels(as.factor(as.character(home_ranges$season)))
for (i in ids) {
  for (j in seasons) {
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) > 1) {
      home_ranges[home_ranges$animal_id == i & 
                    home_ranges$season == j, ]$geometry <- st_union(home_ranges[home_ranges$animal_id == i & 
                                                                                  home_ranges$season == j, ])
    }
  }
}

for (i in 1:nrow(home_ranges)) {
  home_ranges$season_pooled[i] <-  strsplit(home_ranges$season[i], "[_]")[[1]][1]
}

home_ranges <- home_ranges %>%
  distinct()
ratios <- c()
for (i in 1:nrow(home_ranges)) {
  max_dist <- home_ranges$geometry[i] %>% 
    st_cast("POINT") %>% # turn polygon into points
    st_distance() %>% # calculate distance matrix
    as.data.frame() %>% 
    gather(point_id, dist) %>% # convert to long format
    pull(dist) %>% # keep only distance column
    max()
  area <- home_ranges$geometry[i] %>%
    st_area()
  home_ranges$ratio[i] <- max_dist / sqrt(area)
}

for (i in 1:nrow(home_ranges)) {
  if (grepl("C", home_ranges$animal_id[i])) {
    home_ranges$species[i] <- "coyote"
  } else if (home_ranges$animal_id[i] == "F10" |
             home_ranges$animal_id[i] == "F18") {
    home_ranges$species[i] <- "gray fox"
  } else {
    home_ranges$species[i] <- "red fox"
  }
}


rotation = function(a) {
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 




ellipses <- tribble(
  ~animal_id, ~season, ~geometry, ~case, 
)

for (i in ids) {
  for (j in seasons) {
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) == 0) {
      next
    }
    k <- 100
    species <- home_ranges[home_ranges$animal_id == i & 
                             home_ranges$season == j, ]$species
    season <- home_ranges[home_ranges$animal_id == i & 
                          home_ranges$season == j, ]$season_pooled
    tbl_control <- tribble(
      ~animal_id, ~season, ~geometry, ~case
    )
    while (k > 0) {
      x <- c()
      area <- c()
      l <- k
      while (l > 0) {
        den <- density(home_ranges[home_ranges$species == species, ]$ratio)
        newx <- sample(home_ranges[home_ranges$species == species, ]$ratio, l, 
                       replace=TRUE) + rnorm(l, 0, den$bw)
        newx <- newx[newx >= (2 / sqrt(pi))]
        x <- c(x, newx)
        l <- l - length(newx)
      }
      l <- k
      while (l > 0) {
        area_den <- density(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                             home_ranges$season_pooled == 
                                                             season, ]$geometry)))
        new_area <- sample(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                        home_ranges$season_pooled == 
                                                        season, ]$geometry)), l, 
                       replace=TRUE) + rnorm(l, 0, area_den$bw)
        new_area <- new_area[new_area > 0]
        area <- c(area, new_area)
        l <- l - length(new_area)
      }
      angle <- runif(k, 0, 360)
      pts <- st_sample(study_area, k)
      a <- as.numeric((x * sqrt(area)) / 2)
      b <- as.numeric(area / (a * pi))
      tbl <- tibble(ex = a, ey = b, angle = angle, geometry = pts) %>%
        st_as_sf()
      for (m in 1:k) {
        tbl$geometry[m] <- st_multipolygon((st_ellipse(pnt = tbl$geometry[m], 
                                                       ex = tbl$ex[m], 
                                                       ey = tbl$ey[m]) -
                                              tbl$geometry[m]) * 
                                             rotation(tbl$angle[m]) + 
                                             tbl$geometry[m])
      }
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "ellipses") %>%
        select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "ellipses") %>%
      select(animal_id, species, season, case, geometry, analysis_type)
    tbl_all <- tbl_case %>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}

for (i in ids) {
  for (j in seasons) {
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) == 0) {
      next
    }
    k <- 100
    season <- home_ranges[home_ranges$animal_id == i & 
                            home_ranges$season == j, ]$season_pooled
    tbl_control <- tribble(
      ~animal_id, ~season, ~geometry, ~case
    )
    while (k > 0) {
      species <- home_ranges[home_ranges$animal_id == i & 
                               home_ranges$season == j, ]$species
      area <- median(st_area(home_ranges[home_ranges$season_pooled == season &
                                           home_ranges$species == species, ]))
      buffers <- st_sample(study_area, k) %>%
        st_buffer(dist = sqrt(area / pi))
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "hybrid") %>%
        select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "hybrid") %>%
      select(animal_id, species, season, case, geometry, analysis_type)
    tbl_all <- tbl_case %>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}


for (i in ids) {
  for (j in seasons) {
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) == 0) {
      next
    }
    k <- 100
    season <- home_ranges[home_ranges$animal_id == i & 
                            home_ranges$season == j, ]$season_pooled
    tbl_control <- tribble(
      ~animal_id, ~season, ~geometry, ~case
    )
    while (k > 0) {
      species <- home_ranges[home_ranges$animal_id == i & 
                               home_ranges$season == j, ]$species
      area <- median(st_area(home_ranges[home_ranges$season_pooled == season &
                                           home_ranges$species == species, ]))
      buffers <- st_sample(study_area, k) %>%
        st_buffer(dist = sqrt(area / pi))
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "circular") %>%
        select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "circular") %>%
      select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case$geometry <- tbl_case$geometry %>%
      st_centroid() %>%
      st_buffer(dist = sqrt(area / pi))
    tbl_all <- tbl_case %>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}

tib <- as.tibble(st_coordinates(st_centroid(ellipses)))

ggplot() +
  geom_sf(data = study_area, fill = "red") +
  geom_bin2d(aes(tib$X, tib$Y)) +
  coord_sf(default_crs = sf::st_crs(26915)) +
  labs(title = "Centroid locations", x = "long", y = "lat")

hist(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                      home_ranges$season_pooled == season, ]$geometry)), 
     xlab = "area m sq", main = "Empirical vs. empircal PDF within season", 
     prob = TRUE,
     breaks = 12)
lines(density(st_area(home_ranges[home_ranges$species == species & 
                                 home_ranges$season_pooled == season, ]$geometry)))
abline(v = median(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                 home_ranges$season_pooled == season, ]$geometry))),
       col="red", lwd=3, lty=2)

hist(as.numeric(st_area(tbl_control$geometry)), 
     xlab = "area m sq", main = "Drawn vs. empircal PDF within season", 
     prob = TRUE)
lines(density(st_area(home_ranges[home_ranges$species == species & 
                                    home_ranges$season_pooled == season, ]$geometry)))
abline(v = median(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                   home_ranges$season_pooled == season, ]$geometry))),
       col="red", lwd=3, lty=2)

hist(as.numeric(home_ranges[home_ranges$species == species &
                              home_ranges$season_pooled == season, ]$ratio),
     xlab = "length:sqrt(area)", main = "Empirical vs. empircal PDF within season", 
     prob = TRUE,
     breaks = 12)
lines(density(home_ranges[home_ranges$species == species & 
                         home_ranges$season_pooled == season, ]$ratio, bw = "SJ"))
abline(v = median(home_ranges[home_ranges$species == species & 
                                home_ranges$season_pooled == season, ]$ratio),
       col="red", lwd=3, lty=2)

hist(x,
     xlab = "length:sqrt(area)", main = "Drawn vs. empircal PDF within season", 
     prob = TRUE)
lines(density(home_ranges[home_ranges$species == species & 
                            home_ranges$season_pooled == season, ]$ratio))
abline(v = median(home_ranges[home_ranges$species == species & 
                                home_ranges$season_pooled == season, ]$ratio),
       col="red", lwd=3, lty=2)

hist(angle, breaks = 19, prob = TRUE, )

hr <- ellipses[ellipses$case == "case", ]
used <- ellipses[ellipses$case == "case", ]
hrs <- home_ranges[home_ranges$species == species &
                   home_ranges$season_pooled == season, ]

ggplot() +
  geom_sf(data = study_area, aes(fill = "Study area")) +
  geom_sf(data = ellipses, aes(fill = "Available")) +
#  geom_sf(data = hr, aes(fill = "Home range")) +
  geom_sf(data = hr, aes(fill = "Used")) +
  geom_sf(data = c1.1, aes(fill = "50%")) +
  scale_fill_manual(values = c("Study area" = "red", "Available" = "light blue",
                               "Used" = "black",#, 
                               "50%" = "grey"), 
                    guide = guide_legend(override.aes = list(linetype = "blank", 
                                                             shape = NA)))
  

c1.1 <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(season == j & method == "kde_ani_50" & animal_id == i) %>%
  select(!area)

min(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                     home_ranges$season_pooled == season, ]$geometry)))
    


mat_coys <- home_ranges[home_ranges$season_pooled == season &
                          home_ranges$species == species, ]

ggplot() +
  geom_bin2d(aes(x, as.numeric(st_area(tbl_control$geometry)))) +
  geom_point(aes(mat_coys$ratio, as.numeric(st_area(mat_coys$geometry))), color = "red", size = 3) +
  labs(title = "Distribution of home range sizes and LA ratios", x = "ratio", y = "area (m sq)") +
  theme_classic()



