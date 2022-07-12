library(sf)
library(nngeo)
library(tidyverse)
# Read in the study area (100% MCP of all non-dispersal data)
study_area <- st_read(here("data/processed_data/shps/study_area.shp"), 
                      quiet = TRUE)
# Read in 95% anisotropic KDE home ranges
home_ranges <- st_read(here("data/processed_data/shps/home_ranges.shp")) %>%
  filter(season != "annual" & method == "kde_ani_95") %>%
  dplyr::select(!area)
# Re-apply the home range identifiers for multi-home range animals 
home_ranges[(home_ranges$animal_id == "F7_1" | 
               home_ranges$animal_id == "F7_2"), ]$animal_id <- "F7"
home_ranges[(home_ranges$animal_id == "C9_1" | 
               home_ranges$animal_id == "C9_2"), ]$animal_id <- "C9"
# Get ids and seasons (by year) for iterating
ids <- levels(as.factor(as.character(home_ranges$animal_id)))
seasons <- levels(as.factor(as.character(home_ranges$season)))
# Union home ranges where there are multiple home ranges within a season within
# a year.
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
# Create a variable that pools home ranges seasonally rather than by year and 
# season
for (i in 1:nrow(home_ranges)) {
  home_ranges$season_pooled[i] <-  strsplit(home_ranges$season[i], "[_]")[[1]][1]
}
# Make sure all rows are unique
home_ranges <- home_ranges %>%
  distinct()
# Create a distance ratio column, taking length to sqrt(area) ratio
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
# Make sure there is species identification
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
# Define a rotation function for rotating ellipses
rotation = function(a) {
  r = a * pi / 180 #degrees to radians
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 
# Generate used and available points -------------------------------------------
# Create an empty table to hold the new data for RSF use and available
ellipses <- tribble(
  ~animal_id, ~season, ~geometry, ~case, 
)
# Generate available ellipses and used ellipses for comparison, within season
# and within animal ID
for (i in ids) {
  # Season within animal
  for (j in seasons) {
    # If there aren't any observations, skip. If there's one observation, take
    # The else if route
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) == 0) {
      next
    } else if (nrow(home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ]) == 1) {
      # 100 observations to start the while loop
      k <- 100
      # Collect species of this animal
      species <- home_ranges[home_ranges$animal_id == i & 
                               home_ranges$season == j, ]$species
      # Collect the biological season, pooled
      season <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ]$season_pooled
      # Create a table to collect control points
      tbl_control <- tribble(
        ~animal_id, ~season, ~geometry, ~case
      )
      # Start while loop, which repeats until a sample of 100 meeting all 
      # criteria has been generated
      while (k > 0) {
        # Collect area of this home range
        area <- st_area(home_ranges[home_ranges$animal_id == i & 
                                      home_ranges$season == j, ])
        # Collect the ratio of this home range
        x <- home_ranges[home_ranges$animal_id == i & 
                           home_ranges$season == j, ]$ratio
        # Generate random rotation angles
        angle <- runif(k, 0, 360)
        # Generate random points within the study area
        pts <- st_sample(study_area, k)
        # Generate the major axis radius of the ellipse
        a <- as.numeric((x * sqrt(area)) / 2)
        # Generate the minor axis radius of the ellipse
        b <- as.numeric(area / (a * pi))
        # Create a table with these attributes
        tbl <- tibble(ex = a, ey = b, angle = angle, geometry = pts) %>%
          st_as_sf()
        # Generate the ellipse:
        for (m in 1:k) {
          tbl$geometry[m] <- st_multipolygon((st_ellipse(pnt = tbl$geometry[m], 
                                                         ex = tbl$ex[m], 
                                                         ey = tbl$ey[m]) -
                                                # The steps above generate the 
                                                # ellipse, and those below
                                                # rotate it.
                                                tbl$geometry[m]) *
                                               rotation(tbl$angle[m]) + 
                                               tbl$geometry[m])
        }
        # Filter out any polygons that are not within the study area
        tbl <- tbl %>% 
          filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                               remove_self = TRUE)) %>%
          # Create and select the desired attributes
          mutate(animal_id = i, season = j, species = species,
                 case = "control", analysis_type = "ellipse") %>%
          dplyr::select(animal_id, species, season, case, geometry, 
                        analysis_type)
        # Add all points meeting criteria to the control points dataset
        tbl_control <- tbl_control %>%
          rbind(tbl)
        # Change k to be the number of control locations that still need to be
        # generated
        k <- k - nrow(tbl)
      }
      # Create the used, in this case actual HR estimates
      tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
        mutate(case = "case", analysis_type = "hybrid_ellipse") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      # Create elliptical HR estimates
      tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                                  home_ranges$season == j, ] %>%
        mutate(case = "case", analysis_type = "ellipse_ellipse") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      # Make a spatial features datafram of points
      dists <- tbl_case_2$geometry %>% 
        st_cast("POINT") %>% # turn polygon into points
        as.data.frame() %>%
        mutate(point_id = row_number())
      # Find the maximum distance within the polygon
      max_dist <- tbl_case_2$geometry %>% 
        st_cast("POINT") %>% # turn polygon into points
        st_distance() %>% # calculate distance matrix
        as.data.frame() %>% 
        gather(point_id, dist) %>% 
        filter(dist == max(dist)) %>%
        mutate(point_id = as.integer(point_id))
      # Some times you have the same distance for two points, if so go down
      # this path
      if (nrow(max_dist) == 4) {
        # Get the maximum distance
        ex <- max_dist$dist[1]
        # Join the points as spatial objects
        pts <- max_dist %>%
          dplyr::select(point_id) %>%
          left_join(dists, by = "point_id") %>%
          dplyr::select(geometry)
        # Calculate polygon centroid
        centroid <- tbl_case_2$geometry %>%
          st_centroid()
        # Calculate angle of line 1
        angle_pts_1 <- pts$geometry[c(1, 3)] %>%
          st_coordinates() %>%
          as_tibble() %>%
          arrange(X)
        # Calculate the angle of line 2
        angle_pts_2 <- pts$geometry[c(2, 4)] %>%
          st_coordinates() %>%
          as_tibble() %>%
          arrange(X)
        # Extract the end points of the lines
        xy2_1 <- c(angle_pts_1$X[2], angle_pts_1$Y[2])
        xy1_1 <- c(angle_pts_1$X[1], angle_pts_1$Y[1])
        # Calculate the distance between the points
        xylen1_1 <- xy2_1 - xy1_1
        # Get the sign
        s_1 <- sign(xylen1_1[1])
        s_1[s_1 == 0] <- 1
        # Calculate the angle in radians
        ang_rad_1 <- s_1 * (xylen1_1[2] < 0) * pi + atan(xylen1_1[1] / 
                                                           xylen1_1[2])
        # Convert to degrees
        ang_deg_1 <- ang_rad_1 * (180 / pi)
        # Add 90 to get the degrees clockwise from east
        rot_deg_1 <- ang_deg_1 + 90
        # Repeat steps for the second pair
        xy2_2 <- c(angle_pts_2$X[2], angle_pts_2$Y[2])
        xy1_2 <- c(angle_pts_2$X[1], angle_pts_2$Y[1])
        xylen1_2 <- xy2_2 - xy1_2
        s_2 <- sign(xylen1_2[1])
        s_2[s_2 == 0] <- 1
        ang_rad_2 <- s_2 * (xylen1_2[2] < 0) * pi + atan(xylen1_2[1] / 
                                                           xylen1_2[2])
        ang_deg_2 <- ang_rad_2 * (180 / pi)
        rot_deg_2 <- ang_deg_2 + 90
        # Average rotation angles
        rot_deg <- (rot_deg_1 + rot_deg_2) / 2
      } else {
        # These are the same steps as above, but for single point
        ex <- max_dist$dist[1]
        pts <- max_dist %>%
          dplyr::select(point_id) %>%
          left_join(dists, by = "point_id") %>%
          dplyr::select(geometry)
        centroid <- tbl_case_2$geometry %>%
          st_centroid()
        angle_pts <- pts$geometry %>%
          st_coordinates() %>%
          as_tibble() %>%
          arrange(X)
        xy2 <- c(angle_pts$X[2], angle_pts$Y[2])
        xy1 <- c(angle_pts$X[1], angle_pts$Y[1])
        xylen1 <- xy2 - xy1
        s <- sign(xylen1[1])
        s[s == 0] <- 1
        ang_rad <- s * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])
        ang_deg <- ang_rad * (180 / pi)
        rot_deg <- ang_deg + 90
      }
      # b is the minor radius of the ellipse
      b <- as.numeric(st_area(tbl_case_2$geometry) / ((ex / 2) * pi))
      # Fill in table values
      tbl <- tibble(ex = as.numeric(ex / 2), ey = b, angle = rot_deg, 
                    geometry = centroid) %>%
        st_as_sf()
      # Generate ellipse as used
      tbl_case_2$geometry <- st_sfc(st_multipolygon((st_ellipse(pnt = tbl$geometry, 
                                                                    ex = tbl$ex, 
                                                                    ey = tbl$ey) - 
                                                   tbl$geometry) * 
                                                   rotation(tbl$angle) + 
                                                   tbl$geometry))
      # Bind all back together
      tbl_all <- tbl_case_1 %>%
        bind_rows(tbl_case_2) %>%
        bind_rows(tbl_control)
      # Bind to the official list
      ellipses <- ellipses %>% 
        rbind(tbl_all)
      next
    } else {
      # For greater than one animal within species and season, simulate
      # home range sizes and anisotropy from observed values.
      # 100 samples
      k <- 100
      # Collect species
      species <- home_ranges[home_ranges$animal_id == i & 
                               home_ranges$season == j, ]$species
      # Collect pooled season
      season <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ]$season_pooled
      # Create table to store values
      tbl_control <- tribble(
        ~animal_id, ~season, ~geometry, ~case
      )
      # Continue while loop until 100 samples have been generated
      while (k > 0) {
        # Store ratios
        x <- c()
        # Store areas
        area <- c()
        # l will serve as iterator for the inner while loop
        l <- k
        # Enter random ratio while loop
        while (l > 0) {
          # Create a ratio density using SJ bandwidth selection
          den <- density(home_ranges[home_ranges$species == species, ]$ratio, 
                         bw = "SJ")
          # Draw random samples by sampling distribution + random error taken
          # from the bandwidth
          newx <- sample(home_ranges[home_ranges$species == species, ]$ratio, l, 
                         replace=TRUE) + rnorm(l, 0, den$bw)
          # Left truncate to include only above circular radius
          newx <- newx[newx >= (2 / sqrt(pi))]
          # Bind to the ratios list
          x <- c(x, newx)
          # Edit to include only the number still needed after truncation
          l <- l - length(newx)
        }
        # l will serve as iterator for the inner while loop
        l <- k
        # Enter random area while loop
        while (l > 0) {
          # Create an area density using SJ bandwidth selection
          area_den <- density(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                               home_ranges$season_pooled == 
                                                               season, ]$geometry, 
                                                 bw = "SJ")))
          # Draw random samples by sampling distribution + random error taken
          # from the bandwidth
          new_area <- sample(as.numeric(st_area(home_ranges[home_ranges$species == species &
                                                              home_ranges$season_pooled == 
                                                              season, ]$geometry)), l, 
                             replace=TRUE) + rnorm(l, 0, area_den$bw)
          # Make sure there are no non-positive or 0 area ranges
          new_area <- new_area[new_area > 0]
          # Bind to the ratios list
          area <- c(area, new_area)
          # Edit to include only the number still needed after truncation
          l <- l - length(new_area)
        }
        # Generate random angles
        angle <- runif(k, 0, 360)
        # Generate random points within the study area
        pts <- st_sample(study_area, k)
        # Calculate the major axis radius
        a <- as.numeric((x * sqrt(area)) / 2)
        # Calculate the minor axis radius
        b <- as.numeric(area / (a * pi))
        # Create a table containing all relevant information for calculating
        # an ellipse
        tbl <- tibble(ex = a, ey = b, angle = angle, geometry = pts) %>%
          st_as_sf()
        # Generate ellipses, same as above
        for (m in 1:k) {
          tbl$geometry[m] <- st_multipolygon((st_ellipse(pnt = tbl$geometry[m], 
                                                         ex = tbl$ex[m], 
                                                         ey = tbl$ey[m]) -
                                                tbl$geometry[m]) * 
                                               rotation(tbl$angle[m]) + 
                                               tbl$geometry[m])
        }
        # Remove any that do not lie within the study area
        tbl <- tbl %>% 
          filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                               remove_self = TRUE)) %>%
          mutate(animal_id = i, season = j, species = species,
                 case = "control", analysis_type = "hybrid_ellipse") %>%
          dplyr::select(animal_id, species, season, case, geometry, analysis_type)
        # Bind all that are within the study area to the running list
        tbl_control <- tbl_control %>%
          rbind(tbl)
        # Edit k based on the number that meet criteria
        k <- k - nrow(tbl)
      }
      # These steps are the same as in the above section
      tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                                  home_ranges$season == j, ] %>%
        mutate(case = "case", analysis_type = "hybrid_ellipse") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                                  home_ranges$season == j, ] %>%
        mutate(case = "case", analysis_type = "ellipse_ellipse") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      dists <- tbl_case_2$geometry %>% 
        st_cast("POINT") %>% # turn polygon into points
        as.data.frame() %>%
        mutate(point_id = row_number())
      max_dist <- tbl_case_2$geometry %>% 
        st_cast("POINT") %>% # turn polygon into points
        st_distance() %>% # calculate distance matrix
        as.data.frame() %>% 
        gather(point_id, dist) %>% 
        filter(dist == max(dist)) %>%
        mutate(point_id = as.integer(point_id))
      ex <- max_dist$dist[1]
      pts <- max_dist %>%
        dplyr::select(point_id) %>%
        left_join(dists, by = "point_id") %>%
        dplyr::select(geometry)
      line <- st_combine(c(pts[1, ], pts[2, ])) %>%
        st_cast("LINESTRING")
      centroid <- tbl_case_2$geometry %>%
        st_centroid()
      angle_pts <- pts$geometry %>%
        st_coordinates() %>%
        as_tibble() %>%
        arrange(X)
      xy2 <- c(angle_pts$X[2], angle_pts$Y[2])
      xy1 <- c(angle_pts$X[1], angle_pts$Y[1])
      xylen1 <- xy2 - xy1
      s <- sign(xylen1[1])
      s[s == 0] <- 1
      ang_rad <- s * (xylen1[2] < 0) * pi + atan(xylen1[1] / xylen1[2])
      ang_deg <- ang_rad * (180 / pi)
      rot_deg <- ang_deg + 90
      b <- as.numeric(st_area(tbl_case_2$geometry) / ((ex / 2) * pi))
      tbl <- tibble(ex = as.numeric(ex / 2), ey = b, angle = rot_deg, 
                    geometry = centroid) %>%
        st_as_sf()
      tbl_case_2$geometry <- st_sfc(st_multipolygon((st_ellipse(pnt = tbl$geometry, 
                                                                ex = tbl$ex, 
                                                                ey = tbl$ey) - 
                                                       tbl$geometry) * 
                                                      rotation(tbl$angle) + 
                                                      tbl$geometry))
      tbl_all <- tbl_case_1 %>%
        bind_rows(tbl_case_2) %>%
        bind_rows(tbl_control)
      ellipses <- ellipses %>% 
        rbind(tbl_all)
    }
  }
}
# Buffer by median home range radius
for (i in ids) {
  # Within season within animal
  for (j in seasons) {
    # Skip if no observations
    if (nrow(home_ranges[home_ranges$animal_id == i & 
                         home_ranges$season == j, ]) == 0) {
      next
    }
    # We want 100 random samples
    k <- 100
    # Collect the pooled season value
    season <- home_ranges[home_ranges$animal_id == i & 
                            home_ranges$season == j, ]$season_pooled
    # Create a table to hold control observations
    tbl_control <- tribble(
      ~animal_id, ~season, ~geometry, ~case
    )
    # Enter while loop generating circles within the study area
    while (k > 0) {
      # Gather species
      species <- home_ranges[home_ranges$animal_id == i & 
                               home_ranges$season == j, ]$species
      # Gather home range area median within this season
      area <- median(st_area(home_ranges[home_ranges$season_pooled == season &
                                           home_ranges$species == species, ]))
      # Generate buffers
      buffers <- st_sample(study_area, k) %>%
        st_buffer(dist = sqrt(area / pi))
      # Make a feature layer
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      # Filter out those that don't lie within the study area
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "circle_med") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      # Bind to the running table list
      tbl_control <- tbl_control %>%
        rbind(tbl)
      # Edit iterator
      k <- k - nrow(tbl)
    }
    # Generate a used home range for comparison (KDE)
    tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "hybrid_circle_med") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    # Generate a used home range circle
    tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                              home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "circle_circle_med") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    # Change the geometry to be the centroid buffer
    tbl_case_2$geometry <- tbl_case_2$geometry %>%
      st_centroid() %>%
      st_buffer(dist = sqrt(area / pi))
    # Bind all together
    tbl_all <- tbl_case_1 %>%
      bind_rows(tbl_case_2)%>%
      bind_rows(tbl_control)
    # Join to main table
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}
# Buffer by 100 m
# Steps the same as other circular buffers
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
        st_buffer(dist = 100)
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "circle_100") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "hybrid_circle_100") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "circle_circle_100") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2$geometry <- tbl_case_2$geometry %>%
      st_centroid() %>%
      st_buffer(dist = 100)
    tbl_all <- tbl_case_1 %>%
      bind_rows(tbl_case_2)%>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}
# Buffer by 500 m
# Steps the same as other circular buffers
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
        st_buffer(dist = 500)
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "circle_500") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "hybrid_circle_500") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "circle_circle_500") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2$geometry <- tbl_case_2$geometry %>%
      st_centroid() %>%
      st_buffer(dist = 500)
    tbl_all <- tbl_case_1 %>%
      bind_rows(tbl_case_2)%>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}
# Buffer by 1000 m
# Steps the same as other circular buffers
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
        st_buffer(dist = 1000)
      tbl <- tibble(geometry = buffers) %>%
        st_as_sf()
      tbl <- tbl %>% 
        filter(st_covered_by(tbl, study_area, sparse = FALSE, 
                             remove_self = TRUE)) %>%
        mutate(animal_id = i, season = j, species = species,
               case = "control", analysis_type = "circle_1000") %>%
        dplyr::select(animal_id, species, season, case, geometry, analysis_type)
      tbl_control <- tbl_control %>%
        rbind(tbl)
      k <- k - nrow(tbl)
    }
    tbl_case_1 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "hybrid_circle_1000") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2 <- home_ranges[home_ranges$animal_id == i & 
                                home_ranges$season == j, ] %>%
      mutate(case = "case", analysis_type = "circle_circle_1000") %>%
      dplyr::select(animal_id, species, season, case, geometry, analysis_type)
    tbl_case_2$geometry <- tbl_case_2$geometry %>%
      st_centroid() %>%
      st_buffer(dist = 1000)
    tbl_all <- tbl_case_1 %>%
      bind_rows(tbl_case_2)%>%
      bind_rows(tbl_control)
    ellipses <- ellipses %>% 
      rbind(tbl_all)
  }
}
# Export as a shapefile
ellipses %>%
  st_write(here("data/processed_data/shps/random_ranges.shp")) 
