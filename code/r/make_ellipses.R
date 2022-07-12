coy_mat_ranges <- home_ranges[home_ranges$season_pooled == "mate" &
                              home_ranges$species == "coyote", ]


med_area <- median(sqrt(st_area(home_ranges[home_ranges$season_pooled == "mate" &
                                                     home_ranges$species == "coyote", ]$geometry) / pi))

coy_mat_buffs <- home_ranges[home_ranges$season_pooled == "mate" &
                              home_ranges$species == "coyote", ]$geometry %>%
  st_centroid() %>%
  st_buffer(med_area)

coy_mat_buffered <- coy_mat_ranges %>%
  mutate(geometry = coy_mat_buffs,
         buffer = "buffer")

coy_mat_ellipses <- coy_mat_ranges

for (i in 1:nrow(coy_mat_ellipses)) {
  dists <- coy_mat_ranges$geometry[i] %>% 
    st_cast("POINT") %>% # turn polygon into points
    as.data.frame() %>%
    mutate(point_id = row_number())
  max_dist <- coy_mat_ranges$geometry[i] %>% 
    st_cast("POINT") %>% # turn polygon into points
    st_distance() %>% # calculate distance matrix
    as.data.frame() %>% 
    gather(point_id, dist) %>% 
    filter(dist == max(dist)) %>%
    mutate(point_id = as.integer(point_id))
  ex <- max_dist$dist[1]
  pts <- max_dist %>%
    select(point_id) %>%
    left_join(dists, by = "point_id") %>%
    select(geometry)
  line <- st_combine(c(pts[1, ], pts[2, ])) %>%
    st_cast("LINESTRING")
  centroid <- coy_mat_ranges$geometry[i] %>%
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
  b <- as.numeric(st_area(coy_mat_ranges$geometry[i]) / ((ex / 2) * pi))
  tbl <- tibble(ex = as.numeric(ex / 2), ey = b, angle = rot_deg, 
                geometry = centroid) %>%
    st_as_sf()
  coy_mat_ellipses$geometry[i] <- st_sfc(st_multipolygon((st_ellipse(pnt = tbl$geometry, 
                                                                     ex = tbl$ex, 
                                                                     ey = tbl$ey) - 
                                                            tbl$geometry) * 
                                                           rotation(tbl$angle) + 
                                                           tbl$geometry))
}

plot(coy_mat_ranges[1, ]$geometry, lwd = 2)
plot(line, col = "red", lty = 2, lwd = 2, add = TRUE)
plot(st_centroid(coy_mat_ranges[1, ]$geometry), col = "red", pch = 19, 
     add = TRUE)






# Define extraction function for typical weighted average
extractor <- function (i, x, y) {
  # Extract raster values within that buffer
  rast <- x %>%
    terra::extract(vect(y[i, "geometry"]))
  # Exclude any NA values introduced
  rast <- rast[complete.cases(rast), ]
  # Average the DEM values
  extracted <- mean(rast[, 2])
  # Append to columns of the movement data as a proportion
  return(extracted)
}



nums <- 1:nrow(coy_mat_ranges)

coy_mat_ranges$imp <- lapply(nums, extractor, x = imp, y = coy_mat_ranges) %>%
  # Convert to vector
  unlist()

coy_mat_buffered$imp <- lapply(nums, extractor, x = imp, y = coy_mat_fake) %>%
  # Convert to vector
  unlist()

coy_mat_ellipses$imp <- lapply(nums, extractor, x = imp, 
                               y = coy_mat_ellipses) %>%
  # Convert to vector
  unlist()

coy_mat_ellipses <- coy_mat_ellipses %>%
  mutate(buffer = "ellipse")

coy_mat_ranges <- coy_mat_ranges %>%
  mutate(buffer = "KDE (best estimate)") %>%
  rbind(coy_mat_fake) %>%
  rbind(coy_mat_ellipses)



imp_ave <- mean(coy_mat_ranges[coy_mat_ranges$buffer == "KDE", ]$imp)

ggplot(coy_mat_ranges, aes(buffer, imp)) + 
  geom_violin(color = "gray", fill = "gray") +
  geom_boxplot(width=0.1, fill = c("purple4", "dark gray", "chartreuse4"), color = "black") +
  geom_hline(aes(yintercept = mean(coy_mat_ranges[coy_mat_ranges$buffer == "KDE (best estimate)", ]$imp), 
                 color = "KDE"), linetype = 1, size = 1) +
  geom_hline(aes(yintercept = mean(coy_mat_ranges[coy_mat_ranges$buffer == "buffer", ]$imp), 
                 color = "buffer"), linetype = 2, size = 1) +
  geom_hline(aes(yintercept = mean(coy_mat_ranges[coy_mat_ranges$buffer == "ellipse", ]$imp), 
                 color = "ellipse"), linetype = 2, size = 1) +
  scale_colour_manual(values = c("purple1", "chartreuse", "black"), 
                      name = "Mean values") +
  labs(title = "Used home range estimates of % impervious", x = "",
       y = "% Impervious") +
  scale_x_discrete(limits = c("buffer", "KDE (best estimate)", "ellipse")) +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(hjust = 0.5))









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












dists <- coy_mat_ellipses$geometry[i] %>% 
  st_cast("POINT") %>% # turn polygon into points
  as.data.frame() %>%
  mutate(point_id = row_number())
max_dist <- coy_mat_ellipses$geometry[i] %>% 
  st_cast("POINT") %>% # turn polygon into points
  st_distance() %>% # calculate distance matrix
  as.data.frame() %>% 
  gather(point_id, dist) %>% 
  filter(dist == max(dist)) %>%
  mutate(point_id = as.integer(point_id))
ex <- max_dist$dist[1]
pts <- max_dist %>%
  select(point_id) %>%
  left_join(dists, by = "point_id") %>%
  select(geometry)
line2 <- st_combine(c(pts[1, ], pts[2, ])) %>%
  st_cast("LINESTRING")
centroid_2 <- coy_mat_ellipses$geometry[i] %>%
  st_centroid()
coy_mat_ellipse_ho$geometry[i] <- st_sfc(st_multipolygon(st_ellipse(pnt = tbl$geometry, ex = tbl$ex, ey = tbl$ey)))


dists <- coy_mat_ellipse_ho$geometry[1] %>% 
  st_cast("POINT") %>% # turn polygon into points
  as.data.frame() %>%
  mutate(point_id = row_number())
max_dist <- coy_mat_ellipse_ho$geometry[i] %>% 
  st_cast("POINT") %>% # turn polygon into points
  st_distance() %>% # calculate distance matrix
  as.data.frame() %>% 
  gather(point_id, dist) %>% 
  filter(dist == max(dist)) %>%
  mutate(point_id = as.integer(point_id))
ex <- max_dist$dist[1]
pts <- max_dist %>%
  select(point_id) %>%
  left_join(dists, by = "point_id") %>%
  select(geometry)
line3 <- st_combine(c(pts[1, ], pts[2, ])) %>%
  st_cast("LINESTRING")
line_pts3 <- coy_mat_ellipse_ho$geometry[i] %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame()
line_pt1 <- line_pts3 %>%
  mutate(X = (line_pts3$X + (3500 / 2))) %>%
  st_as_sf(coords = c("X", "Y"))
line_pt2 <- line_pts3 %>%
  mutate(X = (line_pts3$X - (3500 / 2))) %>%
  st_as_sf(coords = c("X", "Y"))
line3 <- st_combine(c(line_pt1$geometry, line_pt2$geometry)) %>%
  st_cast("LINESTRING")



# Plot 1
plot(coy_mat_ranges[1, ]$geometry, lwd = 2)
plot(line, col = "red", lty = 2, lwd = 2, add = TRUE)
plot(centroid, col = "red", pch = 19, add = TRUE)

# Plot 2
plot(coy_mat_ranges[1, ]$geometry, lwd = 2)
plot(coy_mat_ellipse_ho[1, ]$geometry, border = "green", lty = 2, lwd = 2, add = TRUE)
plot(line3, col = "green", lty = 2, lwd = 2, add = TRUE)
plot(centroid_3, col = "red", pch = 19, add = TRUE)



plot(coy_mat_ranges[1, ]$geometry, lwd = 2)
plot(coy_mat_ellipses[1, ]$geometry, border = "green", lty = 2, lwd = 2, add = TRUE)
plot(line2, col = "green", lty = 2, lwd = 2, add = TRUE)
plot(centroid_3, col = "red", pch = 19, add = TRUE)


plot(coy_mat_ranges[1, ]$geometry, col = "gray", lty = 2, lwd = 2)
plot(coy_mat_ellipses[1, ]$geometry, col = alpha("green", 0.5), lwd = 2, add = TRUE, alpha = 0.5)
plot(line2, col = "green", lty = 2, lwd = 2, add = TRUE)
plot(centroid_3, col = "red", pch = 19, add = TRUE)

line2 <- (line3 - centroid) * rotation(tbl$angle) + centroid
