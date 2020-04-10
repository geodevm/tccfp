leaflet(covid.collars) %>% addTiles()%>%
  addCircles(covid.collars$gps_longitude, covid.collars$gps_latitude)
leaflet(pre.covid.collars) %>% addTiles()%>%
  addCircles(pre.covid.collars$gps_longitude, pre.covid.collars$gps_latitude)

ggplot(covid.collars, 
       aes(x = gps_latitude, y = gps_longitude))+
  geom_point() +
  facet_wrap(~animal_id, scales = "free")
ggplot(pre.covid.collars, 
       aes(x = gps_latitude, y = gps_longitude))+
  geom_point() +
  facet_wrap(~animal_id, scales = "free")

ggplot(covid.collars, 
       aes(gps_longitude, gps_latitude, color = animal_id, 
           group = animal_id))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")
ggplot(pre.covid.collars, 
       aes(gps_longitude, gps_latitude, color = animal_id, 
           group = animal_id))+
  geom_point() + coord_equal() +
  theme(legend.position = "bottom")
#covid.temp <- direction_rel(covid.nesttrk$data[[1]])
#pre.covid.temp <- direction_rel(pre.covid.nesttrk$data[[1]])
covid.trk1 <- covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd),
  ) %>% unnest()
covid.trk1
pre.covid.trk1 <- pre.covid.trk %>% group_by(id) %>% nest() %>% as.data.frame() %>% 
  mutate(dir_abs = map(data, direction_abs, full_circle = TRUE, zero = "N", clockwise = TRUE), 
         dir_rel = map(data, direction_rel), 
         sl = map(data, step_lengths),
         nsd_=map(data, nsd),
 ) %>% unnest()
pre.covid.trk1

ggplot(covid.trk1, aes(x = dir_abs, y = ..density..)) + 
  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
  scale_x_continuous(limits = c(0, 2 * pi), 
                     breaks = c(0, pi/2, pi, 3 * pi/2), 
                     labels = c("0", "pi/2", "pi", "3pi/2")) +
  facet_wrap( ~ id)
ggplot(pre.covid.trk1, aes(x = dir_abs, y = ..density..)) + 
  geom_histogram(breaks = seq(0, 2 * pi, len = 30))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + labs(y = "Density", title = "Angles Direct") + 
  scale_x_continuous(limits = c(0, 2 * pi), 
                     breaks = c(0, pi/2, pi, 3 * pi/2), 
                     labels = c("0", "pi/2", "pi", "3pi/2")) +
  facet_wrap( ~ id)

ggplot(covid.trk1, aes(x = dir_rel, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~id)
ggplot(pre.covid.trk1, aes(x = dir_rel, y = ..density..)) + 
  geom_histogram(breaks = seq(-pi, pi, length = 20))+
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous(limits = c(-pi, pi), breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                     labels = c("-pi", "-pi/2", "0", "pi/2", "pi")) +
  facet_wrap(~id)

ggplot(covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
  facet_wrap(~id, scales = "free")
ggplot(pre.covid.trk1, aes(x = t_, y = nsd_)) + geom_path()+
  facet_wrap(~id, scales = "free")

ggplot(covid.trk1, aes(x = tod_, y = log(sl))) + 
  geom_boxplot() + geom_smooth() + facet_wrap(~id)
ggplot(pre.covid.trk1, aes(x = tod_, y = log(sl))) + 
  geom_boxplot() + geom_smooth() + facet_wrap(~id)

#covid.trk1 <- track_resample(covid.trk1, rate = min(10), tolerance = min(30)) %>% 
#  filter_min_n_burst(min_n = 1)

#dat <- track_resample(dat, rate = min(10), tolerance = min(30)) %>% 
#  filter_min_n_burst(min_n = 1)

#steps <- dat %>% steps_by_burst() 
#steps %>% ggplot(aes(sl_)) + geom_histogram()


plot.activity <- function (x) {
  tbl <- with(x, table(active, dn))
  barplot(tbl, beside = TRUE, legend = TRUE, main = deparse(substitute(x)))
}
par(mfrow=c(2,2))
plot.activity(c.l.pre.covid.activity)
plot.activity(c.l.covid.activity)
plot.activity(v.v.pre.covid.activity)
plot.activity(v.v.covid.activity)

plot.activity2 <- function (x) {
  tbl <- with(x, table(active))
  barplot(tbl, beside = TRUE, legend = FALSE, main = deparse(substitute(x)))
}

par(mfrow=c(2,2))
plot.activity2(c.l.pre.covid.activity)
plot.activity2(c.l.covid.activity)
plot.activity2(v.v.pre.covid.activity)
plot.activity2(v.v.covid.activity)


