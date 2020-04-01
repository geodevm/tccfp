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
