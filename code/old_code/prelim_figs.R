# COVID-19 "15 days to slow the spread" began 16 March 2020. I mark 00:00 UTC (19:00 CST) as the start of this.
# 27 March 2020 is stay at home order for Minnesota
# Load Packages
library(RColorBrewer)
library(raster)
library(tidyverse)
library(lubridate)
library(amt)
library(sf)
library(survival)
library(lme4)
library(lmerTest)
library(here)
library(ggpubr)
library(extrafont)
# Import collars
# collars <- read.csv(here("data/latest_data.csv"))
# Import rasters
impervious <- raster(here::here("data/impervious/TCMA_Impervious_2000.tif"))
proj4string(impervious) <- CRS("+init=epsg:32615")
landcover <- raster(here::here("data/landcover/tcma_lc_finalv1.tif"))
proj4string(landcover) <- CRS("+init=epsg:32615")
# Import census shp
pop <- read_sf(here::here("data/population/joined_census_blocks_2010.shp"))
pop = st_set_crs(pop, 32615)
# Import and subset water bodies for Crow, Minnesota and Mississippi Rivers
rivers <- read_sf(here::here("data/shp_water_lakes_rivers/LakesAndRivers.shp"))
rivers <- rivers[rivers$NAME_DNR == "Mississippi" | rivers$NAME_DNR == "Crow" | rivers$NAME_DNR == "Minnesota",]
rivers <- st_union(rivers$geometry)
rivers = st_transform(rivers, 32615)
# Convert to population density
pop$density_m_2 <- as.numeric(pop$pop_total / st_area(pop$geometry))
pop <- pop %>% dplyr::select(density_m_2, geometry)
# Make a track using amt that will keep animal id and species, adding time of day
collars_trk <- collars %>%
  make_track(gps_utm_easting,
             gps_utm_northing,
             acquisition_start_time,
             sp = species,
             id = animal_id,
             crs = CRS("+init=epsg:32615")) %>%
  time_of_day()
plot(collars_trk)
# Create a subset of this to be used for RSFs. I am going to assume that resampling to include 5.5 and 11 hour intervals will be sufficient
collars_11hr <- collars_trk %>%
  nest(data = -"id") %>%
  mutate(resample = map(data, function(x) 
    x %>%
      track_resample(rate = hours(11), tolerance = minutes(60)) %>% # 150 min tolerance
      filter_min_n_burst(min_n = 3) %>% # At least 3 in a burst
      extract_covariates(impervious) %>%
      extract_covariates(landcover))) %>%
  dplyr::select(id, resample) %>%
  as_tibble() %>%
  tidyr::unnest(col = "resample")
  
collars_sf <- st_as_sf(collars_11hr, coords = c("x_", "y_"), crs = 32615)

plot(collars_sf)

collars_sf$dist_to_river <- NA

for (i in 1:nrow(collars_sf)) {
  collars_sf$dist_to_river[i] <- as.numeric(min(st_distance(st_geometry(collars_sf)[i], rivers)))
}

collars_sf <- st_join(collars_sf, pop["density_m_2"])
collars_sf$scaled_pop <- (collars_sf$density_m_2 - min(collars_sf$density_m_2, na.rm = TRUE))/(max(collars_sf$density_m_2, na.rm = TRUE) - min(collars_sf$density_m_2, na.rm = TRUE))
collars_sf$scaled_imp <- (collars_sf$TCMA_Impervious_2000 - min(collars_sf$TCMA_Impervious_2000, na.rm = TRUE))/(max(collars_sf$TCMA_Impervious_2000, na.rm = TRUE) - min(collars_sf$TCMA_Impervious_2000, na.rm = TRUE))
collars_sf$urb_index <- collars_sf$scaled_imp + collars_sf$scaled_pop

df_1 <- data.frame(aggregate(dist_to_river ~ id, collars_sf, mean))
df_2 <- data.frame(aggregate(urb_index ~ id, collars_sf, mean))
df_3 <- data.frame(aggregate(scaled_pop ~ id, collars_sf, mean))
df_4 <- data.frame(aggregate(scaled_imp ~ id, collars_sf, mean))
df_5 <- data.frame(aggregate(density_m_2 ~ id, collars_sf, mean))
df_6 <- data.frame(aggregate(TCMA_Impervious_2000 ~ id, collars_sf, mean))



indices_df <- Reduce(merge, list(df_1, df_2, df_3, df_4, df_5, df_6))
indices_df_ngf <- indices_df[indices_df$id != "F10",]

indices_df_ngf$species[substring(indices_df_ngf$id, 1, 1) == "C"] <- "coyote"
indices_df_ngf$species[substring(indices_df_ngf$id, 1, 1) == "F"] <- "fox"
indices_df_ngf$density_km_2 <- indices_df_ngf$density_m_2/0.000001

mean(indices_df_ngf$TCMA_Impervious_2000[substring(indices_df_ngf$id, 1, 1) == "C"]) # Percent imp surface coyotes
mean(indices_df_ngf$TCMA_Impervious_2000[substring(indices_df_ngf$id, 1, 1) == "F"]) # Percent imp surface foxes
mean(indices_df_ngf$density_m_2[substring(indices_df_ngf$id, 1, 1) == "C"])/0.000001 # Human pop density (/km2) selected by coyotes
mean(indices_df_ngf$density_m_2[substring(indices_df_ngf$id, 1, 1) == "F"])/0.000001 # Density selected by foxes

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x[[col]])))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(indices_df_ngf, varname="density_km_2", 
                    groupnames="species")
df2$species=as.factor(df2$species)
head(df2)

a <- ggplot(df2, aes(x=species, y=density_km_2)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=density_km_2, ymax=density_km_2+se), width=.2, 
                position=position_dodge(.9)) +
  labs(title = NULL, x = NULL, 
       y = expression(paste("human pop. density  ", (km^-2)))) +
  theme_classic() +
  theme(axis.text = element_text(family="Times New Roman", size=12),
        axis.title.y = element_text(family="Times New Roman", 
                                    size=12, margin = margin(t = 0, r = 5, b = 0, l = 0)))
a

df3 <- data_summary(indices_df_ngf, varname="TCMA_Impervious_2000", 
                    groupnames="species")
df3$species=as.factor(df3$species)
head(df3)

b <- ggplot(df3, aes(x=species, y=TCMA_Impervious_2000)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=TCMA_Impervious_2000, ymax=TCMA_Impervious_2000+se), width=.2,
                position=position_dodge(.9)) +
  labs(title = NULL, x = NULL, y = "% impervious surface") +
  theme_classic() +
  theme(axis.text = element_text(family="Times New Roman", size=12),
        axis.title.y = element_text(family="Times New Roman", size=12, 
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)))

summary_pop$sp[summary_pop$sp == "red fox"] <- "fox"
summary_imp$sp[summary_imp$sp == "red fox"] <- "fox"

c <- ggplot(summary_pop, aes(x=sp, y=ratio)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=ratio, ymax=ratio+se), width=.2,
                position=position_dodge(.9)) +
  labs(title = NULL, x = NULL, y = "ratio") +
  theme_classic() +
  theme(axis.text = element_text(family="Times New Roman", size=12),
        axis.title.y = element_text(family="Times New Roman", size=12, 
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)))

d <- ggplot(summary_imp, aes(x=sp, y=ratio)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=ratio, ymax=ratio+se), width=.2,
                position=position_dodge(.9)) +
  labs(title = NULL, x = NULL, y = "ratio") +
  theme_classic() +
  theme(axis.text = element_text(family="Times New Roman", size=12),
        axis.title.y = element_text(family="Times New Roman", size=12, 
                                    margin = margin(t = 0, r = 5, b = 0, l = 0)))

#ggarrange(a, b, c, d, ncol = 2, nrow = 2)
ggarrange(a, b, ncol = 2, nrow = 1)

write.csv(indices_df, here::here("data/indices.csv"))
