library(ezknitr)
library(knitr)
library(lubridate)
library(raster)
library(move)
library(amt) 
library(broom)
library(nlme)
library(lme4)
library(here)
library(tidyverse)
options(width=165)
opts_chunk$set(fig.width=12,fig.height=4.5, error=TRUE,cache = F)

#' Set the seed for the random number generator, so it will be possible
#' to reproduce the random points
set.seed(8421)

#' ## Load data
#' trk (fisher tracks) and fisher.dat created in TestVignetteMovebank.R
trk <- read_rds(here("data", "trk.Rdata"))

#' ## RSF prep
#' 
#' Generate random points within MCPs for a single individual using amt functions.
#' Notes:
#' 
#' - It is common to generate points randomly, but other options are possible. 
#' - In particular, it can beneficial to generate a systematically placed sample
#' - Samples can also be generated using the *spsample* function in the `sp` package or
#' using a GIS (note: amt uses the spsample function within its function random_points)
#' - Other home range polygons could be used (e.g., kernel density, local convex hull
#' etc.)
#' 
#' #### Random points: illustrate for 1 individual
trk %>% filter(id=="F1") %>%
  random_points(factor = 50) %>% plot()

#' Illustrate systematic points (to do this, we need to create the mcp first)
trk %>% filter(id=="F1") %>% 
  random_points(factor = 50, type="regular") %>% 
  plot() 

#' Or use just 1 random point for each observed point for better visualization.
trk %>% filter(id=="F1") %>% 
  random_points(factor = 1, type="regular") %>% 
  plot() 

#' Now, lets generate points for all individuals. We can do this
#' efficiently by making use of pipes (`%>%`), nested data frames, and
#' then by adding a new column -- a list-column -- to `trks`
avail.pts <- trk %>% group_by(id) %>% nest %>% 
  mutate(rnd_pts = map(data, ~ random_points(., factor = 20, type="regular"))) %>% 
  select(id, rnd_pts) %>%  # you dont want to have the original point twice, hence drop data
  unnest()

#' Because unnesting loses the class, we will have to manually reset it
class(avail.pts) <- c("random_points", class(avail.pts))

#' Or, we could do this using a loop (commented out, below)
# avail.pts<-NULL
# uid<-unique(trk$id) # individual identifiers
# luid<-length(uid) # number of unique individuals
# for(i in 1:luid){
#  random_points will generate random points within mcp
#  Add on the individual id and combine all data
#   temp<-cbind(id=uid[i],trk%>%filter(id==uid[i])%>%random_points)
#   avail.pts<-rbind(avail.pts, temp)
# }
# avail.pts<-as_tibble(avail.pts)
# avail.pts

#' ### Prepare environmental covariates
#' 
#' We will use three covariates: land use class, elevation and population density

landuse <- raster(here("data/landuse/landuse_study_area.tif"))
elevation <- raster(here("data/elevation/ASTER ASTGTM2 Elevation-20100101120000000-0-0.tif"))
popden <- raster(here("data/pop_den/pop_den.tif"))


#' First, we need to bring all thre layers to the same CRS.
get_crs(trk)
landuse <- projectRaster(landuse, crs = get_crs(trk), method = "ngb")
elevation <- projectRaster(elevation, crs = get_crs(trk))
popden <- projectRaster(popden, crs = get_crs(trk))

#' Plot the raster

plot(landuse)
plot(elevation)
plot(popden)


#' Check the resolution 
res(landuse)
res(elevation)
res(popden)

#' Assign each raster a meaningful name
names(landuse) <- "landclass"
names(elevation) <- "ele"
names(popden) <- "popden"


#' Resolutions (and also extents) are different. This means, that we can not
#' stack the rasters. There two options: 
#' 1. Resample rasters to the coarsest raster (popden) using the function 
#' `raster::resample`. 
#' 2. Extract covariate values from each raster idendependetly.
#' We will continue with second option.


avail.pts <- avail.pts %>% 
  extract_covariates(landuse) %>%  
  extract_covariates(elevation) %>% 
  extract_covariates(popden)

avail.pts


#' Create landcover classes (as suggested by Scott Lapoint :)
rsfdat <- avail.pts %>%  mutate(
  landclass = as.character(landclass), 
  landC = fct_collapse(landclass,
                       agri = c("81", "82"),
                       forest =c("41", "42", "43"),
                       shrub = c("52"),
                       grass = c("31", "71"),
                       wet = c("90", "95"),
                       other = c("11", "21", "22", "23", "24")))

#' Center and scale variables, make response numeric
rsfdat <- rsfdat %>% mutate(elev = scale(ele), 
                            popD = scale(popden), 
                            case_ = as.numeric(case_))

#' Save RSF data for later (multiple animal)
write_rds(rsfdat, "data/rsf_dat.rds")

#' ## Explore the data
#'
#' Look Distribution of habitat class for used and available observations
#+fig.width=8, fig.height=8 
ggplot(rsfdat, aes(x=landC, y=..prop..,group=case_, colour=case_))+
  geom_bar(position="dodge", aes(fill=case_))+facet_wrap(~id, scales="free")

#' ## RSF fitting

#' Weight available data 
rsfdat$w <- ifelse(rsfdat$case_ == 1, 1, 5000)

#' We can fit an RSF model to a single animal using logistic regression
summary(glm(case_ ~ elev + popD + landC, data = subset(rsfdat, id == "M2"), 
            weight = w,family = binomial))

#' Note, this individual did not experience all landcover classes
rsfdat %>% filter(id == "M2") %>% with(table(case_, landC))  
rsfdat %>% filter(id == "M2") %>% count(landC, case_) # this works with the amt development version

rsfdat$used<-as.factor(rsfdat$case_)
rsfdat$used<-fct_recode(rsfdat$used, "avail" = "0", "used" = "1")

#+fig.width=6, fig.height=4
ggplot(subset(rsfdat, id=="M2"),  aes(x=landC,group=used))+
  geom_bar(position=position_dodge(), aes(y=..prop.., fill = used), stat="count") +
  scale_fill_brewer(palette="Paired")+
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.3, position=position_dodge(0.9)) +
  labs(y = "Proportion", fill="used", x="Landcover") 




#' Now, fit an RSF model to data from each animal.  Since not all animals experience
#' all habitat types, lets just explore forest versus non-forest
rsfdat$forest <- ifelse(rsfdat$landC == "forest", 1, 0)

class(rsfdat) <- class(rsfdat)[-1] # this should be fixed in the dev version

rsffits <- rsfdat %>% group_by(id) %>% nest %>% 
  mutate(mod = map(data, function(x) glm(case_ ~ elev + popD + forest, data = x,
                                         weight=w,family = binomial))) 


#' This stores a list of model fits
rsffits 

#' Look at first model
rsffits$mod[[1]]

#' Now, use tidy to extract information about the model fits into a nested
#' data frame
rsffits <- rsffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow) %>% simplify())
rsffits 
rsffits$tidy

#' Now, create data frame w/ the coefficients, etc

rsf_coefs<-rsffits %>% unnest(tidy) %>% 
  select(-(std.error:p.value))
rsf_coefs

#' Plot coefficients
#+fig.width=12, fig.heigh=4
rsf_coefs %>% filter(term!="(Intercept)") %>%
  ggplot(., aes(x=1, y=estimate)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)+
  facet_wrap(~term, scales="free")


#' ## Document Footer	

#' Session Information:	
#' 	
sessionInfo()	  