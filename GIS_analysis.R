#------------------------------------------------------------------------------#

#		Spatial analysis of crime in Rio de janeiro		

#------------------------------------------------------------------------------#

rm(list = ls())



library(rgeos)
library(rgdal)
library(sp)
library(maps)
library(geosphere)
library(viridis)

setwd("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")


RjProj_aze <- CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
RjProj_unp <- CRS("+init=epsg:4326")  


#------------------------------------------------------------------------------#
#### Load Data ####

sim <- read.dta13("data_SIM_2019-01.dta")

#------------------------------------------------------------------------------#
#### Load Shape Files ####

aisp <- readOGR(dsn = "GIS", layer = "lm_aisp_2019")
aisp <- spTransform(aisp, RjProj_unp)

#------------------------------------------------------------------------------#
#### Crime data processing ####

# Aggregate crime data by average of the entire series. I'm not not sure if this
# is the right thing to do, but it's the easiest to write the draft code

sim_ano <- aggregate(cbind(violent_death_sim, 
                           street_robbery,
                           vehicle_robbery) ~ aisp + year, 
                     sum, 
                     data = sim)

sim_ano$population <- aggregate(population ~ aisp + year, 
                                mean, 
                                data = sim)$population

sim_aisp <- aggregate(cbind(violent_death_sim, 
                            street_robbery,
                            vehicle_robbery,
                            population) ~ aisp, mean, data = sim_ano)


#------------------------------------------------------------------------------#
#### Shape Files processing ####

aisp@data$id <- row.names(aisp)


# Calculate Areas
aisp_aze <- spTransform(aisp, RjProj_aze)

aisp_areas <- gArea(aisp_aze, byid=TRUE)/1e+06 

aisp$area[match(names(aisp_areas), aisp@data$id)] <- 
  aisp_areas[match(names(aisp_areas), aisp@data$id)]


#------------------------------------------------------------------------------#
#### Merge ####

aisp <- merge(aisp, sim_aisp, by = "aisp")


#------------------------------------------------------------------------------#
#### Moran's I ####





#------------------------------------------------------------------------------#
#### Heat maps ####


# Only Rio de Janeiro Cidade
rio_aisp_codes <- c(2,3,4,5,6,9,14,16,17,18,19,22,23,27,31,40,41)

# Deaths per 100 thousand
aisp$lv_pop <- aisp$violent_death_sim/(aisp$population/100000)



#### Tidy aisp shapfile
aisp_df <- tidy(aisp)
aisp_df <- merge(aisp_df, 
                 aisp@data, 
                 by = "id")

rio_aisp_df <- aisp_df[aisp_df$aisp %in% rio_aisp_codes,]


ggplot() +
  geom_polygon(data=aisp_df, 
               aes(x=long, 
                   y=lat, 
                   group = group, 
                   fill = lv_pop)) + 
  coord_quickmap() +
  theme_void()  +
  scale_fill_viridis(
    name="Violent death per \n100 thousand \ninhabitants")



ggplot() +
  geom_polygon(data=rio_aisp_df, 
               aes(x=long, 
                   y=lat, 
                   group = group, 
                   fill = lv_pop)) + 
  theme_void()  +
  coord_quickmap() +
  scale_fill_viridis(
    name="Violent death per \n100 thousand \ninhabitants")


