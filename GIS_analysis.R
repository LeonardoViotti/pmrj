#------------------------------------------------------------------------------#

#		Spatial analysis of crime in Rio de janeiro		

#------------------------------------------------------------------------------#

rm(list = ls())

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

sim_f <- aggregate(cbind(violent_death_sim, 
                         street_robbery,
                         vehicle_robbery,
                         population) ~ aisp, mean, data = sim)


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

aisp <- merge(aisp, sim_f, by = "aisp")


#------------------------------------------------------------------------------#
#### Moran's I ####





#------------------------------------------------------------------------------#
#### Heat maps ####


aisp$lv_pop <- aisp$violent_death_sim/(aisp$population/100000)


aisp_df <- tidy(aisp)
aisp_df <- merge(aisp_df, 
                 aisp@data, 
                 by = "id")

ggplot() +
  geom_polygon(data=aisp_df, 
               aes(x=long, 
                   y=lat, 
                   group = group, 
                   fill = lv_pop)) + 
  theme_void()  #+
  # scale_fill_viridis(    
  #   breaks = c(25,50,75,100),
  #   labels = c("25","50","75","100+"),
  #   
  #   name="Number of firms per square kilometre",
  #   guide = guide_legend(keyheight = unit(2, units = "mm"),
  #                        keywidth=unit(11, units = "mm"), 
  #                        label.position = "bottom", 
  #                        title.position = 'top', nrow=1)) +
  # theme(
  #   text = element_text(color = "#22211d"), 
  #   plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   legend.background = element_rect(fill = "#f5f5f2", color = NA),
  #   
  #   plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  #   legend.position = "bottom"
  # ) + 
  # geom_polygon(data=bairros_tidy4, 
  #              aes(x=long, 
  #                  y=lat, 
  #                  group = group), 
  #              fill= "yellow" )



