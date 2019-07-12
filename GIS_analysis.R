#------------------------------------------------------------------------------#

#		Spatial analysis of crime in Rio de janeiro		

#------------------------------------------------------------------------------#



# ProjFolder <- ("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")
# ProjFolder_OUT_plots <- file.path(ProjFolder, "Plots")


RjProj_aze <- CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
RjProj_unp <- CRS("+init=epsg:4326")  


#------------------------------------------------------------------------------#
#### Load Data ####

sim <- read.dta13(file.path(DATA, "data_SIM_2019-01.dta"))

#------------------------------------------------------------------------------#
#### Load Shape Files ####

aisp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")
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
#### New Variables ####

# Deaths per 100 thousand
aisp$lv_pop <- aisp$violent_death_sim/(aisp$population/100000)

# Carjack per 100 thousand
aisp$rv_pop <- aisp$vehicle_robbery /(aisp$population/100000)

# Street robbery per 100 thousand
aisp$rr_pop <- aisp$street_robbery/(aisp$population/100000)

#------------------------------------------------------------------------------#
#### Moran's I ####

# Remove ilha do governador

aisp_simp <- aisp[aisp@data$aisp != 17,]

# Neighbourhood definition
nb <- poly2nb(aisp_simp, queen = T)

# Neighbour weights
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate lagged values
aisp_simp$lv_pop_lag <- lag.listw(lw, aisp_simp$lv_pop)
aisp_simp$rv_pop_lag <- lag.listw(lw, aisp_simp$rv_pop)
aisp_simp$rr_pop_lag <- lag.listw(lw, aisp_simp$rr_pop)


lm(lv_pop_lag ~ lv_pop, data = aisp_simp@data) %>% summary
lm(rv_pop_lag ~ rv_pop, data = aisp_simp@data) %>% summary
lm(rr_pop_lag ~ rr_pop, data = aisp_simp@data) %>% summary

moran_lv <- moran.test(aisp_simp@data$lv_pop, lw)
moran_rv <- moran.test(aisp_simp@data$rv_pop, lw)
moran_rr <- moran.test(aisp_simp@data$rr_pop, lw)


MC_lv <- moran.mc(aisp_simp@data$lv_pop, lw, nsim = 1000)
MC_rv <- moran.mc(aisp_simp@data$rv_pop, lw, nsim = 1000)
MC_rr <- moran.mc(aisp_simp@data$rr_pop, lw, nsim = 1000)


#------------------------------------------------------------------------------#
#### Heat maps ####


# Only Rio de Janeiro Cidade
rio_aisp_codes <- c(2,3,4,5,6,9,14,16,17,18,19,22,23,27,31,40,41)


#### Tidy aisp shapfile
aisp_df <- tidy(aisp)
aisp_df <- merge(aisp_df, 
                 aisp@data, 
                 by = "id")

rio_aisp_df <- aisp_df[aisp_df$aisp %in% rio_aisp_codes,]


#### Heatmaps

heatFUN <- function(data, var, title){
  ggplot() +
    geom_polygon(data=data, 
                 aes_string(x="long", 
                     y="lat", 
                     group = "group", 
                     fill = var),
                 col = "black") + 
    coord_quickmap() +
    theme_void()  +
    scale_fill_viridis(
      name=title) +
    theme(legend.title = element_text(size = 15),
          legend.text = element_text(size = 15))
  
}


# Violent deaths
heat_lv <- heatFUN(data = aisp_df, 
                   var = "lv_pop", 
                   title = "Violent deaths per\n100 thousand \ninhabitants") +
  theme(legend.position = c(0.4, 0.75))

heat_city_lv <- heatFUN(data = rio_aisp_df,
                        var = "lv_pop", 
                   title = "Violent deaths per\n100 thousand \ninhabitants")+
  theme(legend.position = c(0.3, 0.9))



# Car robbery
heat_rv <- heatFUN(data = aisp_df, 
                   var = "rv_pop", 
                   title = "Car jacking per\n100 thousand \ninhabitants")+
  theme(legend.position = c(0.4, 0.75))
  
heat_city_rv <- heatFUN(data = rio_aisp_df, 
                   var = "rv_pop", 
                   title = "Car jacking per\n100 thousand \ninhabitants")+
  theme(legend.position = c(0.3, 0.9)) 
  

# Street robbery

heat_rr <- heatFUN(data = aisp_df, 
                   var = "rr_pop", 
                   title = "Street robberyper \n100 thousand \ninhabitants")+
  theme(legend.position = c(0.4, 0.75))

heat_city_rr <- heatFUN(data = rio_aisp_df, 
                        var = "rr_pop", 
                        title = "Street robbery per \n100 thousand \ninhabitants") +
  theme(legend.position = c(0.3, 0.9)) 



#------------------------------------------------------------------------------#
#### Export plots  ####

png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_lv.png"),
    width = 1000, height = 700)
  heat_lv
dev.off()

png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_city_lv.png"),
    width = 1000, height = 700)
  heat_city_lv
dev.off()


png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_rv.png"),
    width = 1000, height = 700)
  heat_rv
dev.off()


png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_city_rv.png"),
    width = 1000, height = 700)
heat_city_rv
dev.off()


png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_rr.png"),
    width = 1000, height = 700)
heat_rr
dev.off()


png(filename = file.path(ProjFolder_OUT_plots, 
                         "heatMap_city_rr.png"),
    width = 1000, height = 700)
heat_city_rr
dev.off()


# Moran's I monte carlo simulation
png(filename = file.path(ProjFolder_OUT_plots, 
                         "MoranMC.png"))

par(mfrow=c(3,1))
plot(MC_lv, las= 1, xlab = "Violent Death", main = "")
plot(MC_rv, las= 1, xlab = "Car jacking", main = "")
plot(MC_rr, las= 1, xlab = "Street robbery", main = "")

dev.off()





