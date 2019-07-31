#------------------------------------------------------------------------------#

#	 SIM- Placebo target calculation				

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Settings ####



# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}



#------------------------------------------------------------------------------#
#### Load constructed data ####

# sim <- read.dta13(file.path(DATA,"data_SIM_2019-07.dta"))

# Loading data into a new object to be processed
sim <- raw_data
sim <- sim[!is.na(sim$aisp) & !is.na(sim$year) & !is.na(sim$month), ]

# Create this useful numeric version of this variable
sim$year_month <- sim$year*100+ sim$month

# Load shapefiles
aisp_shp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")

# Load chief IDs
cmd <- fread(file = file.path(DATA, "ComandantesBatalhao.csv"),
            encoding = "Latin-1")



#------------------------------------------------------------------------------#
#### Centroids ####

rownames(aisp_shp@data) <- aisp_shp@data$aisp

a_coords <- 
  gCentroid(aisp_shp, 
            byid = T, 
            id = aisp_shp@data$aisp )@coords %>% 
  as.data.frame() %>% 
  rename(latitude = y, longitude = x)

a_coords$aisp <- rownames(a_coords)


#------------------------------------------------------------------------------#
#### Variaveis ####

#### Clean chief names
cmd <- cmd %>% 
  rename(year = vano,
         month = mes,
         cmd_name = nome_comandante_bpm)

# Remover patente
cmd$cmd_name  <- gsub("TEN CEL |CEL |MAJ", "", cmd$cmd_name )

# Add variable to sim
sim <- merge(sim, 
             cmd, 
             by = c("aisp", "year", "month"), 
             all.x = T)


#### Crime lag 

lagFun <- function(x, n){
  dplyr::lag(x,
             n = n,
             default = NA)
}


#### Laged crimes to construct placebos
sim <- 
  sim %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(aisp) %>%
  dplyr::mutate(vd_l = lagFun(violent_death, 12),
                vr_l = lagFun(vehicle_robbery, 12),
                sr_l = lagFun(street_robbery, 12))


#### Add GIS variables
sim <- merge(sim, 
             a_coords, 
             by = "aisp", 
             all.x = T)



#------------------------------------------------------------------------------#
#### Condstrucao do Dataset por semestre ####



# Base semestral
simSem <- sim %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(year, aisp, semester) %>%
  dplyr::summarise(pop = mean(population, na.rm = T),
                   vd = sum(violent_death, na.rm = T),
                   vr = sum(vehicle_robbery, na.rm = T),
                   sr = sum(street_robbery, na.rm = T),
                   tar = unique(target_vd_sem, na.rm = T))

# Indicador defasado
simSem <- 
  simSem %>%
  dplyr::arrange(aisp, year,  semester) %>%
  dplyr::group_by(aisp) %>%
  dplyr::mutate(vd_l = lagFun(vd,2),
                sr_l = lagFun(sr,2),
                vr_l = lagFun(vr,2))


#### Taxa de crime por 100mil hab. por semestre
simSem$vd_tx    <- (simSem$vd/simSem$pop)*10e4
simSem$vr_tx    <- (simSem$vr/simSem$pop)*10e4
simSem$sr_tx    <- (simSem$sr/simSem$pop)*10e4

#### Taxa no mesmo semestre do ano anterior
simSem$vd_tx_l  <- lagFun(simSem$vd_tx,2)
simSem$vr_tx_l  <- lagFun(simSem$vr_tx,2)
simSem$sr_tx_l  <- lagFun(simSem$sr_tx,2)

#------------------------------------------------------------------------------#
#### Quartis ####
# Os quartis de cada indicador são definidos considerando a taxa do indicador 
# no mesmo semestre do ano anterior. Comecou a partir de 2011

# Funcao pra definir os quartis
colQ <- function(x){
  # Calcular os valores
  qx <- quantile(x, na.rm = T)
  
  # Definir os quartos
  newVec <- NA
  newVec[x < qx[2]] <- 1
  newVec[x >= qx[2] & x < qx[3]] <- 2
  newVec[x >=  qx[3] & x < qx[4]] <- 3
  newVec[x > qx[4]] <- 4
  
  return(newVec)
}


#### Adicionar quartis pra base sestral
simSem <- 
  simSem %>% 
  dplyr::group_by(year) %>% # Qartil daquele ano
  mutate(qua_vd = colQ(vd_tx_l),
         qua_vr = colQ(vr_tx_l),
         qua_sr = colQ(sr_tx_l))


#### Merge quartis na base mensal

idVars <- c("aisp",
            "year",
            "semester")

sim <- merge(sim, 
             simSem[,c("aisp",
                       "year",
                       "semester",
                       "qua_vd",
                       "qua_vr",
                       "qua_sr",
                       "vd_tx_l",
                       "vr_tx_l",
                       "sr_tx_l")],
             by = idVars)

#------------------------------------------------------------------------------#
#### Meta placebo ####


#### Criterios de reducao por quartil
reduQ1_vd = 0
reduQ2_vd = 0.04
reduQ3_vd = 0.055
reduQ4_vd = 0.075

reduQ1_vr = 0
reduQ2_vr = 0.03
reduQ3_vr = 0.05
reduQ4_vr = 0.065

reduQ1_sr = 0
reduQ2_sr = 0.03
reduQ3_sr = 0.05
reduQ4_sr = 0.07


#### Criar a booleano de quartil
Qbol<- function(q, var){
  return(sim[,paste0("qua_", var)] == q & !is.na(sim[,paste0("qua_", var)]))
}


#### Placebo targets

# Define empty variable
sim$plaTar_vd <- NA
sim$plaTar_vr <- NA
sim$plaTar_sr <- NA


sim$plaTar_vd[Qbol(1, "vd")] <- round(sim$vd_l[Qbol(1, "vd")]*(1-reduQ1_vd))
sim$plaTar_vd[Qbol(2, "vd")] <- round(sim$vd_l[Qbol(2, "vd")]*(1-reduQ2_vd))
sim$plaTar_vd[Qbol(3, "vd")] <- round(sim$vd_l[Qbol(3, "vd")]*(1-reduQ3_vd))
sim$plaTar_vd[Qbol(4, "vd")] <- round(sim$vd_l[Qbol(4, "vd")]*(1-reduQ4_vd))

sim$plaTar_vr[Qbol(1, "vr")] <- round(sim$vr_l[Qbol(1, "vr")]*(1-reduQ1_vr))
sim$plaTar_vr[Qbol(2, "vr")] <- round(sim$vr_l[Qbol(2, "vr")]*(1-reduQ2_vr))
sim$plaTar_vr[Qbol(3, "vr")] <- round(sim$vr_l[Qbol(3, "vr")]*(1-reduQ3_vr))
sim$plaTar_vr[Qbol(4, "vr")] <- round(sim$vr_l[Qbol(4, "vr")]*(1-reduQ4_vr))

sim$plaTar_sr[Qbol(1, "sr")] <- round(sim$sr_l[Qbol(1, "sr")]*(1-reduQ1_sr))
sim$plaTar_sr[Qbol(2, "sr")] <- round(sim$sr_l[Qbol(2, "sr")]*(1-reduQ2_sr))
sim$plaTar_sr[Qbol(3, "sr")] <- round(sim$sr_l[Qbol(3, "sr")]*(1-reduQ3_sr))
sim$plaTar_sr[Qbol(4, "sr")] <- round(sim$sr_l[Qbol(4, "sr")]*(1-reduQ4_sr))


# Semester target for placebo
sim <- sim %>% 
  group_by(aisp, year, semester) %>% 
  mutate(plaTar_vd_sem = sum(plaTar_vd),
         plaTar_vr_sem = sum(plaTar_vr),
         plaTar_sr_sem = sum(plaTar_sr))


#------------------------------------------------------------------------------#
#### Regression variables placebo ####

#placebo_PRE_bol <- sim$year <2009 | (sim$year == 2009 & sim$semester == 1)


#### Create cummulative targets
sim <- sim %>%
  group_by(aisp, year, semester) %>%
  mutate(plaTar_vd_l = Lag(plaTar_vd, +1),
         plaTar_vd_cum = cumsum(replace_na(plaTar_vd_l,0)),
         plaTar_vr_l = Lag(plaTar_vr, +1),
         plaTar_vr_cum = cumsum(replace_na(plaTar_vr_l,0)),
         plaTar_sr_l = Lag(plaTar_sr, +1),
         plaTar_sr_cum = cumsum(replace_na(plaTar_sr_l,0))) %>% 
  dplyr::select(-c(plaTar_vd_l, plaTar_vr_l, plaTar_sr_l)) # remove lagged variables

# Change zeros to NA if year is 2004
bol04 <- sim$year ==2004

sim$plaTar_vd_cum[bol04] <- NA
sim$plaTar_vr_cum[bol04] <- NA
sim$plaTar_sr_cum[bol04] <- NA


# Fix this so dist is not divided by zero
sim$plaTar_vr_sem <- ifelse(sim$plaTar_vr_sem == 0, NA, sim$plaTar_vr_sem)


# Create target per crime
# First month of semester on target is always NA
sim$on_target_vd_plapre <- (sim$violent_death_sim_cum <=  sim$plaTar_vd_cum) %>% as.numeric()
sim$on_target_vr_plapre <- (sim$vehicle_robbery_cum <=  sim$plaTar_vr_cum) %>% as.numeric()
sim$on_target_sr_plapre <- (sim$street_robbery_cum <=  sim$plaTar_sr_cum) %>% as.numeric()

# Create overall target
sim$on_target_plapre <-  (sim$on_target_vd_plapre==1 &
                          sim$on_target_vr_plapre==1 & 
                          sim$on_target_sr_plapre==1) %>% as.numeric()

#### Create IV vars

# Distance variable
sim$dist_target_vd_plapre <- (sim$violent_death_sim_cum /sim$plaTar_vd_sem) -1 
sim$dist_target_vr_plapre <- (sim$vehicle_robbery_cum /sim$plaTar_vr_sem) -1 
sim$dist_target_sr_plapre <- (sim$street_robbery_cum /sim$plaTar_sr_sem) -1 




# Instrumental variable, one year lagged
sim <- sim %>% 
  group_by(aisp) %>%
  arrange(aisp, year, month) %>% 
  mutate(lag12_dist_target_vd_plapre=Lag(dist_target_vd_plapre, +12),
         lag12_dist_target_vr_plapre=Lag(dist_target_vr_plapre, +12),
         lag12_dist_target_sr_plapre=Lag(dist_target_sr_plapre, +12))


# utils::View(foo %>% select(aisp,
#                            year,
#                            month,
#                            semester,
#                            violent_death_sim_cum,
#                            plaTar_vd_sem,
#                            lag12_dist_target_vd_plapre,
#                            dist_target_vd_plapre)
#             )


#------------------------------------------------------------------------------#
#### Plots do fit  ####

simp <- sim[sim$year>2010,]



if (EXPORT_plots){
  # Letalidade Violenta
  png("Results/lv_placebo_fit.png",
      width = 730, height = 480)
  
  slope_vd <- 1 + (reduQ1_vd + reduQ2_vd + reduQ3_vd + reduQ4_vd)/4
  plot(simp$target_vd, 
       simp$vd_l,
       xlab="Meta real", 
       ylab="L.V. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Letalidade Violenta (2011-2015)")
  abline(a = 0, b= slope_vd, col = "red")
  
  dev.off()
  
  # Roubo de veiculos
  png("Results/rv_placebo_fit.png",
      width = 730, height = 480)
  slope_vr <- 1 + (reduQ1_vr + reduQ2_vr + reduQ3_vr + reduQ4_vr)/4
  plot(simp$target_vr, simp$vr_l,
       xlab="Meta real", 
       ylab="R.V. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Roubo de Veiculos (2011-2015)")
  abline(a = 0, b= slope_vr, col = "red")
  dev.off()
  
  # Roubo de rua
  png("Results/rv_placebo_fit.png",
      width = 730, height = 480)
  slope_sr <- 1 + (reduQ1_sr + reduQ2_sr + reduQ3_sr + reduQ4_sr)/4
  plot(simp$target_sr, simp$sr_l,
       xlab="Meta real", 
       ylab="R.R. mesmo mes do ano anterior")
  title(main="Fit medio percentuais de reducao  - Roubo de Rua (2011-2015)")
  abline(a = 0, b= slope_sr, col = "red")
  dev.off()
  
}


#------------------------------------------------------------------------------#
#### Exportart a base ####

#### Cosmetic changes
sim <-
  sim %>%
  rename("vd_placebo_tar" = "plaTar_vd",
         "vr_placebo_tar" = "plaTar_vr",
         "sr_placebo_tar" = "plaTar_sr")


if (EXPORT_data){

  # Exporting the entire data to run in R
  write.csv(sim,
            file.path(DATA, "data_SIM_2019_constructed.csv"),
            row.names = F,
            na = "")
  
}

