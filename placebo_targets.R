#------------------------------------------------------------------------------#

#	 SIM- Placebo target calculation				

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Settings ####

library(tidyverse)
library(readstata13)
library(rgeos)
library(rgdal)





#------------------------------------------------------------------------------#
#### Load constructed data ####

setwd("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")
sim <- read.dta13("data_SIM_2019-01.dta")


sim <- sim[!is.na(sim$aisp) & !is.na(sim$year) & !is.na(sim$month), ]

# Load shapefiles

aisp_shp <- readOGR(dsn = "GIS", layer = "lm_aisp_2019")


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


#### Crime lag 

lagFun <- function(x, n){
  dplyr::lag(x,
             n = n,
             default = NA)
}


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
# Os quartis de cada indicador sÃ£o definidos considerando a taxa do indicador 
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


#### Criar a variavel de quartil

# Define empty variable
sim$plaTar_vd <- NA
sim$plaTar_vr <- NA
sim$plaTar_sr <- NA

Qbol<- function(q, var){
  return(sim[,paste0("qua_", var)] == q & !is.na(sim[,paste0("qua_", var)]))
}

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


#------------------------------------------------------------------------------#
#### Plots do fit  ####

simp <- sim[sim$year>2010,]

# Letalidade Violenta
png("Results/lv_placebo_fit.png",
    width = 730, height = 480)

  slope_vd <- 1 + (reduQ1_vd + reduQ2_vd + reduQ3_vd + reduQ4_vd)/4
  plot(simp$target_vd, 
       simp$vd_l,
       xlab="Meta real", 
       ylab="L.V. mesmo mês do ano anterior")
  title(main="Fit médio percentuais de redução  - Letalidade Violenta (2011-2015)")
  abline(a = 0, b= slope_vd, col = "red")

dev.off()

# Roubo de veiculos
png("Results/rv_placebo_fit.png",
    width = 730, height = 480)
  slope_vr <- 1 + (reduQ1_vr + reduQ2_vr + reduQ3_vr + reduQ4_vr)/4
  plot(simp$target_vr, simp$vr_l,
       xlab="Meta real", 
       ylab="R.V. mesmo mês do ano anterior")
  title(main="Fit médio percentuais de redução  - Roubo de Veículos (2011-2015)")
  abline(a = 0, b= slope_vr, col = "red")
dev.off()

# Roubo de rua
png("Results/rv_placebo_fit.png",
    width = 730, height = 480)
  slope_sr <- 1 + (reduQ1_sr + reduQ2_sr + reduQ3_sr + reduQ4_sr)/4
  plot(simp$target_sr, simp$sr_l,
       xlab="Meta real", 
       ylab="R.R. mesmo mês do ano anterior")
  title(main="Fit médio percentuais de redução  - Roubo de Rua (2011-2015)")
  abline(a = 0, b= slope_sr, col = "red")
dev.off()
  

#------------------------------------------------------------------------------#
#### Exportart a base ####

# Exporting just the new variables to be merged with the original data. Since the
# original data is .dta, so it won't lose meta data
plaExport <- sim[, c("aisp",
                     "year",
                     "month",
                     "semester",
                     "qua_vd",
                     "qua_vr",
                     "qua_sr",
                     "plaTar_vd",
                     "plaTar_vr",
                     "plaTar_sr", 
                     "latitude",
                     "longitude")]


plaExport <-
  plaExport %>%
  rename("vd_placebo_tar" = "plaTar_vd",
         "vr_placebo_tar" = "plaTar_vr",
         "sr_placebo_tar" = "plaTar_sr")

write.csv(plaExport,
          "placebo_targets_gis.csv",
          row.names = F,
          na = "")


# bar <- sim[sim$year>2010 & sim$target_vd != 0,]
# bar$vd_diff <- ((bar$target_vd - bar$plaTar_vd)/bar$target_vd)*100
# summary(bar$vd_diff)
# sd(bar$vd_diff, na.rm = T)
# hist(bar$vd_diff)
# 
# bar <- sim[sim$year>2010 & sim$target_vd != 0 & sim$aisp !=9 & sim$aisp != 40,]
# bar$vd_diff <- ((bar$target_vd - bar$plaTar_vd)/bar$target_vd)*100
# reg1 <- lm(target_vd ~ vd_l, data = bar)
# 
# 
# 
# utils::View(bar[order(bar$vd_diff), 
#                 c("aisp",
#                   "year",
#                   "month",
#                   "semester",
#                   "violent_death",
#                   "vd_l",
#                   "target_vd",
#                   "plaTar_vd",
#                   "vd_diff",
#                   "vd_tx_l",
#                   "qua_vd")])
# 
# 
# utils::View(bar[bar$aisp == 4 & !is.na(bar$aisp), 
#                 c("aisp",
#                   "year",
#                   "month",
#                   "semester",
#                   "violent_death",
#                   "vd_l",
#                   "target_vd",
#                   "plaTar_vd",
#                   "vd_diff",
#                   "vd_tx_l",
#                   "qua_vd")])

#### Estabilidade
# Sea taxa p100mil menor do que 10 homicÃ­dios


# #------------------------------------------------------------------------------#
# #### Condstrucao do Dataset por semestre ####
# 
# 
# # gambiarra do caralho
# simSem <- sim %>% 
#   dplyr::arrange(aisp, year,  semester) %>%
#   dplyr::group_by(year, aisp, semester) %>%
#   dplyr::summarise(pop = mean(population, na.rm = T),
#                    vd = sum(violent_death, na.rm = T), 
#                    tar = unique(target_vd_sem, na.rm = T))  #%>%
# #dplyr::ungroup(year) %>%


