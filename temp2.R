
library(plm)
library(zoo)


#------------------------------------------------------------------------------#
#### Load Data ####

sr <- fread(file = file.path(DATA, "sim2019.csv"),
            encoding = "UTF-8")

# Keep only analysis years for now
sr <- sr %>% subset(year > 2008 & year < 2016)

# Remove ilha do governador
sr <- sr[!(sr$aisp %in% c(17,41))]

#------------------------------------------------------------------------------#
#### Load Shape Files ####

aisp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")
aisp <- spTransform(aisp, RjProj_unp)

# Remove ilha do governador
aisp <- aisp[!(aisp@data$aisp %in% c(17,41)),]


#------------------------------------------------------------------------------#

# Panel data 
sr$year_month <- sr$year*100 + sr$month
ps <- pdata.frame(sr, index = c("aisp", "year_month"))


# Neighbourhood definition
nb <- poly2nb(aisp, queen = T)

# Neighbour weights
lw <- nb2listw(nb,
               style = "W",
               zero.policy = TRUE)





# data(Produc, package="plm")
# data(usaww)
# usalw <- mat2listw(usaww)
# fm <- log(gsp)~log(pcap)+log(pc)+log(emp)+unemp+slag(log(pcap),
#                                                      listw=usalw)
# slxmod <- spreml(fm, data=Produc, w = usaww,
#                  model="pooling", lag=FALSE, errors="ols")



#Formulas01_str["violent_death_sim"]
#FormulasIV_str["violent_death_sim"]

ftest2 <- violent_death_sim ~ on_target + 
  policemen_aisp + policemen_upp + n_precinct + max_prize + population +
  factor(aisp) + factor(year) + factor(month) 


# SAR with pooled data
foo <- spml(formula = ftest2, 
            data=ff, 
            #model = "pooling",
            model = "within",
            lag = T,
            listw = lw)

# SAR with pooled data and IV
sp_ff <- ps %>% subset(!is.na(violent_death_sim))

ff <- ps %>% subset(complete.cases(population,
                                   on_target,
                                   policemen_aisp,   
                                   policemen_upp,
                                   n_precinct,
                                   max_prize,        
                                   violent_death_sim,
                                   street_robbery,
                                   vehicle_robbery))

fs_formula <- ~ lag12_dist_target_vr + lag12_dist_target_sr + lag12_dist_target_vd

bar <- spgm(formula = ftest2, 
            data=ff, 
            lag = T,
            method = "w2sls",
            instruments = fs_formula,
            lag.instruments = T,
            listw = lw)




sr_sl <- sr_sl %>% subset(complete.cases(population,
                      on_target,
                      policemen_aisp,   
                      policemen_upp,
                      n_precinct,
                      max_prize,        
                      violent_death_sim,
                      street_robbery,
                      vehicle_robbery))




formula = as.formula(Formulas01_sl_str["violent_death_sim"])
data = sr_sl
nbW = lw


spml(formula = as.formula(formula), 
     data=data, 
     model = "pooling",
     lag = T,
     listw = nbW)



foo <- ps %>% subset(complete.cases(population,
                                         on_target,
                                         policemen_aisp,   
                                         policemen_upp,
                                         n_precinct,
                                         max_prize,        
                                         violent_death_sim,
                                         street_robbery,
                                         vehicle_robbery))



ps$id_cmt <- ps$id_cmt %>% as.integer()

sl_vd_02 <- SARlag_reg("violent_death_sim ~ on_target + policemen_aisp + policemen_upp + n_precinct + max_prize + population + factor(aisp) + factor(id_cmt)",
                       data = foo,
                       nbW = lw)

SARlag_reg <- function(formula, data, nbW ){
  spml(formula = as.formula(formula), 
       data=data, 
       model = "within",
       lag = T,
       listw = nbW)
}







