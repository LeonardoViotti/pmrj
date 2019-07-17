#------------------------------------------------------------------------------#

#	 SIM - R&R Analysis

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Load data ####

sr <- fread(file = file.path(DATA, "sim2019.csv"),
            encoding = "UTF-8")
# Create year+month variable
sr$year_month <- sr$year*100+ sr$month

# Load aisps shapefile
aisp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")
aisp <- spTransform(aisp, RjProj_unp)

#------------------------------------------------------------------------------#
#### Process data ####

#### Placebo
sr_pl <- sr[sr$year < 2009,]

#### Spatial analysis

# Keep only analysis years
sr_sl <- sr %>% subset(year_month > 200906 & year_month < 201507)

# Remove ilha do governador and keep balanced panel
sr_sl <- sr_sl[!(sr_sl$aisp %in% c(17,41))]
aisp <- aisp[!(aisp@data$aisp %in% c(17,41)),]


#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery")

indepVars <- c("on_target",
               "policemen_aisp",
               "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )

indepVars_pla <- c("on_target_plapre",
                   #"policemen_aisp",
                   #"policemen_upp",
                   "n_precinct",
                   #"max_prize",
                   "population" )

FEVars <- c("aisp",
            "year", 
            "month", 
            "id_cmt")

FEVars_pla <- c("aisp",
                "year", 
                "month", 
                "cmd_name")

ZVars <- c("lag12_dist_target_vr",
           "lag12_dist_target_sr",
           "lag12_dist_target_vd")

ZVars_pla <- c("lag12_dist_target_vr_plapre",
               "lag12_dist_target_sr_plapre",
               "lag12_dist_target_vd_plapre")


#------------------------------------------------------------------------------#
### OLS formulas ####

# Right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 
rFormula_iv_pla <- paste(indepVars_pla[-1], collapse = " + ") 

# Add FE, cluster and instruments

clusterVars = c("latitude", "longitude" )
#clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)

FeForumala2_pla <- paste(FEVars_pla, collapse = " + ") # with cmd FE
config2_pla <- paste("|", FeForumala2_pla, "| 0 |  ", clusterVars_form)


# IV formula
first_stage_left <- "on_target"
first_stage_left_pla <- "on_target_plapre"

first_stage_right <- paste(ZVars, collapse = " + ")
first_stage_right_pla <- paste(ZVars_pla, collapse = " + ")


formula_1st <-  paste("(", first_stage_left, " ~ ", first_stage_right, " )")
formula_1st_pla <-  paste("(", first_stage_left_pla, " ~ ", first_stage_right_pla, " )")

config_iv <- paste("|", FeForumala2, "|" ,  formula_1st,  "| ", clusterVars_form)
config_iv_pla <- paste("|", FeForumala2_pla, "|" ,  formula_1st_pla,  "| ", clusterVars_form)


#### Final formulas

Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")
FormulasIV_str <- paste(depVars, paste(rFormula_iv, config_iv), sep = " ~ ")

# Placebo
Formulas01_pla_str <- paste(depVars, paste(rFormula_pla, config1), sep = " ~ ")
Formulas02_pla_str <- paste(depVars, paste(rFormula_pla, config2_pla), sep = " ~ ")
FormulasIV_pla_str <- paste(depVars, paste(rFormula_iv_pla, config_iv_pla), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars
names(FormulasIV_str) <- depVars
names(Formulas01_pla_str) <- depVars
names(Formulas02_pla_str) <- depVars
names(FormulasIV_pla_str) <- depVars

#------------------------------------------------------------------------------#
### Spatial lag formulas ####

# Add FEs
sFormulaFE <- paste0("factor(",FEVars,")")
sFormula1 <- paste(c(indepVars, sFormulaFE[1:3]), collapse = " + ")
sFormula2 <- paste(c(indepVars, sFormulaFE), collapse = " + ")

Formulas01_sl_str <- paste(depVars, sFormula1, sep = " ~ ")
Formulas02_sl_str <- paste(depVars, sFormula2, sep = " ~ ")

names(Formulas01_sl_str) <- depVars
names(Formulas02_sl_str) <- depVars


#------------------------------------------------------------------------------#
#### Placebo analysis ####


# Regressions and Consley SEs - placebo
feRegSim_placebo <- function(form){
  form <- as.formula(form)
  model <- felm(form, data = sr_pl, keepCX = T)
  
  # Rename Dep var for IV just for exporting
  if (!is.null(model$endovars)){
    rownames(model$coefficients)[grep("`on_", rownames(model$coefficients))] <- "on_target_plapre"
    rownames(model$beta)[grep("`on_", rownames(model$beta))] <- "on_target_plapre"
    colnames(model$cX)[grep("`on_", colnames(model$cX))] <- "on_target_plapre"
  }
  
  # Return regression object
  return(model)
  
}


#### Placebo OLS
# Tabble 2
p_vd_01 <- feRegSim_placebo(Formulas01_pla_str["violent_death_sim"])
p_vr_01 <- feRegSim_placebo(Formulas01_pla_str["vehicle_robbery"])
p_rr_01 <- feRegSim_placebo(Formulas01_pla_str["street_robbery"])
# p_hm_01 <- feRegSim_placebo(Formulas01_pla_str["homicide"])
# p_pk_01 <- feRegSim_placebo(Formulas01_pla_str["dpolice_killing"])

# # Table 3 - Gaming 
# p_cf_01 <- feRegSim_placebo(Formulas01_pla_str["dbody_found"])
# p_vt_01 <- feRegSim_placebo(Formulas01_pla_str["vehicle_theft"])
# p_st_01 <- feRegSim_placebo(Formulas01_pla_str["street_theft"])


# # Table 4 - Spillovers
# p_or_01 <- feRegSim_placebo(Formulas01_pla_str["other_robberies"])
# p_cr_01 <- feRegSim_placebo(Formulas01_pla_str["cargo_robbery"])
# p_bu_01 <- feRegSim_placebo(Formulas01_pla_str["burglary"])
# p_sr_01 <- feRegSim_placebo(Formulas01_pla_str["store_robbery"])


### Model 2 whith cmnd FE - placebo

# Tabble 2
p_vd_02 <- feRegSim_placebo(Formulas02_pla_str["violent_death_sim"])
p_vr_02 <- feRegSim_placebo(Formulas02_pla_str["vehicle_robbery"])
p_rr_02 <- feRegSim_placebo(Formulas02_pla_str["street_robbery"])
# p_hm_02 <- feRegSim_placebo(Formulas02_pla_str["homicide"])
# p_pk_02 <- feRegSim_placebo(Formulas02_pla_str["dpolice_killing"])

# # Table 3 - Gaming 
# p_cf_02 <- feRegSim_placebo(Formulas02_pla_str["dbody_found"])
# p_vt_02 <- feRegSim_placebo(Formulas02_pla_str["vehicle_theft"])
# p_st_02 <- feRegSim_placebo(Formulas02_pla_str["street_theft"])
 
 
# # Table 4 - Spillovers
# p_or_02 <- feRegSim_placebo(Formulas02_pla_str["other_robberies"])
# p_cr_02 <- feRegSim_placebo(Formulas02_pla_str["cargo_robbery"])
# p_bu_02 <- feRegSim_placebo(Formulas02_pla_str["burglary"])
# p_sr_02 <- feRegSim_placebo(Formulas02_pla_str["store_robbery"])

#### Placebo 2SLS

# Tabble 2
p_vd_IV <- feRegSim_placebo(FormulasIV_pla_str["violent_death_sim"])
p_vr_IV <- feRegSim_placebo(FormulasIV_pla_str["vehicle_robbery"])
p_rr_IV <- feRegSim_placebo(FormulasIV_pla_str["street_robbery"])
# p_hm_IV <- feRegSim_placebo(FormulasIV_pla_str["homicide"])
# p_pk_IV <- feRegSim_placebo(FormulasIV_pla_str["dpolice_killing"])

# # Table 3 - Gaming 
# p_cf_IV <- feRegSim_placebo(FormulasIV_pla_str["dbody_found"])
# p_vt_IV <- feRegSim_placebo(FormulasIV_pla_str["vehicle_theft"])
# p_st_IV <- feRegSim_placebo(FormulasIV_pla_str["street_theft"])

# # Table 4 - Spillovers
# p_or_IV <- feRegSim_placebo(FormulasIV_pla_str["other_robberies"])
# p_cr_IV <- feRegSim_placebo(FormulasIV_pla_str["cargo_robbery"])
# p_bu_IV <- feRegSim_placebo(FormulasIV_pla_str["burglary"])
# p_sr_IV <- feRegSim_placebo(FormulasIV_pla_str["store_robbery"])



#------------------------------------------------------------------------------#
#### Spatial analysis ####


#------------------------#
#### Create variables ####


# Year and month
#sr_sl$year_month <- sr_sl$year*100 + sr_sl$month

# Deaths per 100 thousand
sr_sl$lv_pop <- sr_sl$violent_death_sim/(sr_sl$population/100000)

# Carjack per 100 thousand
sr_sl$rv_pop <- sr_sl$vehicle_robbery /(sr_sl$population/100000)

# Street robbery per 100 thousand
sr_sl$rr_pop <- sr_sl$street_robbery/(sr_sl$population/100000)

# Montly variation - current month minus previous
sr_sl <- sr_sl %>% 
  group_by(aisp) %>% 
  arrange(aisp, year_month) %>% 
  mutate(lv_pop_d = lv_pop - dplyr::lag(lv_pop, n = 1),
         rv_pop_d = rv_pop - dplyr::lag(rv_pop, n = 1),
         rr_pop_d = rr_pop - dplyr::lag(rr_pop, n = 1),
         # Replace inf values
         lv_pop_d = ifelse(!is.finite(lv_pop_d),NA, lv_pop_d),
         rv_pop_d = ifelse(!is.finite(rv_pop_d),NA, rv_pop_d),
         rr_pop_d = ifelse(!is.finite(rr_pop_d),NA, rr_pop_d))


# Set panel data indexes
ps <- pdata.frame(sr_sl, index = c("aisp", "year_month"))


#### Spatial Variables

# Neighbourhood definition
nb <- poly2nb(aisp, queen = T)

# Neighbour weights
lw <- nb2listw(nb,
               style = "W",
               zero.policy = TRUE)

# Calculate spatial lagged values level
ps$lv_pop_slag <- slag(ps$lv_pop, lw)
ps$rv_pop_slag <- slag(ps$rv_pop, lw)
ps$rr_pop_slag <- slag(ps$rr_pop, lw)



#------------------------------------------------------------------------------#
#### Spatial lag models ####

#### Regressions

# SAR model function
SARlag_reg <- function(formula, data, nbW ){
  spml(formula = as.formula(formula), 
       data=data, 
       model = "pooling",
       lag = T,
       listw = nbW)
}



# Column 1
sl_vd_01 <- SARlag_reg(Formulas01_sl_str["violent_death_sim"],
                       data = ps,
                       nbW = lw)
sl_vr_01 <- SARlag_reg(Formulas01_sl_str["vehicle_robbery"],
                       data = ps,
                       nbW = lw)
sl_rr_01 <- SARlag_reg(Formulas01_sl_str["street_robbery"],
                       data = ps,
                       nbW = lw)

# Column 2 - MULTY COLINEARITY!!!
# sl_vd_02 <- SARlag_reg(Formulas02_sl_str["violent_death_sim"],
#                        data = ps,
#                        nbW = lw)
# sl_vr_02 <- SARlag_reg(Formulas02_sl_str["vehicle_robbery"],
#                        data = ps,
#                        nbW = lw)
# sl_rr_02 <- SARlag_reg(Formulas02_sl_str["street_robbery"],
#                        data = ps,
#                        nbW = lw)


#------------------------------------------------------------------------------#
#### Moran's I ####

# Moran's I level
# moran_lv <- lm(lv_pop_lag ~ lv_pop, data = ps)
# moran_rv <- lm(rv_pop_lag ~ rv_pop, data = ps) %>% summary
# moran_rr <- lm(rr_pop_lag ~ rr_pop, data = ps) %>% summary


# Moran's I delta
#can't have NAs, so I'm removing all obs where the
#monthly crime difference is NA
ps_semJAn <- ps %>% subset(month != 1) 

# Calculate spatial lagged values diff
ps_semJAn$lv_pop_d_slag <- slag(ps_semJAn$lv_pop_d, lw)
ps_semJAn$rv_pop_d_slag <- slag(ps_semJAn$rv_pop_d, lw)
ps_semJAn$rr_pop_d_slag <- slag(ps_semJAn$rr_pop_d, lw)


moran_lv <- lm(lv_pop_d_slag ~ lv_pop_d + factor(year) + factor(month), data = ps_semJAn)
moran_rv <- lm(rv_pop_d_slag ~ rv_pop_d + factor(year) + factor(month), data = ps_semJAn)
moran_rr <- lm(rr_pop_d_slag ~ rr_pop_d + factor(year) + factor(month), data = ps_semJAn)

#------------------------------------------------------------------------------#
##### Export ####




