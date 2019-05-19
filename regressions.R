
#------------------------------------------------------------------------------#

#	 SIM - Regressions		

#------------------------------------------------------------------------------#


sr <-  read.dta13(file.path(DROPBOX, "data_SIM_2019-01.dta")) 
placebo_gis <- read.csv(file.path(DROPBOX, "placebo_targets.csv"), header = T)

# Add placebos and coordinates
sr <- merge(sr, placebo_gis, by = c("aisp", "year", "month" , "semester"), all.x = T)

#### List variables

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "homicide",
             "dpolice_killing",
             "vehicle_theft",
             "street_theft",
             "dbody_found",
             "other_robberies",
             "cargo_robbery",
             "burglary",
             "store_robbery")

indepVars <- c("on_target",
               "policemen_aisp",
               "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )

FEVars <- c("aisp",
            "year", 
            "month", 
            "id_cmt")

ZVars <- c("lag12_dist_target_vr",
           "lag12_dist_target_sr",
           "lag12_dist_target_vd")



#------------------------------------------------------------------------------#
### Regression formulas

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_iv <- paste(indepVars[-1], collapse = " + ") 

# Add FE, cluster and instruments

clusterVars = c("latitude", "longitude" )
#clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 | 0 ")


# IV formula
first_stage_left <- "on_target"
first_stage_right <- paste(ZVars, collapse = " + ")
formula_1st <-  paste("(", first_stage_left, " ~ ", first_stage_right, " )")

config_iv <- paste("|", FeForumala2, "|" ,  formula_1st,  "| 0 ")


# Final formulas

Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")
FormulasIV_str <- paste(depVars, paste(rFormula_iv, config_iv), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars
names(FormulasIV_str) <- depVars



#rFormulaFE <- paste0("factor(",FEVars,")")
# rFormula1 <- paste(c(indepVars, rFormulaFE[1:2]), collapse = " + ") 
# rFormula2 <- paste(c(indepVars, rFormulaFE), collapse = " + ") 


#------------------------------------------------------------------------------#
### Spatial lag model formula

# Add FEs
sFormulaFE <- paste0("factor(",FEVars,")")
sFormula1 <- paste(c(indepVars, sFormulaFE[1:2]), collapse = " + ")
sFormula2 <- paste(c(indepVars, sFormulaFE), collapse = " + ")

Formulas01_lg <- paste(depVars, sFormula1, sep = " ~ ")


#------------------------------------------------------------------------------#
#### Regression models ####

feRegSim <- function(form){
  form <- as.formula(form)
  felm(form, data = sr[sr$sem_year >100,])
}


### Model 1 whithout cmnd FE

# Tabble 2
r_vd_01 <- feRegSim(Formulas01_str["violent_death_sim"])
r_vr_01 <- feRegSim(Formulas01_str["vehicle_robbery"])
r_rr_01 <- feRegSim(Formulas01_str["street_robbery"])
r_hm_01 <- feRegSim(Formulas01_str["homicide"])
r_pk_01 <- feRegSim(Formulas01_str["dpolice_killing"])

# Table 3 - Gaming 
g_cf_01 <- feRegSim(Formulas01_str["dbody_found"])
g_vt_01 <- feRegSim(Formulas01_str["vehicle_theft"])
g_st_01 <- feRegSim(Formulas01_str["street_theft"])


# Table $ - Spillovers
s_or_01 <- feRegSim(Formulas01_str["other_robberies"])
s_cr_01 <- feRegSim(Formulas01_str["cargo_robbery"])
s_bu_01 <- feRegSim(Formulas01_str["burglary"])
s_sr_01 <- feRegSim(Formulas01_str["store_robbery"])



### Model 2 whith cmnd FE

# Tabble 2
r_vd_02 <- feRegSim(Formulas02_str["violent_death_sim"])
r_vr_02 <- feRegSim(Formulas02_str["vehicle_robbery"])
r_rr_02 <- feRegSim(Formulas02_str["street_robbery"])
r_hm_02 <- feRegSim(Formulas02_str["homicide"])
r_pk_02 <- feRegSim(Formulas02_str["dpolice_killing"])

# Table 3 - Gaming 
g_cf_02 <- feRegSim(Formulas02_str["dbody_found"])
g_vt_02 <- feRegSim(Formulas02_str["vehicle_theft"])
g_st_02 <- feRegSim(Formulas02_str["street_theft"])


# Table $ - Spillovers
s_or_02 <- feRegSim(Formulas02_str["other_robberies"])
s_cr_02 <- feRegSim(Formulas02_str["cargo_robbery"])
s_bu_02 <- feRegSim(Formulas02_str["burglary"])
s_sr_02 <- feRegSim(Formulas02_str["store_robbery"])

#### Model 3 2SLS

# Tabble 2
r_vd_IV <- feRegSim(FormulasIV_str["violent_death_sim"])
r_vr_IV <- feRegSim(FormulasIV_str["vehicle_robbery"])
r_rr_IV <- feRegSim(FormulasIV_str["street_robbery"])
r_hm_IV <- feRegSim(FormulasIV_str["homicide"])
r_pk_IV <- feRegSim(FormulasIV_str["dpolice_killing"])

# Table 3 - Gaming 
g_cf_IV <- feRegSim(FormulasIV_str["dbody_found"])
g_vt_IV <- feRegSim(FormulasIV_str["vehicle_theft"])
g_st_IV <- feRegSim(FormulasIV_str["street_theft"])


# Table $ - Spillovers
s_or_IV <- feRegSim(FormulasIV_str["other_robberies"])
s_cr_IV <- feRegSim(FormulasIV_str["cargo_robbery"])
s_bu_IV <- feRegSim(FormulasIV_str["burglary"])
s_sr_IV <- feRegSim(FormulasIV_str["store_robbery"])



#------------------------------------------------------------------------------#
#### Spatial lag ####

# #foo <- as.formula(Formulas01_lg)
# foo <- formula("")
# 
# lagsarlm(foo, 
#          listw = lw, 
#          data = sr, 
#          method="Matrix")


forumla.f <-  "violent_death_sim ~ on_target | month_year + aisp  | 0 |  latitude + longitude" 
forumla.f <- as.formula(forumla.f) 

reg.f <- felm(forumla.f, data = sr, keepCX = T)

ConleySEs(reg = reg.f,
          unit = "aisp", 
          time = "month_year",
          dist_cutoff = 5,
          #balanced_pnl = F,
          #dist_fn = "Haversine", lag_cutoff = 5, cores = 1, verbose = FALSE,
          lat = "latitude", lon = "longitude") 


#------------------------------------------------------------------------------#
# ##### Export ####
# foo <- 
# export_summs(r_vd, 
#              r_vr, 
#              r_rr,
#              #to.file = "docx",
#              coefs = "on_target",
#              statistics = c("nobs", "adj.r.squared") 
#              #file.name = "C:/Users/wb519128/Desktop/test.docx"
#              )
# 
# bar <- hux("names" = "FE",
#            "Model 1"  = "No",
#            "Model 2"  = "No",
#            "Model 3"  = "Yes")
# names(bar) <- names(foo)
# 
# fob <- rbind(foo[1:3], bar, foo[4:nrow(foo)])
# 
# export_summs(fob,
#              file.name = "C:/Users/wb519128/Desktop/test.docx"
# )
# 
# ff <- as_flextable(fob)
