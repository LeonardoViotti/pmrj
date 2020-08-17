#------------------------------------------------------------------------------#

#	 SIM - Troop size interactions DRAFT		

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### Process data ####

sr$policemen_all <- (sr$policemen_aisp + sr$policemen_upp)
sr$policenen_75 <- (sr$policemen_all <= quantile(sr$policemen_all, 
                                              na.rm = T, 
                                              probs = 0.25)) %>% as.integer()

sr$on_target_policemen <- sr$on_target * sr$policenen_75
#------------------------------------------------------------------------------#
#### List regression variables ####

####  List regression variables 

####  List regression variables 

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
names(depVars) <- depVars


indepVars <- c("on_target",
               "on_target_policemen",
               # "policemen_aisp",
               # "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )
names(indepVars) <- indepVars

FEVars <- c("aisp",
            "year", 
            "month", 
            "id_cmt")
names(FEVars) <- FEVars


ZVars <- c("lag12_dist_target_vr",
           "lag12_dist_target_sr",
           "lag12_dist_target_vd")
names(ZVars) <- ZVars



#------------------------------------------------------------------------------#
### OLS formulas ####

# Right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 
rFormula_iv_pla <- paste(indepVars_pla[-1], collapse = " + ") 

# Add FE, cluster and instruments

#clusterVars = c("aisp")
clusterVars= "0"

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
#### OLS models ####


# Original regressions and Consley SEs
feRegSim <- function(form, data = sr){
  form <- as.formula(form)
  #model <- felm(form, data = sr[year_month > 200906 & year_month < 201501,], keepCX = T)
  model <- felm(form, data = data, keepCX = T)
  
  
  # Rename Dep var for IV just for exporting
  if (!is.null(model$endovars)){
    rownames(model$coefficients)[grep("`on_", rownames(model$coefficients))] <- "on_target"
    rownames(model$beta)[grep("`on_", rownames(model$beta))] <- "on_target"
    colnames(model$cX)[grep("`on_", colnames(model$cX))] <- "on_target"
    
  }
  
  
  # Return regression object
  return(model)
  
}


### Model 1 whithout cmnd FE

# Tabble 2
r_vd_01 <- feRegSim(Formulas01_str["violent_death_sim"])
r_vd_01_data <-  regData(r_vd_01, regdf = sr)

r_vr_01 <- feRegSim(Formulas01_str["vehicle_robbery"])
r_vr_01_data <-  regData(r_vr_01, regdf = sr)

r_rr_01 <- feRegSim(Formulas01_str["street_robbery"])
r_rr_01_data <-  regData(r_rr_01, regdf = sr)

r_hm_01 <- feRegSim(Formulas01_str["homicide"])
r_hm_01_data <-  regData(r_hm_01, regdf = sr)

r_pk_01 <- feRegSim(Formulas01_str["dpolice_killing"])
r_pk_01_data <-  regData(r_pk_01, regdf = sr)


# Table 3 - Gaming 
g_cf_01 <- feRegSim(Formulas01_str["dbody_found"])
g_cf_01_data <-  regData(g_cf_01, regdf = sr)

g_vt_01 <- feRegSim(Formulas01_str["vehicle_theft"])
g_vt_01_data <-  regData(g_vt_01, regdf = sr)

g_st_01 <- feRegSim(Formulas01_str["street_theft"])
g_st_01_data <-  regData(g_st_01, regdf = sr)


# Table 4 - Spillovers
s_or_01 <- feRegSim(Formulas01_str["other_robberies"])
s_or_01_data <-  regData(s_or_01, regdf = sr)

s_cr_01 <- feRegSim(Formulas01_str["cargo_robbery"])
s_cr_01_data <-  regData(s_cr_01, regdf = sr)

s_bu_01 <- feRegSim(Formulas01_str["burglary"])
s_bu_01_data <-  regData(s_bu_01, regdf = sr)

s_sr_01 <- feRegSim(Formulas01_str["store_robbery"])
s_sr_01_data <-  regData(s_sr_01, regdf = sr)



### Model 2 whith cmnd FE

# Tabble 2
r_vd_02 <- feRegSim(Formulas02_str["violent_death_sim"])
r_vd_02_data <-  regData(r_vd_02, regdf = sr)

r_vr_02 <- feRegSim(Formulas02_str["vehicle_robbery"])
r_vr_02_data <-  regData(r_vr_02, regdf = sr)

r_rr_02 <- feRegSim(Formulas02_str["street_robbery"])
r_rr_02_data <-  regData(r_rr_02, regdf = sr)

r_hm_02 <- feRegSim(Formulas02_str["homicide"])
r_hm_02_data <-  regData(r_hm_02, regdf = sr)

r_pk_02 <- feRegSim(Formulas02_str["dpolice_killing"])
r_pk_02_data <-  regData(r_pk_02, regdf = sr)



# Table 3 - Gaming 
g_cf_02 <- feRegSim(Formulas02_str["dbody_found"])
g_cf_02_data <-  regData(g_cf_02, regdf = sr)

g_vt_02 <- feRegSim(Formulas02_str["vehicle_theft"])
g_vt_02_data <-  regData(g_vt_02, regdf = sr)

g_st_02 <- feRegSim(Formulas02_str["street_theft"])
g_st_02_data <-  regData(g_st_02, regdf = sr)


# Table 4 - Spillovers
s_or_02 <- feRegSim(Formulas02_str["other_robberies"])
s_or_02_data <-  regData(s_or_02, regdf = sr)

s_cr_02 <- feRegSim(Formulas02_str["cargo_robbery"])
s_cr_02_data <-  regData(s_cr_02, regdf = sr)

s_bu_02 <- feRegSim(Formulas02_str["burglary"])
s_bu_02_data <-  regData(s_bu_02, regdf = sr)

s_sr_02 <- feRegSim(Formulas02_str["store_robbery"])
s_sr_02_data <-  regData(s_sr_02, regdf = sr)

#### Model 3 2SLS

# Tabble 2
r_vd_IV <- feRegSim(FormulasIV_str["violent_death_sim"])
r_vd_IV_data <-  regData(r_vd_IV, regdf = sr)

r_vr_IV <- feRegSim(FormulasIV_str["vehicle_robbery"])
r_vr_IV_data <-  regData(r_vr_IV, regdf = sr)

r_rr_IV <- feRegSim(FormulasIV_str["street_robbery"])
r_rr_IV_data <-  regData(r_rr_IV, regdf = sr)

r_hm_IV <- feRegSim(FormulasIV_str["homicide"])
r_hm_IV_data <-  regData(r_hm_IV, regdf = sr)

r_pk_IV <- feRegSim(FormulasIV_str["dpolice_killing"])
r_pk_IV_data <-  regData(r_pk_IV, regdf = sr)


# Table 3 - Gaming 
g_cf_IV <- feRegSim(FormulasIV_str["dbody_found"])
g_cf_IV_data <-  regData(g_cf_IV, regdf = sr)

g_vt_IV <- feRegSim(FormulasIV_str["vehicle_theft"])
g_vt_IV_data <-  regData(g_vt_IV, regdf = sr)

g_st_IV <- feRegSim(FormulasIV_str["street_theft"])
g_st_IV_data <-  regData(g_st_IV, regdf = sr)


# Table 4 - Spillovers
s_or_IV <- feRegSim(FormulasIV_str["other_robberies"])
s_or_IV_data <-  regData(s_or_IV, regdf = sr)

s_cr_IV <- feRegSim(FormulasIV_str["cargo_robbery"])
s_cr_IV_data <-  regData(s_cr_IV, regdf = sr)

s_bu_IV <- feRegSim(FormulasIV_str["burglary"])
s_bu_IV_data <-  regData(s_bu_IV, regdf = sr)

s_sr_IV <- feRegSim(FormulasIV_str["store_robbery"])
s_sr_IV1_data <-  regData(s_sr_IV, regdf = sr)

#------------------------------------------------------------------------------#
##### Export ####

#### Define commun elements
n_aisp_line_3 <- c("Number of aisp", rep("39", 3))
n_aisp_line_9 <- c("Number of aisp", rep("39", 9))
n_aisp_line_12 <- c("Number of aisp", rep("39", 12))

chifeFE_line_3 <- c("Chief FE", rep(c( "No", "Yes", "Yes"), 1))
chifeFE_line_9 <- c("Chief FE", rep(c( "No", "Yes", "Yes"), 3))
chifeFE_line_12 <- c("Chief FE", rep(c( "No", "Yes", "Yes"), 4))

indepVar_label <- "On target"
col_labels_9 <- rep(c("OLS",	"OLS",	"2SLS"), 3)
col_labels_12 <- rep(c("OLS",	"OLS",	"2SLS"), 4)



#### Define formatting functions

# Function to find dep var means of regressions
Ymean <- function(x){
  mean(regData(x, sr)[,regDepVars(x)])
}

# Function to create the row for regression tables
Ymean_row <- function(list){
  c("Y mean", sapply(list, Ymean) %>% round(2))
}


# Export function
createTable <- function(reg_list, 
                        add_lines_list,
                        title,
                        dep_var_labels,
                        outPath){
  stargazer(reg_list,
            keep = ("on_target"),
            covariate.labels = "On target",
            dep.var.labels = dep_var_labels,
            title = title,
            dep.var.caption  = "Number  of  occurrences",
            column.labels   = col_labels_9,
            add.lines = add_lines_list,
            digits = 3,
            omit.stat = c("rsq","ser", "f"),
            out = outPath,
            type = "text"
  )
}


# Table 2
tab2_regs <- 
  list(r_vd_01, 
       r_vd_02, 
       r_vd_IV,
       r_vr_01, 
       r_vr_02,
       r_vr_IV,
       r_rr_01, 
       r_rr_02,
       r_rr_IV)

tab2_addLines <- list(chifeFE_line_9,
                      Ymean_row(tab2_regs),
                      n_aisp_line_9)


createTable(reg_list = tab2_regs,
            add_lines_list = tab2_addLines,
            dep_var_labels = c("Violent deaths", 
                               "Vehicle robbery (Carjacking)",	
                               "Street robbery"),
            title = "Table 2 Effect of expectancy of receiving bonuses on crime rates",
            outPath = file.path(OUTPUTS_final, "tab2_interac.html"))




# Table 3
tab3_regs <- 
  list(g_cf_01, 
       g_cf_02, 
       g_cf_IV, 
       g_vt_01, 
       g_vt_02,
       g_vt_IV,
       g_st_01, 
       g_st_02,
       g_st_IV)


tab3_addLines <- list(chifeFE_line_9,
                      Ymean_row(tab3_regs),
                      n_aisp_line_9)


createTable(reg_list = tab3_regs,
            add_lines_list = tab3_addLines,
            dep_var_labels = c("Cadavers Found (dummy)", 
                               "Car theft",	
                               "Street theft"),
            title = "Table A1 Expectancy of receiving bonuses and gaming",
            outPath = file.path(OUTPUTS_final, "tabA1_interac.html"))



# Table 4
tab4_regs <- 
  list(s_or_01, 
       s_or_02, 
       s_or_IV, 
       s_cr_01, 
       s_cr_02,
       s_cr_IV,
       s_bu_01, 
       s_bu_02,
       s_bu_IV,
       s_sr_01,
       s_sr_02,
       s_sr_IV)

tab4_addLines <- list(chifeFE_line_12,
                      Ymean_row(tab4_regs),
                      n_aisp_line_12)


createTable(reg_list = tab4_regs,
            add_lines_list = tab4_addLines,
            dep_var_labels = c("Robberies not included in the target", 
                               "Cargo robbery	",	
                               "Burglary",
                               "Robbery of commercial stores"),
            title = "Table 3  Expectancy of receiving bonuses and positive spill overs on other crimes",
            outPath = file.path(OUTPUTS_final, "tab3_interac.html")) # Order changed in paper


