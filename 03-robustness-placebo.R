#------------------------------------------------------------------------------#

#	 SIM - Robustness Placebo

#------------------------------------------------------------------------------#


# Add lines in table
# Add DD models




# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}

#------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

#------------------------------------------------------------------------------#
#### Process data ####

#### Placebo
sr_pl <- sr[sr$year < 2009,]

#### Spatial analysis

# Keep only analysis years
# sr_sl <- sr %>% subset(year_month > 200912 & year_month < 201507)
sr_sl <- sr %>% subset(sem_year > 100)


#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery")


indepVars_pla <- c("on_target_plapre",
                   #"policemen_aisp",
                   #"policemen_upp",
                   "n_precinct",
                   #"max_prize",
                   "population" )

FEVars_pla <- c("aisp",
                "year", 
                "month", 
                "cmd_name")

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
p_vd_01_data <- regData(p_vd_01, sr_pl)

p_vr_01 <- feRegSim_placebo(Formulas01_pla_str["vehicle_robbery"])
p_vr_01_data <- regData(p_vr_01, sr_pl)

p_rr_01 <- feRegSim_placebo(Formulas01_pla_str["street_robbery"])
p_rr_01_data <- regData(p_rr_01, sr_pl)


### Model 2 whith cmnd FE - placebo

# Tabble 2
p_vd_02 <- feRegSim_placebo(Formulas02_pla_str["violent_death_sim"])
p_vd_02_data <- regData(p_vd_02, sr_pl)

p_vr_02 <- feRegSim_placebo(Formulas02_pla_str["vehicle_robbery"])
p_vr_02_data <- regData(p_vr_02, sr_pl)

p_rr_02 <- feRegSim_placebo(Formulas02_pla_str["street_robbery"])
p_rr_02_data <- regData(p_rr_02, sr_pl)


#### Placebo DD

#------------------------------------------------------------------------------#
##### Placebo exporting ####


#### Export placebo table ####

indepVar_label <- c("On target" = "on_target")
indepVar_label_pla <- c("On target" = "on_target_plapre")

stats_labels <- c("Observations" = "nobs",  
                  "R2 adjusted" = "adj.r.squared")

#models_labels <- c("Violent Death", "Violent Death", "Carjacking", "Carjacking")


# models_labels <- c("Model 1" = "OLS", 
#                    "Model 2" = "OLS", 
#                    "Model 4" = "OLS",
#                    "Model 5" = "OLS", 
#                    "Model 7" = "OLS", 
#                    "Model 8" = "OLS")

# Table 2 - placebo
tab2_pla_regs <-
  list(p_vd_01,
       p_vd_02,
       p_vr_01,
       p_vr_02,
       p_rr_01,
       p_rr_02)


stargazer(tab2_pla_regs,
          keep = c("on_target_plapre"),
          dep.var.labels = "",
          covariate.labels = c("On target"),
          column.labels = c("Violent deaths", 
                            "Vehicle robbery (Carjacking)",	
                            "Street robbery"),
          title = "Foo",
          dep.var.caption  = "Number  of  occurrences",
          # add.lines = tab5_addLines,
          digits = 3,
          omit.stat = c("rsq","ser", "f"),
          # out = file.path(OUTPUTS_final, "tabB4.html"),
          type = 'text'
)

