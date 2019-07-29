
#------------------------------------------------------------------------------#

#	 SIM - Main Regressions		

#------------------------------------------------------------------------------#

EXPORT_tables = F

#------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

# Keep same sample for all models, i.e from 2010 onwards because of IV
sr <- sr[sem_year > 100,]

#------------------------------------------------------------------------------#
#### List regression variables ####

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
               "policemen_aisp",
               "policemen_upp",
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

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 

# Add FE, cluster and instruments

# clusterVars = c("latitude", "longitude" )
#clusterVars = c("aisp" )
clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)

# IV formula
first_stage_left <- "on_target"

first_stage_right <- paste(ZVars, collapse = " + ")

formula_1st <-  paste("(", first_stage_left, " ~ ", first_stage_right, " )")

config_iv <- paste("|", FeForumala2, "|" ,  formula_1st,  "| ", clusterVars_form)


#### Final formulas

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
#### Poisson formulas ####


form1 <- vehicle_robbery ~ on_target + 
  factor(year) +  factor( month) + factor(aisp) +factor(id_cmt) +
  policemen_aisp + policemen_upp + n_precinct+ offset(log(population))


# Remove max prize for some reason
poisson_indepVars <- indepVars[!(names(indepVars) %in% c("max_prize", "population"))]


# Fixed effects
sFormulaFE_poi <- paste(paste0("factor(",FEVars,")"), collapse = " + ") 

# Construct right hand sied fo eq.
rFormula_poi_0 <- paste(poisson_indepVars, collapse = " + ") 

# Add FEs
rFormula_poi_1 <-  paste(rFormula_poi_0, "+", sFormulaFE_poi)

# Add Exposure variable

# Exposure Variable
exposure_variable <- "population"

paste0(" offset(log(", exposure_variable, ")")

rFormula_poi <-  paste(rFormula_poi_1,
                       "+", 
                       paste0(" offset(log(", 
                              exposure_variable, 
                              "))"
                              )
                       )


# Final formula
Formulas_poi_str <- paste(depVars, rFormula_poi, sep = " ~ ")
names(Formulas_poi_str) <- depVars

#------------------------------------------------------------------------------#
#### OLS models ####


# Original regressions and Consley SEs
feRegSim <- function(form){
  form <- as.formula(form)
  #model <- felm(form, data = sr[year_month > 200906 & year_month < 201501,], keepCX = T)
  model <- felm(form, data = sr, keepCX = T)

  
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
r_vr_01 <- feRegSim(Formulas01_str["vehicle_robbery"])
r_rr_01 <- feRegSim(Formulas01_str["street_robbery"])
r_hm_01 <- feRegSim(Formulas01_str["homicide"])
r_pk_01 <- feRegSim(Formulas01_str["dpolice_killing"])

# Table 3 - Gaming 
g_cf_01 <- feRegSim(Formulas01_str["dbody_found"])
g_vt_01 <- feRegSim(Formulas01_str["vehicle_theft"])
g_st_01 <- feRegSim(Formulas01_str["street_theft"])


# Table 4 - Spillovers
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


# Table 4 - Spillovers
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


# Table 4 - Spillovers
s_or_IV <- feRegSim(FormulasIV_str["other_robberies"])
s_cr_IV <- feRegSim(FormulasIV_str["cargo_robbery"])
s_bu_IV <- feRegSim(FormulasIV_str["burglary"])
s_sr_IV <- feRegSim(FormulasIV_str["store_robbery"])


#------------------------------------------------------------------------------#
#### Poisson models ####


RegPoisson <- function(form){
  model <- 
    glm(as.formula(form),
        family = poisson,
        data = sr)
  return(model)
  
}

p_vd <- RegPoisson(Formulas_poi_str["violent_death_sim"])
p_vr <- RegPoisson(Formulas_poi_str["vehicle_robbery"])
p_rr <- RegPoisson(Formulas_poi_str["street_robbery"])
p_po <- RegPoisson(Formulas_poi_str["dpolice_killing"])


#------------------------------------------------------------------------------#
##### Export ####

indepVar_label <- c("On target" = "on_target")
indepVar_label_pla <- c("On target" = "on_target_plapre")

stats_labels <- c("Observations" = "nobs",  
                  "R2 adjusted" = "adj.r.squared")

#models_labels <- c("Violent Death", "Violent Death", "Carjacking", "Carjacking")


models_labels <- c("Model 1" = "OLS", 
                   "Model 2" = "OLS", 
                   "Model 3" = "IV", 
                   "Model 4" = "OLS",
                   "Model 5" = "OLS", 
                   "Model 6" = "IV",
                   "Model 7" = "OLS", 
                   "Model 8" = "OLS", 
                   "Model 9" = "IV",
                   "Model 10" = "OLS", 
                   "Model 11" = "OLS", 
                   "Model 12" = "IV")



# Table 2
tab2 <- 
  export_summs(r_vd_01, 
               r_vd_02, 
               r_vd_IV,
               r_vr_01, 
               r_vr_02,
               r_vr_IV,
               r_rr_01, 
               r_rr_02,
               r_rr_IV,
               digits = 3,
               scale = TRUE,
               coefs = indepVar_label,
               statistics = stats_labels,
               model.names = models_labels[1:9] #,
               # to.file ="xlsx",
               # file.name = file.path(OUTPUTS,"tab2.xlsx")
               )




# Table 3
tab3 <- 
  export_summs(g_cf_01, 
               g_cf_02, 
               g_cf_IV, 
               g_vt_01, 
               g_vt_02,
               g_vt_IV,
               g_st_01, 
               g_st_02,
               g_st_IV,
               digits = 3,
               scale = TRUE,
               transform.response = T,
               coefs = indepVar_label,
               model.names = models_labels[1:9],
               statistics = stats_labels #,
               # to.file ="xlsx",
               # file.name = file.path(OUTPUTS,"tab3.xlsx")
               )



# Table 4
tab4 <- 
  export_summs(s_or_01, 
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
               s_sr_IV,
               digits = 3,
               scale = TRUE,
               transform.response = T,
               coefs = indepVar_label,
               model.names = models_labels,
               statistics = stats_labels #,
               # to.file ="xlsx",
               # file.name = file.path(OUTPUTS,"tab4.xlsx")
               )

# Poisson model
# tab5 <- 
#   export_summs(p_vd, 
#                p_vr,
#                p_rr,
#                p_po,
#                digits = 3,
#                scale = TRUE,
#                transform.response = T,
#                coefs = indepVar_label,
#                model.names = models_labels,
#                statistics = stats_labels #,
#                # to.file ="xlsx",
#                # file.name = file.path(OUTPUTS,"tab4.xlsx")
#   )



#### Table edits
editTables <- function(regTab, depVarLabel = "Number of occurrences", colTitles, nDepVars = 3) {
  
  # Dependent variables labels
  add_vec <- ""
  for (ndp in 1:length(colTitles)){
    add_vec <- c(add_vec, c("", colTitles[ndp], ""))
  }
  
  add_header <- hux(rbind(add_vec))
  
  # Additional lines in the bottom
  add_lines <- 
    hux(
      rbind(
        c("Chief FE" , rep(c("No", "Yes", "Yes"), length(colTitles)))
      ) )
  
  # Add column numbers
  add_colNumbers <-
    hux(
      rbind(
        c("" , paste0("(", 2:length(regTab)-1, ")"))
      ) )
  
  # Make this more stable
  regTab <- rbind(add_header, 
                  regTab[1], 
                  add_colNumbers,
                  regTab[2:4], 
                  add_lines, 
                  regTab[5:6,])
  
  # Edit cell borders
  bottom_border(regTab)[3, ] <- 0.4
  # Cell merges
  #regTab  <- regTab %>% merge_cells(1:1, 1:ncol(regTab)) 
  
  
  # Formating
  align(regTab) <- "center"
  align(regTab[1:nrow(regTab)-1,1]) <- "left"
  
  font_size(regTab) <- 10
  
  return(regTab)
  
}


tab2_formated <- editTables(tab2, colTitles = c("Violent Death", "Vehicle robbery", "Street robbery"))
tab3_formated <- editTables(tab3, colTitles = c("Cadavers Found (dummy)", "Car theft", "Street theft"))
tab4_formated <- editTables(tab4, 
                            colTitles = c("Robberies not included in the target", 
                                          "Cargo robbery",
                                          "Burglary",
                                          "Robbery of commercial stores"))



# Export

if(EXPORT_tables){
  huxtable::quick_docx(tab2_formated, file = file.path(OUTPUTS_final, "tab2_formated2.docx"))
  huxtable::quick_docx(tab3_formated, file = file.path(OUTPUTS_final, "tab3_formated2.docx"))
  huxtable::quick_docx(tab4_formated, file = file.path(OUTPUTS_final, "tab4_formated2.docx"))
  
}



