
#------------------------------------------------------------------------------#

#	 SIM - Main Regressions		

#------------------------------------------------------------------------------#


# TO DOS:

#               EXPORTAR TABLEAS PARA O WORD
#               RODAR MODELO DE SPATIAL LAG
#               RODAR TESTE DE ENDOGENEIDADE DO INSTRUMENTO
#               EXPLICAR QUE EU NAO CONSIGO FAZER EXATAMENTE A MESMA COISA NO IV PORQUE A BASE E MENOR



EXPORT_tables = F

#------------------------------------------------------------------------------#
#### Load data ####


# sr <-  read.dta13(file.path(DROPBOX, "data_SIM_2019-01.dta")) 
# placebo_gis <- read.csv(file.path(DROPBOX, "placebo_targets.csv"), header = T)
# 
# # Add placebos and coordinates
# sr <- merge(sr, placebo_gis, by = c("aisp", "year", "month" , "semester"), all.x = T)


sr <- fread(file = file.path(DATA, "sim2019.csv"),
             encoding = "UTF-8")

sr$year_month <- sr$year*100+ sr$month


#------------------------------------------------------------------------------#
#### Reconstruct instrument ####

# bar <- sr %>% 
#   subset(year_month > 200906 & year_month < 201507)
# 
# foo <- sr %>% 
#   subset(year_month > 200906 & year_month < 201507)
#   mutate(dist_target_vd=violent_death_sim_cum /target_vd_sem -1,
#          lag12_dist_target_vd = dplyr::lag(dist_target_vd,12))

# gen dist_target_vd=violent_death_sim_cum /target_vd_sem -1 
# bysort aisp (month_year): gen lag12_dist_target_vd=dist_target_vd[_n-12]
# 
# gen dist_target_vr=(vehicle_robbery_cum)/(target_vr_sem ) -1 
# bysort aisp (month_year): gen lag12_dist_target_vr=dist_target_vr[_n-12]
# 
# gen dist_target_sr=street_robbery_cum /target_sr_sem -1 
# bysort aisp (month_year): gen lag12_dist_target_sr=dist_target_sr[_n-12]


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

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 
rFormula_iv_pla <- paste(indepVars_pla[-1], collapse = " + ") 

# Add FE, cluster and instruments

# clusterVars = c("latitude", "longitude" )
#clusterVars = c("aisp" )
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

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars
names(FormulasIV_str) <- depVars

#rFormulaFE <- paste0("factor(",FEVars,")")
# rFormula1 <- paste(c(indepVars, rFormulaFE[1:2]), collapse = " + ") 
# rFormula2 <- paste(c(indepVars, rFormulaFE), collapse = " + ") 


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
#### OLS models ####

# Conley SE function
# clse <- function(reg){
#   vcv <- ConleySEs(reg = reg,
#                    unit = "aisp", 
#                    time = c("year","month"),
#                    lat = "latitude", lon = "longitude")$Spatial_HAC
#   ses <- diag(vcv)
#   return(ses)
# }

# Original regressions and Consley SEs
feRegSim <- function(form){
  form <- as.formula(form)
  #model <- felm(form, data = sr[year_month > 200906 & year_month < 201501,], keepCX = T)
  model <- felm(form, data = sr[sem_year > 100,], keepCX = T)

  
  # Rename Dep var for IV just for exporting
  if (!is.null(model$endovars)){
    rownames(model$coefficients)[grep("`on_", rownames(model$coefficients))] <- "on_target"
    rownames(model$beta)[grep("`on_", rownames(model$beta))] <- "on_target"
    colnames(model$cX)[grep("`on_", colnames(model$cX))] <- "on_target"
    
  }
  
  # # Replace clust. SEs with Conley SEs
  # model$cse <- clse(model)
  # 
  # # Replace p-values with new ones from Conley SEs
  # model$cpval <-2*pt(-abs(model$coefficients/model$cse),
  #                    df=model$df)
  
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


models_labels_pla <- c("Model 1" = "OLS", 
                       "Model 2" = "IV", 
                       "Model 3" = "OLS", 
                       "Model 4" = "IV",
                       "Model 5" = "OLS", 
                       "Model 6" = "IV",
                       "Model 7" = "OLS", 
                       "Model 8" = "IV")



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


colTitles = c("Violent Death", "Vehicle robbery", "Street robbery")

add_vec <- ""
for (ndp in 1:length(colTitles)){
  add_vec <- c(add_vec, c("", colTitles[ndp], ""))
}


