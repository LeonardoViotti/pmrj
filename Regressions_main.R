
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


sr <-  read.dta13(file.path(DROPBOX, "data_SIM_2019-01.dta")) 
placebo_gis <- read.csv(file.path(DROPBOX, "placebo_targets.csv"), header = T)

# Add placebos and coordinates
sr <- merge(sr, placebo_gis, by = c("aisp", "year", "month" , "semester"), all.x = T)


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
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)


# IV formula
first_stage_left <- "on_target"
first_stage_right <- paste(ZVars, collapse = " + ")
formula_1st <-  paste("(", first_stage_left, " ~ ", first_stage_right, " )")

config_iv <- paste("|", FeForumala2, "|" ,  formula_1st,  "| ", clusterVars_form)


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


clse <- function(reg){
  vcv <- ConleySEs(reg = reg,
                   unit = "aisp", 
                   time = c("year","month"),
                   lat = "latitude", lon = "longitude")$Spatial_HAC
  ses <- diag(vcv)
  return(ses)
}


# Original regressions and Consley SEs
feRegSim <- function(form){
  form <- as.formula(form)
  model <- felm(form, data = sr[sr$sem_year >100,], keepCX = T)
  
  
  # Rename Dep var for IV just for exporting
  if (!is.null(model$endovars)){
    rownames(model$coefficients)[grep("`on_", rownames(model$coefficients))] <- "on_target"
    rownames(model$beta)[grep("`on_", rownames(model$beta))] <- "on_target"
    colnames(model$cX)[grep("`on_", colnames(model$cX))] <- "on_target"
    
  }
  
  # Replace clust. SEs with Conley SEs
  model$cse <- clse(model)
  
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

stats_labels <- c("Observations" = "nobs",  
                  "R2 adjusted" = "adj.r.squared")

models_labels <- c("Violent Death", "Violent Death", "Carjacking", "Carjacking")


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
  
  # Make this more stable
  regTab <- rbind(add_header, regTab[1:4,], add_lines, regTab[5:6,])
  
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
  huxtable::quick_docx(tab2_formated, file = file.path(OUTPUTS, "tab2_formated_draft.docx"))
  huxtable::quick_docx(tab3_formated, file = file.path(OUTPUTS, "tab3_formated_draft.docx"))
  huxtable::quick_docx(tab4_formated, file = file.path(OUTPUTS, "tab4_formated_draft.docx"))
}


colTitles = c("Violent Death", "Vehicle robbery", "Street robbery")

add_vec <- ""
for (ndp in 1:length(colTitles)){
  add_vec <- c(add_vec, c("", colTitles[ndp], ""))
}



#------------------------------------------------------------------------------#
#### Spatial lag ####

# #foo <- as.formula(Formulas01_lg)
# foo <- formula("")
# 
# lagsarlm(foo, 
#          listw = lw, 
#          data = sr, 
# #          method="Matrix")
# 
# 
# sr$year_month <- sr$year*100+ sr$month
# 
# #formula.f <-  "violent_death_sim ~ on_target | year_month + aisp  | 0 |  latitude + longitude"
# formula.f <- Formulas01_str[1]
# formula.f <- as.formula(formula.f) 
# 
# reg.f <- felm(formula.f, data = sr, keepCX = T)
# 
# foo <- ConleySEs(reg = reg.f,
#           unit = "aisp", 
#           time = c("year","month"),
#           #time = "year",
#           dist_cutoff = 5,
#           #balanced_pnl = F,
#           #dist_fn = "Haversine", lag_cutoff = 5, cores = 1, verbose = FALSE,
#           lat = "latitude", lon = "longitude") 
# 
# 
# 
# #r_vd_01



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
