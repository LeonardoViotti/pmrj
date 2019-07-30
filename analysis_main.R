
#------------------------------------------------------------------------------#

#	 SIM - Main Regressions		

#------------------------------------------------------------------------------#

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

# Keep same sample for all models, i.e from 2010 onwards because of IV
sr <- sr[sem_year > 100,]

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
#### Poisson models ####


RegPoisson <- function(form){
  model <- 
    glm(as.formula(form),
        family = poisson,
        data = sr)
  return(model)
  
}

p_vd <- RegPoisson(Formulas_poi_str["violent_death_sim"])
p_vd_data <-  regData(p_vd, regdf = sr)

p_vr <- RegPoisson(Formulas_poi_str["vehicle_robbery"])
p_vr_data <-  regData(p_vr, regdf = sr)

p_rr <- RegPoisson(Formulas_poi_str["street_robbery"])
p_rr_data <-  regData(p_rr, regdf = sr)

p_po <- RegPoisson(Formulas_poi_str["dpolice_killing"])
p_po_data <-  regData(p_po, regdf = sr)


#------------------------------------------------------------------------------#
##### Export ####

#### Define commun elements
n_aisp_line_9 <- c("Number of aisp", rep("39", 9))
n_aisp_line_12 <- c("Number of aisp", rep("39", 12))

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
            type = "html")
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
            title = "Table 2 – Effect of expectancy of receiving bonuses on crime rates",
            outPath = file.path(OUTPUTS_final, "tab2.html"))




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
            title = "Table 3– Expectancy of receiving bonuses and gaming",
            outPath = file.path(OUTPUTS_final, "tab3.html"))



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
            title = "Table 4– Expectancy of receiving bonuses and positive spill overs on other crimes",
            outPath = file.path(OUTPUTS_final, "tab4.html"))



# Poisson model
tab5_regs <-
  list(p_vd,
       p_vr,
       p_rr,
       p_po)


tab5_addLines <- list(c("Chief FE", "Yes", "Yes", "Yes", "Yes"),
                      Ymean_row(tab5_regs),
                      c("Number of aisp", rep("39", 4)))


createTable(reg_list = tab5_regs,
            add_lines_list = tab5_addLines,
            dep_var_labels = c("Violent deaths", 
                               "Vehicle robbery (Carjacking)",	
                               "Street robbery",
                               "Police homicide (dummy)"),
            title = "Table 5– Robustness: Poisson Regressions",
            outPath = file.path(OUTPUTS_final, "tab5.html"))


#------------------------------------------------------------------------------#
#### Monthly coef graphs ####

#### Create Variables

# Create month order dummys
sr$m2 <- ifelse(sr$month %in% c(2,8), 1, 0)
sr$m3 <- ifelse(sr$month %in% c(3,9), 1, 0)
sr$m4 <- ifelse(sr$month %in% c(4,10), 1, 0)
sr$m5 <- ifelse(sr$month %in% c(5,11), 1, 0)
sr$m6 <- ifelse(sr$month %in% c(6,12), 1, 0)

# Create on_target X mN interaction
sr$month2 <- sr$m2*sr$on_target
sr$month3 <- sr$m3*sr$on_target
sr$month4 <- sr$m4*sr$on_target
sr$month5 <- sr$m5*sr$on_target
sr$month6 <- sr$m6*sr$on_target


#### Construct monthly regression formulas
month_dummies <- c("month2", 
                   "month3",
                   "month4",
                   "month5",
                   "month6")

rFormula_plot <- paste(c(month_dummies, 
                         # Remove on_targer as it is already in the interactions
                         indepVars[-1]), 
                       collapse = " + ") 
Formulas02_plot_str <- paste(depVars, paste(rFormula_plot, config2), sep = " ~ ")
names(Formulas02_plot_str) <- depVars

#### Monthly regression
rplot_vd <- feRegSim(Formulas02_plot_str["violent_death_sim"])
rplot_vr <- feRegSim(Formulas02_plot_str["vehicle_robbery"])
rplot_rr <- feRegSim(Formulas02_plot_str["street_robbery"])


#### Actual plots

model = rplot_vd
vars = month_dummies

monthCoefPlot <- function(model,
                         vars){
  # Select only month coeffs
  coefs_df <- data.frame(coef = model$coefficients[vars,],
                         month = vars,
                         se = model$rse[vars])
  
  plot <- 
    ggplot(data = coefs_df,
                 aes(y = coef,
                     x = month)) +
    geom_point()+
    geom_errorbar(aes(ymin=coef-se, 
                      ymax=coef+se),
                  width=.2)+
    geom_hline(yintercept=0,
               color = "red")

  return(plot)  
}

coefPlot_vd <- 
  monthCoefPlot(model = rplot_vd,
                vars = month_dummies)

coefPlot_vr <- 
  monthCoefPlot(model = rplot_vr,
                vars = month_dummies)

coefPlot_rr <- 
  monthCoefPlot(model = rplot_rr,
                vars = month_dummies)
