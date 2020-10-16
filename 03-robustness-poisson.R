#------------------------------------------------------------------------------#

#	 SIM - Robustness poisson

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}


#------------------------------------------------------------------------------#
# Load data

# sr <- raw_data %>%
sr <- final_data %>%
  # sr <- org_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))

# Create a data set with only target months
dd_df <- sr %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1)) #%>%


#------------------------------------------------------------------------------#
#### Poisson formulas ####


# Remove max prize for some reason
poisson_indepVars <- indepVars[!(names(indepVars) %in% c("max_prize", "population"))]


# Fixed effects
sFormulaFE_poi1 <- paste(paste0("factor(",FEVars[-4],")"), collapse = " + ") 
sFormulaFE_poi2 <- paste(paste0("factor(",FEVars,")"), collapse = " + ") 


# Construct right hand sied fo eq.
rFormula_poi_0 <- paste(poisson_indepVars, collapse = " + ") 

# Add FEs
rFormula_poi_1 <-  paste(rFormula_poi_0, "+", sFormulaFE_poi1)
rFormula_poi_2 <-  paste(rFormula_poi_0, "+", sFormulaFE_poi2)


# Add Exposure variable

# Exposure Variable
exposure_variable <- "population"

rFormula_poi1 <-  paste(rFormula_poi_1,
                       "+", 
                       paste0(" offset(log(", 
                              exposure_variable, 
                              "))"
                       )
)

rFormula_poi2 <-  paste(rFormula_poi_2,
                       "+", 
                       paste0(" offset(log(", 
                              exposure_variable, 
                              "))"
                       )
)

# Final formula
Formulas_poi_str1 <- paste(depVars, rFormula_poi1, sep = " ~ ")
names(Formulas_poi_str1) <- depVars
Formulas_poi_str2 <- paste(depVars, rFormula_poi2, sep = " ~ ")
names(Formulas_poi_str2) <- depVars

#------------------------------------------------------------------------------#
#### Poisson models ####


# Set regressions model formula
RegPoisson <- function(dep_var,
                       model = 1,
                     formula_vector1 = Formulas_poi_str1,
                     formula_vector2 = Formulas_poi_str2,
                     data = sr){
  if (model == 1){
    formula_vector = formula_vector1
  } else {
    formula_vector = formula_vector2
  }
  
  
  form <- as.formula(formula_vector[dep_var])
  model <- glm(form, family = poisson, data = data)
  
  # Return regression object
  return(model)
  
}


# RegPoisson <- function(form){
#   
#   
#   model <- 
#     glm(as.formula(form),
#         family = poisson,
#         data = sr)
#   return(model)
#   
# }

p_vd <- RegPoisson("violent_death_sim")
p_vd_data <-  regData(p_vd, regdf = sr)

p_vr <- RegPoisson("vehicle_robbery")
p_vr_data <-  regData(p_vr, regdf = sr)

p_rr <- RegPoisson("street_robbery")
p_rr_data <-  regData(p_rr, regdf = sr)


p_vd2 <- RegPoisson("violent_death_sim", model = 2)
p_vd_data2 <-  regData(p_vd2, regdf = sr)

p_vr2 <- RegPoisson("vehicle_robbery", model = 2)
p_vr_data2 <-  regData(p_vr2, regdf = sr)

p_rr2 <- RegPoisson("street_robbery", model = 2)
p_rr_data2 <-  regData(p_rr2, regdf = sr)


#------------------------------------------------------------------------------#
#### Poisson DD ####


poisson_indepVars_dd <- 
  indep_vars_dd[!(indep_vars_dd %in% c("max_prize", "population"))]

# Exposure Variable
exposure_variable <- "population"

rFormula_poi1 <-  paste(rFormula_poi_1,
                        "+", 
                        paste0(" offset(log(", 
                               exposure_variable, 
                               "))")
)


  
# Function to create formulas
reg_formula <- function(dep_vars,
                        indep_vars,
                        FE_vars,
                        exposure_variable = "population"){
  
  paste_plus <- function(x){
    paste(x, collapse =  " + ")
  }
  
  
  indep_vars <- 
    paste(paste_plus(indep_vars),
          paste0(" offset(log(", exposure_variable, "))"),
          paste(paste0("factor(",FE_vars_dd,")"), collapse = " + "), sep = "+")
  
  # Combine all elements
  final_formula <- 
    paste(dep_vars,indep_vars, sep = " ~ ")
  
  
  # Named vector with the dependent variables
  names(final_formula) <- dep_vars
  
  
  # Return named vector of formulas
  return(final_formula)
}


# Second model with chief FE
dd_formulas_poi <-
  reg_formula(depVars,
              poisson_indepVars_dd,
              FE_vars_dd)


#------------------------------------------------------------------------------#
#### Poisson diff in diff ####


# Set regressions model formula
ddPoiSim <- function(dep_var,
                     formula_vector = dd_formulas_poi,
                     data = dd_df){

  
  form <- as.formula(formula_vector[dep_var])
  model <- glm(form, family = poisson, data = data)
  
  # Return regression object
  return(model)
  
}


# Tablev 2
p_vd_dd <- ddPoiSim('violent_death_sim')
# p_vd_data <-  regData(p_vd, regdf = sr)

p_vr_dd <- ddPoiSim("vehicle_robbery")
# p_vr_data <-  regData(p_vr, regdf = sr)

p_rr_dd <- ddPoiSim("street_robbery")
# p_rr_data <-  regData(p_rr, regdf = sr)

#------------------------------------------------------------------------------#
#### Export ####



# Poisson model
tab5_regs <-
  list(p_vd,
       p_vd2,
       p_vd_dd,
       p_vr,
       p_vr2,
       p_vr_dd,
       p_rr,
       p_rr2,
       p_rr_dd)


tab5_addLines <- list(c("Chief FE", rep(c( "No", "Yes", "Yes"), 3)),
                      c("Month FE", rep(c("Yes", "Yes", "No"), 3)),
                      Ymean_row(tab5_regs),
                      c("Number of aisp", rep("39", 9)))

if(EXPORT_tables){
  
  createTable(reg_list = tab5_regs,
              add_lines_list = tab5_addLines,
              dep_var_labels = c("Violent deaths",
                                 "Vehicle robbery (Carjacking)",
                                 "Street robbery"),
              title = "Table B4 Robustness: Poisson Regressions",
              outPath = file.path(OUTPUTS_final, "tabB4.html"))
}

