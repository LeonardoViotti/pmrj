#------------------------------------------------------------------------------#

#	 SIM - Main Regressions nova

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = T

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = T
}


# OUTPUTS_final <- file.path(OUTPUTS_final, "test")

#------------------------------------------------------------------------------#
# Load data

# sr <- raw_data %>%
sr <- final_data %>%
  # sr <- org_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester > 20091))


placebo_data <- final_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset((year*10 + semester <= 20091))


# Diff in diff, a.k.a virada

# Create a data set with only target months
dd_df <- sr %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1))


#------------------------------------------------------------------------------#
#### OLS formulas ####

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 


# Add FE, cluster and instruments

# clusterVars = c("latitude", "longitude" )
clusterVars = c("aisp" )
# clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)



#### Final formulas

Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars

#rFormulaFE <- paste0("factor(",FEVars,")")
# rFormula1 <- paste(c(indepVars, rFormulaFE[1:2]), collapse = " + ") 
# rFormula2 <- paste(c(indepVars, rFormulaFE), collapse = " + ") 

#------------------------------------------------------------------------------#
#### End of semster formulas ####

# Function to create formulas
reg_formula <- function(dep_vars,
                        indep_vars,
                        FE_vars,
                        instr_vars = 0,
                        custer_vars = clusterVars){
  
  paste_plus <- function(x){
    paste(x, collapse =  " + ")
  }
  
  # Set regression, FEs, cluster SEs and IV
  paste_config <- function(FE_vars,
                           custer_vars,
                           instr_vars){
    paste(" ", 
          paste_plus(FE_vars), 
          paste_plus(instr_vars), 
          paste_plus(custer_vars),
          sep = " | ")
  }
  
  # Combine all elements
  final_formula <- paste(dep_vars, 
                         paste(paste_plus(indep_vars),
                               paste_config(FE_vars,
                                            custer_vars,
                                            instr_vars)), 
                         sep = " ~ ")
  
  
  # Named vector with the dependent variables
  names(final_formula) <- dep_vars
  
  
  # Return named vector of formulas
  return(final_formula)
}


# First model without chief FE
dd_formulas_m1 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd[1:2])



# Second model with chief FE
dd_formulas_m2 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd)


#------------------------------------------------------------------------------#
#### Regression tables ####

# Conditional exporting
export <- function(file,
                   export_global = EXPORT_tables,
                   dir = OUTPUTS_final){
  if (export_global){
    out_path <- file.path(OUTPUTS_final, file)
  } else{
    out_path <- NULL
  }
  
}

# If not exporting print tables
if(EXPORT_tables){
  table_type = 'html'
} else{
  table_type = 'text'
}



# Table main
table_fun(c('violent_death_sim',
            'vehicle_robbery',
            'street_robbery'),
          dep_var_labels = c("Violent deaths", 
                             "Vehicle robbery (Carjacking)",	
                             "Street robbery"),
          title = "Table 2 - Effect of expectancy of receiving bonuses on crime rates",
          outPath = export("tab2-adjusted-Oct21.html"),
          type = table_type)

# Table main large contingent

sr_large <- sr %>% subset(policemen_all > 699)
dd_df_large <- dd_df %>% subset(policemen_all > 699)

table_fun(c('violent_death_sim',
            'vehicle_robbery',
            'street_robbery'),
          ols_data = sr_large,
          dd_data = dd_df_large,
          dep_var_labels = c("Violent deaths", 
                             "Vehicle robbery (Carjacking)",	
                             "Street robbery"),
          title = "Table 2 - Effect of expectancy of receiving bonuses on crime rates (large contingent)",
          outPath = export("tab2-adjusted-lcontingent-Oct21.html"),
          type = table_type)

# Table main small contingent

sr_small <- sr %>% subset(policemen_all < 700)
dd_df_small <- dd_df %>% subset(policemen_all < 700)

table_fun(c('violent_death_sim',
            'vehicle_robbery',
            'street_robbery'),
          ols_data = sr_small,
          dd_data = dd_df_small,
          dep_var_labels = c("Violent deaths", 
                             "Vehicle robbery (Carjacking)",	
                             "Street robbery"),
          title = "Table 2 - Effect of expectancy of receiving bonuses on crime rates (small contingent)",
          outPath = export("tab2-adjusted-scontingent-Oct21.html"),
          type = table_type)


