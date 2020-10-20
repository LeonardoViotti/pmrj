#------------------------------------------------------------------------------#

#	 SIM - Robustness Placebo

#------------------------------------------------------------------------------#

# TO DO:

# Checar se definicao e a mesma
# Add lines in table
# Add DD models




# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = T
}

#------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

#------------------------------------------------------------------------------#
#### Process data ####

#### Placebo
sr_pl <- sr[sr$year < 2009,]

#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars_pla <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery")


indepVars_pla <- c("hit_sem_pla_l",
                   # "policemen_aisp",
                   # "policemen_upp",
                   "n_precinct",
                   #"max_prize",
                   "population" )

indepVars_pla_dd <- c(
  "hit_sem_pla_l",
  "last_month_on_target_plapre",
  "last_month",
  # "policemen_aisp",
  # "policemen_upp",
  "n_precinct",
  # "max_prize",
  "population" )


FEVars_pla <- c("aisp",
                "year", 
                "month", 
                "cmd_name")

FEVars_pla_dd <- c("aisp",
                "year", 
                # "month", 
                "cmd_name")


#------------------------------------------------------------------------------#
### OLS formulas ####

# Right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 
rFormula_iv_pla <- paste(indepVars_pla[-1], collapse = " + ") 

# Add FE, cluster and instruments

clusterVars = c("aisp")
# clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)

FeForumala2_pla <- paste(FEVars_pla, collapse = " + ") # with cmd FE
config2_pla <- paste("|", FeForumala2_pla, "| 0 |  ", clusterVars_form)


#### Final formulas

Formulas01_str <- paste(depVars_pla, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars_pla, paste(rFormula, config2), sep = " ~ ")

# Placebo
Formulas01_pla_str <- paste(depVars_pla, paste(rFormula_pla, config1), sep = " ~ ")
Formulas02_pla_str <- paste(depVars_pla, paste(rFormula_pla, config2_pla), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars_pla
names(Formulas02_str) <- depVars_pla
names(Formulas01_pla_str) <- depVars_pla
names(Formulas02_pla_str) <- depVars_pla

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


# Placebo formulas

p_dd_formulas_m2 <-
  reg_formula(depVars_pla,
              indepVars_pla_dd,
              FE_vars_dd)

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



# Create a data set with only target months
dd_df_pla <- sr_pl %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1)) #%>%


# Create a data set with only target months


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
          title = "Table B1 - Robustness: Effect of expectation of receiving bonuses on crime rates (Placebo analysis between 2005 and 2008)",          outPath = export("tabB1.html"),
          type = table_type,
          placebo = T)


