#------------------------------------------------------------------------------#

#	 SIM - Robustness Placebo

#------------------------------------------------------------------------------#

# TO DO:

# Checar se definicao e a mesma
# Add lines in table
# Add DD models




# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = T

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = T
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

depVars <- c("violent_death_sim",
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

#clusterVars = c("aisp")
clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)

FeForumala2_pla <- paste(FEVars_pla, collapse = " + ") # with cmd FE
config2_pla <- paste("|", FeForumala2_pla, "| 0 |  ", clusterVars_form)


#### Final formulas

Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")

# Placebo
Formulas01_pla_str <- paste(depVars, paste(rFormula_pla, config1), sep = " ~ ")
Formulas02_pla_str <- paste(depVars, paste(rFormula_pla, config2_pla), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars
names(Formulas01_pla_str) <- depVars
names(Formulas02_pla_str) <- depVars

#------------------------------------------------------------------------------#
#### End of semster formulas ####

# Function to create formulas
reg_formula <- function(dep_vars,
                        indep_vars,
                        FE_vars,
                        instr_vars = 0,
                        custer_vars = 0){
  
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
  reg_formula(depVars,
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


# Set regressions model formula
ddRegSim <- function(dep_var,
                     model = 2,
                     formula_vector1 = p_dd_formulas_m2,
                     data = dd_df_pla){

  form <- formula_vector1[dep_var]

  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}

# Tablev 2
p_dd_vd_02 <- ddRegSim('violent_death_sim')
p_dd_vd_02_data <-  regData(p_dd_vd_02, regdf = dd_df_pla)

p_dd_vr_02 <- ddRegSim('vehicle_robbery')
p_dd_vr_02_data <-  regData(p_dd_vr_02, regdf = dd_df_pla)

p_dd_rr_02 <- ddRegSim('street_robbery')
p_dd_rr_02_data <-  regData(p_dd_rr_02, regdf = dd_df_pla)



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
       p_dd_vd_02,
       p_vr_01,
       p_vr_02,
       p_dd_vr_02,
       p_rr_01,
       p_rr_02,
       p_dd_rr_02
       )

# Function to find dep var means of regressions
Ymean <- function(x){
  mean(regData(x, sr)[,regDepVars(x)])
}

# Function to create the row for regression tables
Ymean_row <- function(list){
  c("Y mean", sapply(list, Ymean) %>% round(2))
}


n_aisp_line_9 <- c("Number of aisp", rep("39", 9))
chifeFE_line_9 <- c("Chief FE", rep(c( "No", "Yes", "Yes"), 3))
monthFE_line_9 <- c("Month FE", rep(c("Yes", "Yes", "No"), 3))
tab2_pla_addLines <- list(chifeFE_line_9,
                          monthFE_line_9,
                      Ymean_row(tab2_pla_regs),
                      n_aisp_line_9)


# Export function
createTable <- function(reg_list, 
                        add_lines_list,
                        title,
                        dep_var_labels,
                        outPath){
  stargazer(reg_list,
            keep = c(  "hit_sem_pla_l",
                       "last_month_on_target_plapre",
                       "last_month"),
            covariate.labels = c("On target",
                                 "On target * last month",
                                 "Last month"),
            dep.var.labels = dep_var_labels,
            title = title,
            dep.var.caption  = "Number  of  occurrences",
            column.labels   = col_labels_9,
            add.lines = add_lines_list,
            digits = 3,
            omit.stat = c("rsq","ser", "f"),
            out = outPath,
            type = "html"
  )
}





if(EXPORT_tables){
  createTable(reg_list = tab2_pla_regs,
              add_lines_list = tab2_pla_addLines,
              dep_var_labels = c("Violent deaths", 
                                 "Vehicle robbery (Carjacking)",	
                                 "Street robbery"),
              title = "Table C1 - Robustness: Effect of expectation of receiving bonuses on crime rates (Placebo analysis between 2005 and 2008)",
              outPath = file.path(OUTPUTS_final, "tabC1.html"))
  
}

