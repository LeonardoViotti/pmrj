#------------------------------------------------------------------------------#

#	 SIM - Main Regressions nova

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
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
          outPath = export("tab2.html"),
          type = table_type)

# Table gaming 
table_fun(c('dbody_found',
            'vehicle_theft',
            'street_theft'),
          dep_var_labels = c("Cadavers Found (dummy)", 
                             "Car theft",	
                             "Street theft"),
          title = "Table B1 - Expectancy of receiving bonuses and gaming",
          outPath = export("tabB1.html"),
          type = table_type)

# Table spillovers
table_fun(
  # c('other_robberies',
  #         'cargo_robbery',
  #         'burglary',
  #         'store_robbery'),
  
  # New format for this table
  c('dpolice_killing',
    'cargo_robbery',
    'other_robberies'),

  dep_var_labels = c("Police killing (dummy)", 
                     "Cargo robbery	",
                     "Other robberies not included in the target"),
  title = "Table 3 - Expectancy of receiving bonuses and positive spill overs on other crimes",
  outPath = export("tab3.html"),
  type = table_type)


# Table with extra spillovers
# table_fun(c('dpolice_killing',
#             'dassaut_death'),
#           dep_var_labels = c("Police killing (dummy)",
#                              "Assault followed by death (dummy)"),
#           title = "Table ? - Expectancy of receiving bonuses and other spill overs",
#           outPath = export("tabA2.html"),
#           type = table_type)



#------------------------------------------------------------------------------#
#### Monthly coef graphs ####

cpsr <- sr
  

#### Create Variables

# Create month order dummys
cpsr$m2 <- ifelse(cpsr$month %in% c(2,8), 1, 0)
cpsr$m3 <- ifelse(cpsr$month %in% c(3,9), 1, 0)
cpsr$m4 <- ifelse(cpsr$month %in% c(4,10), 1, 0)
cpsr$m5 <- ifelse(cpsr$month %in% c(5,11), 1, 0)
cpsr$m6 <- ifelse(cpsr$month %in% c(6,12), 1, 0)

# Create on_target X mN interaction
cpsr$month2 <- cpsr$m2*cpsr$hit_sem_l
cpsr$month3 <- cpsr$m3*cpsr$hit_sem_l
cpsr$month4 <- cpsr$m4*cpsr$hit_sem_l
cpsr$month5 <- cpsr$m5*cpsr$hit_sem_l
cpsr$month6 <- cpsr$m6*cpsr$hit_sem_l


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
rplot_vd <- feRegSim("violent_death_sim", model = Formulas02_plot_str, data = cpsr)
rplot_vr <- feRegSim("vehicle_robbery",  model = Formulas02_plot_str, data = cpsr)
rplot_rr <- feRegSim("street_robbery",  model = Formulas02_plot_str, data = cpsr)


#### Actual plots

monthCoefPlot <- function(model,
                          vars){
  # Select only month coeffs
  coefs_df <- data.frame(coef = model$coefficients[vars,],
                         month = vars,
                         se = model$rse[vars])
  # Format X axis
  coefs_df$month <- c(2:6)
  
  
  plot <- 
    ggplot(data = coefs_df,
           aes(y = coef,
               x = month)) +
    geom_point(col = "dodgerblue4", size = 2)+
    geom_errorbar(aes(ymin=coef-se, 
                      ymax=coef+se),
                  col = "dodgerblue4",
                  size = .5,
                  width=.1)+
    geom_hline(yintercept=0,
               color = "red")+
    xlab("Month") +
    ylab("")+
    theme_minimal()
  
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



# Export plots
if(EXPORT_plots){
  
  coefPlot_vd  %>% print()
  ggsave(filename = file.path(OUTPUTS_final, "coef_plot_violent_death_sim.png"),
           width = 6,
           height = 4)
  
  coefPlot_rr  %>% print() 
    ggsave(filename = file.path(OUTPUTS_final, "coef_plot_street_robbery.png"),
           width = 6,
           height = 4)
  
  coefPlot_vr  %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "coef_plot_vehicle_robbery.png"),
           width = 6,
           height = 4)
  
  
}


# stargazer(tab6_regs,
#           keep = c("hit_sem_l",
#                    "last_month_on_target",
#                    "last_month"),
#           covariate.labels = c("On target",
#                                "On target * last month",
#                                "Last month"),
#           type = 'html',
#           # out = file.path(OUTPUTS_final, 'pkilling_draft.html')
#           out = NULL
#           # type = 'text'
#           )




