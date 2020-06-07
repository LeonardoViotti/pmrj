#------------------------------------------------------------------------------#

#	 SIM - Draft DD 2

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Load data

sm <- raw_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
  subset(year < 2016 & (year*10 + semester > 20092))

#------------------------------------------------------------------------------#
# Create regression variables
sm %<>%

  # Create monthly on target based on semester
  mutate(
    # Adjust sester target to take into account the 10% marging they had
    # after 2013
    target_vd_sem_adjusted = ifelse(year >= 2013,
                                    target_vd_sem,
                                    target_vd_sem*1.1),
    target_sr_sem_adjusted = ifelse(year >= 2013,
                                    target_sr_sem,
                                    target_sr_sem*1.1),
    target_vr_sem_adjusted = ifelse(year >= 2013,
                                    target_vr_sem,
                                    target_vr_sem*1.1),
    
    # Still within the semester target for each crime. Using _cum2 variables
    # because these are the cumulative sum until the end of the month, that
    # is, for June it sums up to the end of that month. The other variables
    # _cum are just until the start of the month, for June it would only
    # account to all May crime, but no June crime.
    hit_violent_death = as.integer(violent_death_sim_cum2 <= target_vd_sem_adjusted),
    hit_street_robbery = as.integer(street_robbery_cum2 <= target_sr_sem_adjusted),
    hit_vehicle_robbery = as.integer(vehicle_robbery_cum2 <= target_vr_sem_adjusted),

    # If within the semester target for all 3 crimes
    hit_month = as.integer(hit_violent_death==1 & 
                             hit_street_robbery==1 & 
                             hit_vehicle_robbery==1) ) %>% 
  
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_month_l = dplyr::lag(hit_month,
                                  n = 1L)) %>% 
  
  #--------------------------------
  
  # Create dummy if on target or not on month 2
  mutate(on2 = ifelse(cycle == 2,
                       on_target,
                      #hit_month_l,
                      NA)) %>% 
  
  group_by(aisp, year, semester) %>%
  mutate(on2 = max(on2, na.rm = T)) %>% 
  ungroup() %>% 
  
  # Other variables
  mutate(
    last_month = ifelse(month==6 | month==12, 
                        1,0),
    last_month_on2 = last_month*on2
  )
    


#------------------------------------------------------------------------------#
##### Regression data set ### 

# Create a data set with only target months
sm_reg <- sm %>%
  # Keep only regression months
  subset(month %in% c(2,6,8,12)) 

#------------------------------------------------------------------------------#
#### Regression formulas ####

#---------------#
# Set variables #

indep_vars_dd <- c(
  "last_month_on2",
  "on2",
  # "hit_target2",
  # "last_month_hit2",
  "last_month",
  "policemen_aisp",
  "policemen_upp",
  "n_precinct",
  "max_prize",
  "population" )

# Since there are only 4 months in this analysis I'm removing month
# fixed effects to avoid collinearity
FE_vars_dd <- c("aisp",
                "year", 
                # "month",
                "id_cmt")

# Set cluster SE level
cluster_vars_dd= "0"


#--------------#
# Set formulas #

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

# Triple difference with chief FE
dd_formulas_m3 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd)


#------------------------------------------------------------------------------#
#### Run regressions #### 

# Set regressions model formula
feRegSim <- function(dep_var,
                     model = 1,
                     formula_vector1 = dd_formulas_m1,
                     formula_vector2 = dd_formulas_m2,
                     formula_vector3 = dd_formulas_m3,
                     data = sm_reg){
  if(model ==1){
    form <- formula_vector1[dep_var]
  } else if(model == 2){
    form <- formula_vector2[dep_var]
  } else{
    form <- formula_vector3[dep_var]
    
  }
  
  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}


# Full period
stargazer(
  feRegSim('violent_death_sim', model = 1),
  feRegSim('violent_death_sim', model = 2),
  feRegSim('vehicle_robbery', model = 1),
  feRegSim('vehicle_robbery', model = 2),
  feRegSim('street_robbery', model = 1),
  feRegSim('street_robbery', model = 2),
  feRegSim('street_robbery', model = 3),
  keep = c("last_month_on2",
           "on2",
           "phase1",
           "last_month"),
  title = "Full period: 2019-2015",
  omit.stat=c("LL","ser","f"),
  type = 'text')

stargazer(
  feRegSim('dbody_found', model = 1),
  feRegSim('dbody_found', model = 2),
  feRegSim('vehicle_theft', model = 1),
  feRegSim('vehicle_theft', model = 2),
  feRegSim('street_theft', model = 1),
  feRegSim('street_theft', model = 2),
  # feRegSim('street_theft', model = 3),
  # feRegSim('dpolice_killing', model = 1),
  # feRegSim('dpolice_killing', model = 2),
  title = "Full period: 2019-2015",
  keep = c("last_month_on2",
           "on2",
           "last_month"),
  omit.stat=c("LL","ser","f"),
  type = 'text')












