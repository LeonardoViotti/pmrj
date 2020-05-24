#------------------------------------------------------------------------------#

#	 SIM - Lag target exercise

#------------------------------------------------------------------------------#

# This runs a diff in diff for the first and last months of each semester with
# the interest variable being to have been within the target in the previous
# month. The objective is to run an exercise that would flag serial correlation.
# The intuition is that for the last month of the semester, to still be whithin
# the target in the previous month means having a chance of winning the award.
# For the first month of each semester, it shouldn't matter because it's a new
# semester.

#------------------------------------------------------------------------------#
# Questions

# Porque estamos usando essa variável 


#------------------------------------------------------------------------------#
# Load data

sm <- raw_data %>%
  
  
  
  # Also removig from 2016 just to be sure I'm doing the cleanest 
  # exercise
  subset(year < 2016) 
  
# sm <- final_data

#------------------------------------------------------------------------------#
# Create lagged variable

# Since on_target variable is if the AISP is wihtin the expeceted target 
# for that month, regardless if it still has any perspective of being awarded
# in that semester, create a variable (to be lagged) that means the aisp is still within the semester
# target for all three crimes.

# Create regression variables
sm %<>%
  # Create semester sums of all crime variables adding suffix 6. This are
  # used in the orignal model
  group_by(aisp, sem_year) %>% 
  mutate_at( c(depVars, "target_vd", "target_sr", "target_vr"),
             .funs = list("6" = ~sum(., na.rm = T))
  ) %>% 
  ungroup() %>% 
  
  
  
  
  mutate(
    # Replace cumulative sum in first month to zero
    violent_death_sim_cum = ifelse(is.na(violent_death_sim_cum),
                                   0,
                                   violent_death_sim_cum),
    street_robbery_cum = ifelse(is.na(street_robbery_cum),
                                   0,
                                street_robbery_cum),
    vehicle_robbery_cum = ifelse(is.na(vehicle_robbery_cum),
                                 0,
                                 vehicle_robbery_cum),

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
    
    # Still within the semester target for each crime
    hit_violent_death = as.integer(violent_death_sim_cum <= target_vd_sem_adjusted),
    hit_street_robbery = as.integer(street_robbery_cum <= target_sr_sem_adjusted),
    hit_vehicle_robbery = as.integer(vehicle_robbery_cum <= target_vr_sem_adjusted),
    
    # Still within the semester target for each crime 
    # (total sum in the semester)
    # hit_violent_death = as.integer(violent_death_sim_6 <= target_vd_sem),
    # hit_street_robbery = as.integer(street_robbery_6 <= target_sr_sem),
    # hit_vehicle_robbery = as.integer(vehicle_robbery_6 <= target_vr_sem),

    # If within the semester target for all 3 crimes
    hit_month = as.integer(hit_violent_death==1 & 
                           hit_street_robbery==1 & 
                           hit_vehicle_robbery==1),
    
    # Last month dummy
    last_month = ifelse(month==6 | month==12, 
                        1,0)
    
  ) %>% 
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_month_l = dplyr::lag(hit_month,
                                  n = 1L)) %>%
  
  # Hit target original pra comparar
  mutate(hit_target2 = as.numeric(on_target == 1),
         hit_target2 = ifelse(cycle == 1,
                              dplyr::lag(hit_month, 1),
                              hit_target2),
         # Replace NAs with zeros
         hit_target2 = ifelse(is.na(hit_target2),
                             0 ,
                             hit_target2)) %>% 
  
  ungroup() %>% 
  # Interaction
  mutate(last_month_hit = last_month*hit_month_l) %>% 
  mutate(last_month_hit2 = last_month*hit_target2)
  


#------------------------------------------------------------------------------#
##### Regression data set ### 

# Create a data set with only target months
sm_reg <- sm %>%
  # Removing placebo months and 2009
  subset(sem_year>100) %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1))

  
#------------------------------------------------------------------------------#
#### Regression formulas ####

#---------------#
# Set variables #

indep_vars_dd <- c(
  # "last_month_hit",
  # "hit_month_l",
  "hit_target2",
  "last_month_hit2",
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
                         paste(paste_plus(indep_vars_dd),
                               paste_config(FE_vars,
                                            custer_vars,
                                            instr_vars)), 
                         sep = " ~ ")
  
  
  # Named vector with the dependent variables
  names(final_formula) <- dep_vars
  
  
  # Return named vector of formulas
  return(final_formula)
}


# Second model with chief FE
dd_formulas_m2 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd)

# First model without chief FE
dd_formulas_m1 <- 
  reg_formula(depVars,
              indep_vars_dd,
              FE_vars_dd[1:2])

#------------------------------------------------------------------------------#
#### Run regressions #### 

# Set regressions model formula
feRegSim <- function(dep_var,
                     model = 1,
                     formula_vector1 = dd_formulas_m1,
                     formula_vector2 = dd_formulas_m2,
                     data = sm_reg){
  if(model ==1){
    form <- formula_vector1[dep_var]
  } else{
    form <- formula_vector2[dep_var]
  }
  

  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}


tab2 <- 
  stargazer(
    feRegSim('violent_death_sim', model = 1),
    feRegSim('violent_death_sim', model = 2),
    feRegSim('vehicle_robbery', model = 1),
    feRegSim('vehicle_robbery', model = 2),
    feRegSim('street_robbery', model = 1),
    feRegSim('street_robbery', model = 2),
    keep = c("last_month_hit",
             "hit_month_l",
             "last_month"),
    omit.stat=c("LL","ser","f"),
    type = 'text')

tab_gaming <- 
  stargazer(
    feRegSim('dbody_found', model = 1),
    feRegSim('dbody_found', model = 2),
    feRegSim('vehicle_theft', model = 1),
    feRegSim('vehicle_theft', model = 2),
    feRegSim('street_theft', model = 1),
    feRegSim('street_theft', model = 2),
    feRegSim('dpolice_killing', model = 1),
    feRegSim('dpolice_killing', model = 2),

    keep = c("last_month_hit",
             "hit_month_l",
             "last_month"),
    omit.stat=c("LL","ser","f"),
    type = 'text')



#------------------------------------------------------------------------------#
# draft



# sm_reg$y <- sm_reg$street_robbery
# 
# 
# felm(y ~ last_month_hit +  hit_month_l +  last_month  +
#     policemen_aisp + policemen_upp + n_precinct + max_prize +
#     population | year + aisp | 0 | 0,
#     data = sm_reg) %>% stargazer(type = 'text')
# 





# # Check it out
sm[is.na(sm$target_vd_sem),] %>%
  select("aisp",
         "year",
         "semester",
         "month",
         "violent_death_sim",
         "violent_death_sim_cum",
         "violent_death_sim_6",
         "target_vd_sem",

         "street_robbery",
         "street_robbery_cum",
         "street_robbery_6",
         "target_sr_sem",

         "vehicle_robbery",
         "vehicle_robbery_cum",
         "vehicle_robbery_6",
         "target_vr_sem",

         "hit_violent_death",
         "hit_street_robbery",
         "hit_vehicle_robbery",

         # "target_vd_cum",
         # "target_sr_cum",
         # "target_vr_cum",
         # 
         # "on_target_vd",
         # "on_target_sr",
         # "on_target_vr",

         "on_target",
         # "lag1_on_target",
         "hit_month",
         "hit_month_l",
         "last_month",
         "last_month_hit"
  ) %>% View


# Check it out
# sm_reg %>%   
# sm %>%   
#   subset(sem_year>100) %>%
#   select("aisp",
#          "year",
#          "month",
#          "semester",
#          "violent_death_sim",
#          "violent_death_sim_cum",
#          "violent_death_sim_6",
#          "target_vd_sem",
#          "hit_violent_death",
#          # "street_robbery",
#          # "street_robbery_cum",
#          # "target_sr_sem",
#          # 
#          # "vehicle_robbery",
#          # "vehicle_robbery_cum",
#          # "target_vr_sem",
#          "hit_month",
#          "hit_month_l" ) %>% View






















