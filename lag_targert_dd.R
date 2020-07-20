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


#------------------------------------------------------------------------------#
# Load data

sm <- raw_data %>%
  # Removing lower and upper bounds we don't have data on batallion sizes after
  # 2015 and the system begun in the second semmester of 2009
   # subset(year < 2016 & (year*10 + semester > 20091))
  subset((year*10 + semester > 20091))



# source(file.path(GITHUB, 'old or drafts/add_target_for_recent_years_DRAFT.R'))

  
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
  # Create cumulative sum of monthly target until the end of the month.
  # the original one represents what's the expected target up to the
  # start of the month, hence Jan and Jul have NAs.
  mutate(target_vd_cum2 = cumsum(target_vd),
         target_vr_cum2 = cumsum(target_vr),
         target_sr_cum2 = cumsum(target_sr)) %>% 
  ungroup() %>% 

  # Create variables
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
    
    target_vd_cum2_adjusted = ifelse(year >= 2013,
                                    target_vd_cum2,
                                    target_vd_cum2*1.1),
    target_sr_cum2_adjusted = ifelse(year >= 2013,
                                    target_sr_cum2,
                                    target_sr_cum2*1.1),
    target_vr_cum2_adjusted = ifelse(year >= 2013,
                                    target_vr_cum2,
                                    target_vr_cum2*1.1),

    # Still within the semester target for each crime. Using _cum2 variables
    # because these are the cumulative sum until the end of the month, that
    # is, for June it sums up to the end of that month. The other variables
    # _cum are just until the start of the month, for June it would only
    # account to all May crime, but no June crime.
    hit_violent_death = as.integer(violent_death_sim_cum2 <= target_vd_sem_adjusted),
    hit_street_robbery = as.integer(street_robbery_cum2 <= target_sr_sem_adjusted),
    hit_vehicle_robbery = as.integer(vehicle_robbery_cum2 <= target_vr_sem_adjusted),

    # Still within the cum monthly target for each cr ime 
    # hit_violent_death = as.integer(violent_death_sim_cum2 <= target_vd_cum2_adjusted),
    # hit_street_robbery = as.integer(street_robbery_cum2  <= target_sr_cum2_adjusted),
    # hit_vehicle_robbery = as.integer(vehicle_robbery_cum2  <= target_vr_cum2_adjusted),

    # If within the semester target for all 3 crimes
    hit_month = as.integer(hit_violent_death==1 & 
                           hit_street_robbery==1 & 
                           hit_vehicle_robbery==1),
    
    # Last month dummy
    last_month = ifelse(month==6 | month==12,
                        1,0)
    # last_month = ifelse(month==5 | month==6 | month==11 | month==12,
    #                     1,0)
    ) %>% 
  
  # Create lagged variable
  group_by(aisp) %>%
  arrange(aisp, year, month) %>%
  mutate(hit_month_l = dplyr::lag(hit_month,
                                  n = 1L),
         hit_month_l2 = dplyr::lag(hit_month,
                                  n = 2L),
         hit_month_l3 = dplyr::lag(hit_month,
                                  n = 3L),
         hit_month_l4 = dplyr::lag(hit_month,
                                  n = 4L),
         hit_month_l5 = dplyr::lag(hit_month,
                                  n = 5L),
         # Create lag target variable based if on target on the previous 4 months
         #positive_shock = hit_month_l*hit_month_l2*hit_month_l3*hit_month_l4
         positive_shock = hit_month_l
                           ) %>%

  ungroup() %>% 
  # Interaction
  mutate(last_month_shock = last_month*positive_shock)
  # mutate(last_month_hit = last_month*hit_month_l)

#------------------------------------------------------------------------------#
##### Regression data set ### 

# Create a data set with only target months
sm_reg <- sm %>%
  # Keep only regression months
  subset(month %in% c(6,7,12,1)) #%>%
  # subset(month %in% c(5,6,7,8,11,12,1,2)) #%>%
  

#------------------------------------------------------------------------------#
#### Regression formulas ####

#---------------#
# Set variables #

indep_vars_dd <- c(
  "last_month_shock",
  # "hit_month_l",
  "positive_shock",
  "last_month",
  # "policemen_aisp",
  # "policemen_upp",
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
              FE_vars_dd[-length(FE_vars_dd)])

# Second model with chief FE
dd_formulas_m2 <- 
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
    keep = c("last_month_shock",
             "positive_shock",
             "last_month"),
    title = "Full period: 2009-2017 1 sem (no batallion size)",
    omit.stat=c("LL","ser","f"),
    type = 'html')

stargazer(
    feRegSim('dbody_found', model = 1),
    feRegSim('dbody_found', model = 2),
    feRegSim('vehicle_theft', model = 1),
    feRegSim('vehicle_theft', model = 2),
    feRegSim('street_theft', model = 1),
    feRegSim('street_theft', model = 2),
    # feRegSim('dpolice_killing', model = 1),
    # feRegSim('dpolice_killing', model = 2),
    title = "Full period: 2009-2017 1 sem (no batallion size)",
    keep = c("last_month_shock",
             "positive_shock",
             "last_month"),
    omit.stat=c("LL","ser","f"),
    type = 'html')


# stargazer(
#   feRegSim('dpolice_killing', model = 1),
#   feRegSim('dpolice_killing', model = 2),
#   feRegSim('arrest2', model = 1),
#   feRegSim('arrest', model = 2),
#   feRegSim('drug_seizure', model = 1),
#   feRegSim('drug_seizure', model = 2),
#   title = "Full period: 2009-2017 1 sem",
#   keep = c("last_month_hit",
#            "positive_shock",
#            "last_month"),
#   omit.stat=c("LL","ser","f"),
#   type = 'text')


#------------------------------------------------------------------------------#
# draft

summary(sm$hit_month)

# (sm$hit_month_4 != sm$hit_month_l) %>% sum(na.rm = T)

# sm %>%
#   select(aisp,
#          year,
#          month,
#          hit_violent_death,
#          hit_street_robbery,
#          hit_vehicle_robbery,
#          hit_month,
#          hit_month_l,
#          # hit_month_l2,
#          # hit_month_l3,
#          # hit_month_l4,
#          # hit_month_l5,
#          hit_month_4) %>% View


# # Check it out
# sm %>%
#   # subset(year == 2014 & aisp == 2) %>%
#   select("aisp",
#          "year",
#          "semester",
#          "month",
#          "violent_death_sim",
#          # "violent_death_sim_cum",
#          "violent_death_sim_cum2",
#          # "violent_death_sim_6",
#          # "target_vd_cum",
#          "target_vd_cum2",
#          "target_vd_sem",
# 
#          # "street_robbery",
#          # "street_robbery_cum",
#          # "street_robbery_cum2",
#          # # "street_robbery_6",
#          # "target_sr_cum",
#          # "target_sr_cum2",
#          # "target_sr_sem",
#          # 
#          # "vehicle_robbery",
#          # "vehicle_robbery_cum",
#          # # "vehicle_robbery_6",
#          # "target_vr_cum",
#          # "target_vr_cum2",
#          # "target_vr_sem",
# 
#          "hit_violent_death",
#          "hit_street_robbery",
#          "hit_vehicle_robbery",
#          
#          "hit_month",
#          "hit_month_l",
#          "hit_month_4",
# 
#          # "on_target_vd",
#          # "on_target_sr",
#          # "on_target_vr",
# 
#          "on_target",
#          # "lag1_on_target",
#          "hit_month",
#          "hit_month_l",
#          "last_month",
#          "last_month_hit"
#   ) %>% View
# # 
# 
# # # Check it out
# sm_reg %>%
# # sm %>%
#   # subset(sem_year>100) %>%
#   select("aisp",
#          "year",
#          "month",
#          "semester",
#          "cycle",
#          # "violent_death_sim",
#          # "violent_death_sim_cum",
#          # "violent_death_sim_6",
#          # "target_vd_sem",
#          "hit_violent_death",
#          "hit_street_robbery",
#          "hit_vehicle_robbery",
#          # "street_robbery_cum",
#          # "target_sr_sem",
#          # "vehicle_robbery_cum",
#          # "target_vr_sem",
#          "on_target",
#          "hit_month",
#          "hit_month_l",
#          "hit_target2") %>% View



# sm_reg$y <- sm_reg$street_theft
# 
# 
# felm(y ~ last_month_hit +  hit_month_l +  last_month  + phase1 + 
#     policemen_aisp + policemen_upp + n_precinct + max_prize +
#     population | aisp | 0 | 0,
#     data = sm_reg) %>% stargazer(type = 'text')
# 
# 
# lm(y ~ last_month_hit +  hit_month_l +  last_month  + phase1 + 
#        policemen_aisp + policemen_upp + n_precinct + max_prize +
#        population + factor(year) + factor(aisp),
#      data = sm_reg) %>% stargazer(type = 'text')


# # Phase 1
# stargazer(
#   feRegSim('violent_death_sim', model = 1 , data = sm_reg_phase1),
#   feRegSim('violent_death_sim', model = 2, data = sm_reg_phase1),
#   feRegSim('vehicle_robbery', model = 1, data = sm_reg_phase1),
#   feRegSim('vehicle_robbery', model = 2, data = sm_reg_phase1),
#   feRegSim('street_robbery', model = 1, data = sm_reg_phase1),
#   feRegSim('street_robbery', model = 2, data = sm_reg_phase1),
#   keep = c("last_month_hit",
#            "hit_month_l",
#            "last_month"),
#   title = "Phase 1: 2009-2012",
#   omit.stat=c("LL","ser","f"),
#   type = 'text')
# 
# stargazer(
#   feRegSim('dbody_found', model = 1, data = sm_reg_phase1),
#   feRegSim('dbody_found', model = 2, data = sm_reg_phase1),
#   feRegSim('vehicle_theft', model = 1, data = sm_reg_phase1),
#   feRegSim('vehicle_theft', model = 2, data = sm_reg_phase1),
#   feRegSim('street_theft', model = 1, data = sm_reg_phase1),
#   feRegSim('street_theft', model = 2, data = sm_reg_phase1),
#   feRegSim('dpolice_killing', model = 1, data = sm_reg_phase1),
#   feRegSim('dpolice_killing', model = 2, data = sm_reg_phase1),
#   title = "Phase 1: 2009-2012",
#   keep = c("last_month_hit",
#            "hit_month_l",
#            "last_month"),
#   omit.stat=c("LL","ser","f"),
#   type = 'text')
# 
# 
# # Phase 2
# stargazer(
#   feRegSim('violent_death_sim', model = 1 , data = sm_reg_phase2),
#   feRegSim('violent_death_sim', model = 2, data = sm_reg_phase2),
#   feRegSim('vehicle_robbery', model = 1, data = sm_reg_phase2),
#   feRegSim('vehicle_robbery', model = 2, data = sm_reg_phase2),
#   feRegSim('street_robbery', model = 1, data = sm_reg_phase2),
#   feRegSim('street_robbery', model = 2, data = sm_reg_phase2),
#   keep = c("last_month_hit",
#            "hit_month_l",
#            "last_month"),
#   title = "Phase 2: 2013-2015",
#   omit.stat=c("LL","ser","f"),
#   type = 'text')
# 
# stargazer(
#   feRegSim('dbody_found', model = 1, data = sm_reg_phase2),
#   feRegSim('dbody_found', model = 2, data = sm_reg_phase2),
#   feRegSim('vehicle_theft', model = 1, data = sm_reg_phase2),
#   feRegSim('vehicle_theft', model = 2, data = sm_reg_phase2),
#   feRegSim('street_theft', model = 1, data = sm_reg_phase2),
#   feRegSim('street_theft', model = 2, data = sm_reg_phase2),
#   feRegSim('dpolice_killing', model = 1, data = sm_reg_phase2),
#   feRegSim('dpolice_killing', model = 2, data = sm_reg_phase2),
#   title = "Phase 2: 2013-2015",
#   keep = c("last_month_hit",
#            "hit_month_l",
#            "last_month"),
#   omit.stat=c("LL","ser","f"),
#   type = 'text')

















