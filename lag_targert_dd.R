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
sm <- raw_data
# sm <- final_data

#------------------------------------------------------------------------------#
# Create lagged variable

# Since on_target variable is if the AISP is wihtin the expeceted target for that
# month, regardless if it still has any perspective of being awarded in that semester,
# create a variable (to be lagged) that means the aisp is still within the semester
# target for all three crimes.


# Create regression variables
sm %<>% 
  mutate(
    # Still within the semester target for each crime 
    hit_violent_death = as.integer(violent_death_sim_cum <= target_vd_sem), 
    hit_street_robbery = as.integer(street_robbery_cum <= target_sr_sem),
    hit_vehicle_robbery = as.integer(vehicle_robbery_cum <= target_vr_sem),
    
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
  ungroup() %>% 
  # Interaction
  mutate(last_month_hit = last_month*hit_month_l)

# Create the exact same variable from the original code to test
sm %<>% 
  mutate(hit_target = as.numeric(on_target ==1),
         hit_target = ifelse(cycle == 1,
                             as.numeric(hit_month_l ==1),
                             hit_target))




# Check it out
# sm %>%   subset(sem_year>100) %>% 
# select("aisp",
#               "year",
#               "semester",
#               "month",
#               "violent_death_sim",
#               "violent_death_sim_cum",
#               "target_vd_sem",
# 
#               "street_robbery",
#               "street_robbery_cum",
#               "target_sr_sem",
# 
#               "vehicle_robbery",
#               "vehicle_robbery_cum",
#               "target_vr_sem",
# 
#               "on_target_vd",
#               "on_target_sr",
#               "on_target_vr",
# 
#               "on_target",
#               # "lag1_on_target",
#               "hit_month",
#               "hit_month_l"
# ) %>% View


#------------------------------------------------------------------------------#
# Replicate the original analysis exactly

foo <- 
  raw_data %>% 
  mutate(last_moth  = ifelse(month %in% c(6,12),
                             1,
                             0)) %>% 
  # Create semester sums of all crime variables adding suffix 6
  group_by(aisp, sem_year) %>% 
  mutate_at( c(depVars, "target_vd", "target_sr", "target_vr"),
             .funs = list("6" = ~sum(., na.rm = T))
  ) %>% 
  
  # Other varaibles that don't need to be grouped
  ungroup() %>%
  mutate(
    
    # Create award variables
    award_violent_death = as.integer(violent_death_sim_6<=target_vd_6), 
    award_street_robbery = as.integer(street_robbery_6<= target_sr_6),
    # Petential error in original code this was not using the sum var
    award_vehicle_robbery = as.integer(vehicle_robbery_6 <= target_vr_6),
    
    # If awarded in either crime varaible
    awarded= as.integer(award_violent_death==1 & 
                          award_street_robbery==1 & 
                          award_vehicle_robbery==1)
  ) %>% 
  # Hit_target varaible
  group_by(aisp) %>%
  mutate(hit_target = ifelse(cycle == 1,
                             lag(awarded) ,
                             on_target),
         awarded_l = lag(awarded, 1)) %>% 
  ungroup()


foo %>%  subset(sem_year>100 & month %in% c(6,7,12,1)) %>% select(hit_target) %>% summary

sm  %>%  subset(sem_year>100 & month %in% c(6,7,12,1)) %>% select(hit_target) %>% summary
sm  %>%  subset(sem_year>100) %>% select(hit_violent_death) %>% summary

  
#------------------------------------------------------------------------------#
# Regression

# Create a data set with only target months
sm_reg <- sm %>% 
  subset(sem_year>100) %>% 
  subset(month %in% c(6,7,12,1))

sm_reg$y <- sm_reg$violent_death_sim


felm(y ~ last_month_hit +  hit_month_l +  last_month  + 
    policemen_aisp + policemen_upp + n_precinct + max_prize +
    population | month + year | 0 | 0,
    data = sm_reg) %>% summary()



































