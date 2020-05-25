
#------------------------------------------------------------------------------#

#	 SIM - 10 reproduction DRAFT	

#------------------------------------------------------------------------------#

# this should be added to another part of the code probably

sm <- raw_data

sm$last_month <- ifelse(sm$month %in% c(6,12),
                       1,
                       0)
# Create variables for hit target exercise

sm <- 
  sm %>% 
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
    award_vehicle_robbery = as.integer(vehicle_robbery_6 <= target_vr_6),
    
    # If awarded in either crime varaible
    awarded= as.integer(award_violent_death==1 & 
                          award_street_robbery==1  &
                          award_vehicle_robbery==1)
  ) %>% 
  #Hit_target varaible
  group_by(aisp) %>%
  mutate(hit_target = as.numeric(on_target == 1),
         awarded_l = dplyr::lag(awarded, 1),
         hit_target = ifelse(cycle == 1,
                             awarded_l ,
                             on_target),
         # Replace NAs with zeros
         hit_target = ifelse(is.na(hit_target),
                             0 ,
                             hit_target)
         ) %>% 
    ungroup()

  
#------------------------------------------------------------------------------#
# Regression

# Create a data set with only target months
sm_reg <- sm %>% 
  # subset(sem_year>100) %>% 
  subset(month %in% c(6,7,12,1)) %>% 
  # Regression variables 
  mutate(last_month_hit = hit_target*last_month)


# sm_reg$aisp<- relevel(sm_reg$aisp %>% as.character() %>% as.factor(), ref = '40')

sm_reg$y <- sm_reg$violent_death_sim
#sm_reg$y <- sm_reg$vehicle_robbery


felm(y ~ last_month_hit +  hit_target +  last_month  +
       policemen_aisp + policemen_upp + n_precinct + max_prize +
       population | aisp + year | 0 | 0,
     data = sm_reg %>% subset(sem_year>100)) %>% summary()


# lm(y ~ 
#     last_month_hit +  
#     hit_target   +
#     last_month  +
#     policemen_aisp +
#     policemen_upp +
#     n_precinct +
#     max_prize +
#     population +
#     factor(year) +
#     factor(aisp), 
#    data = sm_reg %>% subset(sem_year>100)) %>% stargazer(type = 'text')   


# 
# sm_reg %>% 
#   subset(sem_year>100) %>% 
#   select(aisp, 
#          year, 
#          month,
#          last_month,
#          awarded,
#          hit_target,
#          policemen_aisp,
#          policemen_upp,
#          n_precinct,
#          max_prize,
#          population,
#          month,
#          year)  %>% View()
# 
# sm_reg %>% 
#   subset(sem_year>100) %>% 
#   select(aisp, 
#          year, 
#          month,
#          awarded,
#          awarded_l,
#          year)  %>% View()



