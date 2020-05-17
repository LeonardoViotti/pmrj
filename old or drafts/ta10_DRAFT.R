
#------------------------------------------------------------------------------#

#	 SIM - 10 reproduction DRAFT	

#------------------------------------------------------------------------------#

# this should be added to another part of the code probably

sm <- raw_data

sm$last_moth <- ifelse(sm$month %in% c(6,12),
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
    # Petential error in original code this was not using the sum var
    award_vehicle_robbery = as.integer(vehicle_robbery_6 <= target_vr_6),
    
    # If awarded in either crime varaible
    awarded= as.integer(award_violent_death==1 | 
                          award_street_robbery==1 | 
                          award_vehicle_robbery==1)
    ) %>% 
  # Hit_target varaible
  # group_by(aisp) %>% 
  # mutate(hit_target = ifelse(cycle == 1,
  #                            lag(awarded) ,
  #                            on_target),
  #        awarded_l = lag(awarded, 1))
  # 
  