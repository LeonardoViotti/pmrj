#------------------------------------------------------------------------------#

#	 SIM - Draft ESD

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

# Keep same sample for all models, i.e from 2010 onwards because of IV
sr <- sr[year_month > 201006 & year_month < 201507,]

#------------------------------------------------------------------------------#
#### Anaylsis df ####

# Simplified df
vars <- 
  c("aisp",
    "month",
    "year",
    "year_month",
    "semester",
    "violent_death_sim_cum",
    "vehicle_robbery_cum",
    "street_robbery_cum",
    "target_vd_sem",
    "target_vr_sem",
    "target_sr_sem" #,
    # "on_target_vd",
    # "on_target_vr",
    # "on_target_sr",
    # "on_target"
    )

# View(sr[,vars, with = F])

esdf <- sr %>% select(vars)


#------------------------------------------------------------------------------#
#### Anaylsis variables ####

# Reconstruct on_target to only take into account semester target

esdf$on_target_vd <- as.integer(sr$violent_death_sim_cum <= sr$target_sr_sem)
esdf$on_target_vr <- as.integer(sr$vehicle_robbery_cum <= sr$target_sr_sem)
esdf$on_target_sr <- as.integer(sr$street_robbery_cum <= sr$target_sr_sem)

esdf$on_target <- as.integer(esdf$on_target_vd & esdf$on_target_vr & esdf$on_target_sr)


# Number of months before or after event (hit target)

levels <- rep(c(2010:2015)*100, each=12) +  c(1:12)

esdf$year_month <-factor(esdf$year_month, levels =levels,  ordered=TRUE)


# Function that grabs month when target was hit
hit_month <- function(target, t){
  
  cum_target <- 
    cumsum(
      replace_na(
        target,0)
    )

  hit <- which(cum_target == max(cum_target))[1] 

  return( t[hit])
  
}

# Create t diff from target
esdf %<>% 
  group_by(aisp, year, semester) %>% 
  mutate(hit_month = hit_idx(on_target,year_month),
         t = unclass(year_month) - unclass(hit_month)+1)

#------------------------------------------------------------------------------#
#### ES models ####

#------------------------------------------------------------------------------#
#### ES plots ####
