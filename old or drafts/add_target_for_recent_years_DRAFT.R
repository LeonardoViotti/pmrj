library(anytime)

metas <- fread(file.path(DATA,"esic4560.csv"))

# Process
m_processed <-
  metas %>% 
    mutate(
      # Convert dates
      month = month(anytime(ano_mes)),
      year = year(anytime(ano_mes)),
      
      # Semester
      sem = ifelse(month < 7,
                   1,
                   2)
      
      ) %>% 
    select(-ano_mes) %>% 
    
    # Calculate semester targets
    group_by(aisp, year, sem) %>% 
    mutate(target_vd_sem = sum(meta_lv),
           target_vr_sem = sum(meta_rv),
           target_sr_sem = sum(meta_rr)) %>% 
  ungroup() %>% 
  
  # Match variable names
  rename(target_vd = meta_lv,
         target_vr = meta_rv,
         target_sr = meta_rr) %>%
  # Removing unecessary variables. For now including the monthly,
  # variables I just renamed to make my file easier when joining the
  # data.
  select(-sem, -target_vd, -target_vr, -target_sr)
  
# Process raw data, since 2 2015 has zeros for all targets
bol22015 <- raw_data$year == 2015 & raw_data$semester == 2

raw_data$target_vd_sem[bol22015] <- NA
raw_data$target_vr_sem[bol22015] <- NA
raw_data$target_sr_sem[bol22015] <- NA


# Merge to raw data
sm <- 
raw_data %>% 
  full_join(m_processed, 
        by = c('aisp', 'month', 'year')) %>% 
  mutate(target_vd_sem = ifelse(is.na(target_vd_sem.x), 
                                target_vd_sem.y,
                                target_vd_sem.x),
         target_vr_sem = ifelse(is.na(target_vr_sem.x), 
                                target_vr_sem.y,
                                target_vr_sem.x),
         target_sr_sem = ifelse(is.na(target_sr_sem.x), 
                                target_sr_sem.y,
                                target_sr_sem.x)) %>% 
  select(-c("target_vd_sem.x",
            "target_vd_sem.y",
            "target_vr_sem.x",
            "target_vr_sem.y",
            "target_sr_sem.x",
            "target_sr_sem.y"))
