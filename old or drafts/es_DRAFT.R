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
    "population",
    "violent_death_sim",
    "vehicle_robbery",
    "street_robbery",
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

  # Replace 
  
  
  return( t[hit])
  
}

# Create t diff from target
esdf %<>% 
  group_by(aisp, year, semester) %>% 
  mutate(hit_month = hit_month(on_target,year_month),
         t = unclass(year_month) - unclass(hit_month),
         t = as.factor(t),
         # Replace t if there was no event
         replace_t = ifelse(sum(on_target, na.rm = T) == 5,
                99,
                NA))
esdf$t[esdf$replace_t == 99] <- 99

# Per 100.000 pop crime vars
esdf$vd_ppop <- (esdf$violent_death_sim/esdf$population)*10e4
esdf$vr_ppop <- (esdf$vehicle_robbery/esdf$population)*10e4
esdf$sr_ppop <- (esdf$street_robbery/esdf$population)*10e4

#------------------------------------------------------------------------------#
#### ES models ####

# Too few obs
esdf_reg <- esdf %>% subset(!(t %in% c(4,5)))

# chosing to drop -5 in lm
esdf_reg$t <- relevel(esdf_reg$t %>% as.factor(), ref = "-4") 

# Run models
eslm_vd <- felm(vd_ppop ~ t | aisp | 0 | 0,
                data = esdf_reg)

eslm_vr <- felm(vr_ppop ~ t | aisp | 0 | 0,
                data = esdf_reg)

eslm_sr <- felm(sr_ppop ~ t | aisp | 0 | 0,
                data = esdf_reg)

#------------------------------------------------------------------------------#
#### ES plots ####


plotData <- function(reg){
  
  reg_df <- data.frame(coeff = reg$coeff,
                       se = reg$se)  
  
  names(reg_df) <- c("coeff", "se") # not sure why it doesnt work simply using "coeff =" above
  
  reg_df$monthsFromTreat <- row.names(reg_df)
  
  # Fix variable names
  reg_df$monthsFromTreat <- as.integer(sub("t", "", reg_df$monthsFromTreat))
  reg_df$monthsFromTreat <- as.ordered(reg_df$monthsFromTreat)    
  
  return(reg_df)
}

eslm_vd_df <- plotData(eslm_vd)
eslm_vr_df <- plotData(eslm_vr)
eslm_sr_df <- plotData(eslm_sr)

# Plot
plot_def <- function(regdata, title, xlabel){
  
  ggplot(regdata, aes(x=monthsFromTreat, y=coeff)) +
    geom_point(col = "navyblue")+
    geom_vline(xintercept=5) +
    geom_errorbar(aes(ymin = coeff - se, ymax = coeff + se),  col = "navyblue", width=.2)+
    
    xlab(xlabel) +
    
    theme(
      legend.title=element_blank(),
      panel.background = element_blank(),
      panel.grid.major =element_line(colour = "gray90"),
      panel.grid.minor =element_line(colour = "gray90"),
      plot.title = element_text(hjust = .5)) +
    ggtitle(title)
  
}

png(file= file.path(OUTPUTS, "ES_plot_vd.png"))
plot_def(eslm_vd_df, # data
         "Violent death", # title
         "Months from event") # x label
dev.off()

png(file= file.path(OUTPUTS, "ES_plot_vr.png"))
plot_def(eslm_vr_df, # data
         "Vehicle robbery", # title
         "Months from event") # x label
dev.off()


png(file= file.path(OUTPUTS, "ES_plot_sr.png"))
plot_def(eslm_sr_df, # data
         "Street robbery", # title
         "Months from event") # x label
dev.off()








