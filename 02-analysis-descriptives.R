#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#



# This code depends on analysis_main.R to run! So if this option is not selected
# on master it will be run here
if(!RUN_main_analysis){
  source(file.path(GITHUB, "02-analysis-main.R"))
}



# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = F
  EXPORT_tables = F
}


#------------------------------------------------------------------------------#
#### Load data ####

#sd <- final_data

# Keep same sample for as regressions
sd <- feRegSim('violent_death_sim') %>% regData(regdf = sr)


#------------------------------------------------------------------------------#
#### Table 1 - General descriptives ####

# Create table variabels

sd$pop1000 <- sd$population/1000

# Select variables
tab1Vars <- c(indepVars["hit_sem_l"],
              depVars[1:3], 
              "other_robberies",
              "vehicle_theft",
              "street_theft",
              "body_found",
              "drug_seizure",
              "gun_seizure",
              "arrest2",
              "max_prize",
              "pop1000")

tab1VarLabels <- c("On target",
                   "Violent  deaths",
                   "Vehicle  robbery",
                   "Street  robbery",
                   "Robberies not inclued in the target",
                   "Vehicle theft",
                   "Street theft",
                   "Cadavers found",
                   "Drug aprehension",
                   "Weapons aprehension",
                   "Arrests",
                   "Maximum prize (R$ 1000)",
                   "District population (1000 hab)"
)


stargazer(sd %>% select(tab1Vars),
          summary.stat = c("n", "mean", "sd", "min", "max"),
          type = "html",
          covariate.labels = tab1VarLabels,
          out = file.path(OUTPUTS_final, "tab1.html"))


#------------------------------------------------------------------------------#
#### Box plot - Number of ocurrences per AISP ####


boxPlotSim <- function(var,
                       xlab,
                       ylab,
                       data){
  ggplot(data = data ,
         aes(y = var,
             x =  factor(aisp, levels = unique(data$aisp) %>% sort(decreasing = T))
         )
  ) + 
    geom_boxplot(col = "dodgerblue4") +
    coord_flip() + 
    labs(x= ylab, y = xlab) +  # Because of coord_flip() these are inverted
  theme_minimal()
  
}

bplot_vd <- 
  boxPlotSim(sd$violent_death_sim,
             data = sd,
             xlab = "Victims of Violent Death",
             ylab = "")

bplot_rr <- 
  boxPlotSim(sd$street_robbery,
             data = sd,
             xlab = "Records of Street Robbery",
             ylab = "")

bplot_vr <- 
  boxPlotSim(sd$vehicle_robbery,
             data = sd,
             xlab = "Records of Vehicle Robbery",
             ylab = "")


if(EXPORT_plots){
  
  bplot_vd %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_violent_death_sim.png"),
           width = 6,
           height = 4)
  dev.off()
  
  bplot_rr %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_street_robbery.png"),
           width = 6,
           height = 4)
  dev.off()
  
  bplot_vr %>% print() 
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_vehicle_robbery.png"),
           width = 6,
           height = 4)
  dev.off()
  
}


#------------------------------------------------------------------------------#
#### Bar plot 2 - Number od awards per AISP ####

awar_data <- final_data

# Fix aisps that are more in more than one risp
awar_data$risp[awar_data$aisp == 39] <- 3
awar_data$risp[awar_data$aisp == 40] <- 2
awar_data$risp[awar_data$aisp == 41] <- 2

# Collapse data by aisp
sd_awarded <- awar_data %>% 
  subset(year < 2016) %>% 
  group_by(aisp, risp) %>% 
  summarise(awarded = sum(awarded, na.rm = T)/6) %>% 
  arrange(awarded)

# Create region var

sd_awarded$region  <- NA
sd_awarded$region[sd_awarded$risp %in% c(1,2)] <- "State Capital"
sd_awarded$region[sd_awarded$risp %in% c(3,4)] <- "Metroplitan area"
sd_awarded$region[sd_awarded$risp %in% c(5,6,7)] <- "Other Cities"


# color
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cols <- c("dodgerblue4", "lightblue2", "darkslategrey", "deeppink4", "gray50", "mediumorchid4", "turquoise4")



# Plot
p_awards <- 
  ggplot(data = sd_awarded,
         aes(y = awarded,
             x = factor(aisp, levels = unique(sd_awarded$aisp)) 
         )
  ) +
  geom_bar(stat="identity", 
           width=0.7,
           aes(fill = factor(risp))
  )+
  coord_flip() + 
  ylab("Number of awards") +
  xlab("AISP") +
  #scale_fill_brewer(palette="Accent") + 
  scale_fill_manual("RISP", values=cols) + 

  theme_minimal()



if(EXPORT_plots){
  
  p_awards %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "awards_graph.png"),
           width = 6,
           height = 4)
  
  
}


#------------------------------------------------------------------------------#
#### Bar plot 2 - % of months on target per AISP ####

p_months_plot <- 
  ggplot(data = sd ,
         aes(y = on_target,
             x =  factor(aisp, levels = unique(sd$aisp))
         )
  ) +
  geom_bar(stat="identity", fill = "dodgerblue4")+
  xlab("AISP") +
  ylab("Percentage of months on PFP target") +
  theme_minimal()





if(EXPORT_plots){
  
  p_months_plot  %>% print()
    ggsave(filename = file.path(OUTPUTS_final, "percentage_months_on_target.png"),
           width = 6,
           height = 4)
  
  
}


