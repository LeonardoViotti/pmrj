#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#



# This code depends on analysis_main.R to run! So if this option is not selected
# on master it will be run here
if(!RUN_main_analysis){
  source(file.path(GITHUB, "analysis_main.R"))
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
sd <- regData(reg = r_vd_01, regdf = sr)


#------------------------------------------------------------------------------#
#### Table 1 - General descriptives ####

# Create table variabels

sd$pop1000 <- sd$population/1000

# Select variables
tab1Vars <- c(indepVars["on_target"],
              depVars[1:3], 
              ZVars,
              "other_robberies",
              "vehicle_theft",
              "street_theft",
              "body_found",
              "drug_seizure",
              "gun_seizure",
              "arrest2",
              "max_prize",
              "pop1000",
              "policemen_aisp",
              "policemen_upp")

tab1VarLabels <- c("On target",
                   "Violent  deaths",
                   "Vehicle  robbery",
                   "Street  robbery",
                   "Distance to violent death target",
                   "Distance to vehicle robbery target",
                   "Distance to street robbery target",
                   "Robberies not inclued in the target",
                   "Vehicle theft",
                   "Street theft",
                   "Cadavers found",
                   "Drug aprehension",
                   "Weapons aprehension",
                   "Arrests",
                   "Maximum prize (R$ 1000)",
                   "District population (1000 hab)",
                   "Policemen in the battalion",
                   "Policemen in UPP"
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
             xlab = "Registers of Street Robbery",
             ylab = "")

bplot_vr <- 
  boxPlotSim(sd$vehicle_robbery,
             data = sd,
             xlab = "Registers of Vehicle Robbery",
             ylab = "")


if(EXPORT_plots){
  
  bplot_vd +
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_violent_death_sim.png"),
           width = 6,
           height = 4)
  
  bplot_rr + 
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_street_robbery.png"),
           width = 6,
           height = 4)
  
  bplot_vr + 
    ggsave(filename = file.path(OUTPUTS_final, "Boxplot_vehicle_robbery.png"),
           width = 6,
           height = 4)
  
  
}


#------------------------------------------------------------------------------#
#### Bar plot 2 - Number od awards per AISP ####


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
  
  p_months_plot +
    ggsave(filename = file.path(OUTPUTS_final, "percentage_months_on_target.png"),
           width = 6,
           height = 4)
  
  
}


