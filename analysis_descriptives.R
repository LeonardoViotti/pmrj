#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Load data ####

#sd <- final_data

# Keep same sample for as regressions
sd <- regData(reg = r_vd_01, regdf = sr)


#------------------------------------------------------------------------------#
#### Table 1 - General descriptives ####


tab1Vars <- c(indepVars["on_target"],
              depVars[1:3], 
              ZVars,
              indepVars, 
              FEVars)




stargazer(sd %>% select(tab1Vars),
          type = "text")


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
             xlab = "Registers of Street Robbery",
             ylab = "")

bplot_vd <- 
  boxPlotSim(sd$vehicle_robbery,
             xlab = "Registers of Vehicle Robbery",
             ylab = "")


#------------------------------------------------------------------------------#
#### Bar plot 2 - Number od awards per AISP ####


#------------------------------------------------------------------------------#
#### Bar plot 2 - % of months on target per AISP ####

ggplot(data = sd ,
       aes(y = on_target,
           x =  factor(aisp, levels = unique(sd$aisp) %>% sort(decreasing = T))
           )
       ) +
  geom_bar(stat="identity")

