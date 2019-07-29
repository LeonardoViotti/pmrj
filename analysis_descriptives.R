#------------------------------------------------------------------------------#

#	 SIM - Descriptive Stats	

#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Load data ####

sd <- final_data

# Keep same sample for as regressions
sd <- sd[sem_year > 100,]


#------------------------------------------------------------------------------#
#### Table 1 - General descriptives ####

tab1Data <- regData(reg = r_vd_01, regdf = sr)



tab1Vars <- c(indepVars["on_target"],
              depVars[1:3], 
              ZVars,
              indepVars, 
              FEVars)




stargazer(tab1Data %>% select(tab1Vars),
          type = "text")


#------------------------------------------------------------------------------#
#### Box plot - Number of ocurrences per AISP ####


sg1 <- sr %>% subset(year_month < 201507 & year_month > 200912)
sg2 <- sr %>% subset(sem_year > 100)


sg3 <- regData(reg = r_vd_01, regdf = sr)
sg4 <- regData(reg = r_vd_IV, regdf = sr)


boxPlotSim <- function(var,
                       xlab,
                       ylab,
                       data){
  ggplot(data = data ,
         aes(y = var,
             x =  factor(aisp, levels = unique(data$aisp) %>% sort(decreasing = T))
         )
  ) + 
    geom_boxplot() +
    coord_flip() + 
    labs(x= ylab, y = xlab) # Because of coord_flip() these are inverted
  
  
  
}

#bplot_vd <- 
  boxPlotSim(sg1$violent_death_sim,
             data = sg1,
             xlab = "Victims of Violent Death",
             ylab = "")

  boxPlotSim(sg3$violent_death_sim,
             data = sg3,
             xlab = "Victims of Violent Death",
             ylab = "")


bplot_rr <- 
  boxPlotSim(sg1$street_robbery,
             xlab = "Registers of Street Robbery",
             ylab = "")

bplot_vd <- 
  boxPlotSim(sg1$vehicle_robbery,
             xlab = "Registers of Vehicle Robbery",
             ylab = "")


#------------------------------------------------------------------------------#
#### Bar plot 2 - Number od awards per AISP ####

#------------------------------------------------------------------------------#
#### Bar plot 2 - % of months on target per AISP ####