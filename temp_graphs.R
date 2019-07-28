
#### Descriptive boxplots


boxPlotSim <- function(var,
                       xlab,
                       ylab){
  ggplot(data = sr ,
         aes(y = var,
             x =  factor(aisp, levels = unique(sr$aisp) %>% sort(decreasing = T))
         )
  ) + 
    geom_boxplot() +
    coord_flip() + 
    labs(x= ylab, y = xlab) # Because of coord_flip() these are inverted

  
  
}

boxPlotSim(sr$violent_death_sim,
           xlab = "Victims of Violent Death",
           ylab = "")


boxPlotSim(sr$street_robbery,
           xlab = "Registers of Street Robbery",
           ylab = "")


boxPlotSim(sr$vehicle_robbery,
           xlab = "Registers of Vehicle Robbery",
           ylab = "")



#### Monthly coeficients plots

# Create month order dummys
sr$m2 <- ifelse(sr$month %in% c(2,8), 1, 0)
sr$m3 <- ifelse(sr$month %in% c(3,9), 1, 0)
sr$m4 <- ifelse(sr$month %in% c(4,10), 1, 0)
sr$m5 <- ifelse(sr$month %in% c(5,11), 1, 0)
sr$m6 <- ifelse(sr$month %in% c(6,12), 1, 0)

# Create on_target X mN interaction
sr$month2 <- sr$m2*sr$on_target
sr$month3 <- sr$m3*sr$on_target
sr$month4 <- sr$m4*sr$on_target
sr$month5 <- sr$m5*sr$on_target
sr$month6 <- sr$m6*sr$on_target


# Construct monthly regression formulas
month_dummies <- c("month2", 
                   "month3",
                   "month4",
                   "month5",
                   "month6")

rFormula_plot <- paste(c(month_dummies, 
                         # Remove on_targer as it is already in the interactions
                         indepVars[-1]), 
                       collapse = " + ") 
Formulas02_plot_str <- paste(depVars, paste(rFormula_plot, config2), sep = " ~ ")
names(Formulas02_plot_str) <- depVars

# Monthly regression
foo <- feRegSim(Formulas02_plot_str["violent_death_sim"])

rplot_vd <- feRegSim(Formulas02_plot_str["violent_death_sim"])
rplot_vr <- feRegSim(Formulas02_plot_str["vehicle_robbery"])
rplot_rr <- feRegSim(Formulas02_plot_str["street_robbery"])


#### Actual plots

# Select only month coeffs
coefs_df <- data.frame(coef = rplot_vd$coefficients[month_dummies,],
                       month = month_dummies,
                       se = rplot_vd$rse[month_dummies])


ggplot(data = coefs_df,
       aes(y = coef,
           x = month)) +
  geom_point()+
  geom_errorbar(aes(ymin=coef-se, 
                    ymax=coef+se),
                width=.2)+
  geom_hline(yintercept=0,
             color = "red")
  

