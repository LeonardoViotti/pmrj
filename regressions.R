
#------------------------------------------------------------------------------#

#	 SIM - Regressions		

#------------------------------------------------------------------------------#


sr <-  read.dta13(file.path(DROPBOX, "data_SIM_2019-01.dta")) 


#### List variables

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "homicide",
             "dpolice_killing")

indepVars <- c("on_target",
               "policemen_aisp",
               "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )

FEVars <- c("year", 
            "month", 
            "id_cmt")

#ZVars <- c()

### Regression formulas

# right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 

#rFormulaFE <- paste0("factor(",FEVars,")")
# rFormula1 <- paste(c(indepVars, rFormulaFE[1:2]), collapse = " + ") 
# rFormula2 <- paste(c(indepVars, rFormulaFE), collapse = " + ") 

# FE cluster and instruments

FeForumala1 <- paste(FEVars[1:2], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | 0 ")


Formulas1 <- paste(depVars, paste(rFormula, config1), sep = " ~ ")


#------------------------------------------------------------------------------#
#### Regression models ####

fooFormula <- as.formula(Formulas1[1])

summary(felm(fooFormula, data = sr))


