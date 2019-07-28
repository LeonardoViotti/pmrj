
library(pglm)


poi <- sr %>% subset(cycle !=1)

# poi$vd_pop <- poi$violent_death_sim/poi$population



form1 <- vehicle_robbery ~ on_target + 
  factor(year) +  factor( month) + factor(aisp) +factor(id_cmt) +
  policemen_aisp + policemen_upp + n_precinct+ offset(log(population))

# form2 <- vd_pop ~ on_target + 
#   factor(year) +  factor( month) + factor(aisp) +factor(id_cmt) +
#   policemen_aisp + policemen_upp + n_precinct + offset(log(population))



glm(form1,
    family = poisson,
    data = poi) %>% summary(digits = 3)




# psr <- sr %>% subset(cycle !=1)
# pdf <- pdata.frame(psr, index=c("aisp","year_month"))    
# 
# 
# form <- violent_death_sim ~ on_target + 
#   factor(year) +  factor( month) + factor(id_cmt) +
#   policemen_aisp + policemen_upp + n_precinct
# 
# pglm(form,
#      family = poisson, 
#      data = pdf, 
#      effect = "individual", 
#      model="within") %>% summary()
