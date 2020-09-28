#------------------------------------------------------------------------------#

#	 SIM - Robustness Spatial

#------------------------------------------------------------------------------#

# These are all defined in MASTER.R, only use to explicitly overwrite master.
OVERWRITE_MASTER_SWITCHES = F

if(OVERWRITE_MASTER_SWITCHES){
  EXPORT_data = F
  EXPORT_plots = T
  EXPORT_tables = T
}

#------------------------------------------------------------------------------#
#### Load data ####



# Loading data into a new object to be processed
sr <- final_data

# Load aisps shapefile
aisp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")
aisp <- spTransform(aisp, RjProj_unp)

#------------------------------------------------------------------------------#
#### Process data ####

#### Spatial analysis

# Keep only analysis years
# sr_sl <- sr %>% subset(year_month > 200912 & year_month < 201507)
sr_sl <- sr %>% subset(sem_year > 100)


# Remove ilha do governador and keep balanced panel
sr_sl <- sr_sl[!(sr_sl$aisp %in% c(17,41))]
aisp <- aisp[!(aisp@data$aisp %in% c(17,41)),]


#------------------------------------------------------------------------------#
#### Global objects ####


#------------------------------------------------------------------------------#
#### List regression variables ####

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery")

indepVars <- c("hit_sem_l",
               # "policemen_aisp",
               # "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )


FEVars <- c("aisp",
            "year", 
            "month", 
            "id_cmt")

FEVars_pla <- c("aisp",
                "year", 
                "month", 
                "cmd_name")

ZVars <- c("lag12_dist_target_vr",
           "lag12_dist_target_sr",
           "lag12_dist_target_vd")

ZVars_pla <- c("lag12_dist_target_vr_plapre",
               "lag12_dist_target_sr_plapre",
               "lag12_dist_target_vd_plapre")



#------------------------------------------------------------------------------#
### Spatial lag formulas ####

# Add FEs
sFormulaFE <- paste0("factor(",FEVars,")")
sFormula1 <- paste(c(indepVars, sFormulaFE[1:3]), collapse = " + ")
sFormula2 <- paste(c(indepVars, sFormulaFE), collapse = " + ")

Formulas01_sl_str <- paste(depVars, sFormula1, sep = " ~ ")
Formulas02_sl_str <- paste(depVars, sFormula2, sep = " ~ ")

names(Formulas01_sl_str) <- depVars
names(Formulas02_sl_str) <- depVars


#------------------------------------------------------------------------------#
#### Spatial analysis ####


#------------------------#
#### Create variables ####


# Year and month
#sr_sl$year_month <- sr_sl$year*100 + sr_sl$month

# Deaths per 100 thousand
sr_sl$lv_pop <- sr_sl$violent_death_sim/(sr_sl$population/100000)

# Carjack per 100 thousand
sr_sl$rv_pop <- sr_sl$vehicle_robbery /(sr_sl$population/100000)

# Street robbery per 100 thousand
sr_sl$rr_pop <- sr_sl$street_robbery/(sr_sl$population/100000)

# Montly variation - current month minus previous
sr_sl <- sr_sl %>% 
  group_by(aisp) %>% 
  arrange(aisp, year_month) %>% 
  mutate(lv_pop_d = lv_pop - dplyr::lag(lv_pop, n = 1),
         rv_pop_d = rv_pop - dplyr::lag(rv_pop, n = 1),
         rr_pop_d = rr_pop - dplyr::lag(rr_pop, n = 1),
         # Replace inf values
         lv_pop_d = ifelse(!is.finite(lv_pop_d),NA, lv_pop_d),
         rv_pop_d = ifelse(!is.finite(rv_pop_d),NA, rv_pop_d),
         rr_pop_d = ifelse(!is.finite(rr_pop_d),NA, rr_pop_d))


# Set panel data indexes
ps <- pdata.frame(sr_sl, index = c("aisp", "year_month"))


#### Spatial Variables

# Neighbourhood definition
nb <- poly2nb(aisp, queen = T)

# Neighbour weights
lw <- nb2listw(nb,
               style = "W",
               zero.policy = TRUE)

# Calculate spatial lagged values level
ps$lv_pop_slag <- slag(ps$lv_pop, lw)
ps$rv_pop_slag <- slag(ps$rv_pop, lw)
ps$rr_pop_slag <- slag(ps$rr_pop, lw)



#------------------------------------------------------------------------------#
#### Spatial lag models ####

#### Regressions

# SAR model function
SARlag_reg <- function(formula, data, nbW ){
  spml(formula = as.formula(formula), 
       data=data, 
       model = "pooling",
       lag = T,
       listw = nbW)
}



# Column 1
sl_vd_01 <- SARlag_reg(Formulas01_sl_str["violent_death_sim"],
                       data = ps,
                       nbW = lw)
sl_vr_01 <- SARlag_reg(Formulas01_sl_str["vehicle_robbery"],
                       data = ps,
                       nbW = lw)
sl_rr_01 <- SARlag_reg(Formulas01_sl_str["street_robbery"],
                       data = ps,
                       nbW = lw)

#------------------------------------------------------------------------------#
#### Moran's I ####

# Moran's I level
# moran_lv <- lm(lv_pop_lag ~ lv_pop, data = ps)
# moran_rv <- lm(rv_pop_lag ~ rv_pop, data = ps) %>% summary
# moran_rr <- lm(rr_pop_lag ~ rr_pop, data = ps) %>% summary


# Moran's I delta
#can't have NAs, so I'm removing all obs where the
#monthly crime difference is NA

ps_semJJ <- ps %>% subset(!(month %in% c(1,7)))


# Calculate spatial lagged values for on_target
ps_semJJ$lv_pop_d_slag <- slag(ps_semJJ$lv_pop_d, lw)
ps_semJJ$rv_pop_d_slag <- slag(ps_semJJ$rv_pop_d, lw)
ps_semJJ$rr_pop_d_slag <- slag(ps_semJJ$rr_pop_d, lw)


moran_lv_01 <- lm(lv_pop_d_slag ~ lv_pop_d, data = ps_semJJ)
moran_rv_01 <- lm(rv_pop_d_slag ~ rv_pop_d, data = ps_semJJ)
moran_rr_01 <- lm(rr_pop_d_slag ~ rr_pop_d, data = ps_semJJ)

moran_lv_02 <- lm(lv_pop_d_slag ~ lv_pop_d + factor(year) + factor(month), data = ps_semJJ)
moran_rv_02 <- lm(rv_pop_d_slag ~ rv_pop_d + factor(year) + factor(month), data = ps_semJJ)
moran_rr_02 <- lm(rr_pop_d_slag ~ rr_pop_d + factor(year) + factor(month), data = ps_semJJ)

#------------------------------------------------------------------------------#
##### Initial Exporting tables ####



#### Export spatial lag MANUALLY ####

# Regression variables
regVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "hit_sem_l",
             "policemen_aisp",
             "policemen_upp",
             "n_precinct",
             "max_prize",
             "population",
             "aisp",
             "year",
             "month")

# Regression data
ps_complete_bol <- complete.cases(ps[,regVars])
slregData <- ps[ps_complete_bol,]

coefs <- 
  c(sl_vd_01$coefficients['hit_sem_l'],
    sl_vr_01$coefficients['hit_sem_l'],
    sl_rr_01$coefficients['hit_sem_l'])

# Spatial leg coeff lambda

coefs_lambda <- 
  c(sl_vd_01$arcoef,
    sl_vr_01$arcoef,
    sl_rr_01$arcoef)


# Formatted SEs
seFormatFun <- function(model, var = NULL){
  
  if (is.null(var)){
    se <- abs(model$vcov.arcoef) %>% sqrt()
    coef <- model$arcoef
    
  } else{
    coef <- model$coefficients[var]
    se <- model$vcov[var,var] %>% sqrt()
  }
  
  
  # Calculate pvalue
  if(abs(coef) > 2.576*se){
    star <- "***"
  } else if(abs(coef) > 1.96*se){
    star <- "**"
  } else if (abs(coef) > 1.645*se){
    star <- "*"
  } else{
    star <- ""
  }
  
  # result <- paste0( "(", round(se,3), ")", star)
  result <- rbind( paste0( round(coef,3),star),
                   paste0( "(", round(se,3), ")"))
  
  return(result)
  
}



coefs <-
  cbind(seFormatFun(sl_vd_01, 'hit_sem_l'),
        seFormatFun(sl_vr_01, 'hit_sem_l'),
        seFormatFun(sl_rr_01, 'hit_sem_l'))


coefs_lambda <- 
  cbind(seFormatFun(sl_vd_01,),
        seFormatFun(sl_vr_01,),
        seFormatFun(sl_rr_01,))


# N obs
n_obs <- c(sl_vd_01$residuals %>% length(),
           sl_vr_01$residuals %>% length(),
           sl_rr_01$residuals %>% length())

# Number of AISPs
n_aisp <- c(37,37,37)


# Y mean
Ymean <- c(mean(slregData$violent_death_sim),
           mean(slregData$vehicle_robbery),
           mean(slregData$street_robbery))

# Adjusted R squared
arsq <- 
  function(model,data, depVar){
    n <- nrow(data)
    k <- length(model) -1
    sse <- sum(model$residuals^2)
    sst <- var( data[,depVar] ) * (n-1)
    rsq <- 1-(sse/sst)
    
    arsq <- 1 -(1-rsq)*((n-1)/(n-k-1))
    return(arsq)
  }

adjus_Rsq <- 
  c(arsq(sl_vd_01, slregData, "violent_death_sim"),
    arsq(sl_vr_01, slregData, "vehicle_robbery"),
    arsq(sl_rr_01, slregData, "street_robbery"))


# Add significance level placeholder




#### Create the table ####
#sl_vd_01

slRegTable <- 
  rbind(c("", "Number of occurrences", ""),
        c("Violent  deaths",
          "Vehicle  robbery  (Carjacking)", 
          "Street  robbery"),
        c("SAR", "SAR", "SAR"),
        c("(1)", "(2)", "(3)"),
        coefs,
        coefs_lambda,
        n_obs,
        n_aisp,
        Ymean %>% round(2),
        adjus_Rsq %>% round(2))

#colnames(slRegTable) <- c("violent_death_sim", "vehicle_robbery", "street_robbery")


# Add parenthesis to  SE
# slRegTable[5,] <- paste0("(",slRegTable[5,],")")
# slRegTable[7,] <- paste0("(",slRegTable[7,],")")

# Add stars


# Add row names (gambiarra da porra)
rows <- c("     ",
          "",
          " ",
          "  ",
          "On target",
          "   ",
          "Lamda",
          "    ",
          "Observations",
          "Number of aisp",
          "Y mean",
          "R2 adjusted")

# Remove column and row names
rownames(slRegTable) <- (1:dim(slRegTable)[1])


slRegTable <- slRegTable %>%
  as.data.frame(stringsAsFactors = F)

# slRegTable <- 
#   cbind(rows,
#         slRegTable) 
names(slRegTable) <- NA
rownames(slRegTable) <- rows

#------------------------------------------------------------------------------#
##### Processing tables ####

slRegTable_hux <-
  huxtable(slRegTable, add_colnames = F, add_rownames = T) %>%
  set_tb_padding(2) %>% 
  set_top_border(1, everywhere) %>% 
  set_bottom_border(1, 2:4) %>% 
  set_bottom_border(4, everywhere) %>% 
  set_bottom_border(8, everywhere) %>% 
  set_bottom_border(12, everywhere)
  

#------------------------------------------------------------------------------#
##### Moran's I plots ####

# Graphics formatting definition
moran_plot <- function(reg, label, data){
  moranI <- reg$coefficients[2] %>% round(4)
  pVal <- summary(reg)$coefficients[regIndepVars(reg)
                                    ,"Pr(>|t|)"] %>% round(4)
  title <- paste("Moran's I =",
                 moranI,
                 "\n",
                 "P value ~",
                 pVal)
  with(data, 
       plot(regIndepVars(reg)%>% get(), 
            regDepVars(reg)%>% get(),
            xlab = label,
            ylab = paste("W", label),
            cex.lab=2,
            cex.axis=1.5))
  abline(reg)
  title(main=title, 
        cex.main=2 )
  
}


#### Export graphs
if(EXPORT_plots){
  
  # Violent death  
  png(file = file.path(OUTPUTS_final, 
                       "moran_lv_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_lv_01, 
             "Violent Death",
             ps_semJJ)
  dev.off()
  
  # Vehicle rob
  png(file = file.path(OUTPUTS_final, 
                       "moran_rv_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_rv_01, 
             "Vehicle robbery",
             ps_semJJ)
  dev.off()
  
  # Street rob
  png(file = file.path(OUTPUTS_final, 
                       "moran_rr_01.png"),
      width = 600, 
      height = 600)
  par(mar = c(5, 5, 5, 5))
  moran_plot(moran_rr_01, 
             "Street robbery",
             ps_semJJ)
  dev.off()
  
}

#------------------------------------------------------------------------------#
##### Actually exporting ####

if(EXPORT_tables){
  huxtable::quick_xlsx(slRegTable_hux, file = file.path(OUTPUTS_final, "spatial_lag_formated.xlsx"))

  }

