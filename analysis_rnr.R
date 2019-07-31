#------------------------------------------------------------------------------#

#	 SIM - R&R Analysis

#------------------------------------------------------------------------------#

EXPORT_tables = F
EXPORT_plots = F

#------------------------------------------------------------------------------#
#### Load data ####



# Loading data into a new object to be processed
sr <- final_data

# Load aisps shapefile
aisp <- readOGR(dsn = GIS, layer = "lm_aisp_2019")
aisp <- spTransform(aisp, RjProj_unp)

#------------------------------------------------------------------------------#
#### Process data ####

#### Placebo
sr_pl <- sr[sr$year < 2009,]

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

indepVars <- c("on_target",
               "policemen_aisp",
               "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )

indepVars_pla <- c("on_target_plapre",
                   #"policemen_aisp",
                   #"policemen_upp",
                   "n_precinct",
                   #"max_prize",
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
### OLS formulas ####

# Right hand side without FE
rFormula <- paste(indepVars, collapse = " + ") 
rFormula_pla <- paste(indepVars_pla, collapse = " + ") 

rFormula_iv <- paste(indepVars[-1], collapse = " + ") 
rFormula_iv_pla <- paste(indepVars_pla[-1], collapse = " + ") 

# Add FE, cluster and instruments

#clusterVars = c("aisp")
clusterVars= "0"

clusterVars_form <- paste(clusterVars, collapse =  " + ")

FeForumala1 <- paste(FEVars[1:3], collapse = " + ")
config1 <- paste("|", FeForumala1, "| 0 | ", clusterVars_form )

FeForumala2 <- paste(FEVars, collapse = " + ") # with cmd FE
config2 <- paste("|", FeForumala2, "| 0 |  ", clusterVars_form)

FeForumala2_pla <- paste(FEVars_pla, collapse = " + ") # with cmd FE
config2_pla <- paste("|", FeForumala2_pla, "| 0 |  ", clusterVars_form)


# IV formula
first_stage_left <- "on_target"
first_stage_left_pla <- "on_target_plapre"

first_stage_right <- paste(ZVars, collapse = " + ")
first_stage_right_pla <- paste(ZVars_pla, collapse = " + ")


formula_1st <-  paste("(", first_stage_left, " ~ ", first_stage_right, " )")
formula_1st_pla <-  paste("(", first_stage_left_pla, " ~ ", first_stage_right_pla, " )")

config_iv <- paste("|", FeForumala2, "|" ,  formula_1st,  "| ", clusterVars_form)
config_iv_pla <- paste("|", FeForumala2_pla, "|" ,  formula_1st_pla,  "| ", clusterVars_form)


#### Final formulas

Formulas01_str <- paste(depVars, paste(rFormula, config1), sep = " ~ ")
Formulas02_str <- paste(depVars, paste(rFormula, config2), sep = " ~ ")
FormulasIV_str <- paste(depVars, paste(rFormula_iv, config_iv), sep = " ~ ")

# Placebo
Formulas01_pla_str <- paste(depVars, paste(rFormula_pla, config1), sep = " ~ ")
Formulas02_pla_str <- paste(depVars, paste(rFormula_pla, config2_pla), sep = " ~ ")
FormulasIV_pla_str <- paste(depVars, paste(rFormula_iv_pla, config_iv_pla), sep = " ~ ")

# So it's easier to refernce to elements
names(Formulas01_str) <- depVars
names(Formulas02_str) <- depVars
names(FormulasIV_str) <- depVars
names(Formulas01_pla_str) <- depVars
names(Formulas02_pla_str) <- depVars
names(FormulasIV_pla_str) <- depVars

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
#### Placebo analysis ####


# Regressions and Consley SEs - placebo
feRegSim_placebo <- function(form){
  form <- as.formula(form)
  model <- felm(form, data = sr_pl, keepCX = T)
  
  # Rename Dep var for IV just for exporting
  if (!is.null(model$endovars)){
    rownames(model$coefficients)[grep("`on_", rownames(model$coefficients))] <- "on_target_plapre"
    rownames(model$beta)[grep("`on_", rownames(model$beta))] <- "on_target_plapre"
    colnames(model$cX)[grep("`on_", colnames(model$cX))] <- "on_target_plapre"
  }
  
  # Return regression object
  return(model)
  
}


#### Placebo OLS
# Tabble 2
p_vd_01 <- feRegSim_placebo(Formulas01_pla_str["violent_death_sim"])
p_vd_01_data <- regData(p_vd_01, sr_pl)

p_vr_01 <- feRegSim_placebo(Formulas01_pla_str["vehicle_robbery"])
p_vr_01_data <- regData(p_vr_01, sr_pl)

p_rr_01 <- feRegSim_placebo(Formulas01_pla_str["street_robbery"])
p_rr_01_data <- regData(p_rr_01, sr_pl)


### Model 2 whith cmnd FE - placebo

# Tabble 2
p_vd_02 <- feRegSim_placebo(Formulas02_pla_str["violent_death_sim"])
p_vd_02_data <- regData(p_vd_02, sr_pl)

p_vr_02 <- feRegSim_placebo(Formulas02_pla_str["vehicle_robbery"])
p_vr_02_data <- regData(p_vr_02, sr_pl)

p_rr_02 <- feRegSim_placebo(Formulas02_pla_str["street_robbery"])
p_rr_02_data <- regData(p_rr_02, sr_pl)


#### Placebo 2SLS

# Tabble 2
p_vd_IV <- feRegSim_placebo(FormulasIV_pla_str["violent_death_sim"])
p_vd_IV_data <- regData(p_vd_IV, sr_pl)

p_vr_IV <- feRegSim_placebo(FormulasIV_pla_str["vehicle_robbery"])
p_vr_IV_data <- regData(p_vr_IV, sr_pl)

p_rr_IV <- feRegSim_placebo(FormulasIV_pla_str["street_robbery"])
p_rr_IV_data <- regData(p_rr_IV, sr_pl)


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


#### Export placebo table ####

indepVar_label <- c("On target" = "on_target")
indepVar_label_pla <- c("On target" = "on_target_plapre")

stats_labels <- c("Observations" = "nobs",  
                  "R2 adjusted" = "adj.r.squared")

#models_labels <- c("Violent Death", "Violent Death", "Carjacking", "Carjacking")


models_labels <- c("Model 1" = "OLS", 
                   "Model 2" = "OLS", 
                   "Model 3" = "2SLS", 
                   "Model 4" = "OLS",
                   "Model 5" = "OLS", 
                   "Model 6" = "2SLS",
                   "Model 7" = "OLS", 
                   "Model 8" = "OLS", 
                   "Model 9" = "2SLS")

# Table 2 - placebo
tab2_pla_regs <- 
  list(p_vd_01,
       p_vd_02,
       p_vd_IV,
       p_vr_01,
       p_vr_02, 
       p_vr_IV,
       p_rr_01, 
       p_rr_02, 
       p_rr_IV)


tab2_pla_addLines <- list(chifeFE_line_9,
                      Ymean_row(tab2_pla_regs),
                      n_aisp_line_9)


createTable(reg_list = tab2_pla_regs,
            add_lines_list = tab2_pla_addLines,
            dep_var_labels = c("Violent deaths", 
                               "Vehicle robbery (Carjacking)",	
                               "Street robbery"),
            title = "Table B1– Robustness: Effect of expectation of receiving bonuses on crime rates (Placebo analysis between 2006 and 2008)",
            outPath = file.path(OUTPUTS_final, "tabB1.html"))




#### Export spatial lag MANUALLY ####

# Regression variables
regVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "on_target",
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
  c(sl_vd_01$coefficients['on_target'],
    sl_vr_01$coefficients['on_target'],
    sl_rr_01$coefficients['on_target'])


ses <- 
  c(sl_vd_01$vcov['on_target','on_target'] %>% sqrt(),
    sl_vr_01$vcov['on_target','on_target'] %>% sqrt(),
    sl_rr_01$vcov['on_target','on_target'] %>% sqrt())


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


#### Create the table ####
#sl_vd_01

slRegTable <- 
  rbind(c("Violent  deaths",
          "Vehicle  robbery  (Carjacking)", 
          "Street  robbery"),
        c("SAR", "SAR", "SAR"),
        c("(1)", "(2)", "(3)"),
        coefs %>% round(2),
        ses %>% round(2) ,
        n_obs,
        n_aisp,
        Ymean %>% round(2),
        adjus_Rsq %>% round(2)
  )

#colnames(slRegTable) <- c("violent_death_sim", "vehicle_robbery", "street_robbery")


# add stars
slRegTable[5,] <- paste0("(",slRegTable[5,],")***")

# Add row names
rows <- c("",
          "",
          "",
          "On target",
          "",
          "Observations",
          "Number of aisp",
          "Y mean",
          "R2 adjusted")

rownames(slRegTable) <- (1:dim(slRegTable)[1])

slRegTable <- slRegTable %>%
  as.data.frame(stringsAsFactors = F)

slRegTable <- 
  cbind(rows,
        slRegTable) 

slRegTable_hux <- huxtable(slRegTable)


#------------------------------------------------------------------------------#
##### Processing tables ####


#### Placebo
editTables <- function(regTab, 
                       depVarLabel = "Number of occurrences", 
                       colTitles, 
                       nDepVars = 3) {
  
  # Dependent variables labels
  add_vec <- ""
  for (ndp in 1:length(colTitles)){
    add_vec <- c(add_vec, c("", colTitles[ndp], ""))
  }
  
  add_header <- hux(rbind(add_vec))
  
  # Additional lines in the bottom
  add_lines <- 
    hux(
      rbind(
        c("Chief FE" , rep(c("No", "Yes", "Yes"), length(colTitles)))
      ) )
  
  # Add column numbers
  add_colNumbers <-
    hux(
      rbind(
        c("" , paste0("(", 2:length(regTab)-1, ")"))
      ) )
  
  # Make this more stable
  regTab <- rbind(add_header, 
                  regTab[1], 
                  add_colNumbers,
                  regTab[2:4], 
                  add_lines, 
                  regTab[5:6,])
  
  # Edit cell borders
  #bottom_border(regTab)[4, ] <- 0.4
  # Cell merges
  #regTab  <- regTab %>% merge_cells(1:1, 1:ncol(regTab)) 
  
  
  # Formating
  align(regTab) <- "center"
  align(regTab[1:nrow(regTab)-1,1]) <- "left"
  
  font_size(regTab) <- 10
  
  return(regTab)
  
}




tab2_pla_preForm <- editTables(tab2_pla, 
                                colTitles = c("Violent Death", "Vehicle robbery", "Street robbery"))




# Add Y means
Ymeans_pla <- 
  c(p_vd_01_data$violent_death %>% mean,
    p_vd_02_data$violent_death %>% mean,
    p_vd_IV_data$violent_death %>% mean,
    p_vr_01_data$vehicle_robbery %>% mean,
    p_vr_02_data$vehicle_robbery %>% mean,
    p_vr_IV_data$vehicle_robbery %>% mean,
    p_rr_01_data$street_robbery %>% mean,
    p_rr_02_data$street_robbery %>% mean,
    p_rr_IV_data$street_robbery %>% mean
  ) %>% round(2)
Ymeans_pla <- c("Y mean", Ymeans_pla)

nAisp_pla <- c("Number of aisp", rep(39,9))

tab2_pla_formated <- rbind(tab2_pla_preForm[1:7,],
                           nAisp_pla,
                           Ymeans_pla,
                           tab2_pla_preForm[8:9,])

bottom_border(tab2_pla_formated[3]) <- c(0,rep(0.4, dim(tab2_pla_formated)[2]-1))


caption(tab2_pla_formated) <- "Table XX – Effect of expectancy of receiving bonuses on crime rates (Placebo)"


#### Spatial lag table
bottom_border(slRegTable_hux)[1, ] <- 0.4
bottom_border(slRegTable_hux)[2, ] <- c(0,rep(0.4, dim(slRegTable_hux)[2]-1))
bottom_border(slRegTable_hux)[3, ] <- c(0,rep(0.4, dim(slRegTable_hux)[2]-1))
bottom_border(slRegTable_hux)[5, ] <- 0.4
bottom_border(slRegTable_hux)[9, ] <- 0.4


caption(slRegTable_hux) <- "Table XX – Effect of expectancy of receiving bonuses on crime rates (Spatial Auto-corrlation)"

colnames(slRegTable_hux) <- c(1:dim(slRegTable_hux)[2])


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
  #huxtable::quick_xlsx(tab2_pla_formated, file = file.path(OUTPUTS_final, "tab2_placebo_formated.xlsx"))
  huxtable::quick_xlsx(slRegTable_hux, file = file.path(OUTPUTS_final, "spatial_lag_formated.xlsx"))
}

