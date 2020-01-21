
library(rdrobust)


# Grab variable names from regressions
regIndepVars <- function(x){
        
        # Work with felm and lm
        if(class(x) == "felm"){
                indepV <- rownames(x$coefficients)
        }else if (class(x) == "lm"){
                indepV <- names(x$model)[-1]
        }else{
                warning("Not sure what that regression is")
        }
        
        indepV <- cleanFormulaName(indepV)
        
        return(indepV)
}

regDepVars <- function(x){
        
        # Work with felm and lm
        if(class(x) == "felm"){
                depV <- colnames(x$coefficients)
        }else if (class(x) == "lm"){
                depV <- names(x$model)[1]
        }else{
                warning("Not sure what that regression is")
        }
        
        # Removes log() or simmilar
        depV <- cleanFormulaName(depV)
        
        return(depV)
        
}


# Get variable mean from the same data as regression

regData <- function(reg, regdf){
        if(class(reg) == "felm"){ # get FEs in case it is an felm model
                feVars <- names(reg$fe)
                #clusterVars <- names(reg$clustervar)
                
                if(!is.null(reg$stage1)){
                        instrumentVars <- reg$stage1$instruments
                } else{
                        instrumentVars <- NULL
                }
                
        } else{ # Already in regIndepVars()
                feVars <- NULL
                #clusterVars <- NULL
                instrumentVars <- NULL
        }
        
        # Regression variables
        regVarsAll <- c(regDepVars(reg), 
                        regIndepVars(reg))
                        #feVars,
                        # clusterVars,
                        #instrumentVars)
        
        # Make sure all observarions are the same
        completeBol <- complete.cases(regdf[,regVarsAll, with=FALSE])
        completeData <- regdf[completeBol,]
        
        return(completeData)
        
}



#------------------------------------------------------------------------------#
# 
# # These are all defined in MASTER.R, only use to explicitly overwrite master.
# OVERWRITE_MASTER_SWITCHES = F
# 
# if(OVERWRITE_MASTER_SWITCHES){
#         EXPORT_data = F
#         EXPORT_plots = F
#         EXPORT_tables = F
# }
# 
# #------------------------------------------------------------------------------#
#### Load data ####

# Loading data into a new object to be processed
sr <- final_data

# Keep same sample for all models, i.e from 2010 onwards because of IV
sr <- sr[sem_year > 100,]


# 100thousand rates

sr$vd_ppop <- (sr$violent_death_sim/sr$population)*100000
sr$vr_ppop <- (sr$vehicle_robbery/sr$population)*100000
sr$sr_ppop <- (sr$street_robbery/sr$population)*100000





# Residual RD plots
fit.vd    <- lm(vd_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)
fit.vd2   <- lm(vd_ppop ~ policemen_aisp + policemen_upp + n_precinct + max_prize + as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)

fit.vr    <- lm(vr_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)
fit.vr2   <- lm(vr_ppop ~ policemen_aisp + policemen_upp + n_precinct + max_prize + as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)

fit.sr    <- lm(sr_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)
fit.sr2   <- lm(sr_ppop ~ policemen_aisp + policemen_upp + n_precinct + max_prize + as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)

# sr$res_vd    <- fit.vd$resid
# sr$res_vr    <- fit.vr$resid
# sr$res_sr    <- fit.sr$resid

# Residuals with main specification
reg_vd_data <- regData(fit.vd2, sr)
reg_vd_data$res_vd <- fit.vd2$residuals

reg_vr_data <- regData(fit.vr2, sr)
reg_vr_data$res_vr <- fit.vr2$residuals

reg_sr_data <- regData(fit.sr2, sr)
reg_sr_data$res_sr <- fit.sr2$residuals



#### Simplest RD plots ####

# Residuo da rv_real.tx vs lv_dist

png(filename = file.path(OUTPUTS, "RD/RD_vd.png"))
(rdplot(sr$vd_ppop, 
        sr$dist_target_vd, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2), 
        y.label = "Violent death (per 100.000 pop.)", 
        x.label = "Distance to violent death target", 
        # binselect = "qsmvpr",
        p = 4
))
dev.off()

png(filename = file.path(OUTPUTS, "RD/RD_vr.png"))
(rdplot(sr$vr_ppop, 
        sr$dist_target_vr, 
        # y.lim = c(-8, 13), 
         x.lim = c(-1,2.5), 
        y.label = "Vehicle robbery (per 100.000 pop.)", 
        x.label = "Distance to vehicle robbery target",
        binselect = "qsmvpr",
        p = 4
))
dev.off()


png(filename = file.path(OUTPUTS, "RD/RD_sr.png"))
(rdplot(sr$sr_ppop, 
        sr$dist_target_sr, 
        # y.lim = c(-8, 13), 
        #x.lim = c(-1,3), 
        y.label = "Street robbery (per 100.000 pop.)", 
        x.label = "Distance to street robbery target",
        binselect = "qsmvpr",
        p = 4
))
dev.off()

#### PLOTS WITH RESIDUAS MAIN SPEC. ####

png(filename = file.path(OUTPUTS, "RD/RD_vd_res.png"))
(rdplot(reg_vd_data$res_vd, 
        reg_vd_data$dist_target_vd, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2), 
        y.label = "Resisdual of violent death (per 100.000 pop.)", 
        x.label = "Distance to violent death target",
        binselect = "qsmvpr",
        p = 4
))
dev.off()

png(filename = file.path(OUTPUTS, "RD/RD_vr_res.png"))
(rdplot(reg_vr_data$res_vr, 
        reg_vr_data$dist_target_vr, 
        # y.lim = c(-8, 13), 
        x.lim = c(-1,3),
        y.label = "Resisdual of vehicle robbery (per 100.000 pop.)", 
        x.label = "Distance to vehicle robbery target",
        binselect = "qsmvpr",
        p = 4
))
dev.off()

png(filename = file.path(OUTPUTS, "RD/RD_sr_res.png"))
(rdplot(reg_sr_data$res_sr, 
        reg_sr_data$dist_target_sr, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2),
        y.label = "Resisdual of street robbery (per 100.000 pop.)", 
        x.label = "Distance to street robbery target",
        binselect = "qsmvpr",
        p = 4
))
dev.off()
