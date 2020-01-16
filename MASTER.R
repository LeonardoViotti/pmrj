#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

rm(list = ls())

#------------------------------------------------------------------------------#
#### Section switches ####


# Run differnt sections of analysis

RUN_placebo_targets_construction = F
RUN_main_analysis = F
RUN_desc_analysis = F
RUN_rnr_analysis = F


# Settings switches

EXPORT_data = F
EXPORT_plots = F
EXPORT_tables = F

#------------------------------------------------------------------------------#
#### Packages ####

library(tidyverse)
library(readstata13)
library(lfe)
library(data.table)
# library(shomR) # ConleySEs but not on CRAN
library(spdep) # lagsarlm 
library(plm) # Panel data
library(splm) # Spatial panel data
library(Rcpp)
library(Hmisc)

library(stargazer)
library(huxtable)
library(flextable)
#library(officer)
#library(ReporteRs)
library(jtools)

library(rgeos)
library(rgdal)
library(sp)
library(maps)
library(geosphere)
library(viridis)
library(tmap)
library(spdep) # Moran's I
library(spatialreg) # Spatial lag model
library(readstata13)
library(tidyverse)
library(broom)

#------------------------------------------------------------------------------#
#### Projections ####

RjProj_aze <- CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
RjProj_unp <- CRS("+init=epsg:4326") 


#------------------------------------------------------------------------------#
#### File paths ####


# Leonardo WBG laptop
if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
  DROPBOX <- file.path("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")
  GITHUB  <- file.path("C:/Users/wb519128/Documents/GitHub/pmrj")
}


# Leonardo personal laptop
if (Sys.info()["user"] == "leonardo"){
  DROPBOX <- file.path("/home/leonardo/Dropbox/Work/Insper/PMRJ")
  GITHUB  <- file.path("/home/leonardo/GitHub/pmrj")
  
}


CONLEYse_FUNs <- file.path(GITHUB, "ConleySE")


DATA <- file.path(DROPBOX, "data")
OUTPUTS <- file.path(DROPBOX, "Results")
OUTPUTS_final <- file.path(OUTPUTS, "Final")

GIS <- file.path(DROPBOX, "GIS")


#------------------------------------------------------------------------------#
#### Function definition ####


#### My own functions

# Clean model names of formula functions, e.g. log() and factor()
cleanFormulaName <- function(x){
  if(any(grepl("(|)", x))){
    x <- gsub(".*\\(", "", x)
    x <- gsub("\\).*", "", x)
    return(x)
  }else{
    return(x)
  }
}


# Grab variable names from regressions
regIndepVars <- function(x){
  
  # Work with felm and lm
  if(class(x) == "felm"){
    indepV <- rownames(x$coefficients)
  }else if (class(x) %in% c("lm", "glm")){
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
  }else if (class(x)  %in% c("lm", "glm")){
    depV <- names(x$model)[1]
  }else if (class(x) == "splm"){
    depV <- names(x$coefficients)[-1]
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
  
  regdf <- as.data.frame(regdf)
  
  # Regression variables
  regVarsAll <- c(regDepVars(reg), 
                  regIndepVars(reg),
                  feVars,
                  #clusterVars,
                  instrumentVars)
  if(clusterVars != 0){
    regVarsAll <- c(regVarsAll,
                    clusterVars)
  }
  
  
  
  # Make sure all observarions are the same
  completeBol <- complete.cases(regdf[,regVarsAll])
  completeData <- regdf[completeBol,]
  
  return(completeData)
  
}


#------------------------------------------------------------------------------#
#### Load Data ####


# Load raw data to construct placebo targets
raw_data <- read.dta13(file.path(DATA,"data_SIM_2019-07.dta"))


# Load final data created by construct_placebo_targets.R
if(file.exists(file.path(DATA, "data_SIM_2019_constructed.csv"))){
  final_data <- fread(file = file.path(DATA, "data_SIM_2019_constructed.csv"),
                      encoding = "UTF-8")
}else{
  print("Please, turn RUN_placebo_targets_construction to TRUE and run again.")
}



#------------------------------------------------------------------------------#
#### Globals ####

####  List regression variables 

depVars <- c("violent_death_sim",
             "vehicle_robbery",
             "street_robbery",
             "homicide",
             "dpolice_killing",
             "vehicle_theft",
             "street_theft",
             "dbody_found",
             "other_robberies",
             "cargo_robbery",
             "burglary",
             "store_robbery")
names(depVars) <- depVars


indepVars <- c("on_target",
               "policemen_aisp",
               "policemen_upp",
               "n_precinct",
               "max_prize",
               "population" )
names(indepVars) <- indepVars

FEVars <- c("aisp",
            "year", 
            "month", 
            "id_cmt")
names(FEVars) <- FEVars


ZVars <- c("lag12_dist_target_vr",
           "lag12_dist_target_sr",
           "lag12_dist_target_vd")
names(ZVars) <- ZVars





#------------------------------------------------------------------------------#
#### Sections ####

#------------------------------------------------------------------------------#
#### Create placebo targets ####

if(RUN_placebo_targets_construction){
  source(file.path(GITHUB, "construction_placebo_targets.R"))
  
}

#------------------------------------------------------------------------------#
#### Main Analysis ####

if(RUN_main_analysis){
  source(file.path(GITHUB, "analysis_main.R"))
}


#------------------------------------------------------------------------------#
#### Descriptive Analysis ####

# This code depends on analysis_main.R to run! So if this option is not selected
# on master it will sourced in analysis_descriptives.R


if(RUN_desc_analysis){
  source(file.path(GITHUB, "analysis_descriptives.R"))
}


#------------------------------------------------------------------------------#
#### R&R Analysis ####

if(RUN_rnr_analysis){
  source(file.path(GITHUB, "analysis_rnr.R"))
}




