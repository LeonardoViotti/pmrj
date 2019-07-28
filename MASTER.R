#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

rm(list = ls())

#------------------------------------------------------------------------------#
#### Section switches ####

RUN_placebo_targets_construction = T
RUN_main_analysis = F
RUN_rnr_analysis = F

#------------------------------------------------------------------------------#
#### Packages ####

library(tidyverse)
library(readstata13)
library(lfe)
library(data.table)
library(shomR) # ConleySEs but not on CRAN
library(spdep) # lagsarlm 
library(plm) # Panel data
library(splm) # Spatial panel data
library(Rcpp)
library(Hmisc)

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
                  clusterVars,
                  instrumentVars)
  
  # Make sure all observarions are the same
  completeBol <- complete.cases(regdf[,regVarsAll])
  completeData <- regdf[completeBol,]
  
  return(completeData)
  
}


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
#### R&R Analysis ####

if(RUN_rnr_analysis){
  source(file.path(GITHUB, "analysis_rnr.R"))
}