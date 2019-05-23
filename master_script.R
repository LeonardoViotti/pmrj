#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

rm(list = ls())

#------------------------------------------------------------------------------#
#### Section switches ####

RUN_placebo_targets_construction = F

#------------------------------------------------------------------------------#
#### Packages ####

library(tidyverse)
library(readstata13)
library(lfe)
library(data.table)
library(shomR) # ConleySEs but not on CRAN
library(spdep) # lagsarlm 
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
library(spdep)
library(readstata13)
library(tidyverse)
library(broom)

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

OUTPUTS <- file.path(DROPBOX, "Results")


#------------------------------------------------------------------------------#
#### Function definition ####

# Run cpp functions from shomR package that are not working from package install
# only. https://github.com/shommazumder/shomR

sourceCpp(file.path(CONLEYse_FUNs, "cpp-functions.cpp"))

# Run debugged version of ConleySEs function and dependencies
#source(file.path(CONLEYse_FUNs, "iterate-obs-function_draft.R"))
source(file.path(CONLEYse_FUNs, "ConleySE_fun_draft.R"))



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
#### Section switches ####

RUN_placebo_targets = F


#------------------------------------------------------------------------------#
#### Create placebo targets ####

if(RUN_placebo_targets_construction){
  source(file.path(GITHUB, "placebo_targets.R"))
  
}

#------------------------------------------------------------------------------#
#### GIS analysis ####

#------------------------------------------------------------------------------#
#### Rgression analysis ####