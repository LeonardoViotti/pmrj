#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#
  
rm(list = ls())

#------------------------------------------------------------------------------#
#### Section switches ####


# Run differnt sections of analysis

RUN_var_construction = F

# Main analysis
RUN_main_analysis = T
RUN_desc_analysis = F

# Robustness checks
RUN_placebo_analysis = T
RUN_poisson_analysis = F
RUN_spatial_analysis = F

# Settings switches

EXPORT_data = F
EXPORT_plots = F
EXPORT_tables = T

#------------------------------------------------------------------------------#
#### Packages ####

library(tidyverse)
library(magrittr)
library(readstata13)
library(lfe)
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

library(data.table)

#------------------------------------------------------------------------------#
#### Projections ####

RjProj_aze <- CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
RjProj_unp <- CRS("+init=epsg:4326") 


#------------------------------------------------------------------------------#
#### File paths ####


# Leonardo WBG laptop
if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
  DROPBOX <- file.path("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")
  GITHUB  <- file.path("C:/Users/wb519128/GitHub/pmrj")
}


# Leonardo personal laptop
if (Sys.info()["user"] == "leonardo"){
  DROPBOX <- file.path("/home/leonardo/Dropbox/Work/Insper/PMRJ")
  GITHUB  <- file.path("/home/leonardo/GitHub/pmrj")
  
}


CONLEYse_FUNs <- file.path(GITHUB, "ConleySE")


DATA <- file.path(DROPBOX, "data")
OUTPUTS <- file.path(DROPBOX, "Results")
# OUTPUTS_final <- file.path(OUTPUTS, "Final")
OUTPUTS_final <- OUTPUTS


GIS <- file.path(DROPBOX, "GIS")


#------------------------------------------------------------------------------#
#### Function definition ####

source(file.path(GITHUB, "utils.R"))


#------------------------------------------------------------------------------#
#### Load Data ####


# Load raw data to construct placebo targets
raw_data <- read.dta13(file.path(DATA,"data_SIM_2019-07.dta"))

# Load final data created
if(file.exists(file.path(DATA, "data_SIM_2019_constructed.csv"))){
  org_data <- fread(file = file.path(DATA, "data_SIM_2019_constructed.csv"),
                      encoding = "UTF-8")
  final_data <- fread(file = file.path(DATA, "data_SIM_2019_constructed_extra.csv"),
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
             "police_killing",
             "vehicle_theft",
             "street_theft",
             "dbody_found",
             "other_robberies",
             "cargo_robbery",
             "burglary",
             "store_robbery",
             "arrest",
             "arrest2",
             "drug_seizure",
             "violent_death_fla",
             "assaut_death",
             "police_killing_tot",
             "fraud",
             "dviolent_death_fla",
             "dassaut_death",
             "dpolice_killing_tot",
             "dfraud")

names(depVars) <- depVars


# indepVars <- c(
#   "on_target",
#   # "policemen_aisp",
#   # "policemen_upp",
#   "n_precinct",
#   "max_prize",
#   "population" )


indepVars <- c(
  # "hit_month_l",
  "hit_sem_l",
  # "policemen_aisp",
  # "policemen_upp",
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

#-----------------------------#
# Diff in Diff (a.k.a Virada) #

indep_vars_dd <- c(
  "hit_sem_l",
  "last_month_on_target",
  "last_month",
  # "policemen_aisp",
  # "policemen_upp",
  "n_precinct",
  "max_prize",
  "population" )

# Since there are only 4 months in this analysis I'm removing month
# fixed effects to avoid collinearity
FE_vars_dd <- c("aisp",
                "year", 
                # "month",
                "id_cmt")

# Set cluster SE level
cluster_vars_dd= "0"


#------------------------------------------------------------------------------#
#### Sections ####

#------------------------------------------------------------------------------#
#### Create placebo targets ####

if(RUN_var_construction){
  source(file.path(GITHUB, "01-construction.R"))
  
}

#------------------------------------------------------------------------------#
#### Main Analysis ####

if(RUN_main_analysis){
  source(file.path(GITHUB, "02-analysis-main.R"))
}


#------------------------------------------------------------------------------#
#### Descriptive Analysis ####

# This code depends on analysis_main.R to run! So if this option is not selected
# on master it will sourced in analysis_descriptives.R


if(RUN_desc_analysis){
  source(file.path(GITHUB, "02-analysis-descriptives.R"))
}


#------------------------------------------------------------------------------#
#### Robustuness Analysis ####

if(RUN_poisson_analysis){
  source(file.path(GITHUB, "03-robustness-poisson.R"))
}

if(RUN_spatial_analysis){
  source(file.path(GITHUB, "03-robustness-spatial.R"))
}

if(RUN_placebo_analysis){
  source(file.path(GITHUB, "03-robustness-placebo.R"))
}


