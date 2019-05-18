#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

rm(list = ls())


library(tidyverse)
library(readstata13)
library(lfe)
library(data.table)
library(shomR) # ConleySEs but not on CRAN
library(spdep) # lagsarlm 
library(Rcpp)


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


CPP_FUNCTIONS <- file.path(dirname(GITHUB), "shomR/cpp")


#------------------------------------------------------------------------------#
#### Function definition ####

# Run cpp functions from shomR package that are not working from package install
# only. https://github.com/shommazumder/shomR

sourceCpp(file.path(CPP_FUNCTIONS, "cpp-functions.cpp"))

#------------------------------------------------------------------------------#
#### Section switches ####

RUN_placebo_targets = F


#------------------------------------------------------------------------------#
#### Create placebo targets ####

if(RUN_placebo_targets){
  source(file.path(GITHUB, "placebo_targets.R"))
  
}

#------------------------------------------------------------------------------#
#### GIS analysis ####

#------------------------------------------------------------------------------#
#### Rgression analysis ####