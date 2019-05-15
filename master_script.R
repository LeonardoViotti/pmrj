#------------------------------------------------------------------------------#

#	 SIM- Master script

#------------------------------------------------------------------------------#

# Leonardo WBG laptop
if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){

  DROPBOX <- file.path("C:/Users/wb519128/Dropbox/Work/Insper/PMRJ")
  GITHUB  <- file.path("C:/Users/wb519128/Documents/GitHub/pmrj")

  
}

# Leonardo personal laptop

#------------------------------------------------------------------------------#
#### Section switches ####

RUN_placebo_targets = T


#------------------------------------------------------------------------------#
#### Create placebo targets ####

if(RUN_placebo_targets){
  source(file.path(GITHUB, "placebo_targets.R"))
  
}

#------------------------------------------------------------------------------#
#### GIS analysis ####

#------------------------------------------------------------------------------#
#### Rgression analysis ####