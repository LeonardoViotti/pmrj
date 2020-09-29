#------------------------------------------------------------------------------#

#	 SIM - Utils

#------------------------------------------------------------------------------#

# This code contains functions used across the analysis



#------------------------------------------------------------------------------#
#### Regression utils ####


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
    clusterVars <- NULL
    instrumentVars <- NULL
  }
  
  regdf <- as.data.frame(regdf)
  
  # Regression variables
  regVarsAll <- c(regDepVars(reg), 
                  regIndepVars(reg),
                  feVars,
                  #clusterVars,
                  instrumentVars)
  if(class(reg) == "felm"){
    if(clusterVars != 0){
      regVarsAll <- c(regVarsAll,
                      clusterVars)
    }
  }
  
  
  
  # Make sure all observarions are the same
  completeBol <- complete.cases(regdf[,regVarsAll])
  completeData <- regdf[completeBol,]
  
  return(completeData)
  
}



#### Define formatting functions

# Function to find dep var means of regressions
Ymean <- function(x){
  mean(regData(x, sr)[,regDepVars(x)])
}

# Function to create the row for regression tables
Ymean_row <- function(list){
  c("Y mean", sapply(list, Ymean) %>% round(2))
}




#------------------------------------------------------------------------------#
#### Regression pipeline ####


# Linear regression function
feRegSim <- function(dep_var, 
                     model = 1,
                     formula_vector1 = Formulas01_str, 
                     formula_vector2 = Formulas02_str, 
                     data = sr){
  if(model ==1){
    form <- formula_vector1[dep_var]
  } else{
    form <- formula_vector2[dep_var]
    
  }
  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}


# Export function
createTable <- function(reg_list, 
                        add_lines_list,
                        title = NULL,
                        dep_var_labels,
                        outPath = NULL,
                        type = 'html'){
  stargazer(reg_list,
            keep = c("hit_sem_l",
                     "last_month_on_target",
                     "last_month"),
            covariate.labels = c("On target",
                                 "On target * last month",
                                 "Last month"),
            dep.var.labels = dep_var_labels,
            title = title,
            dep.var.caption  = "Number  of  occurrences",
            column.labels   = col_labels_9,
            add.lines = add_lines_list,
            digits = 3,
            omit.stat = c("rsq","ser", "f"),
            out = outPath,
            type = type
  )
}


# Regression and table formatting pipeline
table_fun <- function(crime_vec,
                      out = NULL,
                      title = "",
                      dep_var_labels = NULL,
                      add_lines_list = NULL,
                      outPath = NULL,
                      type = 'html'){
  
  # Specification block template
  table_list_fun <- function(crime){
    list(feRegSim(crime),
         feRegSim(crime, model =2 ),
         ddRegSim(crime))
  }
  
  
  # Dinamically set the number of blocks based on the number of dep vars
  tab_list <- list()
  for (i in crime_vec){
    tab_list <- append(tab_list, table_list_fun(i))
  }
  
  # Add lines function
  if (is.null(add_lines_list)){
    n_blocks <- length(crime_vec)
    
    add_lines_list <- 
      list(c("Chief FE", rep(c( "No", "Yes", "Yes"), n_blocks)),
           c("Month FE", rep(c("Yes", "Yes", "No"), n_blocks)),
           Ymean_row(tab_list),
           c("Number of aisp", rep("39", 3*n_blocks))
      )
  }
  
  
  # Create final table
  tab_list %>% createTable(add_lines_list = add_lines_list,
                           title = title,
                           dep_var_labels = dep_var_labels,
                           outPath = outPath,
                           type = type)
}