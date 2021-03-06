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
                     formula_vector_pla1 = Formulas01_pla_str,
                     formula_vector_pla2 = Formulas02_pla_str,
                     data = sr){
  if(model ==1){
    form <- formula_vector1[dep_var]
  } else if (model == 2){
    form <- formula_vector2[dep_var]
  } else if (model == 'placebo_01'){
    form <- formula_vector_pla1[dep_var]
  } else if (model == 'placebo_02'){
    form <- formula_vector_pla2[dep_var]
  } else{
    form <- model[dep_var]
  }
  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}



# DD linear regression
ddRegSim <- function(dep_var,
                     model = 2,
                     formula_vector1 = dd_formulas_m1,
                     formula_vector2 = dd_formulas_m2,
                     formula_vector_pla = p_dd_formulas_m2,
                     data = dd_df){
  if(model ==1){
    form <- formula_vector1[dep_var]
  } else if(model == 2){
    form <- formula_vector2[dep_var]
  } else{
    form <- p_dd_formulas_m2[dep_var]
    
  }
  
  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}

# DD linear regression Placebo
ddRegSim_pla <- function(dep_var,
                         model = 2,
                         formula_vector1 = p_dd_formulas_m2,
                         data = dd_df_pla){
  
  form <- formula_vector1[dep_var]
  
  
  form <- as.formula(form)
  model <- felm(form, data = data, keepCX = T)
  
  # Return regression object
  return(model)
  
}

# Export function
createTable <- function(reg_list, 
                        add_lines_list = NULL,
                        title = "",
                        dep_var_labels = NULL,
                        col_labels = NULL,
                        outPath = NULL,
                        type = 'html',
                        placebo = F){
  
  if (placebo){
    keep = c(  "hit_sem_pla_l",
               "last_month_on_target_plapre",
               "last_month")
  }else{
    keep = c("hit_sem_l",
             "last_month_on_target",
             "last_month")
  }
  
  stargazer(reg_list,
            keep = keep,
            covariate.labels = c("On target",
                                 "On target * last month",
                                 "Last month"),
            dep.var.labels = dep_var_labels,
            title = title,
            dep.var.caption  = "Number  of  occurrences",
            column.labels   = col_labels,
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
                      col_labels = NULL,
                      add_lines_list = NULL,
                      outPath = NULL,
                      type = 'html',
                      ols_data = NULL,
                      dd_data = NULL,
                      placebo = F){
  
  if (placebo){
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime, model = 'placebo_01', data = sr_pl),
           feRegSim(crime, model = 'placebo_02', data = sr_pl),
           ddRegSim_pla(crime))
    }
  } else {
    # Specification block template
    table_list_fun <- function(crime){
      list(feRegSim(crime),
           feRegSim(crime, model =2 ),
           ddRegSim(crime))
    }
    
  }
  
  
  # Dinamically set the number of blocks based on the number of dep vars
  tab_list <- list()
  for (i in crime_vec){
    tab_list <- append(tab_list, table_list_fun(i))
  }
  
  # Add column labels
  n_blocks <- length(crime_vec)
  
  if (is.null(col_labels)){
    col_labels <- rep(c("OLS",	"OLS",	"DD"), n_blocks)
  }
  
  # Add lines at the bottom of the table
  if (is.null(add_lines_list)){
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
                           col_labels = col_labels,
                           outPath = outPath,
                           type = type,
                           placebo = placebo)
}


