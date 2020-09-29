

# Original regressions and Consley SEs
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

# Regression table functions
table_list_fun <- function(crime){
  list(feRegSim(crime),
       feRegSim(crime, model =2 ),
       ddRegSim(crime))
}



# Regression and table formatting pipeline
table_fun <- function(crime_vec,
                      out = NULL,
                      title = "",
                      dep_var_labels = NULL,
                      add_lines_list = NULL,
                      outPath = NULL,
                      type = 'html'){
  tab_list <- list()
  for (i in crime_vec){
    tab_list <- append(tab_list, table_list_fun(i))
  }
  
  tab_list %>% createTable(add_lines_list = add_lines_list,
                           title = title,
                           dep_var_labels = dep_var_labels,
                           outPath = outPath,
                           type = type)
}


tab2_addLines <- list(chifeFE_line_9,
                      monthFE_line_9,
                      Ymean_row(tab2_regs),
                      n_aisp_line_9)


table_fun(c('violent_death_sim',
            'vehicle_robbery',
            'street_robbery'),
          type = 'text')



# table_fun(c("dpolice_killing"),
#           col_labels = c("Police killing (dummy)"),
#           out = file.path(OUTPUTS_final, "DRAFT_police_kill.html")
# )
# 


# table_fun(c("dviolent_death_fla", "violent_death_fla"),
#           col_labels = c("Hom. flagrante (dummy)",
#                          "Hom. flagrante"),
#           out = file.path(OUTPUTS_final, "DRAFT_homicidio_flagr.html")
# )
# table_fun(c("fraud", "dassaut_death"),
#           col_labels = c("Estelionato",
#                          "Lesao corp. morte"),
#           out = file.path(OUTPUTS_final, "DRAFT_estelionato_lesao_corp.html"))
# table_fun(c("dpolice_killing"),
#           col_labels = c("Police killing (dummy)"),
#           out = file.path(OUTPUTS_final, "DRAFT_police_kill.html")
# )
# 
