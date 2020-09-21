

# Regression table functions
table_list_fun <- function(crime){
  list(feRegSim(Formulas01_str[crime]),
       feRegSim(Formulas02_str[crime]),
       ddRegSim(crime))
}

table_fun <- function(crime_vec,
                      out = NULL,
                      col_labels = NULL){
  tab_list <- list()
  for (i in crime_vec){
    tab_list <- append(tab_list,table_list_fun(i))
  }
  
  tab_list %>%
    stargazer(
      keep = c("hit_sem_l",
               "last_month_on_target",
               "last_month"),
      covariate.labels = c("On target",
                           "On target * last month",
                           "Last month"),
      dep.var.labels = col_labels,
      type = 'html',
      omit.stat = c("rsq","ser", "f"),
      out = out)
}



table_fun(c("dviolent_death_fla", "violent_death_fla"),
          col_labels = c("Hom. flagrante (dummy)",
                         "Hom. flagrante"),
          out = file.path(OUTPUTS_final, "DRAFT_homicidio_flagr.html")
)
table_fun(c("fraud", "dassaut_death"),
          col_labels = c("Estelionato",
                         "Lesao corp. morte"),
          out = file.path(OUTPUTS_final, "DRAFT_estelionato_lesao_corp.html"))
table_fun(c("dpolice_killing"),
          col_labels = c("Police killing (dummy)"),
          out = file.path(OUTPUTS_final, "DRAFT_police_kill.html")
)

