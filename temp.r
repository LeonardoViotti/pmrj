# Regression and table formatting pipeline
# table_fun <- function(
  crime_vec = c('violent_death_sim',
                'vehicle_robbery',
                'street_robbery')
  dep_var_labels = c("Violent deaths", 
                     "Vehicle robbery (Carjacking)",	
                     "Street robbery")
  title = "Table 2 - Effect of expectancy of receiving bonuses on crime rates"
  # outPath = export("tab2.html")
  type = table_type
                      out = NULL
                      # title = "",
                      # dep_var_labels = NULL,
                      col_labels = NULL
                      add_lines_list = NULL
                      outPath = NULL
                      # type = 'html',
                      ols_data = sr
                      dd_data = dd_df
                      placebo = F
  
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
      list(feRegSim(crime, data = ols_data),
           feRegSim(crime, model =2, data = ols_data ),
           ddRegSim(crime, model =1, data = dd_data ),
           ddRegSim(crime, data = dd_data))
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
    col_labels <- rep(c("OLS",	"OLS",	"DD", "DD"), n_blocks)
  }
  
  # Add lines at the bottom of the table
  if (is.null(add_lines_list)){
    add_lines_list <- 
      list(c("Chief FE", rep(c( "No", "Yes",  "No", "Yes"), n_blocks)),
           c("Month FE", rep(c("Yes", "Yes", "No", "No"), n_blocks)),
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

  
  Ymean_row(tab_list)  

  x <- tab_list[3][[1]]

  # Function to find dep var means of regressions
  Ymean <- function(x, df){
    mean(regData(x, df)[,regDepVars(x)])
  }
  # 
  # # Function to create the row for regression tables
  # Ymean_row <- function(list){
  #   c("Y mean", sapply(list, Ymean) %>% round(2))
  # }
  
  Ymean(x, dd_data)
  
  df
  
  c("Y mean",
    Ymean(tab_list[1][[1]], sr) %>% round(2),
    Ymean(tab_list[2][[1]], sr) %>% round(2),
    Ymean(tab_list[3][[1]], dd_data) %>% round(2),
    Ymean(tab_list[4][[1]], dd_data) %>% round(2)    
    )
 

# Calculate Y means for each model taking into consideration that they have different original data.  
ymean_dfs_list <- rep(
  list(sr, sr, dd_data, dd_data) , 
  length(crime_vec))

Ymean_row <- c("Y mean")

for (i in 1:length(tab_list)){
  i_ymean <- Ymean(tab_list[i][[1]], ymean_dfs_list[i][[1]]) %>% round(2)
  i_df <- 
  Ymean_row <- c(Ymean_row, i_ymean)
  
}

class()



    