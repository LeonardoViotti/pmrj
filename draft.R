

# Load raw data

other_crimes <- read.csv(
  file.path(DATA, 'BaseDPEvolucaoMensalCisp.csv'),
  sep = ';', 
  header = T)

hom_fla <- read.csv(
  file.path(DATA, 'BaseDPLetalidadeFlagrante.csv'),
  sep = ';', 
  header = T)

#------------------------------------------------------------------------------#
#### Process flagrante #### 

# Reshape to cisp level
names(hom_fla) <- c( "ano_mes",
                     "crime",
                     "cisp",
                     "total",
                     "sem_flagrante",
                     "com_flagrante")
hom_simp <- hom_fla %>% 
  mutate(total = as.character(total),
         sem_flagrante = as.character(sem_flagrante),
         com_flagrante =  as.character(com_flagrante)) %>% 
  pivot_longer(cols = !c(ano_mes, cisp, crime),
               names_to = 'flagr',
               values_to = 'count') %>% 
  # Recode
  mutate(crime = recode(crime, 
    "Homicídio doloso" = "hom_doloso",
    "Morte por intervenção de agente do Estado" = "hom_por_interv_policial",
    "Lesão corporal seguida de morte" = "lesao_corp_mort",
    "Roubo seguido de morte - vítimas" = 'latrocinio') ) %>% 
  # Combine columns to reshape
  mutate(crime = paste(crime, flagr, sep = "_")) %>% 
  dplyr::select(-flagr) %>% 
  
  # Reshape wide
  pivot_wider(id_cols = c(ano_mes, cisp),
              names_from = crime,
              values_from = count) %>% 
  # Split time vars
  separate('ano_mes', into = c('year', 'month'), sep = '-') %>% 
  
  # Keep only crimes we want 
  select('year', 'month', 'cisp', 
         "hom_doloso_com_flagrante", 
         "lesao_corp_mort_total",
         "hom_por_interv_policial_total")  %>% 
  # Rename vars
  rename("violent_death_fla" = "hom_doloso_com_flagrante",
         "assaut_death" = "lesao_corp_mort_total",
         "police_killing" = "hom_por_interv_policial_total")
  

#------------------------------------------------------------------------------#
#### Proces other crimes ####

# Process other crimes
oth_simp <- other_crimes %>% 
  rename('aisp' = 'AISP',
         'cisp' = 'CISP',
         'month' = 'mes',
         'year'= 'ano',
         'fraud'= 'estelionato') %>% 
  select(aisp, 
         cisp, 
         month, 
         year, 
         fraud)

#------------------------------------------------------------------------------#
#### Merge ####

# Merge together since hom_fla is in the CISP level and original data
# doesn't contain that var anymore

final <- oth_simp %>% 
  mutate(year = as.character(year),
         month = as.integer(month)) %>% 
  full_join(hom_simp %>% 
              mutate(month = as.integer(month)), 
            by = c('year', 'month', 'cisp')) %>% 
  # Drop cisps that don't match
  subset(!is.na(aisp))

# Aggregate at aisp level
final_agg <- final %>% group_by(aisp, year, month) %>% 
  summarise(fraud = sum(fraud %>% as.integer()),
            violent_death_fla = sum(violent_death_fla %>% as.integer()),
            assaut_death = sum(assaut_death %>% as.integer()),
            police_killing = sum(police_killing %>% as.integer()) ) %>% 
  ungroup()


#------------------------------------------------------------------------------#
####


a <- unique(paste(oth_simp$cisp, oth_simp$year, oth_simp$month))
b <- unique(paste(hom_simp$cisp, hom_simp$year, hom_simp$month))
setdiff(a,b)
setdiff(b,a)


