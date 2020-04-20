


# Neighbourhood definition
nb <- poly2nb(aisp_simp, queen = T)

# Neighbour weights
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)


## Columbus data, as in example(errorsarlm)
data(oldcol)
COL.listw <- nb2listw(COL.nb, style="W")
## transform into 'matrix'
COL.mat <- listw2mat(COL.listw)
## make pooled W, e.g. T=3
COL.mat.p3 <- kronecker(diag(1,3), COL.mat)
## back to 'listw'
COL.listw.p3 <- mat2listw(COL.mat.p3)

#so that a simple function that does the job might be:
  
poolW <- function(w, t) {
  return(mat2listw(kronecker(diag(1,t), listw2mat(w))))
    }

lW <- poolW(lw, 7)


lW$style <- "W" 
attr(lW$weights, "comp")$d<-unlist(lapply(lW$weights, length)) 


#------------------------------------------------------------------------------#

# Test formula
ftest <- violent_death_sim ~ on_target
ftest <- violent_death_sim ~ on_target + policemen_aisp + policemen_upp + n_precinct + max_prize + population


# Spatial lag cross section
foo <- lagsarlm(formula = ftest, 
                data=aisp_simp@data, 
                listw = lw)



nsim <- sim %>% subset(!(aisp %in% c(17, 33)))
# Spatial lag pooled OLS
foo <- lagsarlm(formula = ftest, 
                data=nsim, 
                listw = lW)

# Ols
bar <- lm(formula = ftest, 
          data=aisp_simp@data)


Formulas01_str["violent_death_sim"]
Formulas02_str["violent_death_sim"] 
#FormulasIV_str["violent_death_sim"]



# Cross section
# aisp_simp@data

# Panel
# sr