

# reg = reg.f
# unit = "aisp"
# time = c("year","month")
# #time = "year"
# 
# lat = "latitude"
# lon = "longitude"
# dist_cutoff = 5
# verbose = F
# balanced_pnl = F
# cores = 1
# dist_fn = "Haversine"
# lag_cutoff = 5

ConleySEs <- function(reg,
                      unit, time, lat, lon,
                      kernel = "bartlett", dist_fn = "Haversine",
                      dist_cutoff = 500, lag_cutoff = 5,
                      lat_scale = 111,
                      verbose = FALSE,
                      cores = 1,
                      balanced_pnl = FALSE) {

  Fac2Num <- function(x) {as.numeric(as.character(x))}
  #source("iterate-obs-function.R", local = TRUE)
  if(cores > 1) {invisible(library(parallel))}
  
  if(class(reg) == "felm") {
    Xvars <- rownames(reg$coefficients)
    dt = data.table(reg$cY, reg$cX,
                    fe1 = Fac2Num(reg$fe[[1]]),
                    fe2 = Fac2Num(reg$fe[[2]]),
                    fe3 = Fac2Num(reg$fe[[3]]),
                    coord1 = Fac2Num(reg$clustervar[[1]]),
                    coord2 = Fac2Num(reg$clustervar[[2]]))
    setnames(dt,
             c("fe1", "fe2", "fe3", "coord1", "coord2"),
             c(names(reg$fe)[1:3], names(reg$clustervar))  # GAMBIARRA WARNING
             #c(names(reg$fe)[1:2], names(reg$clustervar))
             )
    dt = dt[, e := as.numeric(reg$residuals)]
    
    
  } else {
    message("Model class not recognized.")
    break
  }
  
  
  n <- nrow(dt)
  k <- length(Xvars)
  
  # GAMBIARRA WARNING: Dado que eu to usando 2 FE de tempo to combinando os 2
  dt$time <- dt$year *100 + dt$month
  
  # Renaming variables:
  # orig_names <- c(unit, time, lat, lon)
  # new_names <- c("unit", "time", "lat", "lon")
  
  orig_names <- c(unit, lat, lon)
  new_names <- c("unit", "lat", "lon")
  
  setnames(dt, orig_names, new_names)

  
  # Empty Matrix:
  XeeX <- matrix(nrow = k, ncol = k, 0)
  
  #================================================================
  # Correct for spatial correlation:
  timeUnique <- unique(dt[, time])
  Ntime <- length(timeUnique)
  setkey(dt, time)
  
  
  # Mover essa merda pra ca porque ta dando um monte de bosta com o escopo dessas
  # porra que esse fdp definiu numa funcao e fica chamando na outra
  iterateObs <- function(sub_index, type, cutoff, ...) {
    
    if(type == "spatial" & balanced_pnl) {
      
      n1 <- nrow(sub_dt)
      if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}
      
      X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE])
      e <- sub_dt[, e]
      
      XeeXhs <- Bal_XeeXhC(d, X, e, n1, k)
      
    } else if(type == "spatial" & !balanced_pnl) {
      
      sub_dt <- dt[time == sub_index]
      n1 <- nrow(sub_dt)
      if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}
      
      X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE])
      e <- sub_dt[, e]
      lat <- sub_dt[, lat]; lon <- sub_dt[, lon]
      
      # If n1 >= 50k obs, then avoiding construction of distance matrix.
      # This requires more operations, but is less memory intensive.
      if(n1 < 5 * 10^4) {
        XeeXhs <- XeeXhC(cbind(lat, lon), cutoff, X, e, n1, k,
                         kernel="bartlett", dist_fn)
      } else {
        XeeXhs <- XeeXhC_Lg(cbind(lat, lon), cutoff, X, e, n1, k,
                            kernel="bartlett", dist_fn)
      }
      
    } else if(type == "serial") {
      sub_dt <- dt[unit == sub_index]
      n1 <- nrow(sub_dt)
      if(n1 > 1000 & verbose){message(paste("Starting on sub index:", sub_index))}
      
      X <- as.matrix(sub_dt[, eval(Xvars), with = FALSE] )
      e <- sub_dt[, e]
      times <- sub_dt[, time]
      
      XeeXhs <- TimeDist(times, cutoff, X, e, n1, k)
    }
    
    return(XeeXhs)
  }
  
  
  # Continua a funcao original
  if(verbose){message("Starting to loop over time periods...")}
  
  if(balanced_pnl){
    sub_dt <- dt[time == timeUnique[1]]
    lat <- sub_dt[, lat]; lon <- sub_dt[, lon]; rm(sub_dt)
    
    if(balanced_pnl & verbose){message("Computing Distance Matrix...")}
    
    d <- DistMat(cbind(lat, lon), cutoff = dist_cutoff, kernel, dist_fn)
    rm(list = c("lat", "lon"))
  }
  
  if(cores == 1) {
    
    # 
    XeeXhs <- lapply(timeUnique, function(t) iterateObs(sub_index = t,
                                                        type = "spatial", cutoff = dist_cutoff))
  } else {
    XeeXhs <- mclapply(timeUnique, function(t) iterateObs(sub_index = t,
                                                          type = "spatial", cutoff = dist_cutoff), mc.cores = cores)
  }
  
  if(balanced_pnl){rm(d)}
  
  # First Reduce:
  XeeX <- Reduce("+",  XeeXhs)
  
  # Generate VCE for only cross-sectional spatial correlation:
  X <- as.matrix(dt[, eval(Xvars), with = FALSE])
  invXX <- solve(t(X) %*% X) * n
  
  V_spatial <- invXX %*% (XeeX / n) %*% invXX / n
  
  V_spatial <- (V_spatial + t(V_spatial)) / 2
  
  if(verbose) {message("Computed Spatial VCOV.")}
  
  #================================================================
  # Correct for serial correlation:
  panelUnique <- unique(dt[, unit])
  Npanel <- length(panelUnique)
  setkey(dt, unit)
  
  if(verbose){message("Starting to loop over units...")}
  
  if(cores == 1) {
    XeeXhs <- lapply(panelUnique, function(t) iterateObs(sub_index = t,
                                                         type = "serial", cutoff = lag_cutoff))
  } else {
    XeeXhs <- mclapply(panelUnique,function(t) iterateObs(sub_index = t,
                                                          type = "serial", cutoff = lag_cutoff), mc.cores = cores)
  }
  
  XeeX_serial <- Reduce("+",  XeeXhs)
  
  XeeX <- XeeX + XeeX_serial
  
  V_spatial_HAC <- invXX %*% (XeeX / n) %*% invXX / n
  V_spatial_HAC <- (V_spatial_HAC + t(V_spatial_HAC)) / 2
  
  return_list <- list(
    "OLS" = reg$vcv,
    "Spatial" = V_spatial,
    "Spatial_HAC" = V_spatial_HAC)
  
  return(return_list)
}
