
# Script that creates all the functions to compute the VIs

# NDVI
calc.ndvi.df <- function(x){
  df <- (x[[1]][['NIR']]-x[[1]][['R']])/(x[[1]][['NIR']]+x[[1]][['R']])
  df <- t(df)
  
  for (o in 2:length(x)){
    ndvi.vec  <- (x[[o]]['NIR']-x[[o]][['R']])/(x[[o]]['NIR']+x[[o]][['R']])
    ndvi.vec <- t(ndvi.vec)
    
    df <- rbind(df, ndvi.vec)
    print(o)
  }
  return(df)
}

# EVI
calc.evi.df <- function(x){
  df <- 2.5*((x[[1]][['NIR']]-x[[1]][['R']]) / (x[[1]][['NIR']] + 2.4 * x[[1]][['R']] + 1))
  df <- t(df)
  
  for (o in 2:length(x)){
    evi.vec  <- 2.5*((x[[o]][['NIR']]-x[[o]][['R']]) / (x[[o]][['NIR']] + (2.4 * x[[o]][['R']]) + 1))
    evi.vec <- t(evi.vec)
    
    df <- rbind(df, evi.vec)
  }
  return(df)
}

# OSAVI
calc.osavi.df <- function(x){
  df <- ((1+0.16)*x[[1]][['NIR']]-x[[1]][['R']]) / (x[[1]][['NIR']]+x[[1]][['R']]+0.16)
  df <- t(df)
  
  for (o in 2:length(x)){
    osavi.vec  <- ((1+0.16)*x[[o]][['NIR']]-x[[o]][['R']]) / (x[[o]][['NIR']]+x[[o]][['R']]+0.16)
    osavi.vec <- t(osavi.vec)
    
    df <- rbind(df, osavi.vec)
  }
  return(df)
}

# CIRE
calc.cire.df <- function(x){
  df <- (x[[1]][['NIR']]/x[[1]][['RE']]) - 1
  df <- t(df)
  
  for (o in 2:length(x)){
    cire.vec  <- (x[[o]][['NIR']]/x[[o]][['RE']]) - 1
    cire.vec <- t(cire.vec)
    
    df <- rbind(df, cire.vec)
  }
  return(df)
}