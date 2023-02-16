library(sp)
library(sf)
library(raster)
library(terra)

# Import 
source('6_CleanClassifiedCrowns.R') # cleaned classified crowns
source('7.1_LoadOrthophotos.R') # reflectance orthophotos


# Extract all reflectance for all tree crowns in all orthophotos
msp.ts <- lapply(orthos, extract, crowns.vec)

# Extract the pixels in the tree crown higher than 80th percentile reflectance in Green band
# Write function to select => 80th percentile and take the average
perc.80th <- function(x){
  
  value.80th <- as.numeric(quantile(x['G'], probs = 0.8, na.rm=T))
  above.80th <- x[x['G'] >= value.80th,]
  
  means.80th <- round(colMeans(above.80th, na.rm=T),5)
  return(means.80th)
}

# apply the function to all pixels in the crown polygons
msp.80th <- list()
for (o in 1:length(orthos)){
  df <- perc.80th(msp.ts[[o]][msp.ts[[o]]['ID']==1,])
  
  for (t in 2:nrow(crowns.vec)){
    mean.msp.tree <- perc.80th(msp.ts[[o]][msp.ts[[o]]['ID']==t,])
    df <- rbind(df, mean.msp.tree)

  }
  df <- data.frame(df, row.names = seq(1:nrow(crowns.vec)))
  msp.80th <- append(msp.80th, list(df))

}

# Save the multi-spectral reflectances after the 80th percentile selection for each tree
for (i in 1:length(msp.80th)){
  write.csv(msp.80th[[i]], paste0('R_data/Output/Tables/MSP extracted/Percentile 80th/', i, '.csv'))
  print(i)
}
