library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)
library(lidR)
library(fieldRS)
library(imager)
library(ForestTools)
library(rLiDAR)
library(maptools)
library(stars)


# Import data
chm <- rast('datasets/output/msp.chm.tif')
msp <- stack('datasets/output/1004_seg_ortho.tif')

val.strata <- st_read(dsn='datasets/data/vector_data', layer = 'ValidationStratified')
val.strata <- st_zm(val.strata)


## VARIABLE WINDOW SIZE + STRATIFIED
varWin <- function(x){
  winSize <- 0.5*((x^2)*0.025) 
  if(winSize>5){
    return(5)
  }
  else{
    return(winSize)
  }}


# Stratify study area into area with Pine tree and area without
chm.var <- mask(chm.sm, val.strata[val.strata$Strata!='Pine',])
chm.spar <- mask(chm.sm, val.strata[val.strata$Strata=='Pine',])

# Find local maxima (fixed window)
ttops.var <- find_trees(chm.var, lmf((winFun = varWin), 8, shape = 'circular'), uniqueness = 'incremental')
ttops.spar <- find_trees(chm.spar, lmf(4, hmin=8, shape='circular'), uniqueness='incremental')

# Assign correct treeID  to datasets
treeID <- seq((nrow(ttops.var)+1), nrow(ttops.var)+nrow(ttops.spar))
ttops.spar$treeID <- treeID

# Bind the 2 ttops point files together
ttops.comb <- rbind(ttops.var, ttops.spar)
crs(ttops.comb) <- crs(msp)

## Perform watershed segmentation
crowns <- mcws(treetops=ttops, CHM=raster(chm), minHeight=7, format='raster', verbose=T)

# Convert raster to polygons
crowns.pol <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(crowns), as_points = FALSE, merge = TRUE))
crowns.pol <- st_as_sf(crowns.pol)
crowns.pol$area <- as.numeric(st_area(crowns.pol))

# Write the treetops points and crown polygons to shapefile
writeOGR(obj=ttops.comb, dsn='datasets/output/vector_data', layer = 'tree_tops', driver='ESRI Shapefile')
st_write(obj=crowns.pol, dsn='datasets/output/vector_data', layer = 'tree_crowns', driver='ESRI Shapefile')

