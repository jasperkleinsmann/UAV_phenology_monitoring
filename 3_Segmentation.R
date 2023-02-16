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
chm <- rast('R_data/Output/msp.chm.tif')
msp <- stack('R_data/Ortho_gr_1004avg.tif')

seg.strata <- st_read(dsn='R_data/Shapefiles', layer = 'SegmentationStrata')
seg.strata <- st_zm(seg.strata)

val.strata <- st_read(dsn='R_data/Shapefiles', layer = 'ValidationStratified')
val.strata <- st_zm(val.strata)


## STATIC WINDOW
# Find local maxima (fixed window)
ttops <- find_trees(chm, lmf(4, hmin=8, shape='circular'), uniqueness='incremental')


## DUAL WINDOW SIZE
# Stratify study area into high and low vegetated areas
chm.high <- mask(chm, seg.strata[seg.strata$Descriptio=='HighVeg',])
chm.low <- mask(chm, seg.strata[seg.strata$Descriptio=='LowVeg',])

ttops.h <- find_trees(chm.high, lmf(4, hmin=8, shape='circular'), uniqueness='incremental')
ttops.l <- find_trees(chm.low, lmf(1, hmin=8, shape='circular'), uniqueness='incremental')

# Bind the 2 ttops point files together
ttops.hl <- rbind(ttops.h, ttops.l)


## VARIABLE WINDOW SIZE + STRATIFIED
varWin <- function(x){
  winSize <- 0.5*((x^2)*0.025) 
  if(winSize>5){
    return(5)
  }
  else{
    return(winSize)
  }}


chm.var <- mask(chm.sm, val.strata[val.strata$Strata!='Pine',])
chm.spar <- mask(chm.sm, val.strata[val.strata$Strata=='Pine',])

ttops.var <- find_trees(chm.var, lmf((winFun = varWin), 8, shape = 'circular'), uniqueness = 'incremental')
ttops.spar <- find_trees(chm.spar, lmf(4, hmin=8, shape='circular'), uniqueness='incremental')

# Assign correct treeID  to datasets
treeID <- seq((nrow(ttops.var)+1), nrow(ttops.var)+nrow(ttops.spar))
ttops.spar$treeID <- treeID

ttops.comb <- rbind(ttops.var, ttops.spar)
crs(ttops.comb) <- crs(msp)

# Write the treetops variable as shapefile
writeOGR(obj=ttops.comb, dsn='R_data/Output/Shapefiles', layer = 'ttopsSM9_V0.5*((x^2)*0.025)III', driver='ESRI Shapefile')


## Perform watershed segmentation
crowns <- mcws(treetops=ttops, CHM=raster(chm), minHeight=7, format='raster', verbose=T)

# Convert raster to polygons
crowns.pol <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(crowns), as_points = FALSE, merge = TRUE))
crowns.pol <- st_as_sf(crowns.pol)
crowns.pol$area <- as.numeric(st_area(crowns.pol))

# Write the crown polygons to shapefile
st_write(obj=crowns.pol, dsn='R_data/Output/Shapefiles', layer = 'tcrowns_msp_chm_v7', driver='ESRI Shapefile')

