
# Script to determine the quality of the tree detection and crown segmentation
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)
library(PBSmapping)

# Load script
source('5a_SegValidation.R') # Import functions to perform validation

################## IMPORT 
# Raster MSP and CHM data
msp <- rast('datasets/output/1004_seg_ortho.tif')
chm <- raster("datasets/output/msp.chm.tif")

# LMF tree tops shapefile
ttops <- st_read(dsn='datasets/outpput/vector_data', layer = 'tree_tops')

# MCWS crown shapefile
crowns <- st_read(dsn='datasets/outpput/vector_data', layer = 'crowns.fill')
crowns <- crowns[,-c(2)]

### Validation data 
# Vlidation plots
val.plots <- st_read(dsn='datasets/data/vector_data', layer = 'ValidationPlots')
val.plots <- st_zm(val.plots)
val.plots <- val.plots[order(val.plots$PlotNr),]

# Validation tree tops
val.tops <- st_read(dsn='datasets/data/vector_data', layer = 'ValidationTtops')
val.tops <- st_zm(val.tops)

# Validation tree crowns
val.crowns <- rgdal::readOGR(dsn='datasets/data/vector_data', layer = 'ValidationTcrowns') # read with rgdal::readOGR due to sf import issue
val.crowns <- st_as_sf(val.crowns) # convert to sf
################### END IMPORT



################## START VALIDATION
### TTOPS: Analyze quality of the tree detection with variable + stratified window
ttop.performance <- combine.stats(ttops, val.crowns, val.plots)


### TREE CROWNS: Perform quality assessment of watershed segmentation
# Select TP tree top in each crown
tp <- tp.matrix(ttops, val.crowns, val.plots)
# Compute segmentation performance of the TP tree crowns
seg.perf <- calc.tp.perf(crowns, tp, val.crowns)
# Aggregate segmentation performance per forest structure type
sum.perf <- tcrown.matrix(seg.perf, val.plots)


# Write segmentation performance df to a csv file
write.csv(sum.perf, "datasets/ooutput/Seg_performance.csv", row.names = T)


