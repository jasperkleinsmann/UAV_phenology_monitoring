
# Script to determine the quality of the various segmentations
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)
library(PBSmapping)

# Import
source('5.2_LoadSegData.R') # validation data & tree top and crown polygons under various parameters
source('5.1_SegValidation.R') # import functions to perform validation

# Import data
msp <- rast('R_data/Ortho_gr_1004avg.tif')
chm <- raster("R_data/Output/msp.chm.tif")

# Import individual rasters
ttops_comb <- st_read(dsn='R_data/Output/Shapefiles', layer = 'ttopsSM9_V0.5_((x^2)_0.025)III')
ttops_fixed <- st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9w4h8')
ttops_stra <- st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw1h8')
ttops_flex <- st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V0025+0')

crowns <- st_read(dsn='R_data/Output/Shapefiles', layer = 'crowns.fill')
crowns <- crowns[,-c(2)]


# TTOPS
# Analyze quality of static window
static.performance <- lapply(ttops.static, combine.stats, val.crowns, val.plots) # window=4 Best
# Analyse quality of dual window size
stratified.performance <- lapply(ttops.statified, combine.stats, val.crowns, val.plots) # window 4+1 best
# Analyse quality variable window
variable.performance <- lapply(ttops.var, combine.stats, val.crowns, val.plots) # Var + stratified --> best
# Best performance
ttops <- ttops.var[[7]]
# For single ttops file
ttop.performance <- combine.stats(ttops, val.crowns, val.plots)


# TREE CROWNS
# Perform quality assessment of watershed segmentation
# Select TP tree top in each crown
tp <- tp.matrix(ttops, val.crowns, val.plots)

# Compute tree crown difference for only TPs
tp.seg.perf <- lapply(crowns.veg, calc.tp.perf, tp, val.crowns)

# Create df with performance per forest part
tp.seg.performance <- lapply(tp.seg.perf, tcrown.matrix, val.plots)


# For single crown file
seg.perf <- calc.tp.perf(crowns_filtered, tp, val.crowns)
overall <- calc.overall.perf(crowns_filtered, val.crowns, val.plots, ttops)
sum.perf <- tcrown.matrix(seg.perf, val.plots)

# Write all.seg.performance to a csv file
write.csv(all.seg.performance,"R_data/Output/Tables/All_Segementation_Performance.csv", row.names = T)
write.csv(static.performance[[2]],"R_data/Output/Tables/ttop_static_best.csv", row.names = T)


