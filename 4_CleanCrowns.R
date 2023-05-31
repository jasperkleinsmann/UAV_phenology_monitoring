
# Script to clean the tree crowns after the watershed segmentation
library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)
library(smoothr)


# Import data
crowns.raw <- st_read(dsn='datasets/output/vector_data', layer = 'tree_crowns')
ttops <- st_read(dsn='datasets/output/vector_data', layer = 'tree_tops')

msp <- rast('datasets/output/1004_seg_ortho')
chm <- rast("datasets/output/msp.chm.tif")


# 1) crowns without tree top excluded
crowns <- data.frame()
for (c in 1:nrow(crowns.raw)){
  
  ttop <- sum(st_within(ttops, crowns.raw[c,], sparse=F))
  
  if (ttop == 1){
    crowns <- rbind(crowns, crowns.raw[c,])
  }
}

# 2) Fill holes in polygons
crowns.fill <- fill_holes(crowns, 1)
crowns.fill <- subset(crowns.fill, select=-c(area))

# 3) Erode the tree objects
crowns_eroded <- st_buffer(crowns.fill, (-0.4))
crowns_eroded$area <- as.numeric(st_area(crowns_eroded))

# 4) Delete crowns that are smaller than 1 sqM after erosion
crowns_filtered <- subset(crowns_eroded, area>1)

# Add and remove  attributes to crowns file
crowns_filtered$Specie <- NA
crowns_filtered$MergeStree <- NA
crowns_filtered$MergeMtree <- NA
crowns_filtered$MixSpecie <- NA
crowns_filtered$Comment <- NA


# Export the filled crowns (used for the validation process)
st_write(crowns.fill, dsn='datasets/output/vector_data', layer='crowns.fill', driver='ESRI Shapefile')
# Export the filled and eroded crowns (used for further analysis)
st_write(crowns_filtered, dsn='datasets/output/vector_data', layer='crowns_filtered', driver='ESRI Shapefile')

