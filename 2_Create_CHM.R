library(sp)
library(rgdal)
library(sf)
library(raster)
library(terra)
library(ggplot2)
library(dplyr)
library(lidR)

# Import data
dtm <- rast("AHN data/M_39FN2.TIF")

msp.dem <- rast("R_data/DEM_gr_1004.tif")

takeoff <- st_read(dsn='R_data/Shapefiles', layer = 'LandingPnt')
takeoff <- st_zm(takeoff)

# crop the DTM study area
dtm.area <- crop(dtm, msp.dem)

# Fill NA values in dtm
# Create function that only averages NA values
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
} 

# Apply the NA fill to the DTM
dtm.fill <- focal(dtm.area, w=15, fun=fill.na)

# Resample dtm.fill using the msp.dem
dtm.resamp <- resample(dtm.fill, msp.dem, method='cubic')

# Extract NAP values at take off place
takeoff.cor <- c(st_coordinates(takeoff))
takeoff.height <- extract(dtm.resamp, takeoff.cor)[1,]

# control the whole dem raster for the takeoff height
msp.nap <- msp.dem + takeoff.height

# Create stack
dems <- c(msp.nap, dtm.resamp)
names(dems) <- c('msp.dsm', 'dtm.lidar')  
  
# Set NA values in msp.dem to dtm
dems$msp.dsm[is.na(dems$msp.dsm)] <- dtm.resamp

# Create Canopy Height Model
msp.chm <- dems$msp.dsm - dems$dtm.lidar
# set negative chm value to 0
msp.chm[msp.chm<0] <- 0
names(msp.chm) <- 'msp.chm'

# Smoothen the CHM
msp.chm <- CHMsmoothing(raster(msp.chm), "mean", ws=9) # CHMsmoothing only takes raster::rasterlayer as input
msp.chm <- rast(msp.chm) # convert chm back to a terra::spatraster
crs(msp.chm) <- crs(msp.dem)

# Add chm to stack
dems <- c(dems, msp.chm) 

# Save raster stack
names(dems)
x <- writeRaster(dems, paste0('R_data/Output/', names(dems), '.tif'), overwrite=TRUE, filetype="GTiff")

# Import finsihed chm raster
chm <- rast("R_data/Output/msp.chm.tif")


# Visualise the CHM values
c <- as.data.frame(chm)
ggplot(c, aes(x=msp.chm)) + 
  geom_histogram(aes(y=..density..), colour='black', fill='white')+
  geom_density(alpha=.2, fill='red')+
  labs(x='CHM (m)')+
  geom_vline(xintercept = 8, colour = "black", linetype="dotted")


