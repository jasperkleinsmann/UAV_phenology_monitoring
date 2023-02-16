library(sp)
library(rgdal)
library(sf)
library(terra)
library(raster)

# Script to load all the segmentation data

# Import validation data 
# plots
val.plots <- st_read(dsn='R_data/Shapefiles', layer = 'ValidationPlots')
val.plots <- st_zm(val.plots)
val.plots <- val.plots[order(val.plots$PlotNr),]

# Tree tops
val.tops <- st_read(dsn='R_data/Shapefiles', layer = 'ValidationTtops')
val.tops <- st_zm(val.tops)

# Tree crowns
val.crowns <- rgdal::readOGR(dsn='R_data/Shapefiles', layer = 'ValidationTcrowns') # read with rgdal::readOGR due to sf import issue
val.crowns <- st_as_sf(val.crowns) # convert to sf

# Import all tree top files
# Import STATTIC window
ttops.static <- list(st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9w3h8'),
                     st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9w4h8'),
                     st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9w5h8'),
                     st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9w6h8'))


# Import DUAL window
ttops.statified <- list(st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw05h8'),
                        st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw1h8'),
                        st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw15h8'),
                        st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw2h8'),
                        st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_sm9Hw4Lw25h8'))


# Import variable window
ttops.var <- list(st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V0025+0'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V0.5_(x^2)_0.009+2.51'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V1.5+0.009_x^2'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V2.5-0.18_x+0.0124_x^2'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V3.5-0.18_x+0.0124_x^2'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V0.5_((x^2)_0.025)II'),
                  st_read(dsn='R_data/Output/Shapefiles', layer = 'ttops_V0.5_((x^2)_0.025)III'))


# Import crowns
crowns.veg <- list(st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v4'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v5'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v5_5'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v6'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v6_5'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v7'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v7_5'),
               st_read(dsn='R_data/Output/Shapefiles', layer = 'tcrowns_v8'))




