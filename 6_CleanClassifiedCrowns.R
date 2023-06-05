library(sp)
library(sf)

# Script to clean the segmented tree crown polygons after the manual tree classification

#### Import crown data
crowns.raw <- st_read(dsn='datasets/data/vector_data', layer = 'crowns_classified')
crowns.raw$Include <- 0


### Clean the classified crown data
# Only include species code that actually indicate trees (<10) 
crowns.raw <- subset(crowns.raw, Specie<10)
# Code crowns with multiple species and/or too much ground in it & exclude them
crowns.raw$Include[(crowns.raw$MixSpecie==0) & (crowns.raw$Comment <=1)] <- 1
crowns.raw <- crowns.raw[crowns.raw$Include==1,] 

## Merge multiple segmented polygons covering a single tree species into one geometry
crowns <- crowns.raw[crowns.raw$MergeStree==0,] # a single segmented polygon covering a single tree species 
merge.crowns <- crowns.raw[crowns.raw$MergeStree!=0,] # multiple segmented polygons covering a single tree species 

# Merge the geometries of the polygons covering a single tree species & bind to the "crowns" object
for (i in unique(merge.crowns$MergeStree)){
  # select polygons that should be merged
  rows <- crowns.raw[crowns.raw$MergeStree==i,]
  merged.geom <- st_union(rows)
  # if covering multiple trees indicate how many
  if (max(rows$MergeMtree) > 0){
    new.row <- rows[rows$MergeMtree==max(rows$MergeMtree),]
  }
  else {
    new.row <- rows[1,]
  }
  # bind the mereged geometries to the "crowns" object
  new.row$geometry <- merged.geom
  crowns <- rbind(crowns, new.row)
}

# Assign the tree species code to the tree species name
specie.nr <- c(1,2,3,4,5,6,9)
names <- c('Scots Pine', 'Douglas Fir', 'Silver Birch', 'American Oak', 'Common Oak', 'Hemlock', 'Common Beech')

crowns$SpecieName <- NA
for (i in 1:length(names)){
  crowns$SpecieName[crowns$Specie==specie.nr[i]] <- names[i]
}

# Write the cleaned classified crown polygon as shapefile
st_write(crowns, dsn='datasets/output/vector_data', layer='crowns.final', driver='ESRI Shapefile')


rm(specie.nr)
rm(names)
rm(crowns.raw)
rm(merged.geom)
rm(new.row)
rm(rows)
rm(i)
rm(merge.crowns)



