

library(terra)
library(RStoolbox)

# The the file names
file.names <- list.files(
  path = "C:\\Users\\jaspe\\Documents\\Documenten\\School\\WUR\\Thesis\\Data\\Orthophotos\\MSP\\Georeferenced ortho's", 
  pattern = ".tif")

# Create list with file paths
file.paths <- list()
paths <- paste0("C:\\Users\\jaspe\\Documents\\Documenten\\School\\WUR\\Thesis\\Data\\Orthophotos\\MSP\\Georeferenced ortho's\\", 
               file.names)
for (i in 1:length(paths)){
  file.paths <- append(file.paths, paths[i])
}

# Import the msp rasters
orthos <- lapply(file.paths, rast)


# Remove not-needed mask band for some rasters
for (i in c(8, 11, 12)){
  orthos[[i]] <- orthos[[i]][[-c(6)]]
}

# Assign correct names to all the bands
for (i in 1:length(orthos)){
  names(orthos[[i]]) <- c('B', 'G', 'R', 'RE', 'NIR', 'Mask')
}

# Make lists with all the masks
masks <- list()
for (i in 1:length(orthos)){
  mask.rast <- orthos[[i]][['Mask']]
  
  masks <- append(masks, list(mask.rast))
}

# Preprocessing rasters
orthos <- mapply(mask, orthos, masks, maskvalues=0, updatevalue=NA) # set mask to 0
orthos.vi <- lapply(orthos, terra::subset, c('B','G','R','RE','NIR')) # get rid of mask band
orthos.vi <- lapply(orthos.vi, rescaleImage, xmin=0, xmax=65535, ymin=0, ymax=1) # scale values to 0-1
orthos.vi <- lapply(orthos.vi, round, 4) # round all pixels the same


# Write prepared rasters to file
for (i in 1:length(orthos.vi)){
  name <- paste0("C:\\Users\\jaspe\\Documents\\Documenten\\School\\WUR\\Thesis\\Data\\Orthophotos\\MSP\\Reflectance ortho's\\", 
                 file.names[i])
  writeRaster(orthos.vi[[i]], name, overwrite=T)
  print(i)
}


# Remove files that are not needed
rm(masks)
rm(mask.rast)
rm(orthos)
rm(orthos.vi)



