
library(terra)

# Script to load the orthophotos in the "Reflectance ortho's" folder


# The the file names
file.names <- list.files(
  path = "datasets\\output\\Reflectance ortho's", 
  pattern = ".tif")

# Create list with file paths
file.paths <- list()
paths <- paste0("datasets\\output\\Reflectance ortho's\\", 
                file.names)
for (i in 1:length(paths)){
  file.paths <- append(file.paths, paths[i])
}

# Import the msp raster
orthos <- lapply(file.paths, rast)

# Remove not needed files
rm(file.paths)
rm(i)
rm(paths)
rm(file.names)
