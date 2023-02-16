

library(terra)

# The the file names
file.names <- list.files(
  path = "C:\\Users\\jaspe\\Documents\\Documenten\\School\\WUR\\Thesis\\Data\\Orthophotos\\MSP\\Reflectance ortho's", 
  pattern = ".tif")

# Create list with file paths
file.paths <- list()
paths <- paste0("C:\\Users\\jaspe\\Documents\\Documenten\\School\\WUR\\Thesis\\Data\\Orthophotos\\MSP\\Reflectance ortho's\\", 
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
