
# Script to load the dataframes with all the 80th percentile reflectances per crown object 

file.names <- list.files(
  path = "R_data/Output/Tables/MSP extracted/Percentile 80th")

names <- seq(1:length(file.names))

# Create list with file paths
file.paths <- list()
paths <- paste0("R_data/Output/Tables/MSP extracted/Percentile 80th/", 
                names, ".csv")
for (i in 1:length(paths)){
  file.paths <- append(file.paths, paths[i])
}

msp.80th <- lapply(file.paths, read.csv)

# Remove not needed files
rm(file.names)
rm(i)
rm(names)
rm(paths)
rm(file.paths)

