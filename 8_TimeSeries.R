library(sp)
library(sf)
library(raster)
library(terra)
library(ggplot2)
library(RStoolbox)
library(reshape2)
library(greenbrown)
library(phenex)
library(cowplot)
library(ggpubr)
library(dplyr)

# Load the needed datasets
source('6_CleanClassifiedCrowns.R')
source('7.1_LoadOrthophotos.R')
source ('8.1_LoadReflectanceValues.R')
source('8.2_Calc_VIs.R')

# DOY values of flights
doy <- c(110, 123, 130, 138, 145, 151, 158, 172, 179, 207, 228, 277, 309, 347)


##################### TREE LEVEL ANALYSIS ####################
## Create tree-specific VI time series dataframe
# NDVI
ndvi.df <- calc.ndvi.df(msp.80th)
colnames(ndvi.df) <- seq(1:nrow(crowns))
ndvi.df <- cbind(data.frame(DOY=doy), ndvi.df)
rownames(ndvi.df) <- seq(1:length(msp.80th))
# Reshape the dataframe for plotting
ndvi.plt <- melt(ndvi.df, id.vars='DOY', variable.name ='Tree')
ndvi.plt$Specie <- rep(crowns$Specie, each=length(msp.80th))

# EVI
evi.df <- calc.evi.df(msp.80th)
colnames(evi.df) <- seq(1:nrow(crowns))
evi.df <- cbind(data.frame(DOY=doy), evi.df)
rownames(evi.df) <- seq(1:length(msp.80th))
# Reshape the dataframe for plotting
evi.plt <- melt(evi.df, id.vars='DOY', variable.name ='Tree')
evi.plt$Specie <- rep(crowns$Specie, each=length(msp.80th))

# OSAVI
osavi.df <- calc.osavi.df(msp.80th)
colnames(osavi.df) <- seq(1:nrow(crowns))
osavi.df <- cbind(data.frame(DOY=doy), osavi.df)
rownames(osavi.df) <- seq(1:length(msp.80th))
# Reshape the dataframe for plotting
osavi.plt <- melt(osavi.df, id.vars='DOY', variable.name ='Tree')
osavi.plt$Specie <- rep(crowns$Specie, each=length(msp.80th))

# CIRE
cire.df <- calc.cire.df(msp.80th)
colnames(cire.df) <- seq(1:nrow(crowns))
cire.df <- cbind(data.frame(DOY=doy), cire.df)
rownames(cire.df) <- seq(1:length(msp.80th))
# Reshape the dataframe for plotting
cire.plt <- melt(cire.df, id.vars='DOY', variable.name ='Tree')
cire.plt$Specie <- rep(crowns$Specie, each=length(msp.80th))


## Determine number of double logistics iteration 
# Test the RMSE under various double logistic regression iterations 
dlog.nit <- c(10,20,30,40,50,75,100,150,200,250,300,350,400) # iteration vector
set.seed(1)
rnd.trees <- sample.int(nrow(crowns), 10) # select 10 random trees

# Create dataframe with the double logistic estimates for each of the 10 trees and under various iterations
dlog.rmse.nit <- data.frame(N_iterations=dlog.nit)
for (t in rnd.trees){
  dlog.col <- data.frame()
  
  for (i in dlog.nit){
    dlog <- FitDoubleLogBeck(ndvi.df[,t], doy, tout=doy, weighting=T, ninit=i)
    rmse.tree <- mean(sqrt((dlog$predicted - ndvi.df[,t])^2), na.rm=T)
    
    dlog.col <- rbind(dlog.col, RMSE.tree=rmse.tree)
    
    print(i)
  }  
  dlog.rmse.nit <- cbind(dlog.rmse.nit, dlog.col)
}
colnames(dlog.rmse.nit) <- c('N_iterations', as.character(rnd.trees))
dlog.rmse.nit$MeanRMSE <- rowMeans(dlog.rmse.nit[,2:11])

# Plot average RMSE under various iteration levels
dl_iteration_plot


## Compute double logistic  model per tree
# Create empty dataframe
pheno <- data.frame(matrix(ncol = 20, nrow = nrow(crowns)))
colnames(pheno) <- c('NDVI.SOS','NDVI.SOS.SE','NDVI.EOS','NDVI.EOS.SE', 'N.RMSE', 'EVI.SOS','EVI.SOS.SE', 'EVI.EOS','EVI.EOS.SE', 
                     'E.RMSE', 'MCOSAVI.SOS','MCOSAVI.SOS.SE', 'MCOSAVI.EOS','MCOSAVI.EOS.SE', 'M.RMSE', 'CIRE.SOS','CIRE.SOS.SE',
                     'CIRE.EOS','CIRE.EOS.SE', 'C.RMSE')
pheno <- cbind(pheno, Specie=crowns$Specie, SpecieName=crowns$SpecieName)

# Compute double logistic per tree and per VI and put results in data
# NDVI
VI.df <- ndvi.df
for (i in 2:ncol(VI.df)){
  dlog.tree <- FitDoubleLogBeck(VI.df[,i], doy, tout=doy, weighting=T, ninit=300, hessian=T)
  
  dif.vec <- as.numeric(dlog.tree$predicted - VI.df[,i])
  rmse <- sqrt((sum(dif.vec, na.rm=T))^2)
  
  pheno[(i-1),1] <- dlog.tree$params[3] # SOS for tree
  pheno[(i-1),3] <- dlog.tree$params[5] # EOS for tree
  
  pheno[(i-1),2] <- dlog.tree$stdError[3] # SOS standard error
  pheno[(i-1),4] <- dlog.tree$stdError[5] # EOS standard error
  
  pheno[(i-1),5] <- rmse # add RMSE in the table

}
# EVI
VI.df <- evi.df
for (i in 2:ncol(VI.df)){
  dlog.tree <- FitDoubleLogBeck(VI.df[,i], doy, tout=doy, weighting=T, ninit=300, hessian=T)
  
  dif.vec <- as.numeric(dlog.tree$predicted - VI.df[,i])
  rmse <- sqrt((sum(dif.vec, na.rm=T))^2)
  
  pheno[(i-1),6] <- dlog.tree$params[3] # SOS for tree
  pheno[(i-1),8] <- dlog.tree$params[5] # EOS for tree
  
  pheno[(i-1),7] <- dlog.tree$stdError[3] # SOS standard error
  pheno[(i-1),9] <- dlog.tree$stdError[5] # EOS standard error
  
  pheno[(i-1),10] <- rmse # add RMSE in the table
  
}
# OSAVI
VI.df <- osavi.df
for (i in 2:ncol(VI.df)){
  dlog.tree <- FitDoubleLogBeck(VI.df[,i], doy, tout=doy, weighting=T, ninit=300, hessian=T)
  
  dif.vec <- as.numeric(dlog.tree$predicted - VI.df[,i])
  rmse <- sqrt((sum(dif.vec, na.rm=T))^2)
  
  pheno[(i-1),11] <- dlog.tree$params[3] # SOS for tree
  pheno[(i-1),13] <- dlog.tree$params[5] # EOS for tree
  
  pheno[(i-1),12] <- dlog.tree$stdError[3] # SOS standard error
  pheno[(i-1),14] <- dlog.tree$stdError[5] # EOS standard error
  
  pheno[(i-1),15] <- rmse # add RMSE in the table
  
}
# CIRE
VI.df <- cire.df
for (i in 2:ncol(VI.df)){
  dlog.tree <- FitDoubleLogBeck(VI.df[,i], doy, tout=doy, weighting=T, ninit=300, hessian=T)
  
  dif.vec <- as.numeric(dlog.tree$predicted - VI.df[,i])
  rmse <- sqrt((sum(dif.vec, na.rm=T))^2)
  
  pheno[(i-1),16] <- dlog.tree$params[3] # SOS for tree
  pheno[(i-1),18] <- dlog.tree$params[5] # EOS for tree
  
  pheno[(i-1),17] <- dlog.tree$stdError[3] # SOS standard error
  pheno[(i-1),19] <- dlog.tree$stdError[5] # EOS standard error
  
  pheno[(i-1),20] <- rmse # add RMSE in the table
  
}
# Add tree index to pheno dataframe
pheno <- cbind(pheno, Index=seq(seq(1:nrow(crowns))))

# Write results
write.csv(pheno, 'R_data/Output/Tables/Phenology.csv', row.names=F)
# OR - adjust for visualisation
pheno <- read.csv('R_data/Output/Tables/Phenology.csv')

# Select only deciduous trees for plotting
pheno <- pheno %>% 
  filter(SpecieName %in% c('Silver Birch', 'Common Oak', 'American Oak', 'Common Beech'))

# Plot the individual tree SOS and EOS distribution per VI
source('10_Visualisation.R')
tree_sos_eos_plot

pdf('Figures/boxplot_all.pdf', width = 10, height = 7)
tree_sos_eos_plot
dev.off()
##################### END TREE LEVEL ANALYSIS ####################



##################### SPECIES LEVEL ANALYSIS ####################
# Create variables connecting species code and species name
tree.nr <- c(1,2,3,4,5,6,9)
tree.name <- c('Scots Pine', 'Douglas Fir', 'Silver Birch', 'American Oak', 'Common Oak', 'Hemlock', 'Common Beech')

# Calculate the double logistic model per species and per VI
# NDVI
VI <- ndvi.plt
sum.ts.ndvi <- list()
for (SpecieNr in tree.nr){
  
  sum.ts <- data.frame()
  for (i in doy){
    vi.mean <- mean(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    vi.sd <- sd(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    
    row <- data.frame(SD=vi.sd, Mean=vi.mean)
    sum.ts <- rbind(sum.ts, row)
  }
  sum.ts$ymin <- sum.ts$Mean - sum.ts$SD
  sum.ts$ymax <- sum.ts$Mean + sum.ts$SD
  sum.ts <- cbind(data.frame(DOY=doy), sum.ts)
  
  # Compute the DL fit per specie
  d.log <- FitDoubleLogBeck(sum.ts$Mean, doy, tout=doy, weighting=T, ninit=300, hessian=F, plot=T)
  pred <- data.frame(Predicted = d.log$predicted)

  sum.ts <- cbind(sum.ts, pred)
  sum.ts$RtSqrtError <- sqrt((sum.ts$Mean-sum.ts$Predicted)^2)

  sum.ts.ndvi <- append(sum.ts.ndvi, list(sum.ts))
}
# EVI
VI <- evi.plt
sum.ts.evi <- list()
for (SpecieNr in tree.nr){
  
  sum.ts <- data.frame()
  for (i in doy){
    vi.mean <- mean(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    vi.sd <- sd(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    
    row <- data.frame(SD=vi.sd, Mean=vi.mean)
    sum.ts <- rbind(sum.ts, row)
  }
  sum.ts$ymin <- sum.ts$Mean - sum.ts$SD
  sum.ts$ymax <- sum.ts$Mean + sum.ts$SD
  sum.ts <- cbind(data.frame(DOY=doy), sum.ts)
  
  # Compute the DL fit per specie
  d.log <- FitDoubleLogBeck(sum.ts$Mean, doy, tout=doy, weighting=T, ninit=300, hessian=F, plot=T)
  pred <- data.frame(Predicted = d.log$predicted)
  
  sum.ts <- cbind(sum.ts, pred)
  sum.ts$RtSqrtError <- sqrt((sum.ts$Mean-sum.ts$Predicted)^2)
  
  sum.ts.evi <- append(sum.ts.evi, list(sum.ts))
}
# OSAVI
VI <- osavi.plt
sum.ts.osavi <- list()
for (SpecieNr in tree.nr){
  
  sum.ts <- data.frame()
  for (i in doy){
    vi.mean <- mean(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    vi.sd <- sd(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    
    row <- data.frame(SD=vi.sd, Mean=vi.mean)
    sum.ts <- rbind(sum.ts, row)
  }
  sum.ts$ymin <- sum.ts$Mean - sum.ts$SD
  sum.ts$ymax <- sum.ts$Mean + sum.ts$SD
  sum.ts <- cbind(data.frame(DOY=doy), sum.ts)
  
  # Compute the DL fit per specie
  d.log <- FitDoubleLogBeck(sum.ts$Mean, doy, tout=doy, weighting=T, ninit=300, hessian=F, plot=T)
  pred <- data.frame(Predicted = d.log$predicted)
  
  sum.ts <- cbind(sum.ts, pred)
  sum.ts$RtSqrtError <- sqrt((sum.ts$Mean-sum.ts$Predicted)^2)
  
  sum.ts.osavi <- append(sum.ts.osavi, list(sum.ts))
}
# CIRE
VI <- cire.plt
sum.ts.cire <- list()
for (SpecieNr in tree.nr){
  
  sum.ts <- data.frame()
  for (i in doy){
    vi.mean <- mean(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    vi.sd <- sd(VI$value[VI$DOY==i & VI$Specie==SpecieNr], na.rm=T)
    
    row <- data.frame(SD=vi.sd, Mean=vi.mean)
    sum.ts <- rbind(sum.ts, row)
  }
  sum.ts$ymin <- sum.ts$Mean - sum.ts$SD
  sum.ts$ymax <- sum.ts$Mean + sum.ts$SD
  sum.ts <- cbind(data.frame(DOY=doy), sum.ts)
  
  # Compute the DL fit per specie
  d.log <- FitDoubleLogBeck(sum.ts$Mean, doy, tout=doy, weighting=T, ninit=300, hessian=F, plot=T)
  pred <- data.frame(Predicted = d.log$predicted)
  
  sum.ts <- cbind(sum.ts, pred)
  sum.ts$RtSqrtError <- sqrt((sum.ts$Mean-sum.ts$Predicted)^2)
  
  sum.ts.cire <- append(sum.ts.cire, list(sum.ts))
}

# Number of trees per species (for plotting purposes)
obs <- data.frame(SpecieName=tree.name, SpecieNr=tree.nr)
obs$Observations <- NA
for (i in 1:length(tree.nr)){
  n.species <- nrow(crowns[crowns$Specie==tree.nr[i],])
  obs$Observations[i] <- n.species
}

# Plot on species level
source('10_Visualisation.R')
per_species_vi_plot

pdf('Figures/VI_curves.pdf', width = 13, height = 13)
per_species_vi_plot
dev.off()
##################### END SPECIES LEVEL ANALYSIS ####################

