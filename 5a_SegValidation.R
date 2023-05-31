
# Script that takes the ground truth tree tops and crowns and compares it with the automatic segmentation output

library(sp)
library(rgdal)
library(sf)
library(terra)


# Function to determine the TP of the tree detection
determineTP <- function(crowns, pred.ttops, val.plots){
  
  pred.ttops$TP <- NA
  
  # Assing 0 when in plot
  in.plot <- st_intersection(pred.ttops, val.plots, sparse=F)
  pred.ttops$TP[in.plot$treeID] <- 0
  
  for (pol in 1:nrow(crowns)){
    
    in.crown <- st_intersection(pred.ttops, crowns[pol,], sparse=F)
    in.crown.count <- nrow(in.crown)
  

    if (in.crown.count==1){
      pred.ttops$TP[in.crown$treeID] <- 1
    }
    
    if (in.crown.count>1){

      maxZ.index <- in.crown$treeID[in.crown$Z==max(in.crown$Z)]
      pred.ttops$TP[maxZ.index] <- 1
    }
  }
  
  return(pred.ttops)
}



# Function to create the overall confusion matrix
TtopValidation <- function(crowns, pred.ttops, val.plots){
  
  # Create empty confusion matrix
  conf.matrix <- data.frame(matrix(data=0, ncol=3, nrow = 3))
  rownames(conf.matrix) <- c('Tree observed', 'No tree observed', 'Total predicted')
  colnames(conf.matrix) <- c('Tree predicted', 'No tree predicted', 'Total observed')
  
  # Create the variables to fill the confusion matrix
  TP <- TN <- FP <- FN <- 0
  total.obs <- nrow(crowns)
  
  pred.in.plot <- st_intersection(pred.ttops, val.plots, sparse=F)
  total.pred <- nrow(pred.in.plot)
  
  # Determine predicted trees outside crown polygons
  in.crowns <- st_intersection(pred.ttops, crowns, sparse=F)
  outside.crowns <- total.pred - nrow(in.crowns)
  
  FP <- FP + outside.crowns
  
  for (pol in 1:nrow(crowns)){
    
    in.crown <- st_intersection(pred.ttops, val.crowns[pol,], sparse=F)
    in.crown.count <- nrow(in.crown)
    
    if (in.crown.count==0){
      # State as FN
      FN <- FN + 1
    }
    
    if (in.crown.count==1){
      # state as TP
      TP <- TP + 1
    }
    
    if (in.crown.count>1){
      # Pick one with highest Z value as TP, other as FP
      TP <- TP + 1
      
      FP <- FP + (nrow(in.crown) - 1)
    }
    
  }
  # Fill the confusion matrix
  conf.matrix[1,1] <- TP
  conf.matrix[1,2] <- FN
  conf.matrix[2,1] <- FP
  conf.matrix[2,2] <- TN
  conf.matrix[1,3] <- total.obs
  conf.matrix[3,1] <- total.pred
  
  # Create confusion matrix of all predicted points
  return(conf.matrix)
}



# Create function to make confusion matrix from observed and predicted number of trees
TtopValidationPlot <- function(crowns, pred.ttops, plots){
  
  conf.matrices <- list()
  
  for (plot in 1:nrow(plots)){
    
    # Create empty confusion matrix
    conf.matrix <- data.frame(matrix(data=0, ncol = 3, nrow = 3))
    rownames(conf.matrix) <- c('Trees observed', 'No trees observed', 'Total predicted')
    colnames(conf.matrix) <- c('Trees predicted', 'No trees predicted', 'Total observed')
    
    # Determine predicted and observed trees outside and inside crowns and plots
    pred.plot <- st_intersection(pred.ttops, plots[plot,], sparse=F)
    pred.crowns <- st_intersection(pred.plot, crowns, sparse=F)
    
    obs.plot <- crowns[crowns$PlotNr==plot,]
    
    total.obs.plot <- nrow(obs.plot)
    total.pred.plot <- nrow(pred.plot)
    
    outside.crowns <- total.pred.plot - nrow(pred.crowns)
    
    # Create the variables to fill the confusion matrix
    TP <- TN <- FP <- FN <- 0
    
    # All predicted trees inside plot but outside crown are per definition FP
    FP <- FP + outside.crowns
    
    # Go trhough each crown polygon in each plot
    for (pol in 1:nrow(obs.plot)){
      
      in.crown <- st_intersection(pred.plot, obs.plot[pol,], sparse=F)
      in.crown.count <- nrow(in.crown)
      
      if (in.crown.count==0){
        # State as FN
        FN <- FN + 1
      }
      
      if (in.crown.count==1){
        # state as TP
        TP <- TP + 1
      }
      
      if (in.crown.count>1){
        # Pick one with highest Z value as TP, other as FP
        TP <- TP + 1
        
        FP <- FP + (nrow(in.crown) - 1)
      }
      
    }
    # Fill the confusion matrix
    conf.matrix[1,1] <- TP
    conf.matrix[1,2] <- FN
    conf.matrix[2,1] <- FP
    conf.matrix[2,2] <- TN
    conf.matrix[1,3] <- total.obs.plot
    conf.matrix[3,1] <- total.pred.plot
    
    # Append plot specific confusion matrix to list
    conf.matrices <- append(conf.matrices, list(conf.matrix))
  }
  
  return(conf.matrices)
  }


# Function to compute classification performance based on confusion matrix
calc.stat <- function(conf.matrix){
  
  stat.df <- data.frame(matrix(data=0, ncol = 3, nrow = 1))
  colnames(stat.df) <- c('Producer accuracy (recall)', 'User accuracy (precision)','F-score')
  
  # Define variables
  TP <- conf.matrix[1,1]
  FN <- conf.matrix[1,2]
  FP <- conf.matrix[2,1]
  TN <- conf.matrix[2,2]
  total.nr.ref <- conf.matrix[1,3]
  total.nr.classified <- conf.matrix[3,1]
  
  # Compute stattistics
  overall.accuracy <- round(TP/total.nr.ref,2)
  prod.acc <- round(TP/total.nr.ref,2)
  user.acc <- round(TP/total.nr.classified,2)
  precision <- round(TP/(TP+FP),2)
  recall <- round(TP/(TP+FN),2)
  f.score <- round(2 * ((precision * recall)/(precision + recall)),2)
  
  # Fill the df with the calculated statistical metrics
  stat.df[1,1] <- prod.acc
  stat.df[1,2] <- user.acc
  stat.df[1,3] <- f.score

  
  return(stat.df)

}


# Combine all statistics together in one plot
combine.stats <- function(pred.ttops, val.crowns, val.plots){
  
  overall.acc <- TtopValidation(val.crowns, pred.ttops, val.plots)
  plot.acc <- TtopValidationPlot(val.crowns, pred.ttops, val.plots)
  
  accuracy.df <- data.frame()
  
  for (m in 1:6){
    accuracy.row <- calc.stat(plot.acc[[m]])
    accuracy.df <- rbind(accuracy.df, accuracy.row)
  }
  
  # Averaged plot statistics
  avg.stat.row <- data.frame(cbind(round(mean(accuracy.df[,1]),2), round(mean(accuracy.df[,2]),2), round(mean(accuracy.df[,3]),2)))
  colnames(avg.stat.row) <- c('Producer accuracy (recall)', 'User accuracy (precision)','F-score')
  accuracy.df <- rbind(accuracy.df, avg.stat.row) 
  
  # Add statistics of entire prediction
  accuracy.df <- rbind(accuracy.df, calc.stat(overall.acc)) 
  
  rowNames <- c('1','2','3','4','5','6','Average', 'Total')
  row.names(accuracy.df) <- rowNames
  
  return(accuracy.df)
}



# Create dataset with only tp
tp.matrix <- function(ttops.pred, val.crowns, val.plots){
  
  ttops <- determineTP(val.crowns, ttops.pred, val.plots)
  
  tp <- st_intersection(ttops, val.plots, sparse=F)
  tp <- subset(tp, TP==1)
  
  return(tp)
  
}


# Function to calculate performance of the tp tree crowns
calc.tp.perf <- function(tcrowns, tp, val.crowns){
  
  tcrowns$area <- st_area(tcrowns)
  
  #tp$OverlapIndex <- 0
  tp$OverSeg <- 0
  tp$UnderSeg <- 0
  tp$IoU <- 0
  
  tcrowns <- st_buffer(tcrowns, 0)
  
  for (i in 1:nrow(tp)){
    
    within.val <- st_within(tp[i,], val.crowns, sparse=T)
    within.val <- within.val[[1]]
    
    within.ws <- st_within(tp[i,], tcrowns, sparse=T)
    within.ws <- within.ws[[1]]
    
    ws.cr.area <- tcrowns[within.ws,]$area
    val.cr.area <- val.crowns[within.val,]$Shape_Area
    
    crowns_buff0 <- st_buffer(tcrowns[within.ws,], 0)
    
    overlap.poly <- st_intersection(val.crowns[within.val,], crowns_buff0)
    overlap.area <- st_area(overlap.poly)
    
    union.cr <- st_union(val.crowns[within.val,], crowns_buff0)
    union.area <- st_area(union.cr)
    
    #OverlapIndex <- (as.numeric(overlap.area)/as.numeric(val.cr.area))*(as.numeric(overlap.area)*as.numeric(ws.cr.area))
    OverSeg <- 1 - (as.numeric(overlap.area) / as.numeric(val.cr.area))
    UnderSeg <- 1 - (as.numeric(overlap.area) / as.numeric(ws.cr.area))
    IoU <- (as.numeric(overlap.area) / as.numeric(union.area))
    
    #tp$OverlapIndex[i] <- OverlapIndex
    tp$OverSeg[i] <- OverSeg
    tp$UnderSeg[i] <- UnderSeg
    tp$IoU[i] <- IoU
  }
  return(tp)
}


# Aggregate the tree segmentation result per forest type
tcrown.matrix <- function(tp, val.plots){
  
  rnames <- c(val.plots$Descriptio)
  rnames <- c(val.plots$Descriptio, 'Average', 'Total')
  
  ws.matrix <- data.frame(PlotNr=rnames)
  #ws.matrix$AvgOverlapIndex <- 0
  ws.matrix$AvgOverSeg <- 0
  ws.matrix$AvgUnderSeg <- 0
  ws.matrix$AvgIoU <- 0
  #ws.matrix$AbsoluteMeanDif_TP <- 0
  
  
  for (i in 1:nrow(val.plots)){
    #avg.overlap <- round(mean(tp$OverlapIndex[tp$PlotNr==i]),2)
    avg.overseg <- round(mean(tp$OverSeg[tp$PlotNr==i]),2)
    avg.underseg <- round(mean(tp$UnderSeg[tp$PlotNr==i]),2)
    avg.IoU <- round(mean(tp$IoU[tp$PlotNr==i]),2)
    #abs.dif <- round(mean(tp$Difference[tp$PlotNr==i]),2)
    
    means <- colMeans(ws.matrix[1:6, 2:4])
    totals <- c(#round(mean(tp$OverlapIndex),2),
                round(mean(tp$OverSeg),2),
                round(mean(tp$UnderSeg),2),
                round(mean(tp$IoU),2))
    
    
    #ws.matrix[i, 2] <- round(avg.overlap,2)
    ws.matrix[i, 2] <- round(avg.overseg,2)
    ws.matrix[i, 3] <- round(avg.underseg,2)
    ws.matrix[i, 4] <- round(avg.IoU,2)
    #ws.matrix[i, 5] <- round(abs.dif,2)
    ws.matrix[7, 2:4] <- round(means,2)
    ws.matrix[8, 2:4] <- round(totals,2)
  }
  
  return(ws.matrix)
  
}
