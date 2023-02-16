library(ggplot2)
library(gridExtra)
library(ggpubr)

# Script to aggregate historic temperature and rainfall and to compare it to 2021 conditions

# Load KNMI data
w <- read.csv('R_data/KNMI_deelen.txt', header=T, sep=',', dec='.')

# Select appropriate columns
w$RH[w$RH==-1] <- 0 # remove -1 values for 0 
w <- w[w$YYYYMMDD>19591231,] # select only data with both temperature and rain
w21 <- w[w$YYYYMMDD>20201231,] # select 2021 data

## Aggregate historic temperature and rain
# Historic average per day
d.year <- unique(substr(w$YYYYMMDD,5,8))
avg.w <- data.frame(Day = NA, Temp = NA, Rain = NA)
for (i in 1:length(d.year)){
  
  day <- d.year[i]
  avg.day.t <- mean(w$TG[substr(as.character(w$YYYYMMDD),5,8)==as.character(d.year[i])], na.rm=T)
  avg.day.r <- mean(w$RH[substr(as.character(w$YYYYMMDD),5,8)==as.character(d.year[i])], na.rm=T)
  
  avg.day.row <- data.frame(Day = day, Temp = avg.day.t, Rain = avg.day.r)
  
  avg.w <- rbind(avg.w, avg.day.row)
  
}
avg.w <- avg.w[-c(1, 367),]
avg.w$DOY <- seq(1:365)
# Historic average per month
m.year <- unique(substr(w$YYYYMMDD,5,6))
avg.m <- data.frame(Day = NA, Temp = NA, Rain = NA)
for (i in 1:length(m.year)){
  
  month <- m.year[i]
  avg.m.t <- mean(w$TG[substr(as.character(w$YYYYMMDD),5,6)==as.character(m.year[i])], na.rm=T)
  avg.m.r <- mean(w$RH[substr(as.character(w$YYYYMMDD),5,6)==as.character(m.year[i])], na.rm=T)
  
  avg.month.row <- data.frame(Day = month, Temp = avg.m.t, Rain = avg.m.r)
  
  avg.m <- rbind(avg.m, avg.month.row)
  
}
avg.m <- avg.m[-c(1),]
avg.m$DOY <- c(1,32,60,91,121,152,182,213,244,274,305,335)

## Actual temperature and rain in 2021
# Actual per month
avg.m21 <- data.frame(Day = NA, Temp = NA, Rain = NA)
for (i in 1:length(m.year)){
  
  month <- m.year[i]
  avg.m.t <- mean(w21$TG[substr(as.character(w$YYYYMMDD),5,6)==as.character(m.year[i])], na.rm=T)
  avg.m.r <- mean(w21$RH[substr(as.character(w$YYYYMMDD),5,6)==as.character(m.year[i])], na.rm=T)
  
  avg.month.row <- data.frame(Day = month, Temp = avg.m.t, Rain = avg.m.r)
  
  avg.m21 <- rbind(avg.m21, avg.month.row)
}
avg.m21 <- avg.m21[-c(1),]

# Combine historic average and 2021 data
w.data <- cbind(avg.w, T21 = w21$TG, R21 = w21$RH)
w.data$Temp <- w.data$Temp/10
w.data$T21 <- w.data$T21/10

avg.m <- cbind(avg.m, R21 = avg.m21$Rain)

# Plot the historic average temperature and rainfall vs the 2021 conditions
source('10_Visualisation.R')
temp_rain_plot

