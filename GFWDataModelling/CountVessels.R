rm(list=ls())
library(dplyr)
library(data.table)
library(beepr)

t0<-Sys.time()
cat("GFW data processing started\n")

print(Sys.time())
pathtofiles<-"C:/Users/Utente/Downloads/GWFData"

yeartoselect<-1

core_columns<-c("mmsi","timestamp","lon","lat","is_fishing")

allfiles<-list.files(pathtofiles, pattern=NULL, all.files=F, full.names=T)
listoftables<-list()
allvessels<-c()
nrecords=0
for (file in allfiles){
  # Read CSV efficiently
  cat("reading",file,"\n")
  large_data <- fread(file)
  
  false_pos<-which(large_data$source=="false_positives")
  
  if (length(false_pos)>0){
    cat("deleting false positives\n")
    large_data<-large_data[-false_pos,]
  }
  
  nulltimes<-which(is.na(large_data$timestamp))
  if (length(nulltimes)>0){
    cat("deleting null times\n")
    large_data<-large_data[-nulltimes,]
  }
  
  nullvessels<-which(is.na(large_data$mmsi))
  if (length(nullvessels)>0){
    cat("deleting null vessels\n")
    large_data<-large_data[-nullvessels,]
  }
  
  large_data$mmsi<-as.character(large_data$mmsi)
  nrecords=nrecords+dim(large_data)[1]
  cat("getting vessel ids\n")
  vessels<-unique(large_data$mmsi)
  cat("number of distinct vessels:",length(vessels),"\n")
  allvessels<-c(allvessels,vessels)
}


cat("all vessels:",length(allvessels),"\n")

cat("distinct vessels:",length(unique(allvessels)),"\n")

cat("#records:",nrecords,"\n")
