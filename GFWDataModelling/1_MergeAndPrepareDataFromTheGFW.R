rm(list=ls())
library(jsonlite)
library(tidyverse)
library(dplyr)
library(raster)
library(stringr)

cat("GFW data processing started\n")
print(Sys.time())
pathtofiles<-"C:/Users/Utente/Downloads/GWFData"
#2013 -> 5000
#2015 -> 82637
#2014 -> 8696

yeartoselect<-1

core_columns<-c("mmsi","timestamp","lon","lat","is_fishing")

allfiles<-list.files(pathtofiles, pattern=NULL, all.files=F, full.names=T)
listoftables<-list()

for (file in allfiles){
  cat("processing file:",file,"\n")
  classification <- str_extract(file, "(?<=gfw_)[^\\.]+")
  cat("classification:",classification,"\n")
  csvdata<-as.data.frame(fread(file))
  cat("getting core columns\n")
  #csvdata_core<-subset(csvdata, select = core_columns)
  #delete false positives
  false_pos<-which(csvdata$source=="false_positives")
  
  if (length(false_pos)>0){
    cat("deleting false positives\n")
    csvdata<-csvdata[-false_pos,]
  }
  csvdata_core<-csvdata[,core_columns]
  cat("converting time to posix\n")
  nulltimes<-which(is.na(csvdata_core$timestamp))
  if (length(nulltimes)>0){
    csvdata_core<-csvdata_core[-nulltimes,]
  }
  nullvessels<-which(is.na(csvdata_core$mmsi))
  if (length(nullvessels)>0){
    csvdata_core<-csvdata_core[-nullvessels,]
  }
  
  
  csvdata_core$timestamp<-as.POSIXct(csvdata_core$timestamp, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  
  if (yeartoselect>1){
    cat("extracting years\n")
    years<-format(csvdata_core$timestamp, "%Y")
    rowsforselectedyear<-which(years==yeartoselect)
    if (length(rowsforselectedyear)>0){
      csvdata_core<-csvdata_core[rowsforselectedyear,]
    }
  }
  
  cat("applying suffix to mmsi\n")
  csvdata_core$mmsi<-as.character(csvdata_core$mmsi)
  csvdata_core$mmsi<-paste0(csvdata_core$mmsi,"_",classification)
 
  listoftables[[length(listoftables)+1]]<-csvdata_core
  
}
cat("binding data\n")
alldatayear<-bind_rows(listoftables)
alldatayear_fishing<-alldatayear[which(alldatayear$is_fishing>0),]

cat("saving data\n")
write.csv(x=alldatayear,row.names=F,file=paste0("C:/Users/Utente/Downloads/gfw_all_",yeartoselect,".csv"))
write.csv(x=alldatayear_fishing,row.names=F,file=paste0("C:/Users/Utente/Downloads/gfw_fishing_",yeartoselect,".csv"))

cat("all vessels:",length(alldatayear$mmsi),"\n")

cat("distinct vessels:",length(unique(alldatayear$mmsi)))

cat("done\n")
