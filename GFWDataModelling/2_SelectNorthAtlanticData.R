rm(list=ls())
library(dplyr)
library(data.table)
library(beepr)

t0<-Sys.time()
cat("GFW data processing started\n")
print(Sys.time())
vesseldata<-"C:/Users/Utente/Downloads/gfw_all_1.csv"

cat("reading data\n")
# Read CSV efficiently
large_data <- fread(vesseldata)
large_data$mmsi<-as.character(large_data$mmsi)
large_data<-as.data.frame(large_data)

bbx_min<--62
bbx_max<-0
bby_min<-30
bby_max<-60
target_year<-2015

large_data_BB<-large_data[which(large_data$lon>=bbx_min &
                                  large_data$lon<=bbx_max &
                                  large_data$lat>=bby_min &
                                  large_data$lat<=bby_max 
                                  ),]

indices <- which(format(large_data_BB$time, "%Y") == as.character(target_year))
large_data_BB<-large_data_BB[indices,]

write.csv(x=large_data_BB,file="C:/Users/Utente/Downloads/gfw_all_north_atlantic_oneyear.csv")
