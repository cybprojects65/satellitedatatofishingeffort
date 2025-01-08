rm(list=ls())
library(jsonlite)
library(tidyverse)
library(dplyr)
library(raster)
library(data.table)
library(neuralnet)

#PREREQUISITE 1: A CSV TABLE WITH THE FOLLOWING COLUMNS: MMSI, LON, LAT, TIMESTAMP
#PREREQUISITE 2: DATA SHOULD ALREADY BE RELIABLE (FILTERED BASED ON CONFIDENCE)
#PREREQUISITE 3: DATA SHOULD correspond to vessel barycenter if an uncertainty range occurs (FILTERED BASED ON CONFIDENCE)
t0=Sys.time()
cat("Vessel data workflow process started\n")
cat("Reading the properties file\n")
# Read the properties file into a data frame
properties <- read.table("config.properties", 
                         sep = "=", 
                         stringsAsFactors = FALSE, 
                         comment.char = "#", 
                         col.names = c("key", "value"))

# Convert to named list or vector
properties <- setNames(properties$value, properties$key)

# Print the properties
print(properties)

cachefolder<-as.character(properties["cache"])
if (!dir.exists(cachefolder)){
  dir.create(cachefolder)
}
cat("Vessel data processing started\n")
print(Sys.time())
input_data<-as.character(properties["input_data"])

cat(" Reading the data\n")
vessel_data<-as.data.frame(fread(input_data))

cat(" Step 1: Data preprocessing\n")
source("Preprocessing.R")
#rm(list = setdiff(ls(), c("vessel_data","properties","resolution","bounding_box_minX","bounding_box_maxX","bounding_box_minY","bounding_box_maxY")))

cat(" Step 2: Vessel Behaviour Analysis\n")
source("BehaviouralAnalysis.R")
#rm(list = setdiff(ls(), c("vessel_data","properties","resolution","bounding_box_minX","bounding_box_maxX","bounding_box_minY","bounding_box_maxY")))

vessel_data<-vessel_data[,c("mmsi","lon","lat","timestamp")]
names(vessel_data)<-c("vesselid","longitude","latitude","time")
original_data<-vessel_data
cat(" Step 3: Vessel-count grid snapping\n")
source("./GridCounting.R")
vessel_data<-vessel_data_gridded_counts

cat(" Step 4: Removing locations too close to the ports\n")
source("./PortRemoval.R")
vessel_data<-vessel_data_filtered

cat(" Step 5: Number of returing vessels on the grid\n")
source("./GridFrequency.R")
vessel_data<-vessel_data_frequency_gridded_counts

cat(" Step 6: Vessel number interpolation based on oceanic currents\n")
exclude_zeros<-T
source("./Interpolation.R")
vessel_data<-vessel_data_intepolated

cat(" Step 7-8: Vessel pings and returning vessel density\n")
source("./KernelDensityNVessels.R")
vessel_data<-vessel_data_density

cat(" Step 9: Fishing probability given bathymetry\n")
source("./BathymetryProbability.R")
vessel_data<-vessel_data_bathymetry

write.csv(x = vessel_data, file = as.character(properties["output_features"]), row.names = F)

cat(" Step 10: Anomaly detection\n")
source("./AnomalyDetection.R")
vessel_data<-vae_classification

cat(" Step 11: Fishing effort estimation\n")
source("./FishingEffortEstimation.R")
vessel_data<-vessel_data_fishing

#Accuracy: 73.02279 
#Precision: 87.61734 
#Recall: 78.77814 
#F1: 0.8296296 
cat(" Step 12: Comparison against the GFW data\n")
source("./CalcPerformancewrtGFW.R")
vessel_data<-vessel_data_gfw

write.csv(x = vessel_data, file = as.character(properties["output_analysis"]), row.names = F)

t1=Sys.time()

cat("Vessel data workflow finished\n")
cat("Elapsed time:\n")
print(t1-t0)
