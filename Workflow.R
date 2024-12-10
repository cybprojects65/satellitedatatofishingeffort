rm(list=ls())
library(jsonlite)
library(tidyverse)
library(dplyr)
library(raster)

cat("Norsat-3 data processing started\n")
print(Sys.time())
output_file<-"./Norsat_enriched.csv"

output_file_for_behaviour_analysis<-"./Vessel_barycenters_per_window.csv"
minimum_confidence<-60
too_close_to_harbour<-1.2 #in NM #set 800 for testing
resolution<-0.5
analysis_window<-90*60 #90 minutes in secs
bounding_box_extension<-5

json_files<-c("Norsat3-N1-JSON-Message-All-2024-04-24T010155Z.json","Norsat3-N1-JSON-Message-All-2024-04-24T010155Z_2.json")

source("./JSONDataPreprocessing.R")
window_csv_data_list<-csv_data_list

window_csv_data_list_preprocessed<-list() #data with barycenters
for(i in 1:length(window_csv_data_list)){
  csv_data<-window_csv_data_list[[i]]
  source("./DataWindowPreprocessing.R")
  window_csv_data_list_preprocessed[[i]]<-vessel_data_preprocessed
}

#merge the data in the list into one DF
vessel_data_windows <- do.call(rbind, window_csv_data_list_preprocessed)

write.csv(x = vessel_data_windows, file = output_file_for_behaviour_analysis, row.names = F)

vessel_data<-vessel_data_windows

#remove ports
source("./DataFiltering.R")

vessel_data<-vessel_data_filtered

source("./DataGridCounter.R")

vessel_data<-vessel_data_gridded_counts

source("./DataGridFrequencyCounter.R")

vessel_data<-vessel_data_frequency_gridded_counts

source("./DataInterpolation.R")

vessel_data<-vessel_data_intepolated

source("./DataKernelDensity.R")

vessel_data<-vessel_data_density

#number of stocks

#frequent vessels

write.csv(x = vessel_data, file = output_file, row.names = F)

print(Sys.time())
cat("Norsat-3 data processing finished\n")
