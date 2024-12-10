cat("#DATA FREQUENCY GRID COUNTER STARTED#\n")

vessel_baricenters_over_time<-vessel_data_windows
grid_val<-vessel_data_gridded_counts
grid_val$frequencies<-0
all_vessels<-unique(vessel_baricenters_over_time$vesselid)
  
for (i in 1:dim(grid_val)[1]){
  
  x <- grid_val$longitude[i]
  y <- grid_val$latitude[i]
  
  iv<-which(vessel_baricenters_over_time$longitude>=(x-resolution) & 
              vessel_baricenters_over_time$longitude<=(x+resolution) &
              vessel_baricenters_over_time$latitude>=(y-resolution) & 
              vessel_baricenters_over_time$latitude<=(y+resolution)
  )
  if (length(iv)>0){
    #calc how many vessels have returned in this place
    vessel_strings<-vessel_baricenters_over_time[iv,]$vesselid
    # Count occurrences of each string
    vessel_string_counts <- table(vessel_strings)
    # Identify strings with duplicates
    strings_with_duplicates <- sum(vessel_string_counts > 1)
    grid_val$frequencies[i]<-strings_with_duplicates
  }else{
    grid_val$frequencies[i]<-0
  }
}

vessel_data_frequency_gridded_counts<<-grid_val
names(vessel_data_frequency_gridded_counts)<-c("longitude","latitude","nvessels","nreturning_vessels")
cat("#DATA FREQUENCY GRID COUNTER FINISHED#\n")
