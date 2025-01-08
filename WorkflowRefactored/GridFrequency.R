cat("#NUMBER OF RECURRING VESSELS ON GRID STARTED#\n")

frequencies<-list()
all_vessels<-unique(original_data$vesselid)
  
for (i in 1:dim(vessel_data_gridded_counts)[1]){
  
  x <- vessel_data_gridded_counts$longitude[i]
  y <- vessel_data_gridded_counts$latitude[i]
  
  iv<-which(original_data$longitude>=(x-resolution) & 
              original_data$longitude<=(x+resolution) &
              original_data$latitude>=(y-resolution) & 
              original_data$latitude<=(y+resolution)
  )
  if (length(iv)>0){
    #calc how many vessels have returned in this place
    vessel_strings<-original_data[iv,]$vesselid
    # Count occurrences of each string
    vessel_string_counts <- table(vessel_strings)
    # Identify strings with duplicates: number of returning vessels
    strings_with_duplicates <- sum(vessel_string_counts > 1)
    frequencies[[i]]<-strings_with_duplicates
  }else{
    frequencies[[i]]<-0
  }
  
  if (i%%1000==0)
    cat(i*100/dim(grid_val)[1]," ")
}

cat(" \n")
vector_freqs<-unlist(frequencies)
vessel_data_frequency_gridded_counts<-vessel_data_gridded_counts
vessel_data_frequency_gridded_counts$frequencies<-vector_freqs
names(vessel_data_frequency_gridded_counts)<-c("longitude","latitude","nvessels","nreturning_vessels")
cat("#NUMBER OF RECURRING VESSELS ON GRID FINISHED#\n")
