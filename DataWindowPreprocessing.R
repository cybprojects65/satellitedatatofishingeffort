######FUNCTIONS######


# Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

calc_distances<-function(csv_data){
  csv_data$distance<-0
  csv_data$time_diff_sec<-0
# Cycle on every vessel ID
  for (vessel in unique(csv_data$vesselid)) {
    indices <- which(csv_data$vesselid == vessel)
    if (length(indices) > 1) {
      for (i in 2:length(indices)) {
        #cat("Vessel: ",vessel,"\n")
        prev_index <- indices[i - 1]
        curr_index <- indices[i]
        csv_data$distance[curr_index] <- euclidean_distance(csv_data$longitude[prev_index], csv_data$latitude[prev_index], csv_data$longitude[curr_index], csv_data$latitude[curr_index])
        csv_data$time_diff_sec[curr_index] <- as.numeric(difftime(csv_data$time[curr_index], csv_data$time[prev_index], units = "secs"))
        #cat("Current time: ", as.character(csv_data$time[curr_index]),"\n")
        #cat("Prev time: ",as.character(csv_data$time[prev_index]),"\n")
        #cat("Diff time: ",csv_data$time_diff_sec[curr_index],"\n")
      }
    }
  }
return(csv_data)
}

calc_speed<-function(csv_data_dist){
  
  csv_data_dist$speed_deg_sec<-0
  csv_data_dist$speed_kn<-0
  
# Cycle on every vessel ID to calculate speed
  for (vessel in unique(csv_data_dist$vesselid)) {
    indices <- which(csv_data_dist$vesselid == vessel)
      if (length(indices) > 1) {
        for (i in 2:length(indices)) {
          curr_index <- indices[i]
          # Calculate speed in distance over hours
          csv_data_dist$speed_deg_sec[curr_index] <- csv_data_dist$distance[curr_index] / csv_data_dist$time_diff_sec[curr_index]
          csv_data_dist$speed_kn[curr_index] <- csv_data_dist$speed_deg_sec[curr_index] * 60 * 3600
        }
      }
  }
  return(csv_data_dist)
}


calc_barycenters<-function(csv_data){
  
  csv_data$lon_bary<-0
  csv_data$lat_bary<-0
  csv_data$avg_confidence<-0
  csv_data$init_time<-csv_data$time[1]
  
  # Cycle on every vessel ID to calculate speed
  for (vessel in unique(csv_data$vesselid)) {
    indices <- which(csv_data$vesselid == vessel)
    vessel_rows<-csv_data[indices,]
    bary_lat<-mean(vessel_rows$latitude)
    bary_lon<-mean(vessel_rows$longitude)
    bary_conf<-mean(vessel_rows$confidence)
    csv_data$lat_bary[indices]<-bary_lat
    csv_data$lon_bary[indices]<-bary_lon
    csv_data$avg_confidence[indices]<-bary_conf
    csv_data$init_time[indices]<-vessel_rows$time[1]
  }
  
  return(csv_data)
}

#####END FUNCTIONS#####


cat("#DATA PREPROCESSING STARTED#\n")

cat(" Removing records without vessel id or low confidence\n")
csv_data<-csv_data[!is.na(csv_data$vesselid), ]
csv_data<-csv_data[which(csv_data$confidence>=minimum_confidence), ]

cat(" Enriching the dataset with distance and speed information\n")
csv_data_stdist<-calc_distances(csv_data)
csv_data_stdist_speed<-calc_speed(csv_data_stdist)

cat(" Calculating vessel barycenters\n")
csv_data_barycenters<-calc_barycenters(csv_data)
vessel_data_preprocessed<<-subset(csv_data_barycenters, select = c("lon_bary","lat_bary","vesselid","init_time","avg_confidence"))
names(vessel_data_preprocessed)<-c("longitude","latitude","vesselid","time","confidence")
vessel_data_preprocessed <- vessel_data_preprocessed[!duplicated(vessel_data_preprocessed), ]

cat("#DATA PREPROCESSING FINISHED#\n")
