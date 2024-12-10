cat("#JSON DATA PREPROCESSING STARTED#\n")

json_data<-NULL

for (json_file in json_files){
  # read the JSON
  if (is.null(json_data)){
    json_data <- fromJSON(json_file)
    rownames(json_data) <- NULL
  }else{
    json_data_current<-fromJSON(json_file)
    rownames(json_data_current) <- NULL
    json_data<-dplyr::bind_rows(json_data,json_data_current)
  }
  
}

convert2csv<-function(csvfile,json_data){
  
  # Convert to dataframe
  data_df <- as.data.frame(json_data)
  
  # Expand  the columns
  data_expanded <- data_df %>%
    unnest_wider(CandidateList) %>%
    unnest_wider(NRDEmitterPosition) %>%
    unnest_wider(CollectionInformation)
  
  # Select the important columns
  data_subset <- data_expanded %>%
    dplyr::select(longitude = Longitude,
                  latitude = Latitude,
                  vesselid = CandidateNo,
                  time = CollectionTime,
                  confidence = CandidateConfidenceCurrent)
  
  # Format day and hours
  data_subset <- data_subset %>%
    dplyr::mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  time = format(time, format = "%d/%m/%Y %H:%M:%S"))
  
  data_subset$time<-as.POSIXct(data_subset$time, format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
  
  data_subset <- data_subset[order(data_subset$time), ]
  
  # Save to intermediate CSV file
  if (!is.na(csvfile)){
    write.csv(data_subset, csvfile, row.names = FALSE)
  }
  return(data_subset)
}


# csv file output
cat(" Converting JSON to time-ordered CSV\n")
csv_data<-convert2csv(NA,json_data)

cat(" Removing records without vessel id or low confidence\n")
csv_data<-csv_data[!is.na(csv_data$vesselid), ]
csv_data<-csv_data[which(csv_data$confidence>=minimum_confidence), ]

cat(" Calculating Bounding Box\n")

bounding_box_minX<-min(csv_data$longitude)
bounding_box_maxX<-max(csv_data$longitude)
bounding_box_minY<-min(csv_data$latitude)
bounding_box_maxY<-max(csv_data$latitude)

#extend the bounding box a bit
bounding_box_minX<<-(round(bounding_box_minX / resolution) * resolution)-(bounding_box_extension*resolution)
bounding_box_maxX<<-(round(bounding_box_maxX / resolution) * resolution)+(bounding_box_extension*resolution)
bounding_box_minY<<-(round(bounding_box_minY / resolution) * resolution)-(bounding_box_extension*resolution)
bounding_box_maxY<<-(round(bounding_box_maxY / resolution) * resolution)+(bounding_box_extension*resolution)

cat(" Bounding Box: X:[",bounding_box_minX,",", bounding_box_maxX, "]; Y:[",bounding_box_minY, "," ,bounding_box_maxY,"]\n")

#CHUNKIZER: discriminator of time windows
cat(" Dividing datasets into well-separate temporal snapshots (max gap =",analysis_window," sec)\n")

utimes<-unique(csv_data$time)
times<-utimes[order(utimes)]
indices <- seq(1: length(times))
time_diff_sec<-rep(0,times=length(times))

for (i in 2:length(indices)) {
  prev_index <- indices[i - 1]
  curr_index <- indices[i]
  time_diff_sec[curr_index] <- as.numeric(difftime(times[curr_index], times[prev_index], units = "secs"))
}

chunktimes<-times[which(time_diff_sec>analysis_window)]
t0<-0
chunktimes<-c(chunktimes,Inf)
csv_data_list<<-list()
for (chunk in chunktimes){
  t1<-chunk
  csv_data_chunk<-csv_data[which(csv_data$time>=t0 & csv_data$time<t1),]
  csv_data_list[[length(csv_data_list)+1]]<-csv_data_chunk
  t0<-t1
}

cat("#JSON DATA PREPROCESSING FINISHED#\n")