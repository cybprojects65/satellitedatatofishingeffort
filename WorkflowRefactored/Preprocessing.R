cat("#VESSEL DATA PREPROCESSING STARTED#\n")

cat(" Calculating Bounding Box\n")
resolution<-as.numeric(properties["resolution"])
bounding_box_extension<-as.numeric(properties["bounding_box_extension"])

bounding_box_minX<-min(vessel_data$lon)
bounding_box_maxX<-max(vessel_data$lon)
bounding_box_minY<-min(vessel_data$lat)
bounding_box_maxY<-max(vessel_data$lat)

#extend the bounding box a bit
bounding_box_minX<<-max(-180,(round(bounding_box_minX / resolution) * resolution)-(bounding_box_extension*resolution))
bounding_box_maxX<<-min(180,(round(bounding_box_maxX / resolution) * resolution)+(bounding_box_extension*resolution))
bounding_box_minY<<-max(-90,(round(bounding_box_minY / resolution) * resolution)-(bounding_box_extension*resolution))
bounding_box_maxY<<-min(90,(round(bounding_box_maxY / resolution) * resolution)+(bounding_box_extension*resolution))

cat(" Bounding Box: X:[",bounding_box_minX,",", bounding_box_maxX, "]; Y:[",bounding_box_minY, "," ,bounding_box_maxY,"]\n")

cat(" Ordering the dataset temporally\n")
is_posix <- inherits(vessel_data$timestamp, "POSIXt")

if (!is_posix){
  cat(" Transforming time into posix time")
  vessel_data <- vessel_data %>%
    dplyr::mutate(timestamp = as.POSIXct(vessel_data$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                  timestamp = format(timestamp, format = "%d/%m/%Y %H:%M:%S"))  
}

vessel_data <- vessel_data %>% arrange(timestamp)
sampling_periods<-list()
vcounter<-1
for (v in unique(vessel_data$mmsi)){
  single_vessel_data<-vessel_data[which(vessel_data$mmsi==v),]
  if (dim(single_vessel_data)[1]>0){
    Torig<-single_vessel_data$timestamp[1:(length(single_vessel_data$timestamp)-1)]
    Tlagged<-single_vessel_data$timestamp[2:(length(single_vessel_data$timestamp))]
    sampling_period<- as.numeric(difftime(Tlagged, Torig, units = "hours"))
    avg_sp<-mean(sampling_period,na.rm = TRUE)
    if (avg_sp==0){
      avg_sp<-24
    }
    sampling_periods[[vcounter]]<-avg_sp
  }else{
    sampling_period<-24 #hours
    sampling_periods[[vcounter]]<-sampling_period
  }
  
  vcounter<-vcounter+1
}

sampling_periods<-unlist(sampling_periods)
sampling_period_consistent<-sampling_periods[which(sampling_periods<=as.numeric(properties["maximum_fishing_hours_per_point"]))]
if (length(sampling_period_consistent)>0){
  avg_sampling_period<<-exp(mean(log(sampling_period_consistent)))
  low_sampling_period<<-exp( mean ( log(sampling_period_consistent) - (sd( log(sampling_period_consistent) ))))
  upper_sampling_period<<-exp( mean ( log(sampling_period_consistent) + (sd( log(sampling_period_consistent) ))))
}else{
  avg_sampling_period<<- as.numeric(properties["maximum_fishing_hours_per_point"])
  low_sampling_period<<- as.numeric(properties["minimum_fishing_hours_per_point"])
  upper_sampling_period<<- as.numeric(properties["maximum_fishing_hours_per_point"])
}
cat(" inferred sampling period (hours) for fishing activity: [",low_sampling_period,":",avg_sampling_period,":",upper_sampling_period,"]\n")
cat("#VESSEL DATA PREPROCESSING FINISHED#\n")
