cat("#GFW COMPARISON STARTED#\n")

gfw_raster<-as.character(properties["GFW_raster_reference"])

bbox <- c(xmin = bounding_box_minX, xmax = bounding_box_maxX, ymin = bounding_box_minY, ymax = bounding_box_maxY)
# Crop the raster to the bounding box
asc_file<-raster(gfw_raster)
cropped_raster_gfw <- crop(asc_file, extent(bbox))
NAvalue(cropped_raster_gfw) <- -9999
cropped_raster_gfw_file<-paste0(as.character(properties["cache"]),"/tmp_gfw.asc")
aggregation_factor <- resolution/0.1
# Aggregate the raster (e.g., summing fishing hours within each 0.5° cell)
aggregated_raster <- aggregate(cropped_raster_gfw, fact = aggregation_factor, fun = sum)
writeRaster(x = aggregated_raster, filename = cropped_raster_gfw_file, format = "ascii", overwrite = TRUE)

vessel_data_grid<-vessel_data
vessel_data_grid<-vessel_data_grid[,c("longitude","latitude")]
names(vessel_data_grid)<-c("x","y")

asc_file<-raster(cropped_raster_gfw_file,NAvalue = -9999)
gfw_data<-raster::extract(x=asc_file,y=vessel_data_grid,method='simple')
vessel_data_gfw<-vessel_data
vessel_data_gfw$gfw_data<-gfw_data
vessel_data_gfw$gfw_data[which(is.na(gfw_data))]<-0

#use the 50% to set a threshold on high and non-high fishing intensity
gfw_threshold<-as.numeric(quantile(vessel_data_gfw$gfw_data[which(vessel_data_gfw$gfw_data>0)])[3] )

gfw_vessel_data_grid<-vessel_data_grid
gfw_vessel_data_grid$fishing<-vessel_data_gfw$gfw_data
total_fishing_gfw<-sum(gfw_vessel_data_grid$fishing,na.rm = T)

gfw_vessel_data_grid$xystr<-paste0(gfw_vessel_data_grid$x,";",gfw_vessel_data_grid$y)

vae_coherent_areas_thr<-as.numeric(quantile(vessel_data$average_fishing_effort_estimate[which(vessel_data$average_fishing_effort_estimate>0)])[2])
vae_zones<-vessel_data[which(vessel_data$average_fishing_effort_estimate>=vae_coherent_areas_thr),]

vae_zones$xystr<-paste0(vae_zones$longitude,";",vae_zones$latitude)

TP<-0
FP<-0
TN<-0
FN<-0

for (i in 1:dim(gfw_vessel_data_grid)[1]){
  
  xg<-gfw_vessel_data_grid$x[i]
  yg<-gfw_vessel_data_grid$y[i]
  fg<-gfw_vessel_data_grid$fishing[i]
  highfish<-F
  if (fg>=gfw_threshold)
    highfish<-T
  
  refxy<-which( abs(vae_zones$longitude-xg)<resolution & abs(vae_zones$latitude-yg)<resolution)
  if (length(refxy)>0){
    if (highfish){
      TP=TP+1
    }else{
      FP=FP+1
    }
  }else{
    if (highfish){
      FN=FN+1
    }else{
      TN=TN+1
    }
  }
}


accuracy<-(TP+TN)/(TP+FP+TN+FN)
precision<-TP/(TP+FP)
recall<-TP/(TP+FN)
f1<-2*precision*recall/(precision+recall)

cat("###SUMMARY:\n")
cat("Accuracy Fishing hours:",round(accuracy*100,2),"%\n")
cat("Precision Fishing hours:",round(precision*100,2),"%\n")
cat("Recall Fishing hours:",round(recall*100,2),"%\n")
cat("F1 Fishing hours:",round(f1,2),"\n")

vae_coherent_areas_thr_prob<- as.numeric(quantile(vessel_data$reconstruction_log_probability)[3])
vae_zones<-vessel_data[which(vessel_data$reconstruction_log_probability<=vae_coherent_areas_thr_prob),]
vae_zones$xystr<-paste0(vae_zones$longitude,";",vae_zones$latitude)

TP<-0
FP<-0
TN<-0
FN<-0

for (i in 1:dim(gfw_vessel_data_grid)[1]){
  
  xg<-gfw_vessel_data_grid$x[i]
  yg<-gfw_vessel_data_grid$y[i]
  fg<-gfw_vessel_data_grid$fishing[i]
  highfish<-F
  if (fg>=gfw_threshold)
    highfish<-T
  
  refxy<-which( abs(vae_zones$longitude-xg)<resolution & abs(vae_zones$latitude-yg)<resolution)
  if (length(refxy)>0){
    if (highfish){
      TP=TP+1
    }else{
      FP=FP+1
    }
  }else{
    if (highfish){
      FN=FN+1
    }else{
      TN=TN+1
    }
  }
}

accuracyPF<-(TP+TN)/(TP+FP+TN+FN)
precisionPF<-TP/(TP+FP)
recallPF<-TP/(TP+FN)
f1PF<-2*precisionPF*recallPF/(precisionPF+recallPF)


lower_total_fishing<-sum(vessel_data$lower_fishing_effort_estimate)
upper_total_fishing<-sum(vessel_data$upper_fishing_effort_estimate)
avg_total_fishing<-sum(vessel_data$average_fishing_effort_estimate)

consistent_estimate<-(total_fishing_gfw<=upper_total_fishing && total_fishing_gfw>=lower_total_fishing)
absolute_error<-upper_total_fishing-lower_total_fishing
relative_error<-(total_fishing_gfw-avg_total_fishing)/total_fishing_gfw



cat("\nAccuracy Potential Fishing Area:",round(accuracyPF*100,2),"%\n")
cat("Precision Potential Fishing Area:",round(precisionPF*100,2),"%\n")
cat("Recall Potential Fishing Area:",round(recallPF*100,2),"%\n")
cat("F1 Potential Fishing Area:",round(f1PF,2),"\n")

cat("\nGFW intense fishing threshold:",gfw_threshold," hours\n")
cat("Workflow intense fishing threshold:",vae_coherent_areas_thr," hours\n")
cat("Probability threshold for intense fishing:",vae_coherent_areas_thr_prob,"\n")

cat("\nIs the fishing effort estimate consistent? ",consistent_estimate,"\n")
cat("Reference GFW total fishing:",total_fishing_gfw,"\n")
cat("Lower bound of total fishing:",lower_total_fishing,"\n")
cat("Average total fishing:",avg_total_fishing,"\n")
cat("Upper bound of total fishing:",upper_total_fishing,"\n")
cat("Absolute error on total fishing:",absolute_error,"\n")
cat("Relative error on total fishing wrt GFW:",round(relative_error*100,2),"%\n")


cat("#GFW COMPARISON FINISHED#\n")
