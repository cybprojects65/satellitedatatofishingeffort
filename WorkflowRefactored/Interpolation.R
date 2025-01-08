cat("#DATA INTERPOLATION STARTED#\n")

source("./BIMAC_functions.R")

currents_u_file = as.character(properties["currents_u_file"])
currents_v_file = as.character(properties["currents_v_file"])
bbox <- c(xmin = bounding_box_minX, xmax = bounding_box_maxX, ymin = bounding_box_minY, ymax = bounding_box_maxY)

punctual_data_file<-paste0(as.character(properties["cache"]),"/tmp_bimac_vessels.csv")

# Crop the raster to the bounding box
ru <- raster(currents_u_file)
cropped_raster_u <- crop(ru, extent(bbox))
NAvalue(cropped_raster_u) <- -9999
currents_u_file<-paste0(as.character(properties["cache"]),"/tmp_crop_u.asc")
writeRaster(x = cropped_raster_u, filename = currents_u_file, format = "ascii", overwrite = TRUE)

# Explicitly remove raster1 from memory
rm(ru,cropped_raster_u)
gc() 

rv <- raster(currents_v_file)
cropped_raster_v <- crop(rv, extent(bbox))
NAvalue(cropped_raster_v) <- -9999
currents_v_file<-paste0(as.character(properties["cache"]),"/tmp_crop_v.asc")
writeRaster(cropped_raster_v, currents_v_file, format = "ascii", overwrite = TRUE)
rm(rv,cropped_raster_v)
gc() 

if (exists("exclude_zeros") && exclude_zeros==T){
  cat(" excluding zeros from the interpolation\n")
  vessel_data_to_interpolate<-vessel_data[which(vessel_data$nvessels>0),]
}else{
  cat(" including zeros in the interpolation\n")
  vessel_data_to_interpolate<-vessel_data
}

vessel_data_to_interpolate<-subset(vessel_data_to_interpolate, select = c("longitude","latitude","nvessels"))
write.csv(file=punctual_data_file,row.names = F,x = vessel_data_to_interpolate)


output<-bimac_currents_depthboundaries(punctual_data_file,
                                       currents_u_file,
                                       currents_v_file,
                                       analysis_depth=-1,
                                       moving_average_points=1, 
                                       fast_solving=T, 
                                       sd_advection_equation=0.1
)

bimac_output<-as.character(output["posterior_data_output"])
asc_file<-raster(bimac_output)
vessel_data_grid<-subset(vessel_data, select = c("longitude","latitude"))
names(vessel_data_grid)<-c("x","y")
grid_values<-raster::extract(x=asc_file,y=vessel_data_grid,method='simple')
vessel_data_intepolated<-vessel_data
vessel_data_intepolated$bimac<-grid_values
na_values_bimac<-which(is.na(vessel_data_intepolated$bimac))
if (length(na_values_bimac)>0)
  vessel_data_intepolated<-vessel_data_intepolated[-na_values_bimac,]

bimac_density<-density(vessel_data_intepolated$bimac)
plot(bimac_density$y,type='l')
points(bimac_density$y,col='red')
bimac_derivative<-bimac_density$y[2:length(bimac_density$y)]-bimac_density$y[1:(length(bimac_density$y)-1)]
sign_bimac_derivative<-as.numeric(sign(bimac_derivative))
first_descend<-min(which(sign_bimac_derivative==-1))-1
#second_ascend<- first_descend+min(which(sign_bimac_derivative[first_descend:length(sign_bimac_derivative)]==1))-1

plot(bimac_density$y[1:first_descend],type='l',col='blue')

bimac_lower_limit<-bimac_density$x[first_descend]
vessel_data_intepolated$bimac[which(vessel_data_intepolated$bimac>bimac_lower_limit)]<-0


cat("#DATA INTERPOLATION FINISHED#\n")
