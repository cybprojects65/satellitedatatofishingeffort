cat("#DATA INTERPOLATION STARTED#\n")

source("./BIMAC_functions.R")

currents_u_file = "./auxiliaryfiles/currents_d0_east_05res.asc"
currents_v_file = "./auxiliaryfiles/currents_d0_north_05res.asc"
bbox <- c(xmin = bounding_box_minX, xmax = bounding_box_maxX, ymin = bounding_box_minY, ymax = bounding_box_maxY)

punctual_data_file<-"./tmp_bimac_vessels.csv"


# Crop the raster to the bounding box
ru <- raster(currents_u_file)
cropped_raster_u <- crop(ru, extent(bbox))
NAvalue(cropped_raster_u) <- -9999
currents_u_file<-"./tmp_crop_u.asc"
writeRaster(x = cropped_raster_u, filename = currents_u_file, format = "ascii", overwrite = TRUE)

# Explicitly remove raster1 from memory
rm(ru,cropped_raster_u)
gc() 

rv <- raster(currents_v_file)
cropped_raster_v <- crop(rv, extent(bbox))
NAvalue(cropped_raster_v) <- -9999
currents_v_file<-"./tmp_crop_v.asc"
writeRaster(cropped_raster_v, currents_v_file, format = "ascii", overwrite = TRUE)
rm(rv,cropped_raster_v)
gc() 

#vessel_data_to_interpolate<-subset(vessel_data, select = c("longitude","latitude"))
#vessel_data_to_interpolate$nvessels<-100
vessel_data_to_interpolate<-vessel_data[which(vessel_data$nvessels>0),]
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

bimac_output<-"./output/BIMAC_interpolation.asc"
asc_file<-raster(bimac_output)
vessel_data_grid<-subset(vessel_data, select = c("longitude","latitude"))
names(vessel_data_grid)<-c("x","y")
grid_values<-raster::extract(x=asc_file,y=vessel_data_grid,method='simple')
vessel_data_intepolated<-vessel_data
vessel_data_intepolated$bimac<-grid_values

cat("#DATA INTERPOLATION FINISHED#\n")