cat("#BATHYMETRY PROBABILITY STARTED#\n")

vessel_data_grid<-vessel_data
vessel_data_grid<-vessel_data_grid[c("longitude","latitude")]

names(vessel_data_grid)<-c("x","y")

min_depth<--10000
max_depth<-0

bathymetry_raster<-"gebco_30sec_8.asc"
asc_file<-raster(bathymetry_raster)
depth_values<-raster::extract(x=asc_file,y=vessel_data_grid,method='simple')

normalize <- function(x) (x -min_depth) / (max_depth - min_depth)
density_x_samples <- normalize(depth_values)

load(file=properties["bathymetryANN"])

data_selftest_features<-data.frame(b=density_x_samples)

prediction_self<- neuralnet::compute(nn, data_selftest_features)

vessel_data_bathymetry<-cbind(vessel_data,prediction_self$net.result)
names(vessel_data_bathymetry)<-c(names(vessel_data),"bathymetry_prob")
vessel_data_bathymetry$bathymetry_prob[which(depth_values>0)]<-0

cat("#BATHYMETRY PROBABILITY FINISHED#\n")
