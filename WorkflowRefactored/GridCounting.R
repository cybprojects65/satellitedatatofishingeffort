cat("#GRID COUNTING STARTED#\n")

xseq<-seq(from=bounding_box_minX,to=bounding_box_maxX,by=resolution)
yseq<-seq(from=bounding_box_minY,to=bounding_box_maxY,by=resolution)
grid_of_points<-expand.grid(x = xseq, y = yseq) #combine the x and y coordinate to generate pairs
grid_val<-grid_of_points
grid_val$value<-0


for (i in 1:dim(grid_of_points)[1]){
  
  x <- grid_of_points$x[i]
  y <- grid_of_points$y[i]
  
  ix<-which(vessel_data$longitude>=(x-resolution) & vessel_data$longitude<=(x+resolution))
  if (length(ix)>0){
    iy<-which(vessel_data[ix,]$latitude>=(y-resolution) & vessel_data[ix,]$latitude<=(y+resolution))
    if (length(iy)>0){
      grid_val$value[i]<-length(iy)
      #cat("found",grid_val$value[i],"vessels around (",x,y, ")\n")
    }
  }
  if (i%%1000 ==0)
    cat( (i*100/dim(grid_of_points)[1])," ")
}

cat(" \n")
vessel_data_gridded_counts<<-grid_val
names(vessel_data_gridded_counts)<-c("longitude","latitude","nvessels")
cat("#GRID COUNTING FINISHED#\n")