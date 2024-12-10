#remove points too close to the ports
cat("#DATA FILTERING STARTED#\n")

  cat(" Removing points too close to harbours\n")
  #harbors and ports taken from Marine Vessel Traffic and EMODNET
  ports<-read.csv("./auxiliaryfiles/cleaned_ports_QGIS.csv")
  #out-harbor selection
  xycentroids_comp<-data.frame(vessel_data$longitude, vessel_data$latitude)
  
  in_port<-sapply(1:dim(vessel_data)[1], function(i){
    
    row<-vessel_data[i,]
    dx<-row$longitude-ports$X
    dy<-row$latitude-ports$Y
    
    d<-sqrt((dx*dx)+(dy*dy))
    
    miles<-min(d)*60
    if (miles<too_close_to_harbour)
      return(T)
    else
      return(F)
  },simplify = T)
  
  port_idx<-which(in_port)
  if (length(port_idx)>0){
    cat(" Removing",length(port_idx),"data too close to ports\n")
    vessel_data_filtered<<-vessel_data[-port_idx,]
    if (dim(vessel_data_filtered)[1]==0){
      cat("WARNING: ALL POINTS WERE CLOSE TO PORTS!\n")
      stop("NO MORE POINTS TO PROCESS")
    }
  }else{
    cat(" No data too close to ports (<",too_close_to_harbour,"NM) found\n")
    vessel_data_filtered<<-vessel_data
  }

cat("#DATA FILTERING FINISHED#\n")