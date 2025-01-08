rm(list=ls())
library(dplyr)
library(data.table)
library(beepr)

load(file="kmeans_trained.bin")

vesseldata<-"C:/Users/Utente/Downloads/gfw_all_north_atlantic_oneyear.csv"

cat("reading data\n")
# Read CSV efficiently
large_data <- fread(vesseldata)

large_data$mmsi<-as.character(large_data$mmsi)

cat("preparing port data\n")
ports<-read.csv("../auxiliaryfiles/cleaned_ports_QGIS.csv")
portsX<-ports$X
portsY<-ports$Y

cat("getting vessel ids\n")
vessels<-unique(large_data$mmsi)

vessels_dmeans<-vector()
vessels_pmeans<-vector()
set.seed(42)  # For reproducibility
counter<-1
cat("starting the processing of ",length(vessels),"vessels\n")
for(v in vessels){
  cat("processing vessel",v,"\n")
  v_data<-large_data[which(large_data$mmsi==v)]
  xydata<-v_data[,c("lon","lat")]
  names(xydata)<-c("x","y")
  nran<-20000
  
  cat("vessel",v,"has",dim(xydata)[1],"data\n")
  
  if (dim(xydata)[1]>nran){
    cat("topping data to",nran,"\n")
    ranidx<-sample(1:dim(xydata)[1], nran, replace = FALSE)
    xydata<-xydata[ranidx,]
  }
  
  cat(" calculating pairwise distances\n")
  pairwise_distances <- dist(xydata)
  dmean<-mean(pairwise_distances)
  vessels_dmeans[counter]<-dmean
  n_points<-dim(xydata)[1]
  
  cat(" calculating minimum barycenter distance from ports\n")
  
  ports_mins<-sapply(1:n_points, function(i){
    dx<-(xydata[i,]$x-portsX)
    dy<-(xydata[i,]$y-portsY)
    d<-(dx*dx)+(dy*dy)
    dmin<-sqrt(min(d))
  },simplify = T)
  
  vessels_pmeans[counter]<-mean(ports_mins)
  #vessels_pmeans[counter]<-dpmin
  if (counter%%10==0){
    cat("\n   ",round((counter*100)/length(vessels)),"% \n")
  }
  counter<-counter+1
  #if (counter==5)
  # break()
}

beep("mario")

cat("vessel behaviour assignment\n")
vessel_behaviours<-data.frame(vessel=vessels,mean_mutual_dist=vessels_dmeans,mean_port_distance=vessels_pmeans)
centroid_good<-2
centroid_medium<-3
centroid_bad<-1

good_vessels<-c()
medium_vessels<-c()
bad_vessels<-c()

centroids<-kmeans_result$centers
for (i in 1:dim(vessel_behaviours)[1]){
  vb<-vessel_behaviours[i,]
  if (!is.na(vb$mean_mutual_dist)){
  distbx1<-vb$mean_mutual_dist-centroids[,1]
  distbx2<-vb$mean_port_distance-centroids[,2]
  distb<-(distbx1*distbx1)+(distbx2*distbx2)
  cluster_classification<-which(as.vector(distb==min(distb)))
  }else{
    cluster_classification<-nonfishingcluster
  }
  if (cluster_classification==centroid_good){
    good_vessels<-c(good_vessels,vessels[i])
  }else if (cluster_classification==centroid_medium){
    medium_vessels<-c(medium_vessels,vessels[i])
  }else if (cluster_classification==centroid_bad){
    bad_vessels<-c(bad_vessels,vessels[i])
  }
}


good_indices<-which(large_data$mmsi%in%good_vessels)
medium_indices<-which(large_data$mmsi%in%medium_vessels)

large_data$classification<-"bad"
large_data$classification[good_indices]<-"good"
large_data$classification[medium_indices]<-"medium"
large_data$xres<-round(large_data$lon/0.5)*0.5
large_data$yres<-round(large_data$lat/0.5)*0.5

vessels_to_take<-large_data

cat("saving\n")
write.csv(x=vessels_to_take,file="C:/Users/Utente/Downloads/gfw_all_north_atlantic_selected_behaviour.csv")
