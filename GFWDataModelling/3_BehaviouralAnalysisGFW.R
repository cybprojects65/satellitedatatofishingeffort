rm(list=ls())
library(dplyr)
library(data.table)
library(beepr)

t0<-Sys.time()
cat("GFW data processing started\n")
print(Sys.time())
vesseldata<-"C:/Users/Utente/Downloads/gfw_all_1.csv"

if (!file.exists("gfw_vessel_behaviours.csv")){
cat("reading data\n")
# Read CSV efficiently
large_data <- fread(vesseldata)

large_data$mmsi<-as.character(large_data$mmsi)

cat("preparing port data\n")
ports<-read.csv("./auxiliaryfiles/cleaned_ports_QGIS.csv")
portsX<-ports$X
portsY<-ports$Y

cat("getting vessel ids\n")
vessels<-unique(large_data$mmsi)
vessels_dmeans<-vector()
vessels_pmeans<-vector()
set.seed(42)  # For reproducibility
counter<-1
cat("processing vessels\n")
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
  
  #dpx<-(mean(xydata$x)-portsX)
  #dpy<-(mean(xydata$y)-portsY)
  #dp<-(dpx*dpx)+(dpy*dpy)
  #dpmin<-sqrt(min(dp)) #min distance of the barycenter from a port
  
  ports_mins<-sapply(1:n_points, function(i){
    dx<-(xydata[i,]$x-portsX)
    dy<-(xydata[i,]$y-portsY)
    d<-(dx*dx)+(dy*dy)
    dmin<-sqrt(min(d))
  },simplify = T)
  
  vessels_pmeans[counter]<-mean(ports_mins)
  #vessels_pmeans[counter]<-dpmin
  if (counter%%10==0){
    cat(round((counter*100)/length(vessels)),"% ")
  }
  counter<-counter+1
  #if (counter==5)
   # break()
}

beep("mario")

vessel_behaviours<-data.frame(vessel=vessels,mean_mutual_dist=vessels_dmeans,mean_port_distance=vessels_pmeans)
write.csv(x=vessel_behaviours,file="gfw_vessel_behaviours.csv",row.names = F)

vessel_behaves<-data.frame(mean_mutual_dist=vessels_dmeans,mean_port_distance=vessels_pmeans)
}else{
  cat("using cached file for vessel behaviour analysis")
  vessel_behaviours<-read.csv("gfw_vessel_behaviours.csv")
  vessel_behaves<-data.frame(mean_mutual_dist=vessel_behaviours$mean_mutual_dist,mean_port_distance=vessel_behaviours$mean_port_distance)
}

cat("clustering\n")
#vessels have low mutual distance and distance from ports
# Perform k-means clustering with 3 clusters

kmeans_result <- kmeans(vessel_behaves, centers = 3)

# View clustering results
print(kmeans_result)
plot(kmeans_result$cluster)


centroids<-kmeans_result$centers
c_scores<-as.vector(centroids[,1]*centroids[,2])
#the maximum score is for the vessels to remove because they are sparse and distant from ports  
nonfishingcluster<-which(c_scores==max(c_scores))

save(file="kmeans_trained.bin",list = c("kmeans_result","nonfishingcluster"))
good_vessels<-c()
good_vessel_clusters<-c()

for (i in 1:dim(vessel_behaviours)[1]){
    vb<-vessel_behaviours[i,]
    distbx1<-vb$mean_mutual_dist-centroids[,1]
    distbx2<-vb$mean_port_distance-centroids[,2]
    distb<-(distbx1*distbx1)+(distbx2*distbx2)
    cluster_classification<-which(as.vector(distb==min(distb)))
    if (cluster_classification!=nonfishingcluster){
      good_vessels<-c(good_vessels,vessels[i])
      good_vessel_clusters<-c(good_vessel_clusters,cluster_classification)
    }
}


beep("mario")
t1<-Sys.time()
cat("Elapsed time: ")
print(t1-t0)