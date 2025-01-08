library(dplyr)
library(data.table)
library(beepr)
cat("#VESSEL BEHAVIOURAL ANALYSIS STARTED#\n")
t00<-Sys.time()
print(Sys.time())
cache_file<-paste0(as.character(properties["cache"]),"/vessel_behaviour.csv")
vessel_data$mmsi<-as.character(vessel_data$mmsi)
vessels<-unique(vessel_data$mmsi)

if (!file.exists(cache_file)){
  cat("preparing port data\n")
  ports<-read.csv(as.character(properties["ports_file"]))
  portsX<-ports$X
  portsY<-ports$Y
  cat("getting vessel ids\n")
  vessels_dmeans<-vector()
  vessels_pmeans<-vector()
  set.seed(42)  # For reproducibility
  counter<-1
  cat("calculating mutual distances and port distances for all vessels\n")
  for(v in vessels){
    cat("processing vessel",v,"\n")
    v_data<-vessel_data[which(vessel_data$mmsi==v),]
    xydata<-v_data[,c("lon","lat")]
    names(xydata)<-c("x","y")
    nran<-as.numeric(properties["max_samples_vessel_behaviour"])
    cat("vessel",v,"has",dim(xydata)[1],"data\n")
  
    if (dim(xydata)[1]>nran){
      cat(" topping data to",nran,"\n")
      ranidx<-sample(1:dim(xydata)[1], nran, replace = FALSE)
      xydata<-xydata[ranidx,]
    }
    
    cat(" calculating pairwise distances\n")
    pairwise_distances <- dist(xydata)
    dmean<-mean(pairwise_distances)
    vessels_dmeans[counter]<-dmean
    n_points<-dim(xydata)[1]
    cat(" calculating distances from ports\n")
    ports_mins<-sapply(1:n_points, function(i){
      dx<-(xydata[i,]$x-portsX)
      dy<-(xydata[i,]$y-portsY)
      d<-(dx*dx)+(dy*dy)
      dmin<-sqrt(min(d))
    },simplify = T)
  
    vessels_pmeans[counter]<-mean(ports_mins)
  
    if (counter%%10==0){
      cat(round((counter*100)/length(vessels)),"% ")
    }
    counter<-counter+1
    
}

vessel_behaviours<-data.frame(vessel=vessels,mean_mutual_dist=vessels_dmeans,mean_port_distance=vessels_pmeans)
write.csv(x=vessel_behaviours,file=cache_file,row.names = F)
vessel_behaves<-data.frame(mean_mutual_dist=vessels_dmeans,mean_port_distance=vessels_pmeans)

}else{
  cat("using cached file for vessel behaviour analysis")
  vessel_behaviours<-read.csv(cache_file)
  vessel_behaves<-data.frame(mean_mutual_dist=vessel_behaviours$mean_mutual_dist,mean_port_distance=vessel_behaviours$mean_port_distance)
}

cat("loading clustering model\n")
#load the kmeans global behaviour model based on GFW data
load(as.character(properties["behaviour_trained"]))

cat("Showing centroids:\n")
print(kmeans_result)
plot(kmeans_result$cluster)
centroids<-kmeans_result$centers
c_scores<-as.vector(centroids[,1]*centroids[,2])

#the maximum score indicates for the vessels to remove because they are sparse and distant from ports  
fishingcluster<-which(c_scores==min(c_scores))
cat("the fishing cluster is",fishingcluster,"\n")

cat("filtering the fishing vessels based on the clusterization\n")
good_vessels<-c()

for (i in 1:dim(vessel_behaviours)[1]){
    vb<-vessel_behaviours[i,]
    distbx1<-vb$mean_mutual_dist-centroids[,1]
    distbx2<-vb$mean_port_distance-centroids[,2]
    distb<-(distbx1*distbx1)+(distbx2*distbx2)
    cluster_classification<-which(as.vector(distb==min(distb)))
    if (length(cluster_classification)>0 && cluster_classification==fishingcluster){
      good_vessels<-c(good_vessels,vessels[i])
    }
}

good_indices<-which(vessel_data$mmsi%in%good_vessels)
vessel_data$classification<-"bad"
vessel_data$classification[good_indices]<-"good"

cat(" selecting fishing vessels only\n")
vessel_data<-vessel_data[which(vessel_data$classification=="good"),]

t11<-Sys.time()
cat("Elapsed time: ")
print(t11-t00)
cat("#VESSEL BEHAVIOURAL ANALYSIS FINISHED#\n")
