cat("#DATA FREQUENCY DENSITY STARTED#\n")
library(dplyr)

# Quartic kernel function
quartic_kernel <- function(d, s) {
  if (d > s) return(0)
  (15 / 16) * (1 - (d / s)^2)^2
}

data_numeric2 <- vessel_data %>% dplyr::select(longitude, latitude)
ndist<-dim(data_numeric2)[1]

#library(parallel)
#library(foreach)
#library(doParallel)

#cl <- makeCluster(detectCores() - 1)
#on.exit(stopCluster(cl))  # Ensure cleanup
tol<-2*resolution
cat(" using the following spatial correlation for kernel density:",tol,"\n")
ker <- vector()
cat(" processing...\n")
#ker <- foreach (i = 1 : ndist, .combine='rbind', .packages='foreach', .inorder=TRUE) %dopar%{
for (i in 1 : ndist){
  #quartic <- sapply(dist_matrix[i, ], quartic_kernel, s = spatial_corr)
  lo<-data_numeric2$longitude[i]
  la<-data_numeric2$latitude[i]
  idx<-which(data_numeric2$longitude>=(lo-tol) & data_numeric2$longitude<=(lo+tol) &
        data_numeric2$latitude>=(la-tol) & data_numeric2$latitude<=(la+tol))
  lons<-data_numeric2$longitude[idx]
  lats<-data_numeric2$latitude[idx]
  nvess<-vessel_data$nreturning_vessels[idx]
  
  dx<-lo-lons
  dy<-la-lats
  d<-sqrt((dx*dx)+(dy*dy))
  quartic<-(15 / 16) * (1 - (d / tol)^2)^2
  quartic[which(d>tol)]<-0
  kerm <- sum(nvess * quartic)
  ker<-c(ker,kerm)
}

#stopImplicitCluster()

# View results
#print(grid)
vessel_data_density<<-vessel_data
vessel_data_density$density_freq<-ker

#write.csv(x=grid,file="kernel.csv",row.names = F)
cat("#DATA FREQUENCY DENSITY FINISHED#\n")
