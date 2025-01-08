cat("#DATA DENSITY STARTED#\n")

# Quartic kernel function
quartic_kernel <- function(d, s) {
  if (d > s) return(0)
  (15 / 16) * (1 - (d / s)^2)^2
}

grid_for_kernel <- vessel_data %>% dplyr::select(longitude, latitude)
ndist<-dim(grid_for_kernel)[1]

tol<-2*resolution
cat(" using the following spatial correlation for kernel density:",tol,"\n")
ker <- list()
kerfreq <- list()
cat(" processing...\n")
for (i in 1 : ndist){
  lo<-grid_for_kernel$longitude[i]
  la<-grid_for_kernel$latitude[i]
  idx<-which(grid_for_kernel$longitude>=(lo-tol) & grid_for_kernel$longitude<=(lo+tol) &
               grid_for_kernel$latitude>=(la-tol) & grid_for_kernel$latitude<=(la+tol))
  
  lons<-grid_for_kernel$longitude[idx]
  lats<-grid_for_kernel$latitude[idx]
  
  nvess<-vessel_data$nvessels[idx]
  nvessfreq<-vessel_data$nreturning_vessels[idx]
  
  dx<-lo-lons
  dy<-la-lats
  d<-sqrt((dx*dx)+(dy*dy))
  quartic<-(15 / 16) * (1 - (d / tol)^2)^2
  quartic[which(d>tol)]<-0
  kerm <- sum(nvess * quartic)
  kerf <- sum(nvessfreq * quartic)
  ker[[i]]<-kerm
  kerfreq[[i]]<-kerf
}

ker_values<-as.numeric(unlist(ker))
kerf_values<-as.numeric(unlist(kerfreq))

vessel_data_density<<-vessel_data
vessel_data_density$density<-ker_values
vessel_data_density$density_freq<-kerf_values

cat("#DATA DENSITY FINISHED#\n")
