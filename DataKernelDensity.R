cat("#DATA DENSITY STARTED#\n")
library(dplyr)

# Select only numeric columns
vessel_data_positive<-vessel_data[which(vessel_data$nvessels>0),]
data_numeric <- vessel_data_positive %>% dplyr::select(longitude, latitude)

# Calculate the Euclidean distance matrix
dist_matrix <- dist(data_numeric, method = "euclidean")

# Convert the distance matrix to a conventional matrix
dist_matrix <- as.matrix(dist_matrix)

# Replace the diagonal of the matrix with NA to avoid considering the distance with itself
diag(dist_matrix) <- NA

# Find the avg distance for each line
avg_distance <- apply(dist_matrix, 1, mean, na.rm = TRUE)
avg_distance<-as.numeric(avg_distance)
spatial_corr<-as.numeric(quantile(avg_distance)[1])

cat(" Spatial correlation:",spatial_corr,"deg\n")
# Quartic kernel function
quartic_kernel <- function(d, s) {
  if (d > s) return(0)
  (15 / 16) * (1 - (d / s)^2)^2
}

# Calculate kernel density
kernel_density <- function(vessel_data, grid_lon, grid_lat, spatial_corr) {
  # Initialize density
  density <- 0
  
  #d <- distm(c(grid_lon, grid_lat), c(data$longitude[i], data$latitude[i]))
  dx<-vessel_data$longitude-grid_lon
  dy<-vessel_data$latitude-grid_lat
  d<-sqrt((dx*dx)+(dy*dy))
  
  # Loop through all points in the dataset
  for (i in 1:nrow(vessel_data)) {
    # Compute distance from grid point to data point
    # Add weighted kernel density
    density <- density + vessel_data$nvessels[i] * quartic_kernel(d[i], spatial_corr)
  }
  
  return(density)
}

# Calculate density at each grid point
grid<-subset(vessel_data, select = c("longitude","latitude"))

grid$density <- apply(grid, 1, function(row) {
  kernel_density(vessel_data, row["longitude"], row["latitude"], spatial_corr)
})

# View results
#print(grid)
vessel_data_density<<-vessel_data
vessel_data_density$density<-grid$density

#write.csv(x=grid,file="kernel.csv",row.names = F)
cat("#DATA DENSITY FINISHED#\n")