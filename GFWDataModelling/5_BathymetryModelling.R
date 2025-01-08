rm(list=ls())
library(jsonlite)
library(tidyverse)
library(dplyr)
library(raster)
library(data.table)

cat("GFW data processing started\n")
print(Sys.time())
input_file<-"C:/Users/Utente/Downloads/gfw_all_1.csv"

resolution<-0.5

cat(" Reading data\n")
csv_data<-as.data.frame(fread(input_file))
csv_data$mmsi<-as.character(csv_data$mmsi)

fishing_locations<-csv_data[which(csv_data$is_fishing>0),]
fishing_locations<-fishing_locations[c("lon","lat")]
fishing_locations <- distinct(fishing_locations)

bathymetry_raster<-"../gebco_30sec_8.asc"
asc_file<-raster(bathymetry_raster)

nsamples=500
min_depth<--10000
max_depth<-0
fishing_locations_xy<-fishing_locations
names(fishing_locations_xy)<-c("x","y")
depth_values_fish<-raster::extract(x=asc_file,y=fishing_locations_xy,method='simple')
depth_values_fish<-depth_values_fish[which(depth_values_fish<0)]
depth_values_fish_densities<-density(depth_values_fish)
fish_depth_samples<-seq(from=min(min_depth), to=max(max_depth), length.out = nsamples)
densities_fish_depth_samples<-sapply(1:length(fish_depth_samples), function(i){
  density_at_x <- approx(depth_values_fish_densities$x, depth_values_fish_densities$y, xout = fish_depth_samples[i])$y
},simplify = T)

densities_fish_depth_samples[which(is.na(densities_fish_depth_samples))]<-0
densities_fish_depth_samples<-densities_fish_depth_samples/max(densities_fish_depth_samples)



cat("Training the ANN...\n")
library(neuralnet)
set.seed(46)

normalize <- function(x) (x -min_depth) / (max_depth - min_depth)
density_x_samples <- normalize(fish_depth_samples)

data_training_features<-data.frame(b=density_x_samples,t=densities_fish_depth_samples)
#100,50,10 9.304094
#100,100,10 8.310104
#100,50,20 5.265847
#100,30,20 8.994065
#100,30,10 8.499628
#100,30,5  9.250118
#100,30 39.25185
#100,30,1 8.417176 - optimal for the Atlantic Sea

#200,100,20 22.68121
#100,30,20,20,1 13.08837
#100,30,30,20,10,10,5 5.891387
hidden_neurons_per_layer<-c(100,30,30,20,10,10,5)
cat("Training with hidden neurons =",paste0(hidden_neurons_per_layer,collapse = ","),"\n")

dependency_target_vs_training<-paste("t~b")

f <- as.formula(dependency_target_vs_training)

### Artificial Neural Network Parameters ###
# rp - number of repetitions for the training
rp=10
# thld - threshold for minimum decrease in overall error, default 0.01 = 1%
thld=0.01
# stp - the maximum steps for the training of the neural network, default 1e+05
stp=1e+05
# alg - possible: backprop, rprop+, rprop-, sag, or slr
alg ="rprop+"
# act.fct - possible: "logistic" (=sigmoid) or "tanh"; linear.output must be 
act.fct ="logistic"
#learning rate
learningrate=0.1

#train the ANN with the hidden neurons
nn <- neuralnet(f,
                data = data_training_features,
                hidden = hidden_neurons_per_layer, 
                threshold = thld, 
                stepmax = stp, 
                rep = rp,
                learningrate = learningrate,
                act.fct = act.fct, 
                linear.output = FALSE, #activation function will be present also on the output nodes
                lifesign = "minimal", 
                algorithm = alg)


# Compute predictions on the training data
training_features<-c("b")
target_features<-c("t")

data_selftest_features<-(subset(data_training_features, select = c(training_features) ))
prediction_self<- compute(nn, data_selftest_features)

data_selftest_output<-cbind(data_selftest_features,prediction_self$net.result)
names(data_selftest_output)<-c(training_features,target_features)

par(mfrow = c(1, 2))

plot(y=densities_fish_depth_samples,x=fish_depth_samples,type="l")
points(y=densities_fish_depth_samples,x=fish_depth_samples,col="red")

plot(y=data_selftest_output$t,x=fish_depth_samples,type="l")

#####Similarity calculation for one-output ANN

optimal_similarity<-0

differences<-data_training_features[target_features]-data_selftest_output[target_features]
absdiff<-abs( differences )
#delete infinites
absdiff <- absdiff[!apply(absdiff, 1, function(row) any(is.infinite(row))), ]

#mean absolute error
mae<-mean(absdiff)

#return the mean relative error
mre<-mae/mean(unlist(data_training_features[target_features]),na.rm = T)

cat("Mean Relative Error=",100*mre,"%\n")

save(file="../auxiliaryfiles/bathymetryANN.bin",list = c("nn"))
