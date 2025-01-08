cat("#FISHING EFFORT ESTIMATION STARTED#\n")
#estimate sampling period:

upper_fishing_thr<-as.numeric(quantile(vessel_data$reconstruction_log_probability)[3])
fishing_idx<-which(vessel_data$reconstruction_log_probability<=upper_fishing_thr)
vae_zones<-vessel_data[fishing_idx,]

sumvae<-sum(vae_zones$density,na.rm=T)
cat(" total times vessels have been in the fishing zones:",sumvae,"\n")

#permanence depends on the sampling frequency of the dataset
upper_hours<-sumvae*(low_sampling_period+0.5*low_sampling_period)
lower_hours<-sumvae*(low_sampling_period-0.5*low_sampling_period)
average_hours<-sumvae*(low_sampling_period)

cat(" Lower number of hours spent in the area:",lower_hours,"\n")
cat(" Upper number of hours spent in the area:",upper_hours,"\n")

vessel_data_fishing<-vessel_data
vessel_data_fishing$lower_fishing_effort_estimate<-lower_hours*vessel_data_fishing$density/sumvae
vessel_data_fishing$lower_fishing_effort_estimate[-fishing_idx]<-0

vessel_data_fishing$average_fishing_effort_estimate<-average_hours *vessel_data_fishing$density/sumvae
vessel_data_fishing$average_fishing_effort_estimate[-fishing_idx]<-0

vessel_data_fishing$upper_fishing_effort_estimate<-upper_hours*vessel_data_fishing$density/sumvae
vessel_data_fishing$upper_fishing_effort_estimate[-fishing_idx]<-0

cat("#FISHING EFFORT ESTIMATION FINISHED#\n")