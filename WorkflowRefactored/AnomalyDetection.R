cat("#ANOMALY DETECTION STARTED#\n")
####CALL VAE###
cat(" calling VAE:\n")
java_command<-paste0("java -cp \"",as.character(properties["vaejar"]),"\" it.cnr.anomaly.JavaVAE -i\"",as.character(as.character(properties["output_features"])),"\" -v\"bimac,density,density_freq,bathymetry_prob\" -h4 -e1000 -o\"",as.character(as.character(properties["output_vae_folder"])),"\" -r16 -ttrue")

print(java_command)

# Run the command and capture the output
joutput <- system(java_command, intern = TRUE)
# Print the output
cat(joutput, sep = "\n")

vae_classification<-read.csv(as.character(properties["output_vae_file"]))
vae_classification$longitude<-vessel_data$longitude
vae_classification$latitude<-vessel_data$latitude

cat("#ANOMALY DETECTION FINISHED#\n")