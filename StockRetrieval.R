cat("#STOCK RETRIEVAL STARTED#\n")
library(robis)
library(sf)
library(dplyr)

cat("\tReading and preparing the GRSF\n")
#select the stocks with a geometry
FAO_file_for_gear_category<-paste0("./auxiliaryfiles/ASFIS_sp_2022_geom_all.txt")
cat("\tRetrieving GRSF information from",FAO_file_for_gear_category,"\n")
FAOlist<-read.delim(FAO_file_for_gear_category,header=T,sep=",",encoding="UTF-8")
FAOlist<-FAOlist[,c("TAXOCODE","Scientific_name","geometry")]
names(FAOlist)[names(FAOlist) == "Scientific_name"] <- "scientificName"
names(FAOlist)[names(FAOlist) == "geometry"] <- "geometries"
FAOlist<-FAOlist[-which(nchar(as.character(FAOlist$geometries))==0),]
if (length(which(FAOlist$geometries=="MULTIPOLYGON()")>0))
  FAOlist<-FAOlist[-which(FAOlist$geometries=="MULTIPOLYGON()"),]

FAOlist <- dplyr::distinct(FAOlist)

#cast geometries to ST objects
FAOlist_SF<-st_as_sf(FAOlist,wkt="geometries")
FAOlist_SF %>% st_cast()
fao_geometries<-FAOlist_SF$geometries

cat("\tExtracting high density locations\n")

cat("\tExtracting stocks from the GRSF\n")
grid<-subset(vessel_data, select = c("longitude","latitude"))
grid$points_wkt<-paste0("POINT(",grid$longitude," ",grid$latitude,")")
point_geometries<-st_as_sfc(grid$points_wkt)
#select the stocks with a geometry
valid_fao_geometries<-st_make_valid(fao_geometries)

cat("\tExtracting stocks in the point grid\n")
#for each geometry, intersect it with the grid points

inters<-sapply(1:length(point_geometries), function(i){
  point_geom = point_geometries[i]
  intersections<-sum(st_intersects(point_geom, valid_fao_geometries, sparse = FALSE))
  return(intersections)
},simplify = T)

vessel_data_stocks<-vessel_data
vessel_data_stocks$stocks_gears<-inters
