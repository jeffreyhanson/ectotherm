basedir<-getwd()
workdir<-"/vlsci/VR0212/shared/NicheMapR_Working/projects/chortoicetes/"

longlat<-c(139.3109, -33.888)
prevdir<-getwd()
setwd('/hsm/VR0212/shared/CSIRO Soil and Landscape Grid')
x<-cbind(longlat[1],longlat[2])
library('raster')
library('rgdal')
files<-c('density/BDW_000_005_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_005_015_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_015_030_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_030_060_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_060_100_EV_N_P_AU_NAT_C_20140801.tif','density/BDW_100_200_EV_N_P_AU_NAT_C_20140801.tif')
bdw<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
bdw[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('clay/CLY_000_005_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_005_015_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_015_030_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_030_060_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_060_100_EV_N_P_AU_NAT_C_20140801.tif','clay/CLY_100_200_EV_N_P_AU_NAT_C_20140801.tif')
cly<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
cly[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('silt/SLT_000_005_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_005_015_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_015_030_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_030_060_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_060_100_EV_N_P_AU_NAT_C_20140801.tif','silt/SLT_100_200_EV_N_P_AU_NAT_C_20140801.tif')
slt<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
slt[ii]<-extract(a,x)
rm(a)
gc()
}
files<-c('sand/SND_000_005_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_005_015_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_015_030_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_030_060_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_060_100_EV_N_P_AU_NAT_C_20140801.tif','sand/SND_100_200_EV_N_P_AU_NAT_C_20140801.tif')
snd<-rep(NA,6)
for(ii in 1:6){
a<-raster(files[ii])
snd[ii]<-extract(a,x)
rm(a)
gc()
}
soilpro<-cbind(bdw,cly,slt,snd)
colnames(soilpro)<-c('blkdens','clay','silt','sand')
write.csv(soilpro,'testsoilpro.csv')