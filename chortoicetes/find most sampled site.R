## make animation of plague locust through time

require(foreign) # read/write dbf
library(ggplot2) # Plotting maps.
library(maps)    # Map data.
library(oz)      # Map data for Australia.
library(scales)  # Functions: alpha() comma()
library(ggmap)   # Google maps.
library(zoom)
library(rgeos)   # for gContains(...)


# read australia map data
ds <- read.csv(file.path("C:/NicheMapR_Working/projects/Chortoicetes/survey_james/ALPC data and R scripts/", "ozdata.csv"))
# ds  <- subset(ds, border == "coast") # dont keep state lines
mapdata<-ds[,c(2,3,6)] # long, lat, state
mapdata$ausmap <- rep(TRUE,nrow(mapdata)) # ausmap
mapdata$NDENS <- rep(-1,nrow(mapdata)) # nymph density
mapdata$ADENS <- rep(NA,nrow(mapdata)) # adult density
mapdata$DATE_ <- rep(NA,nrow(mapdata)) # sampling date
mapdata$OLDVEG     <- rep(NA,nrow(mapdata))  # old veg condition index (does not separate between perenial and ephem)
mapdata$E     <- rep(NA,nrow(mapdata))  # ephemeral plant greeness
mapdata$PEREN <- rep(NA,nrow(mapdata)) # perennial plant greeness
mapdata<-subset(mapdata, state != "TAS")

# Load locust survey data
surveylist <- c("survey90-92.dbf", "survey93-95.dbf","surveys96-98.dbf", 
                "survey98-01.dbf", "survey02-05.dbf" , "survey05-09.dbf") 
datalist <- list()
datalength <- 0
for(i in 1:length(surveylist)){
  dat <- read.dbf(file.path("C:/NicheMapR_Working/projects/Chortoicetes/survey_james/ALPC data and R scripts/",surveylist[i]))
  if(i==1){
    datalist[length(datalist)+1]<-list(data.frame(dat$X_COORD, dat$Y_COORD, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  dat$CONDITION, rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat))))
 
  }
  if(i==2){
    datalist[length(datalist)+1]<-list(data.frame(dat$X_COORD, dat$Y_COORD, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  dat$CONDITION, rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat))))
    
  }
  if(i==3){
    datalist[length(datalist)+1]<-list(data.frame(dat$X_COORD, dat$Y_COORD, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  dat$CONDITION, rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat)),rep(NA,nrow(dat))))
    
    
  }
  if(i==4){
    datalist[length(datalist)+1]<-list(data.frame(dat$X_COORD, dat$Y_COORD, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  rep(NA,nrow(dat)),E=dat$E, PEREN=dat$P,
                                                  Enew=rep(NA,nrow(dat)),PERENnew=rep(NA,nrow(dat))))
#     word1 <- unique(datalist[[i]]$OLDVEG)
#     word2 <- unique(datalist[[i]]$E)
#     word3 <- unique(datalist[[i]]$PEREN)
#     words<-unique(word1,word2,word3)
    # code grass labels as numeric
      words<-c('NA', 'G', 'GS', 'DO', 'D',   'GT', 'DGB', 'D-LG', 'DGB-LG', 'GT-LG')
      key  <-c(NA, 0.2,  0.2,  0.15, 0.05, 0.15,  0.1,   0.1,    0.1,      0.15 )
    
    # Replace vegetation code with numerical approximation
    for(j in 1:nrow(dat)){
      Eno<-which(words == datalist[[i]]$E[j])
      Pno<-which(words == datalist[[i]]$PEREN[j])
      if(length(Eno)==0){# catch integer(0) 
        datalist[[i]]$Enew[j]<-NA
      }else{
        datalist[[i]]$Enew[j]<-key[Eno]
      }
      if(length(Pno)==0){# catch integer(0) 
        datalist[[i]]$PERENnew[j]<-NA
      }else{
        datalist[[i]]$PERENnew[j]<-key[Pno]
      }
    }
  }
  if(i==5){
    datalist[length(datalist)+1]<-list(data.frame(dat$X_COORD, dat$Y_COORD, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  rep(NA,nrow(dat)),E=dat$E, PEREN=dat$P,
                                                  Enew=rep(NA,nrow(dat)),PERENnew=rep(NA,nrow(dat))))
  #         word1 <- unique(datalist[[i]]$OLDVEG)
  #         word2 <- unique(datalist[[i]]$E)
  #         word3 <- unique(datalist[[i]]$PEREN)
  #         words<-unique(word1,word2,word3)
  
  # code grass labels as numeric
  words<-c(NA, 'G','GS','DGB-LG', 'DO', 'D', 'D-LG', 'DGB','GT', 'GT-LG', 'D#', 'DLG', 'DGT', 'DGS')
  key  <-c(NA, 0.2,  0.2, 0.1,    0.15, 0.05,  0.1,   0.1,  0.15,  0.15 )
  
  # Replace vegetation code with numerical approximation
  for(j in 1:nrow(dat)){
    Eno<-which(words == datalist[[i]]$E[j])
    Pno<-which(words == datalist[[i]]$PEREN[j])
    if(length(Eno)==0){# catch integer(0) 
      datalist[[i]]$Enew[j]<-NA
    }else{
      datalist[[i]]$Enew[j]<-key[Eno]
    }
    if(length(Pno)==0){# catch integer(0) 
      datalist[[i]]$PERENnew[j]<-NA
    }else{
      datalist[[i]]$PERENnew[j]<-key[Pno]
    }
  }
  }
  if(i==6){
    datalist[length(datalist)+1]<-list(data.frame(dat$POINT_X, dat$POINT_Y, 
                                                  rep(NA,nrow(dat)),rep(FALSE,nrow(dat)),
                                                  dat$NDENS, dat$ADENS, dat$DATE_, 
                                                  rep(FALSE,nrow(dat)),E=dat$E, PEREN=dat$PEREN,
                                                  Enew=rep(NA,nrow(dat)),PERENnew=rep(NA,nrow(dat))))
    # code grass labels as numeric
    words<-c(NA, 'G','GS','DGB-LG', 'DO', 'D', 'D-LG', 'DGB','GT', 'GT-LG', 'D#', 'DLG', 'DGT', 'DGS')
    key  <-c(NA, 0.2,  0.2, 0.1,    0.15, 0.05,  0.1,   0.1,  0.15,  0.15 )
    
    # Replace vegetation code with numerical approximation
    for(j in 1:nrow(dat)){
      Eno<-which(words == datalist[[i]]$E[j])
      Pno<-which(words == datalist[[i]]$PEREN[j])
      if(length(Eno)==0){# catch integer(0) 
        datalist[[i]]$Enew[j]<-NA
      }else{
        datalist[[i]]$Enew[j]<-key[Eno]
      }
      if(length(Pno)==0){# catch integer(0) 
        datalist[[i]]$PERENnew[j]<-NA
      }else{
        datalist[[i]]$PERENnew[j]<-key[Pno]
      }
    }
    
  }
  names(datalist[[i]])<-c("long",   "lat",    "state",  "ausmap", "NDENS",  "ADENS",  "DATE_",  
                          "OLDVEG", "E",      "PEREN", "Enew", "PERENnew" )
  datalength<-datalength + nrow(dat) # sum of all data rows
}

# Show sample of coded veg data for "survey98-01.dbf", "survey02-05.dbf" , "survey05-09.dbf"
# E (ephemeral veg) and PEREN (perenial veg) are coded by APLC while Enew and PERENnew are 
# numerical conversions specified by Mike
head(datalist[[4]])
head(datalist[[5]])
head(datalist[[6]])

# # stitch together data
# locustdata<-rep(NA,datalength*ncol(mapdata))
# dim(locustdata)<-c(datalength,ncol(mapdata))
# start<-1
# for(i in 1:length(datalist)){
#   end<-start+nrow(datalist[[i]])
#   locustdata[start:(end-1),]<-datalist[[i]]
#   start <- end
# }


locustdata<-datalist[[1]]
for(i in 2:6){
locustdata<-rbind(locustdata,datalist[[i]])
}

locustdata<-as.data.frame(locustdata)
locustdata$DATE_<-as.Date(locustdata$DATE_, ,origin="1970-01-01")

# get rid of anomolous data long/lat = 1 = -54
locustdata<-subset(locustdata, lat!=-54)
locustdata<-subset(locustdata, long!=1)


# round long lat measurements to nearest 0.5 degrees 
locustdata1 <- as.data.frame(locustdata)
locustdata1$lat <- round(locustdata$lat*2,0)/2 # round to nearest 0.5
locustdata1$long <- round(locustdata$long*2,0)/2
surveyfreq<-as.data.frame(table(locustdata1$long,locustdata1$lat))
names(surveyfreq)<-c('long','lat', 'freq')
surveyfreq$lat<-as.numeric(as.character(surveyfreq$lat))
surveyfreq$long<-as.numeric(as.character(surveyfreq$long))
surveyfreq$freq<-as.numeric(as.character(surveyfreq$freq))



# plot survey distribution 
p <- ggplot() + coord_fixed()
basemap <- p + geom_polygon(data=mapdata, aes(x=long,y=lat,group=state, fill = 'red'), colour = "white")
p <- basemap + geom_point(data=surveyfreq[which(surveyfreq$freq>0),],
                          aes(x=long,
                              y=lat,
                              size = freq))
p

# order long/lats by survey freq
attach(surveyfreq)
sforder <-surveyfreq[order(-freq),]
detach(surveyfreq)

# print top surveyed coordinates
head(sforder)

# get subset for most sampled longlat 
attach(locustdata1)
locustsub<-locustdata1[which((long == sforder[1,1])&(lat==sforder[1,2])),]
detach(locustdata1)

plot(locustsub$DATE_, locustsub$NDENS, type='h')


# get veg condition codes and give rank order values

library(XML)
file<-'C:/NicheMapR_Working/projects/Chortoicetes/survey data/survey98-05.xml'
doc<-xmlInternalTreeParse(file)

#Condition of Perennial and Ephemeral Grasses 
nodes<-length(xmlChildren(xmlRoot(doc)[[5]][[1]][[15]][[8]]))
condition<-data.frame(data = 0, nrow = nodes, ncol = 3)
colnames(condition)<-c('code','desc','val')
for(i in 1:nodes){
condition[i,1]<-xmlValue(xmlRoot(doc)[[5]][[1]][[15]][[8]][[i]][[1]])
condition[i,2]<-xmlValue(xmlRoot(doc)[[5]][[1]][[15]][[8]][[i]][[2]])
}

condition[,3]<-c(11,6,10,9,8,4,2,1,7,5,3)

file<-'C:/NicheMapR_Working/projects/Chortoicetes/survey data/survey90-92.shp.xml'
doc<-xmlInternalTreeParse(file)

xmlValue(xmlRoot(doc)[[5]][[1]][[11]][[8]][[1]][[1]])

#Condition of Perennial and Ephemeral Grasses 
nodes<-length(xmlChildren(xmlRoot(doc)[[5]][[1]][[11]][[8]]))
condition2<-data.frame(data = 0, nrow = nodes, ncol = 3)
colnames(condition2)<-c('code','desc','val')
for(i in 1:nodes){
condition2[i,1]<-xmlValue(xmlRoot(doc)[[5]][[1]][[11]][[8]][[i]][[1]])
condition2[i,2]<-xmlValue(xmlRoot(doc)[[5]][[1]][[11]][[8]][[i]][[1]])
}
condition2[,3]<-seq(8,1) # got this order in part from survey90-92.shp.xml which has them in order, but missing some combination categories


# now assign values to observed veg conditions


colnames(locustsub)[10]<-'code'
library(plyr)
dataset_peren = join(locustsub,condition,by='code')
colnames(locustsub)[10]<-'PEREN'
colnames(locustsub)[9]<-'code'
library(plyr)
dataset_ephem = join(locustsub,condition,by='code')
colnames(locustsub)[9]<-'E'

dataset_ephem$DATE_<-as.POSIXct(dataset_ephem$DATE_,format="%Y-%m-%d")
dataset_peren$DATE_<-as.POSIXct(dataset_peren$DATE_,format="%Y-%m-%d")


# grass presence vector
soilpot<-read.csv(file=paste(microin,'soilpot.csv',sep=""),sep=",")
soilmoist<-read.csv(file=paste(microin,'soilmoist.csv',sep=""),sep=",")
grass<-soilpot$PT5cm
grass[is.na(grass)] <- 0
grassthresh<--500
grass[grass<=grassthresh]<-0
grass2<-grass
grass2[grass2>grassthresh & grass2<0]<-1
plot(grass2~metout$dates,type='l',col='dark green')
days<-rep(seq(1,length(grass2)/24),24)
days<-days[order(days)]
grass_pres<-as.matrix(cbind(days,grass2))
grass_pres<-aggregate(grass_pres[,2],by=list(grass_pres[,1]), FUN=max)
grass_pres<-cbind(dates2,grass_pres)
colnames(grass_pres)<-c('date','day','grass')
plot(grass_pres$grass~grass_pres$date,type='l',col='dark green')

soilpotent<-as.matrix(cbind(days,soilpot$PT20cm))
soilpotent<-aggregate(soilpotent[,2],by=list(soilpotent[,1]), FUN=mean)
soilpotent<-cbind(dates2,soilpotent)
colnames(soilpotent)<-c('date','day','pot')
soilpotent$pot[soilpotent$pot<(-1100)]<-0
soilpotent$pot<-(soilpotent$pot+1100)/100


#plot ephemerals
for(yr in 1998:2009){
plotsoilpot<-subset(soilpotent,as.numeric(format(soilpotent$date, "%Y"))==yr)
plot(plotsoilpot$pot~plotsoilpot$date,type='l',col='dark green',main=yr)
points(dataset_ephem$val~dataset_ephem$DATE_,ylim=c(0,11),col='blue',type='h')
}

#plot perennials
for(yr in 1998:2009){
plotsoilpot<-subset(soilpotent,as.numeric(format(soilpotent$date, "%Y"))==yr)
plot(plotsoilpot$pot~plotsoilpot$date,type='l',col='dark green',main=yr)
points(dataset_peren$val~dataset_peren$DATE_,ylim=c(0,11),col='blue',type='h')
}

soilmoisture<-as.matrix(cbind(days,soilmoist$WC10cm))
soilmoisture<-aggregate(soilmoisture[,2],by=list(soilmoisture[,1]), FUN=mean)
soilmoisture<-cbind(dates2,soilmoisture)
colnames(soilmoisture)<-c('date','day','moist')
plot(soilmoisture$moist~soilmoisture$date)
soilmoisture$moist<-soilmoisture$moist/max(soilmoisture$moist)*11
soilmoisture$moist<-(soilpotent$moist+1100)/100

#plot ephemerals
for(yr in 1998:2009){
plotsoilmoist<-subset(soilmoisture,as.numeric(format(soilmoisture$date, "%Y"))==yr)
plot(plotsoilmoist$moist~plotsoilmoist$date,type='l',col='dark green',main=yr,ylim=c(0,11))
points(dataset_ephem$val~dataset_ephem$DATE_,col='blue',type='h')
}

#plot perennials
for(yr in 1998:2009){
plotsoilmoist<-subset(soilmoisture,as.numeric(format(soilmoisture$date, "%Y"))==yr)
plot(plotsoilmoist$moist~plotsoilmoist$date,type='l',col='dark green',main=yr,ylim=c(0,11))
points(dataset_peren$val~dataset_peren$DATE_,col='blue',type='h')
}



 grassgrowths<-as.data.frame(soilpot)
  soilmoist2b<-as.data.frame(soilmoist)
  soilmoist2b<-subset(soilmoist2b,TIME==720)
  grassgrowths<-subset(grassgrowths,TIME==720)
  grassgrowths<-grassgrowths$PT60cm # assume plant growth driven by 5cm depth
    
    grow<-grassgrowths
    grow[grow>-1500]<-1 # find times when below permanent wilting point
    grow[grow<=-1500]<-0
    counter<-0
    grow2<-grow*0
      for(j in 1:length(grow)){
        if(j==1){
            if(grow[j]==1){
            counter<-counter+1
            }
          grow2[j]<-counter
        }else{
          if(grow[j-1]>0 & grow[j]==1){
            counter<-counter+1
          }else{
            counter<-0
          }
          grow2[j]<-counter
        }
      }
     grow3<-grow2
     grow3[grow3<2]<-0 # use one week in a row as time required for plats to come back after PWP has been hit
     grow3[grow3>0]<-1 # make vector of 0 and 1 where 1 means plants could have come back from drought
    
 soilmoist2b<-soilmoist2b$WC60cm
  grassgrowths<-as.data.frame(cbind(grassgrowths,soilmoist2b))
  colnames(grassgrowths)<-c('pot','moist')
  grassgrowths$pot[grassgrowths$pot>-200]<-FoodWater # assume plants start wilting at about 2 bar, but above this they are at max water content
  grassgrowths$moist<-grassgrowths$moist*100 # convert to percent
  potmult<-grassgrowths$pot
  potmult[potmult!=82]<-0
  potmult[potmult!=0]<-1  
  wilting<-subset(grassgrowths,pot==FoodWater) # find soil moisture range corresponding to values above the wilting point
  wilting<-min(wilting$moist) # get the min soil moisture at which plants aren't wilting
  grassgrowths<-grassgrowths$moist
  grassgrowths[grassgrowths>wilting]<-FoodWater # now have vector of either max plant water content or soil moisture content - need to convert the latter into a smooth decline to zero from max value
  #minmoist<-min(grassgrowths[grassgrowths<FoodWater])
  minmoist<-0
  grassgrowths[grassgrowths<FoodWater]<-(grassgrowths[grassgrowths<FoodWater]-minmoist)/(wilting-minmoist)*FoodWater # for just the values less than max water content, make them equal to the 
  grassgrowths<-grassgrowths/100*grow3
  #grassgrowths<-c(grassgrowths,rep(0,nrow(metout)/24-length(grassgrowths))) # this was to ensure a vector of 20 years x 24 hours, but not needed anymore
  #grassgrowths[grassgrowths<FoodWater/100]<-0
  grasstsdms<-grassgrowths


grassmoist<-grassgrowths
grassmoist<-as.data.frame(cbind(dates2,grassgrowths))
colnames(grassmoist)<-c('date','moist')
grassmoist$date<-dates2
grassmoist$moist<-grassmoist$moist/0.82*11

#plot plant growth
for(yr in 1998:2009){
plotgrassmoist<-subset(grassmoist,as.numeric(format(grassmoist$date, "%Y"))==yr)
plot(plotgrassmoist$moist~plotgrassmoist$date,type='l',col='dark green',main=yr,ylim=c(0,11))
points(dataset_ephem$val~dataset_ephem$DATE_,col='red',type='h',lwd=2)
points(dataset_peren$val~dataset_peren$DATE_,col='blue',type='h',lwd=1)
points(rainfall$rainfall/10~rainfall$dates,lty=2,type='h',col='light blue')
}


