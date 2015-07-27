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

dataset_ephem$date<-as.character(dataset_ephem$DATE_,format="%Y-%m-%d")
dataset_peren$date<-as.character(dataset_peren$DATE_,format="%Y-%m-%d")


# predict grass moisture index

for(kk in 3:9){
  ##################### parameters:
  root_deep<-6#kk # how deep do the roots go? 2 to 10, corresopnding to 1, 3, 5, 10, 20, 30, 60, 90 and 200 cm
  root_shallow<-5#kk-1 # how shallow do the roots go? 2 to 10, corresopnding to 1, 3, 5, 10, 20, 30, 60, 90 and 200 cm
  growth_delay<-1 # days after suitable soil moisture that new growth occurs
  wilting_thresh<-200*-1 # water potential for wilting point J/kg (divide by 100 to get bar)
  permanent_wilting_point<-1500*-1 # water potential for permanent wilting point J/kg (divide by 100 to get bar)
  #################################
  
  deps<-c(0,1,3,5,10,20,30,60,90,200)
  
  grassgrowths<-as.data.frame(soilpot)
  soilmoist2b<-as.data.frame(soilmoist)
  soilmoist2b<-subset(soilmoist2b,TIME==720)
  grassgrowths<-subset(grassgrowths,TIME==720)
  grassgrowths<-grassgrowths[,((root_shallow+3):(3+root_deep))] # get range of depths to take max of
  grassgrowths<-apply(grassgrowths, 1, max)
  
  grow<-grassgrowths
  grow[grow>permanent_wilting_point]<-1 # find times when below permanent wilting point
  grow[grow<=permanent_wilting_point]<-0
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
  grow3[grow3<growth_delay]<-0 # use one week in a row as time required for plats to come back after PWP has been hit
  grow3[grow3>0]<-1 # make vector of 0 and 1 where 1 means plants could have come back from drought
  
  soilmoist2b<-soilmoist2b[,((root_shallow+3):(3+root_deep))] # get range of depths to take max of
  soilmoist2b<-apply(soilmoist2b, 1, max)
  
  grassgrowths<-as.data.frame(cbind(grassgrowths,soilmoist2b))
  colnames(grassgrowths)<-c('pot','moist')
  grassgrowths$pot[grassgrowths$pot>wilting_thresh]<-FoodWater # assume plants start wilting at about 2 bar, but above this they are at max water content
  grassgrowths$moist<-grassgrowths$moist*100 # convert to percent
  potmult<-grassgrowths$pot
  potmult[potmult!=82]<-0
  potmult[potmult!=0]<-1  
  wilting<-subset(grassgrowths,pot==FoodWater) # find soil moisture range corresponding to values above the wilting point
  wilting<-min(wilting$moist) # get the min soil moisture at which plants aren't wilting
  grassgrowths<-grassgrowths$moist
  grassgrowths[grassgrowths>wilting]<-FoodWater # now have vector of either max plant water content or soil moisture content - need to convert the latter into a smooth decline to zero from max value
  minmoist<-0
  grassgrowths[grassgrowths<FoodWater]<-(grassgrowths[grassgrowths<FoodWater]-minmoist)/(wilting-minmoist)*FoodWater # for just the values less than max water content, make them equal to the 
  grassgrowths<-grassgrowths/100*grow3
  grasstsdms<-grassgrowths
  grassmoist<-grassgrowths
  grassmoist<-as.data.frame(cbind(dates2,grassgrowths))
  colnames(grassmoist)<-c('date','moist')
  grassmoist$date<-dates2
  grassmoist$moist<-grassmoist$moist/max(grassmoist$moist)*11 # put in units scaling from 0-11
  
  #plot plant growth metric against observed plant growth index
  for(yr in 1998:2009){
  plotgrassmoist<-subset(grassmoist,as.numeric(format(grassmoist$date, "%Y"))==yr)
  plot(plotgrassmoist$moist~plotgrassmoist$date,type='l',col='dark green',main=paste("year ",yr," roots ",deps[root_deep]," cm regrow thresh ",growth_delay," days",sep=""),ylim=c(0,11))
  points(dataset_ephem$val~dataset_ephem$DATE_,col='red',type='h',lwd=2)
  points(dataset_peren$val~dataset_peren$DATE_,col='blue',type='h',lwd=1)
  points(rainfall$rainfall/10~rainfall$dates,lty=2,type='h',col='light blue')
  }
  
  grassmoist$date<-as.character(grassmoist$date)
  merge_results<-merge(grassmoist,dataset_peren,by="date")
  merge_results<-subset(merge_results,val!='NA')
  plot(merge_results$moist~jitter(merge_results$val),ylim=c(0,11),main=paste("perennial plants, roots ",deps[root_deep]," cm ",sep=""))
  text(3,8,paste("r=",round(cor(merge_results$moist,merge_results$val),2),sep=""))
  
  merge_results<-merge(grassmoist,dataset_ephem,by="date")
  merge_results<-subset(merge_results,val!='NA')
  plot(merge_results$moist~jitter(merge_results$val),ylim=c(0,11),main=paste("ephemeral plants, roots ",deps[root_deep]," cm ",sep=""))
  text(3,8,paste("r=",round(cor(merge_results$moist,merge_results$val),2),sep=""))
}

#plot for earlier years, with just text code for grass condition for now, per year
for(yr in 1990:1997){
  plotgrassmoist<-subset(grassmoist,as.numeric(format(grassmoist$date, "%Y"))==yr)
  plot(plotgrassmoist$moist~plotgrassmoist$date,type='l',col='dark green',main=yr,ylim=c(0,11))
  text(x=dataset_ephem$DATE_,y=jitter(rep(5,nrow(dataset_ephem)),amount=2),labels=dataset_ephem$OLDVEG,cex=0.5)
  #text(x=dataset_ephem$DATE_,y=10,labels=dataset_ephem$OLDVEG,cex=0.5)
  points(rainfall$rainfall/10~rainfall$dates,lty=2,type='h',col='light blue')
}

# now per month for a closer look
mons<-c("01","02","03","04","05","06","07","08","09","10","11","12")
#plot plant growth
for(yr in 1990:1997){
  for(mon in 1:12){
    plotgrassmoist<-subset(grassmoist,format(grassmoist$date, "%Y-%m")==paste(yr,"-",mons[mon],sep=""))
    plot(plotgrassmoist$moist~plotgrassmoist$date,type='l',col='dark green',main=yr,ylim=c(0,11))
    text(x=dataset_ephem$DATE_,y=jitter(rep(5,nrow(dataset_ephem)),amount=2),labels=dataset_ephem$OLDVEG,cex=0.5)
    #text(x=dataset_ephem$DATE_,y=10,labels=dataset_ephem$OLDVEG,cex=0.5)
    points(rainfall$rainfall/10~rainfall$dates,lty=2,type='h',col='light blue')
  }
}

