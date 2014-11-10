setwd("c:/git/ectotherm/soil moisture test/")

# read in sites data
workdir<-"C:/Users/mrke/My Dropbox/Soil moisture data/oznet/"
#workdir<-"C:/Users/jamesmaino/Dropbox/My Manuscripts/Plague Locust/Soil moisture data/oznet/"
sitedata<-read.csv(paste(workdir,"oznetsiteinfo.csv",sep=""))
allsitenames<-as.character(sitedata$name)
# delete sites with no data
allsitenames<-allsitenames[allsitenames!="k14"]
allsitenames<-allsitenames[allsitenames!="a2"]
allsitenames<-allsitenames[allsitenames!="m3"]
sitedata<-subset(sitedata,name!="k14")
sitedata<-subset(sitedata,name!="a2")
sitedata<-subset(sitedata,name!="m3")

# start loop through sites
for(isite in 1:length(allsitenames)){

oznetsite <- sitedata[isite,1] # CHOOSE OZNET SITE HERE!
site<-subset(sitedata,name==oznetsite) # get site info for chosen site
oznetdatafreq <- site$min # frequency of measurement (usually 6min, 20min, or 30min)
# get long lat for oznet site
library("XML")
mps <- paste("http://www.oznet.org.au/",oznetsite,".html",sep="")  # URL of interest: 
mps.doc <- htmlParse(mps)# parse the docXXument for R representation: 
mps.tabs <- readHTMLTable(mps.doc) # get all the tables in mps.doc as data frames
longlat<-unlist(mps.tabs[4]$'NULL')
# find location of longlat in website
for(i in 1:length(longlat)){
  if(grepl("Longitude",longlat[i])==1){lloc<-i}
}
# extract long lat from line
longlat<-as.character(longlat[lloc])
longlat<-gsub(",","",longlat)
longlat<-strsplit(as.character(longlat), split=" ")
longlat<-unlist(longlat)
longlat<-as.numeric(cbind(longlat[4],longlat[2]))

# read in oznet data
library("raster")
XLdat <- read.table(paste(workdir,oznetsite,'_',oznetdatafreq,'min_sm.txt', sep=""), header = TRUE, skip = 2) 
rownames(XLdat)<-NULL
if(ncol(XLdat)==9){
  colnames(XLdat)  <-c('DATE', 'TIME', 'Temp_2.5cm', 'Temp_15cm', 'SM_0_5cm', 'SM_0_30cm', 'SM_30_60cm', 'SM_60_90cm', 'Rainfall')
}else if(ncol(XLdat)==15){colnames(XLdat)  <-c('DATE', 'TIME', 'Temp_4cm', 'Temp_15cm','Temp_45cm','Temp_62.5cm', 'SM_0_5cm', 'SM_0_30cm', 'SM_30_60cm', 'SM_57_87cm','suction4cm','suction15cm','suction45cm','suction63cm', 'Rainfall')
}else{ warning("Oznet data has wrong number of columns!")
}
XLdat$DATE<- as.Date(XLdat$DATE,"%d/%m/%Y")
# take only hourly data (data in 20min intervals)
XLdat <- XLdat[seq(1,nrow(XLdat),60/oznetdatafreq),]
XLdat1 <- subset(XLdat,  DATE > as.Date(paste('01/01/',ystart, sep = ""), "%d/%m/%Y"))
XLdat1 <- subset(XLdat1, DATE < as.Date(paste('01/01/',yfinish+1, sep = ""), "%d/%m/%Y"))

# read in microclimate model data and append dates
metout<-as.data.frame(read.csv(paste('metout',oznetsite,'.csv',sep="")))
soil<-as.data.frame(read.csv(paste('soil',oznetsite,'.csv',sep="")))
rainfall<-as.data.frame(read.csv(paste('rainfall',oznetsite,'.csv',sep="")))
metout<-metout[,-1]
soil<-soil[,-1]
rainfall<-rainfall[,-1]
ystart<-2007
yfinish<-2008
nyears<-yfinish-ystart+1
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
metout<-cbind(dates,metout)
soil<-cbind(dates,soil)
dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
rainfall<-as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)<-c("dates","rainfall")

# prepare for loop through hours of the day for all days of the year
rows<-nrow(metout)
condep<-10 # 'container' depth in mm
fieldcap<-quantile(XLdat1$SM_0_5cm,0.98) # make field capacity the 98th percentile of the observed soil moisture
#fieldcap<-max(XLdat1$SM_0_5cm)
#fieldcap<-read.csv(paste('ectoin',oznetsite,'.csv',sep=""))[5,2]
#fieldcap<-30
maxdep<-fieldcap
wilting<-read.csv(paste('ectoin',oznetsite,'.csv',sep=""))[6,2] # use value from digital atlas of Australian soils
contwet<-2 # 
rainmult<-.5#.11

condepths<-matrix(nrow=rows,ncol=1,data = 0) # empty vector for results

# start loop through all hours of the day of all days of the simulation
for(i in 1:(rows-1)){
  
  hr<-metout[i,3]/60 # get the current hour
  day<-floor(i/24)+1 # get the current day
  if(hr==0){ # make rain fall at midnight in one hit (rain in mm)
  condep<-condep+rainfall[day,2]*rainmult 
  }
  if(condep>maxdep){ # don't let it overflow
    condep<-maxdep
  }
  Tair<-metout[i,4] # local height air temperature
  vel<-metout[i,8] # local height wind speed
  Tskin<-soil[i,6] # soil temp at 5cm deep
  RHair<-metout[i,6] # local height relative humidity
  L<-.05 # characterhistic dimension - made it 5cm since that is the soil depth, usually cube root of volume
  A_air<-1 # area exposed to air (1m2)
  RHskin<-100 # humidity at the boundary - assume 100%
  
  # code for getting convection coefficient and then mass transfer coefficient, using forced and free convection
  press<-101325 #atmospheric pressure, pa
  DENSTY<-press/(287.04*(Tair+273)) # air density, kg/m3
  THCOND<-0.02425+(7.038*10^-5*Tair) # air thermal conductivity, W/(m.K)
  VISDYN<-(1.8325*10^-5*((296.16+120)/((Tair+273)+120)))*(((Tair+273)/296.16)^1.5) # dynamic viscosity of air, kg/(m.s)
  Re<-DENSTY*vel*L/VISDYN # Reynolds number
  PR<-1005.7*VISDYN/THCOND # Prandlt number
  NUfor<-0.102*Re^0.675*PR^(1./3.)
  hc_forced<-NUfor*THCOND/L # convection coefficent, forced
  GR<-abs(DENSTY^2*(1/(Tair+273.15))*9.80665*L^3*(Tskin-Tair)/VISDYN^2) # Grashof number
  Raylei<-GR*PR # Rayleigh number
  NUfre=0.55*Raylei^0.25
  hc_free<-NUfre*THCOND/L # convection coefficent, forced
  hc_comb<-hc_free+hc_forced
  Rconv<-1/(hc_comb*A_air)
  Nu<-hc_comb*L/THCOND # Nu combined
  D<-0.0000226*(((Tair+273.15)/273.15)^1.81)*(100000/(press)) #DIFFUSIVITY OF WATER VAPOR IN AIR (SQUARE METRE PER SECOND)
  Sc<-VISDYN/(D*DENSTY) # Schmidt number
  Sh<-Nu*(Sc/PR)^(1/3) #Sherwood number
  hd<-Sh*D/L # mass transfer coefficient
  
  if(Tskin>0){
    loge<--7.90298*(373.16/(Tskin+273)-1.)+5.02808*log10(373.16/(Tskin+273))-1.3816E-07*(10.**(11.344*(1.-(Tskin+273)/373.16))-1.)+8.1328E-03*(10.**(-3.49149*(373.16/(Tskin+273)-1.))-1.)+log10(1013.246)
  }else{
    loge<--9.09718*(273.16/(Tskin+273)-1.)-3.56654*log10(273.16/(Tskin+273))+.876793*(1.-(Tskin+273)/273.16)+log10(6.1071) 
  }
  ETs<-(10^loge)*100*(RHskin/100) # VAPOR PRESSURE (PASCAL)
  if(Tair>0){
    loge<--7.90298*(373.16/(Tair+273)-1.)+5.02808*log10(373.16/(Tair+273))-1.3816E-07*(10.**(11.344*(1.-(Tair+273)/373.16))-1.)+8.1328E-03*(10.**(-3.49149*(373.16/(Tair+273)-1.))-1.)+log10(1013.246)
  }else{
    loge<--9.09718*(273.16/(Tair+273)-1.)-3.56654*log10(273.16/(Tair+273))+.876793*(1.-(Tair+273)/273.16)+log10(6.1071) 
  }
  ETa<-(10^loge)*100*(RHair/100) # VAPOR PRESSURE (PASCAL)
  rhos<-(ETs*0.018016/(0.998*8.31434*(Tskin+273.15))) # vapour density skin
  rhoa<-(ETa*0.018016/(0.998*8.31434*(Tair+273.15))) # vapour density air
  
  
  # now adjust surface area wetness
  if((fieldcap-condep)<0){
   skinw<-1
  }else{
    skinw<-(10*(fieldcap - 0.5*wilting)*0.1-(fieldcap-condep))/(10*(fieldcap - 0.5*wilting)*0.1-0)        
  }
  if(skinw<0){
  skinw<-0
  }
  skinw<-contwet*skinw
  
  # now compute the mass flux of water (mass transfer coefficient times vapour density gradient times area exposed times fraction wet
  evap<-hd*(rhos-rhoa)*A_air*(skinw/100) # kg/s
  evap.m3.hr<-evap*3600*1000*10e-6 # m3/h
  evap.mm<-evap.m3.hr*1000 # mm/h
  condep<-condep-evap.mm # new container depth
  if(condep<0){
    condep<-0
  }
  condepths[i]<-condep
}
condepths<-as.data.frame(condepths)
condepths<-cbind(dates,condepths)
colnames(condepths)<-c('dates','depth')

# computations now done - plot the results against observations
plot(XLdat1$DATE, XLdat1$SM_0_5cm, 'l', ylim = c(0,50), col = "red",
     xlab="date", ylab="moisture (% vol)")
lines(as.Date(condepths$dates), condepths$depth, 'l')
title(main=oznetsite)
legend("topleft", inset=.05,
       c("data","predict"), 
       fill=c("red","black"),bty="n", 
       horiz=TRUE, bg=NULL, cex=0.8)
abline(0,5)
}
