basedir<-getwd()
workdir<-"/vlsci/VR0212/shared/NicheMapR_Working/projects/sleepy/"

args <- (commandArgs(TRUE))
simnum<-as.numeric(args[1])
bioregion<-as.numeric(args[2])

barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
load(paste(barcoo,'longlat.bin',sep=''))
longlat <- data[simnum,1:2]

numsites<-ceiling(nrow(data)/2/1000)
jstart<-numsites*(simnum-1)+1
jfinish<-numsites*(simnum-1)+numsites

if(jstart<=nrow(data)){
  
  if(jfinish>nrow(data)){
    jfinish<-nrow(data)
  }
  
  
  for(jobnum in jstart:jfinish){
    
    longlat <- data[jobnum,1:2]


spatial<-"c:/Australian Environment/" # place where climate input files are kept
setwd("/vlsci/VR0212/shared/NicheMapR_Working/")
############## location and climatic data  ###################################
sitemethod <- 0 # 0=specified single site long/lat, 1=place name search using geodis (needs internet)
#longlat<-c(139.3109, -33.888) #c(139.3109, -33.888) #Mt Mary site#c(139.35, -33.93)<- Kerr and Bull 2004 site #
loc <- "Tibooburra, Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
rungads<-1 # use the Global Aerosol Database?
dailywind<-1 # use daily windspeed database?
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-0 # include soil data for Australia (1) or not (0)?
snowmodel<-0 # run snow version? (slower!)
ystart <- 1990# start year for weather generator calibration dataset or AWAP database
yfinish <- 2009# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)

#longlats<-read.csv("/vlsci/VR0212/shared/NicheMapR_Working/bioregion_points_final.csv")
#longlats<-subset(longlats,RASTERVALU!=0)
#lng<-which.min(abs(longlats[,1]-longlat[1]))
#coarselong<-longlats[lng,1]
#longlats<-subset(longlats,longlats$long==longlats[lng,1])
#lt<-which.min(abs(longlats[,2]-longlat[2]))
#coarselat<-longlats[lt,2]
#bioregion<-longlats[lt,3]
#barcoo<-paste('/scratch/VR0212/bio',bioregion,'/',sep="")
#load(paste(barcoo,'longlat.bin',sep=''))
#jobnum<-as.numeric(rownames(subset(data,data$V2==coarselong & data$V3==coarselat)))
quadrangle<-jobnum

############# microclimate model parameters ################################
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
# Next for parameters are segmented velocity profiles due to bushes, rocks etc. on the surface, IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO!
Z01 <- 0. # Top (1st) segment roughness height(m)
Z02 <- 0. # 2nd segment roughness height(m)
ZH1 <- 0. # Top of (1st) segment, height above surface(m)
ZH2 <- 0. # 2nd segment, height above surface(m)
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <- 2.0 # Integrator error for soil temperature calculations
DEP <- c(0., 1.,  3.,  5,  10,  20.,  30.,  60.,  90.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature


############# soil properties ################################
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
setwd(prevdir)
soilpro<-as.data.frame(cbind(bdw,cly,slt,snd))
colnames(soilpro)<-c('blkdens','clay','silt','sand')

soil_depths<-c(2.5,7.5,22.5,45,80,150)



DEP2<-rep(0,18)
j<-1
for(i in 1:length(DEP2)){
  if(i%%2==0){
    DEP2[i]<-DEP2[i-1]+(DEP[j]-DEP2[i-1])/2
  }else{
    DEP2[i]<-DEP[j]
    j<-j+1
  }
}
DEP2<-as.data.frame(floor(DEP2))
colnames(DEP2)<-"DEPTH"
 
#par(mfrow=c(2,2))
#par(mar = c(2,2,1,2) + 0.1) # margin spacing stuff 
#par(mar = c(5,5,5,5) + 0.1) # margin spacing stuff 

CampNormTbl9_1<-read.csv('/vlsci/VR0212/shared/NicheMapR_Working/CampNormTbl9_1.csv')
Richter<-read.csv('/vlsci/VR0212/shared/NicheMapR_Working/Richter_Table1_SI.csv')
dclay<-0.001 #mm
dsilt<-0.026 #mm
dsand<-1.05 #mm
a<-(soilpro$clay/100)*log(dclay) + (soilpro$sand/100)*log(dsand) + (soilpro$silt/100)*log(dsilt)
b.1<-(((soilpro$clay/100)*log(dclay)^2+(soilpro$sand/100)*log(dsand)^2+(soilpro$silt/100)*log(dsilt)^2)-a^2)^(1/2)
dg<-exp(a)
sigma_g<-exp(b.1)
PES<-(0.5*dg^(-1/2))*-1
b<--2*PES+0.2*sigma_g
PE<-PES*(soilpro$blkdens/1.3)^(0.67*b)
KS<-0.004*(1.3/soilpro$blkdens)^(1.3*b)*exp(-6.9*soilpro$clay/100-3.7*soilpro$silt/100)
BD<-soilpro$blkdens
   

#plot(KS~soil_depths,xlim=c(-1,200),ylim=c(0.000017,0.0058))
KS_spline <-spline(soil_depths,KS,n=201,xmin=0,xmax=200,method='natural')
#points(KS_spline$y,col='red',type='l')
KS_spline<-as.data.frame(cbind(KS_spline$x,KS_spline$y))
colnames(KS_spline)<-c('DEPTH','VALUE')
KS<-merge(DEP2,KS_spline)
KS<-c(KS[1,2],KS[,2])
KS[KS<0.000017]<-0.000017
   
#plot(PE~soil_depths,xlim=c(-1,200),ylim=c(-15,0))
PE_spline <-spline(soil_depths,PE,n=201,xmin=0,xmax=200,method='natural')
#points(PE_spline$y,col='red',type='l')
PE_spline<-as.data.frame(cbind(PE_spline$x,PE_spline$y))
colnames(PE_spline)<-c('DEPTH','VALUE')
PE<-merge(DEP2,PE_spline)
PE<-c(-1*PE[1,2],-1*PE[,2])
   
#plot(b~soil_depths,xlim=c(-1,200),ylim=c(2,24))
b_spline <-spline(soil_depths,b,n=201,xmin=0,xmax=200,method='natural')
#points(b_spline$y,col='red',type='l')
b_spline<-as.data.frame(cbind(b_spline$x,b_spline$y))
colnames(b_spline)<-c('DEPTH','VALUE')
b<-merge(DEP2,b_spline)
BB<-c(b[1,2],b[,2])
   
#plot(BD~soil_depths,xlim=c(-1,200),ylim=c(1,1.6))
BD_spline <-spline(soil_depths,BD,n=201,xmin=0,xmax=200,method='natural')
#points(BD_spline$y,col='red',type='l')
BD_spline<-as.data.frame(cbind(BD_spline$x,BD_spline$y))
colnames(BD_spline)<-c('DEPTH','VALUE')
BD<-merge(DEP2,BD_spline)
BD<-c(BD[1,2],BD[,2])
  
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 1300 # soil bulk density (kg/m3)
cap<-1 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
Clay <- 20 # clay content for matric potential calculations (%)
SoilMoist <- 0 # fractional soil moisture (decimal %)
rainmult<-1 # rain multiplier for surface soil moisture (use to induce runoff), proportion
runmoist<-1 # run soil moisture model (0=no, 1=yes)?
SoilMoist_Init<-c(0.1,0.12,0.15,0.3,0.4,0.4,0.4,0.4,0.4,0.4) # initial soil water content, m3/m3
evenrain<-0 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (0)
maxpool<-10000#6 # max depth for water pooling on the surface, mm (to account for runoff)
L<-c(0,0,rep(4,9),1.8,0.95,0.85,0.8,0.4,0.366,0,0)*10000
L<-c(0,0,8.18990859,7.991299442,7.796891252,7.420411664,7.059944542,6.385001059,5.768074989,4.816673431,4.0121088,1.833554792,0.946862989,0.635260544,0.804575,0.43525621,0.366052856,0,0)*10000
LAI<-0.1 # leaf area index, used to partition traspiration/evaporation from PET
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter
REFL<-0.15 # soil reflectance (decimal %)
slope<-0. # slope (degrees, range 0-90)
aspect<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(0,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
PCTWET<-40 # percentage of surface area acting as a free water surface (%)
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise          												
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-0. # minimum available shade (%)
maxshade<-80. # maximum available shade (%)
runshade<-1. # run the model twice, once for each shade level (1) or just for the first shade level (0)?
manualshade<-1 # if using soildata, which includes shade, this will override the data from the database and force max shade to be the number specified above
Usrhyt <- 3# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
rainwet<-1.5 # mm rain that causes soil to become 90% wet
snowtemp<-1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.4 # snow density (mg/m3)
snowmelt<-1. # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.016 # paramter in equation that melts snow with rainfall as a function of air temp
write_input<-1 # write csv files of final input to working directory? 1=yes, 0=no.
warm<-0 # uniform warming of air temperature input to simulate climate change
loop<-0 # if doing multiple years, this shifts the starting year by the integer value

# run the model
niche<-list(L=L,LAI=LAI,SoilMoist_Init=SoilMoist_Init,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loop=loop,warm=warm,rainwet=rainwet,manualshade=manualshade,dailywind=dailywind,terrain=terrain,soildata=soildata,loc=loc,ystart=ystart,yfinish=yfinish,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmelt=rainmelt,rainmult=rainmult,runshade=runshade)
source('NicheMapR_Setup_micro.R')
nicheout<-NicheMapR(niche)

# get output
metout1<-nicheout$metout # above ground microclimatic conditions, min shade
shadmet1<-nicheout$shadmet # above ground microclimatic conditions, max shade
soil1<-nicheout$soil # soil temperatures, minimum shade
shadsoil1<-nicheout$shadsoil # soil temperatures, maximum shade
soilmoist1<-nicheout$soilmoist # soil water content, minimum shade
shadmoist1<-nicheout$shadmoist # soil water content, maximum shade
humid1<-nicheout$humid # soil humidity, minimum shade
shadhumid1<-nicheout$shadhumid # soil humidity, maximum shade
soilpot1<-nicheout$soilpot # soil water potential, minimum shade
shadpot1<-nicheout$shadpot # soil water potential, maximum shade
RAINFALL1<-nicheout$RAINFALL
MAXSHADES1<-nicheout$MAXSHADES
elev1<-as.numeric(nicheout$ALTT)
REFL1<-as.numeric(nicheout$REFL)
longlat1<-as.matrix(nicheout$longlat)
fieldcap1<-as.numeric(nicheout$fieldcap)
wilting1<-0.11 # soil moisture at node 3 that means no food available
longlat1<-nicheout$longlat

setwd("/vlsci/VR0212/shared/NicheMapR_Working/")

microin<-"" # subfolder containing the microclimate input data

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
timeinterval<-365 # number of time intervals in a year
#ystart<-read.csv('ectoin.csv')[7,2]
#yfinish<-read.csv('ectoin.csv')[8,2]
#nyears<-ceiling(nrow(read.csv('rainfall.csv'))/365) # number of years the simulation runs for (work out from input data)
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
#longlat<-c(read.csv('ectoin.csv')[3,2],read.csv('ectoin.csv')[4,2])
grasshade<-0 # use grass shade values from microclimate model as min shade values (1) or not (0)? (simulates effect of grass growth on shading, as a function of soil moisture)

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
soilnode<-4. # soil node at which eggs are laid (overridden if frogbreed is 1)
minshade<-0. # minimum available shade (percent)
maxshade<-80. # maximum available shade (percent)
REFL<-rep(0.20,timeinterval*nyears) # substrate reflectances 

# morphological traits
rinsul<-0. # m, insulative fat layer thickness
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
# 5=custom (cylinder geometry is automatically invoked when container model operates)
lometry<-3 # organism shape (see above)
# 'custallom' below operates if lometry=5, and consists of 4 pairs of values representing 
# the parameters a and b of a relationship AREA=a*mass^b, where AREA is in cm2 and mass is in g.
# The first pair are a and b for total surface area, then a and b for ventral area, then for  
# sillhouette area normal to the sun, then sillhouette area perpendicular to the sun
customallom<-c(10.4713,.688,0.425,0.85,3.798,.683,0.694,.743) # custom allometry coefficients (see above)
shape_a<-1. 
shape_b<-3
shape_c<-0.6666666667
Flshcond<-0.5 # W/mC, thermal conductivity of flesh (range: 0.412-2.8 )
Spheat<-4185 # J/(kg-K), specific heat of flesh
Andens<-1000 # kg/m3, density of flesh
ABSMAX<-0.866 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
ABSMIN<-0.866 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
EMISAN<-1. # emissivity of animal
ptcond<-0.1 # decimal % of surface contacting the substrate
FATOSK<-0.4 # configuration factor to sky
FATOSB<-0.4 # configuration factor to substrate

# wing model, for butterflies
wings<-0 # wing model off (0) or on (1)
rho1_3<-0.2 # decimal %, wing reflectance
trans1<-0.00 # decimal %, wing transmissivity
aref<-0.26 # cm, width of surface #2 (back or horizontal or reference surface)
bref<-2.04 # cm, common length where the two rectangles join
cref<-1.47 # cm, width of surface #1 (wing)
phi<-179. # degrees, initial wing angle (90 = vertical relative to body)
phimax<- phi # degrees, max wing angle (90 = vertical relative to body)
phimin<- phi # degrees, min wing angle (90 = vertical relative to body

# physiological traits
TMAXPR<-39. # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection)
TMINPR<-26. # degrees C, voluntary thermal minimum (lower body temperature for foraging)
TBASK<-26. # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-8.5 # degrees C, temperature at which animal will move to a basking site
ctmax<-43.  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-3.5 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-1 #if 1, animal dies when it hits critical thermal limits
TPREF<-33.5 # preferred body temperature (animal will attempt to regulate as close to this value as possible)
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.1 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
minwater<-7 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-2.5 # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
gutfill<-75. # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage

# behavioural traits
dayact<-1 # diurnal activity allowed (1) or not (0)?
nocturn<-0 # nocturnal activity allowed (1) or not (0)?
crepus<-0 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-0 #
mindepth<-3 # minimum depth (soil node) to which animal can retreat if burrowing
maxdepth<-10 # maximum depth (soil node) to which animal can retreat if burrowing
CkGrShad<-1 # shade seeking allowed (1) or not (0)?
climb<-0 # climbing to seek cooler habitats allowed (1) or not (0)?
fosorial<-0 # fossorial activity (1) or not (0)
rainact<-0 # activity is limited by rainfall (1) or not (0)?
actrainthresh<-0.1 # threshold mm of rain causing activity if rainact=1
breedactthresh<-1 # threshold numbers of hours active after start of breeding season before eggs can be laid (simulating movement to the breeding site)
flyer<-0 # does the animal fly?
flyspeed<-5 # flying speed, m/s
flymetab<-0.1035 # flight metabolic excess, w/g

# containter simulation settings
container<-0 # run the container model? (aquatic start of life cycle, e.g. frog or mosquito)
conth<-10 # cylindrical container/pond height (cm)
contw<-100. # cylindrical container/pond diameter (cm)
contype<-1 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
rainmult<-1 # rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
continit<-0 # initial container water level (cm)
conthole<- 0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
contonly<-1 # just run the container model and quit?
contwet<-80 # percent wet value for container
wetmod<-0 # run the wetland model?
soilmoisture<-0 # run the soil moisture model? (models near-surface soil moisture rather than a pond as a function of field capacity and wilting point)

# which energy budget model to use? 
DEB<-1 # run the DEB model (1) or just heat balance, using allometric respiration below (0)

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-300. # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

################### Dynamic Enregy Budget Model Parameters ################
debpars<-as.data.frame(read.csv('c:/DEB/SleepyLizards/Munns Data/new_add_my_pet/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1
fract<-1
f<-1.
MsM<-186.03*6. # J/cm3 produces a stomach volume of 5.3 cm3/100 g, as measured for Disosaurus dorsalis, adjusted for Egernia cunninghami
z<-debpars[8]*fract
delta<-debpars[9]
kappa_X<-debpars[11]#0.85
v_dotref<-debpars[13]/24.
kappa<-debpars[14]
p_Mref<-debpars[16]/24.
E_G<-debpars[19]
k_R<-debpars[15]
k_J<-debpars[18]/24.
E_Hb<-debpars[20]*fract^3
E_Hj<-E_Hb*fract^3
E_Hp<-debpars[21]*fract^3
h_aref<-debpars[22]/(24.^2) #3.61e-11/(24.^2) 
s_G<-debpars[23]

E_Egg<-debpars[24]*fract^3# J, initial energy of one egg 
E_m<-(p_Mref*z/kappa)/v_dotref
p_Xm<-13290#12420 # J/h.cm2, maximum intake rate when feeding
p_Am<-v_dotref*E_m
K<-500#p_Am/p_Xm # half-saturation constant
X<-11.7#3#11.7 # max food density J/cm2, approximation based on 200 Tetragonia berries per 1m2 (Dubasd and Bull 1990) assuming energy content of Lilly Pilly (http://www.sgapqld.org.au/bush_food_safety.pdf)
wilting<-0.11

# for insect model
metab_mode<-0 # 0 = off, 1 = holometabolous with Dyar's rule scaling, 2 = holometabolous linear scaling, 3 = hemimetabolous with Dyar's rule scaling, 4 = hemimetabolous linear scaling
stages<-8 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
p_Am1<-0.9296852/24*100
p_AmIm<-2.068836/24*100
disc<-0.0307
gam<-1.6

# these next five parameters control the thermal response, effectively generating a thermal response curve
T_REF<-debpars[1]-273 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-debpars[2]
TAL<-debpars[5]
TAH<-debpars[6]
TL<-debpars[3]
TH<-debpars[4]

# life-stage specific parameters
arrhenius<-matrix(data = 0, nrow = 8, ncol = 5)
arrhenius[,1]<-TA # critical thermal minimum
arrhenius[,2]<-TAL # critical thermal maximum
arrhenius[,3]<-TAH # voluntary thermal minimum
arrhenius[,4]<-TL # voluntary thermal maximum
arrhenius[,5]<-TH # basking threshold  

thermal_stages<-matrix(data = 0, nrow = 8, ncol = 6)
thermal_stages[,1]<-ctmin # critical thermal minimum
thermal_stages[,2]<-ctmax # critical thermal maximum
thermal_stages[,3]<-TMINPR # voluntary thermal minimum
thermal_stages[,4]<-TMAXPR # voluntary thermal maximum
thermal_stages[,5]<-TBASK # basking threshold
thermal_stages[,6]<-TPREF # preferred body temperature

behav_stages<-matrix(data = 0, nrow = 8, ncol = 14)

behav_stages[,1]<-dayact
behav_stages[,2]<-nocturn
behav_stages[,3]<-crepus
behav_stages[,4]<-burrow
behav_stages[,5]<-shdburrow
behav_stages[,6]<-mindepth
behav_stages[,7]<-maxdepth
behav_stages[,8]<-CkGrShad
behav_stages[,9]<-climb
behav_stages[,10]<-fosorial
behav_stages[,11]<-rainact
behav_stages[,12]<-actrainthresh
behav_stages[,13]<-breedactthresh
behav_stages[,14]<-flyer

water_stages<-matrix(data = 0, nrow = 8, ncol = 8)

water_stages[,1]<-skinwet
water_stages[,2]<-extref
water_stages[,3]<-PFEWAT
water_stages[,4]<-PTUREA
water_stages[,5]<-FoodWater
water_stages[,6]<-minwater
water_stages[,7]<-raindrink
water_stages[,8]<-gutfill

# composition related parameters
andens_deb<-1. # g/cm3, density of structure 
d_V<-0.3 # density of structure (reflects fraction of mass that is dry)
d_E<-0.3 # density of reserve (reflects fraction of mass that is dry)
eggdryfrac<-0.3 # decimal percent, dry mass of eggs
mu_X<-525000 # J/cmol, chemical potential of food
mu_E<-585000 # J/cmol, chemical potential of reserve
mu_V<-500000 # J/cmol, chemical potential of structure 
mu_P<-480000 # J/cmol, chemical potential of product (faeces)
kappa_X_P<-0.1 # fraction of food energy into faeces
nX<-c(1,1.8,0.5,.15) # composition of food (atoms per carbon atoms for CHON)
nE<-c(1,1.8,0.5,.15) # composition of reserve (atoms per carbon atoms for CHON)
nV<-c(1,1.8,0.5,.15) # composition of structure (atoms per carbon atoms for CHON)
nP<-c(1,1.8,0.5,.15) # composition of product/faeces (atoms per carbon atoms for CHON)
N_waste<-c(1,4/5,3/5,4/5) # chemical formula for nitrogenous waste product, CHON, e.g. Urea c(0,3,0,1), Uric acid c(5/5,4,3,4)

# breeding life history
clutchsize<-2. # clutch size
clutch_ab<-c(0,0) # paramters for relationship between length and clutch size: clutch size = a*SVL-b, make zero if fixed clutch size
viviparous<-1 # 1=yes, 0=no
batch<-1 # invoke Pequerie et al.'s batch laying model?

# the following four parameters apply if batch = 1, i.e. animal mobilizes
breedrainthresh<-0 # rain dependent breeder? 0 means no, otherwise enter rainfall threshold in mm
# photoperiod response triggering ovulation, none (0), summer solstice (1), autumnal equinox (2),  
# winter solstice (3), vernal equinox (4), specified daylength thresholds (5)
photostart<- 3 # photoperiod initiating breeding
photofinish<- 1 # photoperiod terminating breeding
daylengthstart<- 12.5 # threshold daylength for initiating breeding
daylengthfinish<- 13. # threshold daylength for terminating breeding
photodirs <- 1 # is the start daylength trigger during a decrease (0) or increase (1) in day length?
photodirf <- 0 # is the finish daylength trigger during a decrease (0) or increase (1) in day length?
startday<-90 # make it 90 for T. rugosa loop day of year at which DEB model starts
breedtempthresh<-200 # body temperature threshold below which breeding will occur
breedtempcum<-24*7 # cumulative time below temperature threshold for breeding that will trigger breeding

reset<-0 # reset options, 0=quit simulation upon death, 1=restart at emergence, 2=restart at first egg laid, 3=restart at end of breeding season, 4=reset at death

# frog breeding mode 0 is off, 
# 1 is exotrophic aquatic (eggs start when water present in container and within breeding season)
# 2 is exotrophic terrestrial/aquatic (eggs start at specified soil node within breeding season, 
# diapause at birth threshold, start larval phase if water present in container)
# 3 endotrophic terrestrial (eggs start at specified soil node within breeding season and continue
# to metamorphosis on land)
# 4 turtle mode (eggs start at specified soil node within breeding season, hatch and animals enter
# water and stay there for the rest of their life, but leave the water if no water is present)
frogbreed<-0 # frog breeding mode
frogstage<-0 # 0 is whole life cycle, 1 is just to metamorphosis (then reset and start again)

# metabolic depression
aestivate<-0
depress<-0.3

# DEB model initial conditions
v_init<-3e-9
E_init<-E_Egg/v_init
E_H_init<-0
stage<-0
v_init<-(3.82^3)*fract^3 #hatchling
E_init<-E_m
E_H_init<-E_Hb+5
stage<-1
# v_init<-(7.063^3)*fract^3*0.85
# E_init<-E_m
# E_H_init<-E_Hp+1
# stage<-3

# mortality rates
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year

ystrt<-0 # year to start the simulation (if zero, starts at first year, but if greater than 1, runs at year ystart+1 and then loops back to the rest after)
#set up call to NicheMapR function
for (ystrt in 0:19){ 
setwd("/vlsci/VR0212/shared/NicheMapR_Working/")
metout<-metout1
shadmet<-shadmet1
soil<-soil1
shadsoil<-shadsoil1
soilmoist<-soilmoist1
shadmoist<-shadmoist1
humid<-humid1
shadhumid<-shadhumid1
soilpot<-soilpot1
shadpot<-shadpot1
RAINFALL<-RAINFALL1
MAXSHADES<-MAXSHADES1
elev<-elev1
REFL<-REFL1
longlat<-longlat1
fieldcap<-fieldcap1
wilting<-wilting1
ectoin<-ectoin1
longlat<-longlat1

niche<-list(clutch_ab=clutch_ab,wilting=wilting,ystrt=ystrt,soilmoisture=soilmoisture,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,p_Am1=p_Am1,p_AmIm=p_AmIm,arrhenius=arrhenius,disc=disc,gam=gam,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,stage=stage,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet,microin=microin,mac=mac,grasshade=grasshade)
source('NicheMapR_Setup_ecto.R')
nicheout<-NicheMapR_ecto(niche)


# retrieve output
#metout<-as.data.frame(nicheout$metout)[1:(nyears*365*24),]
#shadmet<-as.data.frame(nicheout$shadmet)[1:(nyears*365*24),]
#soil<-as.data.frame(nicheout$soil)[1:(nyears*365*24),]
#shadsoil<-as.data.frame(nicheout$shadsoil)[1:(nyears*365*24),]
#rainfall<-as.data.frame(nicheout$RAINFALL)
#grassgrowths<-as.data.frame(nicheout$grassgrowths)[1:(nyears*365),]
#grasstsdms<-as.data.frame(nicheout$grasstsdms)
#environ<-as.data.frame(nicheout$environ[1:(365*24*nyears),])
#enbal<-as.data.frame(nicheout$enbal[1:(365*24*nyears),])
#masbal<-as.data.frame(nicheout$masbal[1:(365*24*nyears),])

yearout<-as.data.frame(nicheout$yearout)
if(nyears>1){
  yearsout<-as.data.frame(nicheout$yearsout[1:nyears,])
}else{
  yearsout<-t(as.data.frame(nicheout$yearsout))
}
if(container==1){
  wetlandTemps<-as.data.frame(environ$WATERTEMP)
  wetlandDepths<-as.data.frame(environ$CONDEP)
}

# append dates
#tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
#dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
#dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
#if(DEB==1){
#  debout<-as.data.frame(nicheout$debout[1:(365*24*nyears),])
#  debout<-cbind(dates,debout)
#}
#environ<-cbind(dates,environ)
#masbal<-cbind(dates,masbal)
#enbal<-cbind(dates,enbal)
#soil<-cbind(dates,soil)
#metout<-cbind(dates,metout)
#shadsoil<-cbind(dates,shadsoil)
#shadmet<-cbind(dates,shadmet)

#dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
#dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
#grass<-cbind(dates2,grassgrowths,grasstsdms)
#colnames(grass)<-c("dates","growth","tsdm")
#rainfall<-as.data.frame(cbind(dates2,rainfall))
#colnames(rainfall)<-c("dates","rainfall")

yearoutput<-cbind(longlat[1],longlat[2],yearout)
yearsoutput<-cbind(longlat[1],longlat[2],yearsout)
setwd("/hsm/VR0212/shared/NicheMapR_Working/projects/sleepy/")
write.table(yearoutput, file = paste("yearoutput_",ystrt,".csv",sep=""), sep = ",", col.names = F, qmethod = "double", append = T)
write.table(yearsoutput, file = paste("yearsoutput_",ystrt,".csv",sep=""), sep = ",", col.names = F, qmethod = "double",append = T)

if(ystrt==0){
  yearout_loop<-yearout
  yearsout_loop<-yearsout
}else{
  yearout_loop<-rbind(yearout_loop,yearout)
  yearsout_loop<-rbind(yearsout_loop,yearsout)
}  

  } #end loop through 20 year blocks
#yloop_lsite_noct_1stjan<-yearout_loop
#ysloop__lsite_noct_1stjan<-yearsout_loop


#########################Getting output using aggregate###################################################################

yearoutput_loop<-cbind(longlat,yearout_loop)
yearsoutput_loop<-cbind(longlat,yearsout_loop)
setwd("/hsm/VR0212/shared/NicheMapR_Working/projects/sleepy/")

write.table(yearoutput_loop, file = "yearoutput_loop.csv", sep = ",", col.names = F, qmethod = "double", append = T)
write.table(yearsoutput_loop, file = "yearsoutput_loop.csv", sep = ",", col.names = F, qmethod = "double",append = T)

}}