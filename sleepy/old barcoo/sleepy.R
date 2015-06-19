# R Implementation of an integration of Warren Porter's Niche Mapper system and 
# Bas Kooijman's Standard Dynamic Energy Budget model
# Michael Kearney February 2012

# This version uses the Australia Water Availability Project (AWAP) daily 5km climate
# layers for Australia for air temperature, relative humidity, rainfall and cloud cover
# and uses monthly soil moisture estimates (splined) and the Australia Soils database to
# obtain soil properties, including their change through time due to soil moisture.
# Humidity is only from 1971 onward. Cloud cover is only from 1990 onward (and is based
# on daily solar layers relative to clear sky calculations from NicheMapR).

# # rm(list=ls())
#  setwd("C:/NicheMapR_source/micro_source/") # set the working directory where the fortran program is
#  cmnd<- "rcmd SHLIB micr2011b.f BLKDATA.f dchxy.f dexpi.f DRYAIR.f DSUB.f error.f EVALXZ.f EVAP.f FUN.f gamma.f iomet1.f iomet2.f iosolr.f JREAD.f Micro.f MicroSegmt.f Osub.f Pttabl.f Rdctrl.f Rdtabl.f RelHumLocal.f Sfode.f sinec.f soilprops.f solrad.f Soylnods.f Tab.f VAPPRS.f vsine.f WETAIR.f ZBRAC.f ZBRENT.f"
#  system(cmnd) # run the compilation
# #  file.copy('micr2011b.dll','c:/NicheMapR/microclimate/micr2011b.dll',overwrite=TRUE)
# #   
args <- (commandArgs(TRUE))
simnum<-as.numeric(args[1])
bioregion<-as.numeric(args[2])



prerun<-0

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
    
warm<-0

############## climate and terrain data ###################################
# first choose a method for bringin in sites:
#0=specified single site long/lat
#1=place name search using geodis
#2=look up B0M weather station
#3=open a csv file with a list of sites (long/lat, no header)
sitemethod <- 0 # choose an option from above
loc <- "Mount Isa australia" # type in a location here
#longlat<-c(139.3109, -33.888)#c(139.35,-33.917)#c(139.152, -34.111)#c(136.9166667,-30.48333333)#c(139.152, -34.111)#c(139.152, -34.111)#c(139.35, -33.93)<- Kerr and Bull 2004 site c(sitestodo[k,1],sitestodo[k,2])#c(138.3960,-23.7915)#c(149.04945,-35.24894)#c(x[k,1],x[k,2]) # type a long/lat here in decimal degrees
timezone<-0 # if timezone=1 (needs internet connection), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
sitesfile <- "/NicheMapR_Working/projects/WST/aust_coarse.csv" 
quad<-0 # run over a quadrangle of points (1) or a single site/list (0)?
quadres<-0.16666666666 # quadrangle resolution (degrees)
quadlen<-5 # quadrangle length (number of steps)
quadwid<-5 # quadrangel width (number of steps)
timeinterval<-365 # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
weeknum<-1 # week of year at which simulation starts
dbase <-'AWAP' # choose the database (global or ausclim or bom or AWAP)
dailywind<-1 # use daily windspeed database?
mysqlout<- 2 # save output to mysql database tables? 0=no data, 1=all data, 2=just site summaries
terrain<-0 # include terrain (slope, aspect, horizon angles) (1) or not (0)?
soildata<-1 # include soil data for Australia (1) or not (0)?
aussiegrass<-0 # use Aussie Grass dataset (1) or not (0)?
weather_gen<-0 # use weather generator for rainfall (1), rainfall and max/min temperature (2) or not (0)
snowmodel<-0 # use snow model? (slower)
ystart <- 1990# start year for weather generator calibration dataset or AWAP database
yfinish <- 2009# end year for weather generator calibration dataset
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model, only for AWAP data (!!max 10 years!!)
loop<-0
microrun<-1 # run microclimate model? (if 0, then reads previously saved results in prior to callling ectotherm model)
############# microclimate model parameters ################################

EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.002 # Roughness height (m)
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %)
ERR <- 2.0 # Integrator error
DEP <- c(0., 1.5,  5.,  10.,  15.,  20.,  30.,  50.,  100.,  200.) # Soil nodes (cm)
Thcond <- 2.79 #2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 790. #870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 2560. # soil bulk density
Clay <- 20 # clay content for matric potential calculatinos (%)
SatWater <- # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
  SoilMoist <- 0 # fractional soil moisture
REFL<-rep(0.18,timeinterval) # soil reflectances 0.18 = granite (Kearney, measured with pyranometer facing down then up)
slope<-0 # slope (degrees)
aspect<-0 # aspect (degrees, 0 = North)
PCTWET<-0. # percentage of surface area acting as a free water surface
snowtemp<--102.4 # temperature at which precipitation is snow
snowdens<-3.6  # factor by which initial snow density is multiplied
snowmelt<-0.8  # value by which the snow evaporation calculation is divided
undercatch<-1.1

# Next for parameters are segmented velocity profiles due to bushes, rocks etc. on the surface
#IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO!
Z01 <- 0. # Top (1st) segment roughness height(m)
Z02 <- 0. # 2nd segment roughness height(m)
ZH1 <- 0. # Top of (1st) segment, height above surface(m)
ZH2 <- 0. # 2nd segment, height above surface(m)
CMH2O <- 1. # cm H2O in air column
TIMAXS <- c(1.0, 1.0 ,0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud        														
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud
minshade<-0. # minimum available shade (percent)
maxshade<-70. # maximum available shade (percent)
manualshade<-1 # if using soildata, which includes shade, this will override the data from the database and force max shade to be the number specified above
Usrhyt <- 3 # local height (cm) at which animal/container calculations will be made 

############# ectotherm model parameters ################################

# simulation settings
ectomodel<-1 # run the ectotherm model (1) or not (0)?
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
metchamber<-0 # run model for a constant, specified environment (1) or not (0)?
chambertemp<-20 # deg C, constant temperature to run
chamberrh<-.1 # %, constant humidity to run
chamberwind<-0.01 # m/s, constant wind speed to run

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
soilnode<-4. # soil node at which eggs are laid (overridden if frogbreed is 1)

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
shape_b<-1.16666666667
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
TMAXPR<-39 #34 ** degrees C, voluntary thermal maximum (upper body temperature for foraging) Pamula 1997 - where frequency dropped substantially, rather than extreme (Fig. 3.42)
TMINPR<-26.0 #26.0 # ** degrees C, voluntary thermal minimum (lower body temperature for foraging) Pamula 1997 (Fig. 3.42)
TBASK<-26.0#26.#23.1 # degrees C, minimum basking temperature Pamula Table 3.14
TEMERGE<-8.5#8.5 # degrees C, temperature at which animal will move to a basking site *based on Kerr and Bull 2004
ctmax<-43.0 # ** degrees C, critical thermal maximum (used by program to determine depth selected when inactive and burrowing) (43.0, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.)
ctmin<-3.5 # ** degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing) (3.5, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.)
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-0 #if 1, animal dies when it hits critical thermal limits
TPREF<-33.5 # ** preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.25 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-5. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
gutfill<-75. # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage

# behavioural traits
dayact<-1 # diurnal activity allowed (1) or not (0)?
nocturn<-0 # nocturnal activity allowed (1) or not (0)?
crepus<-0 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-0 #
mindepth<-2 # minimum depth (soil node) to which animal can retreat if burrowing
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
container<-1 # run the container model? (aquatic start of life cycle, e.g. frog or mosquito)
conth<-10 # cylindrical container/pond height (cm)
contw<-100. # cylindrical container/pond diameter (cm)
contype<-1 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
rainmult<-1 # rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
continit<-0 # initial container water level (cm)
conthole<- 0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
contonly<-0 # just run the container model and quit?
contwet<-80 # percent wet value for container
wetmod<-0 # run the wetland model?

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

fract<-1
f<-1.
MsM<-186.03*6. # J/cm3 produces a stomach volume of 5.3 cm3/100 g, as measured for Disosaurus dorsalis, adjusted for Egernia cunninghami
z<-7.174*fract
delta<- 0.217
kappa_X<-0.85#0.85
v_dotref<-0.05591/24.
kappa<-0.8501 
p_Mref<-45.14/24.
E_G<-7189
k_R<-0.95
k_J<-0.00628/24.
E_Hb<-6.533e+04*fract^3
E_Hj<-E_Hb*fract^3
E_Hp<-1.375e+05*fract^3
h_aref<-3.61e-13/(24.^2) #3.61e-11/(24.^2) 
s_G<-0.01

E_Egg<-1.04e+06*fract^3# J, initial energy of one egg # this includes the residual yolk, which is eaten upon hatching
svl_met<-11 # mm, snout vent length at metamorphosis
E_m<-(p_Mref*z/kappa)/v_dotref
p_Xm<-13290#12420 # J/h.cm2, maximum intake rate when feeding
K<-1 # half-saturation constant
X<-10 # food density J/cm2, approximation based on 200 Tetragonia berries per 1m2 (Dubasd and Bull 1990) assuming energy content of Lilly Pilly (http://www.sgapqld.org.au/bush_food_safety.pdf)

# for insect model
metab_mode<-0 # 0 = off, 1 = holometabolous with Dyar's rule scaling, 2 = holometabolous linear scaling, 3 = hemimetabolous with Dyar's rule scaling, 4 = hemimetabolous linear scaling
stages<-8 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
p_Am1<-0.9296852/24*100
p_AmIm<-2.068836/24*100
disc<-0.0307
gam<-1.6

# these next five parameters control the thermal response, effectively generating a thermal response curve
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-7130
TAL<-5.305e+04
TAH<-9.076e+04
TL<-288.
TH<-315.

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
N_waste<-c(5,4,3,4) # chemical formula for nitrogenous waste product, CHON, e.g. Urea c(0,3,0,1), Uric acid c(5,4,3,4)


# breeding life history
clutchsize<-2. # clutch size
eggmass<-3.787 # initial dry mass of an egg (g)
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
#  v_init<-(7.063^3)*fract^3*0.85
#  E_init<-E_m
#  E_H_init<-E_Hp+1
# stage<-3
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year
for (loop in 0:19){ 
#set up call to NicheMapR function
  quadrangle<-jobnum
  niche<-list(barcoo=barcoo,quadrangle=quadrangle,microrun=microrun,loc=loc,quad=quad,quadres=quadres,quadlen=quadlen,quadwid=quadwid,timeinterval=timeinterval,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,SatWater=SatWater,SoilMoist=SoilMoist,Z01=Z01,Z02=Z02,ZH1=ZH1,ZH2=ZH2,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,ectomodel=ectomodel,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,svl_met=svl_met,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,sitemethod=sitemethod,longlat=longlat,sitesfile=sitesfile,eggmass=eggmass,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,metchamber=metchamber,chambertemp=chambertemp,chamberrh=chamberrh,chamberwind=chamberwind,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,weeknum=weeknum,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,timezone=timezone,EC=EC,weather_gen=weather_gen,terrain=terrain,soildata=soildata,ystart=ystart,yfinish=yfinish,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,p_Am1=p_Am1,p_AmIm=p_AmIm,arrhenius=arrhenius,disc=disc,gam=gam,aussiegrass=aussiegrass,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,PCTWET=PCTWET,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,stage=stage,loop=loop,warm=warm,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet)
setwd("/hsm/VR0212/shared/NicheMapR_Working/projects/sleepy")
source('NicheMapR_Setup_bio.R')
nicheout<-NicheMapR(niche)

#} #end loop through sites

x1<-as.data.frame(nicheout$x)
ALTITUDES<-as.data.frame(nicheout$ALTITUDES)
sites<-nrow(x1)
julnum<-nicheout$julnum
metout<-as.data.frame(nicheout$metout[1:(julnum*24*sites),])
shadmet<-as.data.frame(nicheout$shadmet[1:(julnum*24*sites),])
soil<-as.data.frame(nicheout$soil[1:(julnum*24*sites),])
shadsoil<-as.data.frame(nicheout$shadsoil[1:(julnum*24*sites),])
rainfall<-as.data.frame(nicheout$RAINFALL)
juldays<-nicheout$juldays
times<-nicheout$times
grassgrowths<-as.data.frame(nicheout$grassgrowths)
grasstsdms<-as.data.frame(nicheout$grasstsdms)

if(ectomodel==1){
  
  environ<-as.data.frame(nicheout$environ[1:(julnum*24*sites),])
  enbal<-as.data.frame(nicheout$enbal[1:(julnum*24*sites),])
  masbal<-as.data.frame(nicheout$masbal[1:(julnum*24*sites),])
  debout<-as.data.frame(nicheout$debout[1:(julnum*24*sites),])
  yearout<-as.data.frame(nicheout$yearout)
  yearsout<-as.data.frame(nicheout$yearsout[1:nyears,])
  
}
if(container==1){
  if(wetmod==1){
    wetlandTemps<-as.data.frame(nicheout$wetlandTemps[1:(julnum*24*sites),1])
    wetlandTemps<-wetlandTemps[,1]
    wetlandDepths<-as.data.frame(nicheout$wetlandDepths[1:(julnum*24*sites),1])
    wetlandDepths<-wetlandDepths[,1]
  }else{
    wetlandTemps<-as.data.frame(environ$WATERTEMP)
    wetlandDepths<-as.data.frame(environ$CONDEP)
  }
}

yearoutput<-cbind(longlat,yearout)
yearsoutput<-cbind(longlat,yearsout)
setwd("/hsm/VR0212/shared/NicheMapR_Working/projects/sleepy/")
write.table(yearoutput, file = paste("yearoutput_",loop,".txt",sep=""), sep = ",", col.names = F, qmethod = "double", append = T)
write.table(yearsoutput, file = paste("yearsoutput_",loop,".txt",sep=""), sep = ",", col.names = F, qmethod = "double",append = T)

if(loop==0){
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

write.table(yearoutput_loop, file = "yearoutput_loop.txt", sep = ",", col.names = F, qmethod = "double", append = T)
write.table(yearsoutput_loop, file = "yearsoutput_loop.txt", sep = ",", col.names = F, qmethod = "double",append = T)

}}