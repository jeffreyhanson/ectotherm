############# ectotherm model parameters ################################
maindir<-getwd()
setwd("source/") # set the working directory where the fortran program is
cmnd<- "rcmd SHLIB ectotherm.f dopri5.f dget_aELE.f SOLOUT.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

# copy microclimate model outputs to current directory
file.copy('/git/micro_australia/metout.csv','metout.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadmet.csv','shadmet.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soil.csv','soil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadsoil.csv','shadsoil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soilmoist.csv','soilmoist.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soilpot.csv','soilpot.csv',overwrite=TRUE)
file.copy('/git/micro_australia/humid.csv','humid.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadmoist.csv','shadmoist.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadpot.csv','shadpot.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadhumid.csv','shadhumid.csv',overwrite=TRUE)
file.copy('/git/micro_australia/rainfall.csv','rainfall.csv',overwrite=TRUE)
file.copy('/git/micro_australia/ectoin.csv','ectoin.csv',overwrite=TRUE)
file.copy('/git/micro_australia/DEP.csv','DEP.csv',overwrite=TRUE)
file.copy('/git/micro_australia/MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)

microin<-"" # subfolder containing the microclimate input data

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
timeinterval<-365 # number of time intervals in a year
ystart<-read.csv('ectoin.csv')[1,8]
yfinish<-read.csv('ectoin.csv')[1,9]
nyears<-ceiling(nrow(read.csv('rainfall.csv'))/365) # number of years the simulation runs for (work out from input data)
write_input<-1 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv('ectoin.csv')[1,4],read.csv('ectoin.csv')[1,5])
grasshade<-0 # use grass shade values from microclimate model as min shade values (1) or not (0)? (simulates effect of grass growth on shading, as a function of soil moisture)

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-0.10 # substrate thermal conductivity (W/mC)
soilnode<-1. # soil node at which eggs are laid (overridden if frogbreed is 1)
minshade<-0. # minimum available shade (percent)
maxshade<-90. # maximum available shade (percent)
REFL<-rep(0.18,timeinterval*nyears) # substrate reflectances 

# morphological traits
rinsul<-0. # m, insulative fat layer thickness
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
# 5=custom (cylinder geometry is automatically invoked when container model operates)
lometry<-2 # organism shape (see above)
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
ABSMAX<-0.71 # decimal %, maximum solar absorptivity
ABSMIN<-0.71 # decimal %, minimum solar absorptivity
EMISAN<-1. # emissivity of animal
ptcond<-0.1 # decimal % of surface contacting the substrate
FATOSK<-0.4 # configuration factor to sky
FATOSB<-0.4 # configuration factor to substrate

# wing model, for butterflies
wings<-1 # wing model off (0) or on (1)
rho1_3<-0.2 # decimal %, wing reflectance
trans1<-0.01 # decimal %, wing transmissivity
aref<-0.4 # cm, width of surface #2 (back or horizontal or reference surface)
bref<-2. # cm, common length where the two rectangles join
cref<-3. # cm, width of surface #1 (wing)
phi<-86. # degrees, initial wing angle (90 = vertical relative to body)
phimax<- 135. # degrees, max wing angle (90 = vertical relative to body)
phimin<- 86. # degrees, min wing angle (90 = vertical relative to body

# physiological traits
TMAXPR<-30. # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection)
TMINPR<-6. # degrees C, voluntary thermal minimum (lower body temperature for foraging)
TBASK<-5. # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-4. # degrees C, temperature at which animal will move to a basking site
ctmax<-43  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-2 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-1 #if 1, animal dies when it hits critical thermal limits
TPREF<-20 # preferred body temperature (animal will attempt to regulate as close to this value as possible)
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-1 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 1% wet)
extref<-10. # %, oxygen extraction efficiency
PFEWAT<-10. # %, fecal water
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-80 # %, water content of food (from Shine's thesis, clover)
minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-.9 # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available) %less than 1 means 0
gutfill<-101. # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage

# behavioural traits
dayact<-0 # diurnal activity allowed (1) or not (0)?
nocturn<-1 # nocturnal activity allowed (1) or not (0)?
crepus<-0 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-1 # burrow in maximum shade (1) or in minimum shade (0)
mindepth<-2 # minimum depth (soil node) to which animal can retreat if burrowing
maxdepth<-9 # maximum depth (soil node) to which animal can retreat if burrowing
CkGrShad<-1 # shade seeking allowed (1) or not (0)?
climb<-0 # climbing to seek cooler habitats allowed (1) or not (0)?
fosorial<-0 # fossorial activity (1) or not (0)
rainact<-0 # activity is limited by rainfall (1) or not (0)?
actrainthresh<-0 # threshold mm of rain causing activity if rainact=1
breedactthresh<-1 # threshold numbers of hours active after start of breeding season before eggs can be laid (simulating movement to the breeding site)
flyer<-1 # does the animal fly?
flyspeed<-1 # flying speed, m/s
flymetab<-0.1035 # flight metabolic excess, w/g

# containter simulation settings
container<-0 # run the container model? (aquatic start of life cycle, e.g. frog or mosquito)
conth<-36. # cylindrical container/pond height (cm)
contw<-30. # cylindrical container/pond diameter (cm)
contype<-0 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
rainmult<-1 # rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
continit<-0 # initial container water level (cm)
conthole<-0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
contonly<-0 # just run the container model and quit?
contwet<-100 # percent wet value for container
wetmod<-0 # run the wetland model?
soilmoisture<-0 # run the soil moisture model? (models near-surface soil moisture rather than a pond as a function of field capacity and wilting point)

# which energy budget model to use? 
DEB<-1 # run the DEB model (1) or just heat balance, using allometric respiration below (0)

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-4. # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

################### Dynamic Enregy Budget Model Parameters ################
debpars<-as.data.frame(read.csv('c:/DEB/Heteronympha/acceleration/DEB_pars_Heteronympha_merope.csv',header=FALSE))$V1
fract<-1
f<-1.
MsM<-186.03
z<-debpars[4]*fract
delta<-debpars[5]
kappa_X<-debpars[7]#0.85
v_dotref<-debpars[9]/24.
kappa<-debpars[10]
p_Mref<-debpars[12]/24.
E_G<-debpars[15]
k_R<-debpars[11]
k_J<-debpars[14]/24.
E_Hb<-debpars[16]*fract^3 
E_Hj<-E_Hb*fract^3 # E_Hegg for holometabolus insect
E_Hp<-debpars[20]*fract^3 # E_He for holometabolus insect
h_aref<-debpars[21]/(24.^2) #3.61e-11/(24.^2) 
s_G<-1e-4

E_Egg<-2.7278*fract^4# J, initial energy of one egg # this includes the residual yolk, which is eaten upon hatching
E_m<-(p_Mref*z/kappa)/v_dotref
p_Xm<-13290#12420 # J/h.cm2, maximum intake rate when feeding
K<-0.01 # half-saturation constant
X<-10 # food density J/cm2, approximation based on 200 Tetragonia berries per 1m2 (Dubasd and Bull 1990) assuming energy content of Lilly Pilly (http://www.sgapqld.org.au/bush_food_safety.pdf)

# for insect model
metab_mode<-2 # 0 = off, 1 = hemimetabolus model (to do), 2 = holometabolous model
stages<-8 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
y_EV_l<-debpars[19] # mol/mol, yield of imago reserve on larval structure
S_instar<-c(rep(debpars[17],4)) # -, stress at instar n: L_n^2/ L_n-1^2
s_j<-debpars[18] # -, reprod buffer/structure at pupation as fraction of max

# these next five parameters control the thermal response, effectively generating a thermal response curve
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-8215.
T_A1<-7143
T_A2<-5148
T_A3<-2543
T_A4<-1005
T_A5<-1843
T_A6<-4319
T_A7<-7219
T_A8<-7219
TAL<-50000.
TAH<-90000.
TL<-278.
TH<-312.

# life-stage specific parameters
arrhenius<-matrix(data = 0, nrow = 8, ncol = 5)
arrhenius[,1]<-TA # critical thermal minimum
arrhenius[,2]<-TAL # critical thermal maximum
arrhenius[,3]<-TAH # voluntary thermal minimum
arrhenius[,4]<-TL # voluntary thermal maximum
arrhenius[,5]<-TH # basking threshold 

arrhenius[1,1]<-T_A1 # critical thermal minimum
arrhenius[2,1]<-T_A2 # critical thermal minimum
arrhenius[3,1]<-T_A3 # critical thermal minimum
arrhenius[4,1]<-T_A4 # critical thermal minimum
arrhenius[5,1]<-T_A5 # critical thermal minimum
arrhenius[6,1]<-T_A6 # critical thermal minimum
arrhenius[7,1]<-T_A7 # critical thermal minimum
arrhenius[8,1]<-T_A8 # critical thermal minimum

#-----------------Setting thermal constraints---------------
thermal_stages<-matrix(data = 0, nrow = 8, ncol = 6)
thermal_stages[,1]<-ctmin # critical thermal minimum
thermal_stages[,2]<-ctmax # critical thermal maximum
thermal_stages[,3]<-TMINPR # voluntary thermal minimum
thermal_stages[,4]<-TMAXPR # voluntary thermal maximum
thermal_stages[,5]<-TBASK # basking threshold
thermal_stages[,6]<-TPREF # preferred body temperature


thermal_stages[8,1]<-12
thermal_stages[8,2]<-43
thermal_stages[8,3]<-17
thermal_stages[8,4]<-36
thermal_stages[8,5]<-13
thermal_stages[8,6]<-27
#make egg thermal tolerannt
#thermal_stages[1,2]<-45

#-----------------Setting Behavioural constraints---------------
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

# make adult diurnal
behav_stages[8,1]<-1
behav_stages[8,2]<-0

# restrict burrowing in egg and larvae
behav_stages[1,7]<-3 # egg 5cm max
behav_stages[2,7]<-6 # larvae 20 cm max
behav_stages[3,7]<-6
behav_stages[4,7]<-6
behav_stages[5,7]<-6
behav_stages[6,7]<-6

# make eggs burrow
behav_stages[1,1]<-0
behav_stages[1,2]<-0
behav_stages[1,3]<-0
behav_stages[1,11]<-0

# make pupal stage inactive
behav_stages[7,1]<-0
behav_stages[7,2]<-0
behav_stages[7,3]<-0
behav_stages[7,11]<-0

#-----------------Setting Desiccation constraints---------------
water_stages<-matrix(data = 0, nrow = 8, ncol = 8)
  
water_stages[,1]<-skinwet
water_stages[,2]<-extref
water_stages[,3]<-PFEWAT
water_stages[,4]<-PTUREA
water_stages[,5]<-FoodWater
water_stages[,6]<-minwater
water_stages[,7]<-raindrink
water_stages[,8]<-gutfill

water_stages[8,6]<-15
water_stages[8,7]<-.9

# composition related parameters
andens_deb<-1. # g/cm3, density of structure 
d_V<-0.2 # density of structure (reflects fraction of mass that is dry)
d_E<-0.2 # density of reserve (reflects fraction of mass that is dry)
eggdryfrac<-0.2 # decimal percent, dry mass of eggs
mu_X<-525000 # J/cmol, chemical potential of food
mu_E<-550000 # J/cmol, chemical potential of reserve
mu_V<-500000 # J/cmol, chemical potential of structure 
mu_P<-480000 # J/cmol, chemical potential of product (faeces)
kappa_X_P<-0.1 # fraction of food energy into faeces
nX<-c(1,1.8,0.5,.15) # composition of food (atoms per carbon atoms for CHON)
nE<-c(1,1.8,0.5,.15) # composition of reserve (atoms per carbon atoms for CHON)
nV<-c(1,1.8,0.5,.15) # composition of structure (atoms per carbon atoms for CHON)
nP<-c(1,1.8,0.5,.15) # composition of product/faeces (atoms per carbon atoms for CHON)
N_waste<-c(0,3,0,1) # chemical formula for nitrogenous waste product, CHON, e.g. Urea c(0,3,0,1), Uric acid c(5/5,4,3,4)

# breeding life history
clutchsize<-1 # clutch size
clutch_ab<-c(0,0) # paramters for relationship between length and clutch size: clutch size = a*SVL-b, make zero if fixed clutch size
viviparous<-0 # 1=yes, 0=no
batch<-1 # invoke Pequerie et al.'s batch laying model?

# the following four parameters apply if batch = 1, i.e. animal mobilizes
breedrainthresh<-0 # rain dependent breeder? 0 means no, otherwise enter rainfall threshold in mm
# photoperiod response triggering ovulation, none (0), summer solstice (1), autumnal equinox (2),  
# winter solstice (3), vernal equinox (4), specified daylength thresholds (5)
photostart<- 5 # photoperiod initiating breeding
photofinish<- 5 # photoperiod terminating breeding
daylengthstart<- 12.5 # threshold daylength for initiating breeding
daylengthfinish<- 11. # threshold daylength for terminating breeding
photodirs <- 0 # is the start daylength trigger during a decrease (0) or increase (1) in day length?
photodirf <- 0 # is the finish daylength trigger during a decrease (0) or increase (1) in day length?
startday<-90 # make it 90 for T. rugosa loop day of year at which DEB model starts
breedtempthresh<-200 # body temperature threshold below which breeding will occur
breedtempcum<-24*7 # cumulative time below temperature threshold for breeding that will trigger breeding

reset<-8 # reset options, 0=quit simulation upon death, 1=restart at emergence, 2=restart at first egg laid, 3=restart at end of breeding season, 4=reset at death

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
# v_init<-(debpars[22]^3)*fract^3 #hatchling
# E_init<-E_m
# E_H_init<-E_Hb+5
# stage<-1
# v_init<-(debpars[23]^3)*fract^3*0.85
# E_init<-E_m
# E_H_init<-E_Hp+1
# stage<-3

# mortality rates
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year
wilting<-1
ystrt<-0

write_input<-0

path<-"c:/NicheMapR_Debug/ecto_source/build/"
setwd(path)
flist <- list.files(path, full.names = TRUE)
file.copy(flist, "c:/git/ectotherm/source", overwrite = TRUE)
setwd(maindir)
setwd("source/") # set the working directory where the fortran program is
#R CMD SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f
cmnd<- "rcmd SHLIB ectotherm.f dopri5.f dget_aELE.f SOLOUT.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

setwd("c:/git/ectotherm/")
#set up call to NicheMapR function
source('NicheMapR_Setup_ecto.R')
nicheout<-NicheMapR_ecto(niche)

# retrieve output
metout<-as.data.frame(nicheout$metout)
shadmet<-as.data.frame(nicheout$shadmet)
soil<-as.data.frame(nicheout$soil)
shadsoil<-as.data.frame(nicheout$shadsoil)
rainfall<-as.data.frame(nicheout$RAINFALL)
grassgrowths<-as.data.frame(nicheout$grassgrowths)
grasstsdms<-as.data.frame(nicheout$grasstsdms)
environ<-as.data.frame(nicheout$environ)
enbal<-as.data.frame(nicheout$enbal)
masbal<-as.data.frame(nicheout$masbal)

yearout<-as.data.frame(nicheout$yearout)
if(nyears>1){
  yearsout<-as.data.frame(nicheout$yearsout)
}else{
  yearsout<-t(as.data.frame(nicheout$yearsout))
}
if(container==1){
  wetlandTemps<-as.data.frame(environ$WATERTEMP)
  wetlandDepths<-as.data.frame(environ$CONDEP)
}

# append dates
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
if(DEB==1){
  debout<-as.data.frame(nicheout$debout[1:(365*24*nyears),])
  debout<-cbind(dates,debout)
}
environ<-cbind(dates,environ)
masbal<-cbind(dates,masbal)
enbal<-cbind(dates,enbal)
soil<-cbind(dates,soil)
metout<-cbind(dates,metout)
shadsoil<-cbind(dates,shadsoil)
shadmet<-cbind(dates,shadmet)

dates2<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
grass<-cbind(dates2,grassgrowths,grasstsdms)
colnames(grass)<-c("dates","growth","tsdm")
rainfall<-as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)<-c("dates","rainfall")


############### plot results ######################
library(lattice)
library('latticeExtra')
# environ<-subset(environ,VEL!=0)
# plotenviron<-subset(environ,YEAR==1)
# plotenviron<-environ
# forage<-subset(plotenviron,ACT==2)
# bask<-subset(plotenviron,ACT==1)
# night<-subset(metout,ZEN==90)
# with(night,plot(TIME/60~JULDAY,pch=15,cex=.5,ylab='hour of day',ylim=c(0,23),xlab='day of year'))
# with(bask,points((TIME-1)~DAY,pch=15,cex=.5,col='blue'))
# with(forage,points((TIME-1)~DAY,pch=15,cex=.5,col='grey'))


plotdebout<-subset(debout,as.numeric(format(debout$dates, "%Y"))<1994)
#plotdebout<-debout[320:720,]
# plotdebout$days<-plotdebout$DAY-1+plotdebout$TIME/24-1/24
# plot(plotdebout$WETMASS~plotdebout$days,type='l')
# offset<-60/24
# tWWT<-read.csv('Aedes//time_wwt.csv')
# tWWT$days<-tWWT$days+offset
# points(tWWT$wwt/1000~tWWT$days)
# plot(tWWT$wwt~tWWT$days)




with(plotdebout, {xyplot(WETMASS~dates,type = "p",groups=Stage,auto.key=list(columns = 5),ylim=c(-0.01,0.35))})


xyplot(plotdebout$V^(1/3)*delta*10~plotdebout$dates,type = "p",groups=plotdebout$Stage,auto.key=list(columns = 5),panel = function(x, y) {
         panel.xyplot(x, y)
         panel.abline(3.295979,0)
       })

as.layer(xyplot(abline(3.295979,0)))
with(plotdebout, {xyplot(WETMASS~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(SVL~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})

with(plotdebout, {xyplot(V^(1/3)*delta*10~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(V*RESERVE_DENS~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(RESERVE_DENS~dates,ylim=c(0,10000),type = "p",groups=Stage,auto.key=list(columns = 5))})

with(plotdebout, {xyplot(E_H~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})

with(plotdebout, {xyplot(WETMASS_STD~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})


with(plotdebout, {xyplot(CUMREPRO~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(CUMBATCH~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, plot(CUMREPRO/V~dates,type = "l"))
abline(507.0445,0)
with(plotdebout, plot(WETMASS~dates,type = "l"))
plot(debout$RESERVE_DENS,type='l',ylim=c(0,20000))
environ<-as.data.frame(environ)
plotenviron <- environ[1:(24*365*nyears),]
#plotenviron <- environ[(24*365*1):(24*365*4),]
with(plotenviron, {xyplot(TC+FLYING*15+SHADE/10+DEP/10~dates,type = "l")})
with(plotenviron, {xyplot(TC~dates,type = "l")})
with(plotenviron, {xyplot(DAYLENGTH~dates,type = "l")})

with(plotenviron, {xyplot(FLYTIME+FLYING*5+SHADE/10+DEP/10~dates,type = "l",scales=list(tick.number=nyears*12))})




with(environ,plot(CONDEP~dates,type='l'))
points(rainfall$rainfall~rainfall$dates,type='h',col='blue')
with(environ,plot(WATERTEMP~dates,type='l'))

  
with(plotdebout, {xyplot(WETMASS~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(MASS_GUT~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(RESERVE_DENS~dates,type = "p",ylim=c(0,500000),groups=Stage,auto.key=list(columns = 5))})
with(plotdebout, {xyplot(V~dates,type = "p",groups=Stage,auto.key=list(columns = 5))})


maxgen<-aggregate(debout$Stage,by=list((substr(debout$dates,1,10))),max)
maxgen$x[maxgen$x>6]<-6
julday<-aggregate(debout$JULDAY,by=list((substr(debout$dates,1,10))),max)
summer.years<-c(as.numeric(substr(julday$Group.1,1,4))-1,rep(yfinish,365))
summer.years<-summer.years[185:(185+365*nyears-1)]
pond<-aggregate(environ$CONDEP,by=list((substr(debout$dates,1,10))),max)
pond.temp<-aggregate(environ$WATERTEMP,by=list((substr(debout$dates,1,10))),max)
pond.rh<-aggregate(environ$RELHUM,by=list((substr(debout$dates,1,10))),max)
pond$x[pond$x<=0.1]<-0

maxgens<-cbind(julday,summer.years,maxgen$x,pond$x)
colnames(maxgens)<-c('date','julday','year','maxgen','maxdep')
numgens.summer<-subset(maxgens,maxgen==6)
numgens.summer<-aggregate(numgens.summer$maxgen,by=list(numgens.summer$year),length)


max.stage<-aggregate(debout$Stage,by=list((substr(debout$dates,1,10))),max)
max.stage$x[max.stage$x>6]<-6
pond.dep<-aggregate(environ$CONDEP,by=list((substr(debout$dates,1,10))),min)
pond.temp<-aggregate(environ$WATERTEMP,by=list((substr(debout$dates,1,10))),mean)
pond.rh<-aggregate(environ$RELHUM,by=list((substr(debout$dates,1,10))),mean)


blank<-matrix(nrow=nrow(maxgen),ncol=10,data=0)
popdyn<-cbind(pond.dep,pond.temp$x,pond.rh$x,max.stage$x,blank)
popdyn<-as.data.frame(popdyn)
colnames(popdyn)<-c('date','depth','temp','rh','stage','gen','diapause','days','sumTW','sumRH','meanTW','meanRH','eggmort','N','Nfinal')
popdyn$date<-as.POSIXct(max.stage$Group.1)
for(i in 1:(nrow(popdyn)-1)){
  if(popdyn[i,5]>=6){
    popdyn[i,6]<-1
  }
  if(popdyn[i,2]<=0.1){
    popdyn[i,7]<-1
  }
  if(i==1){
    popdyn[i,8]<-1
    popdyn[i,9]<-popdyn[i,3]
    popdyn[i,10]<-popdyn[i,4]
  }else{
    if(popdyn[i,7]==1){
    popdyn[i,8]<-popdyn[(i-1),8]+1
    popdyn[i,9]<-popdyn[(i-1),9]+popdyn[i,3]
    popdyn[i,10]<-popdyn[(i-1),10]+popdyn[i,4]   
    }
  }
  if(popdyn[i,8]>0){
    popdyn[i,11]<-popdyn[i,9]/popdyn[i,8]
    popdyn[i,12]<-popdyn[i,10]/popdyn[i,8]
  }
}

N0 <- 1000 #starting population size
K <- 1000 #carrying capacity
R0<-1.5
#N<-N*R0**(1-N/K) #Ricker model for population growth to obtain new N


for(i in 1:(nrow(popdyn)-1)){
if(popdyn[i,7]==1 &popdyn[(i+1),7]==0){
  popdyn[i,13]<-exp(-6.402+0.088*popdyn[i,12]-0.141*popdyn[i,11]-0.049*popdyn[i,8]-0.0016*popdyn[i,8]*popdyn[i,12]+0.0073*popdyn[i,11]*popdyn[i,8]+0.0001*popdyn[i,12]*popdyn[i,11])/(exp(-6.402+0.088*popdyn[i,12]-0.141*popdyn[i,11]-0.049*popdyn[i,8]-0.0016*popdyn[i,8]*popdyn[i,12]+0.0073*popdyn[i,11]*popdyn[i,8]+0.0001*popdyn[i,12]*popdyn[i,11])+1)
}
if(i==1){
  popdyn[i,14]<-N0
  popdyn[i,15]<-N0
}else{
  if(popdyn[i,6]==0){
   popdyn[i,14]<-popdyn[(i-1),14]*(1-popdyn[i,13])
  }else{
   popdyn[i,14]<-popdyn[(i-1),14]*(R0*popdyn[i,6])^(1-popdyn[(i-1),14]/K)
  }
  if(popdyn[i,2]<=0.1){
   popdyn[i,15]<-popdyn[i,14]*(1-exp(-6.402+0.088*popdyn[i,12]-0.141*popdyn[i,11]-0.049*popdyn[i,8]-0.0016*popdyn[i,8]*popdyn[i,12]+0.0073*popdyn[i,11]*popdyn[i,8]+0.0001*popdyn[i,12]*popdyn[i,11])/(exp(-6.402+0.088*popdyn[i,12]-0.141*popdyn[i,11]-0.049*popdyn[i,8]-0.0016*popdyn[i,8]*popdyn[i,12]+0.0073*popdyn[i,11]*popdyn[i,8]+0.0001*popdyn[i,12]*popdyn[i,11])+1))
  }else{
   popdyn[i,15]<-popdyn[i,14]
  }
}
}

latitude<-rep(as.numeric(longlat[2]),nyears*365)
longitude<-rep(as.numeric(longlat[1]),nyears*365) 
popdyn<-as.data.frame(cbind(latitude,longitude,popdyn))


setwd("/hsm/VR0212/shared/NicheMapR_Working/projects/aedes/")

#colnames(popdyn)<-c('date','depth','temp','rh','stage','gen','diapause','days','sumTW','sumRH','meanTW','meanRH','eggmort','N')
maxmort<-aggregate(popdyn$eggmort,by=list((substr(popdyn$date,1,10))),max)
maxN<-aggregate(popdyn$Nfinal,by=list((substr(popdyn$date,1,10))),max)
maxNs<-cbind(summer.years,maxmort$x,maxN$x)
colnames(maxNs)<-c('year','maxmort','maxN')
popdyn2<-cbind(summer.years,popdyn)
maxN.summer<-aggregate(popdyn2$Nfinal,by=list(summer.years),max)
meanN.summer<-aggregate(popdyn2$Nfinal,by=list(summer.years),mean)
maxMort.summer<-aggregate(popdyn2$eggmort,by=list(summer.years),max)

allyears<-as.data.frame(matrix(nrow=21,ncol=2,data=0))
allyears$V1<-seq(1989,2009,1)
all.numgen<-allyears
all.maxN<-allyears
all.meanN<-allyears
all.maxMort<-allyears
for(i in 1:21){
 for(j in 1:nrow(numgens.summer)){
  if(numgens.summer[j,1]==allyears[i,1]){
    all.numgen[i,2]<-numgens.summer[j,2]
  }
}
}
numgens.summer2<-cbind(longlat[2],longlat[1],t(all.numgen$V2))
write.table(numgens.summer2, file = "numgens.summer.csv", sep = ",", col.names = F, qmethod = "double", append = T)

for(i in 1:21){
 for(j in 1:nrow(numgens.summer)){
  if(meanN.summer[j,1]==allyears[i,1]){
    all.meanN[i,2]<-round(meanN.summer[j,2],digits=1)
  }
}
}
meanN.summer2<-cbind(longlat[2],longlat[1],t(all.meanN$V2))
write.table(meanN.summer2, file = "meanN.summer.csv", sep = ",", col.names = F, qmethod = "double", append = T)

for(i in 1:21){
 for(j in 1:nrow(numgens.summer)){
  if(numgens.summer[j,1]==allyears[i,1]){
    all.maxN[i,2]<-round(maxN.summer[j,2],digits=1)
  }
}
}
maxN.summer2<-cbind(longlat[2],longlat[1],t(all.numgen$V2))
write.table(maxN.summer2, file = "maxN.summer.csv", sep = ",", col.names = F, qmethod = "double", append = T)

for(i in 1:21){
 for(j in 1:nrow(numgens.summer)){
  if(maxMort.summer[j,1]==allyears[i,1]){
    all.maxMort[i,2]<-round(maxMort.summer[j,2],digits=4)
  }
}
}
maxMort.summer2<-cbind(longlat[2],longlat[1],t(all.maxMort$V2))
write.table(maxMort.summer2, file = "maxMort.summer.csv", sep = ",", col.names = F, qmethod = "double", append = T)



popdyn$date<-as.numeric(as.POSIXct(popdyn[,3]))
popdyn$latitude<-round(popdyn$latitude,digits=2)
popdyn$longitude<-round(popdyn$longitude,digits=2)
popdyn$dep<-round(popdyn$dep,digits=2)
popdyn$temp<-round(popdyn$temp,digits=1)
popdyn$rh<-round(popdyn$rh,digits=1)
popdyn$stage<-round(popdyn$stage,digits=1)
popdyn$sumTW<-round(popdyn$sumTW,digits=1)
popdyn$sumRH<-round(popdyn$sumRH,digits=1)
popdyn$meanTW<-round(popdyn$meanTW,digits=1)
popdyn$meanRH<-round(popdyn$meanRH,digits=1)
popdyn$eggmort<-round(popdyn$eggmort,digits=1)
popdyn$N<-round(popdyn$N,digits=1)
popdyn$Nfinal<-round(popdyn$Nfinal,digits=1)
popdyn2<-cbind(popdyn$latitude,popdyn$longitude,popdyn$date,popdyn$depth,popdyn$temp,popdyn$rh,popdyn$stage,popdyn$gen,popdyn$diapause,popdyn$days,popdyn$meanTW,popdyn$meanRH,popdyn$eggmort,popdyn$Nfinal)


