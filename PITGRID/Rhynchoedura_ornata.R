############# ectotherm model parameters ################################

setwd("source/") # set the working directory where the fortran program is
cmnd<- "rcmd SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
#R CMD SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

# get input microclimate files
file.copy('/git/micro_australia/roxby_metout.csv','metout.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_shadmet.csv','shadmet.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_soil.csv','soil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_shadsoil.csv','shadsoil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_rainfall.csv','rainfall.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_ectoin.csv','ectoin.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_DEP.csv','DEP.csv',overwrite=TRUE)
file.copy('/git/micro_australia/roxby_MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)

microin<-"" # subfolder containing the microclimate input data

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
timeinterval<-365 # number of time intervals in a year
ystart<-read.csv('ectoin.csv')[7,2]
yfinish<-read.csv('ectoin.csv')[8,2]
nyears<-ceiling(nrow(read.csv('rainfall.csv'))/365) # number of years the simulation runs for (work out from input data)
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv('ectoin.csv')[3,2],read.csv('ectoin.csv')[4,2])
grasshade<-0 # use grass shade values from microclimate model as min shade values (1) or not (0)? (simulates effect of grass growth on shading, as a function of soil moisture)

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
soilnode<-7. # soil node at which eggs are laid (overridden if frogbreed is 1)
minshade<-0. # minimum available shade (percent)
maxshade<-70. # maximum available shade (percent)
REFL<-rep(0.18,timeinterval*nyears) # substrate reflectances 

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
TMAXPR<-37 # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection) # Licht 1966 thermal gradient
TMINPR<-12 # degrees C, voluntary thermal minimum (lower body temperature for foraging) # Kearney Obs (PhD field trip)
TBASK<-10 # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-10.5 # degrees C, temperature at which animal will move to a basking site
ctmax<-45.  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-6. # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-0 #if 1, animal dies when it hits critical thermal limits
TPREF<-36 # preferred body temperature (animal will attempt to regulate as close to this value as possible) # Licht 1966 thermal gradient
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.1 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-0. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
gutfill<-75. # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage

# behavioural traits
dayact<-0 # diurnal activity allowed (1) or not (0)?
nocturn<-1 # nocturnal activity allowed (1) or not (0)?
crepus<-1 # crepuscular activity allowed (1) or not (0)?
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
amass<-5. # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

################### Dynamic Enregy Budget Model Parameters ################

fract<-1
f<-1.
MsM<-186.03*6. # J/cm3 produces a stomach volume of 5.3 cm3/100 g, as measured for Disosaurus dorsalis, adjusted for Egernia cunninghami
z<-1.306*fract
delta<- 0.226
kappa_X<-0.85#0.85
v_dotref<-0.03374/24.
kappa<-0.9193 
p_Mref<-79.57/24.
E_G<-6922
k_R<-0.95
k_J<-0.0115/24.
E_Hb<-72*fract^3
E_Hj<-E_Hb*fract^3
E_Hp<-353.4*fract^3
h_aref<-3.568e-07/(24.^2) #3.61e-11/(24.^2) 
s_G<-0.01

E_Egg<-1518*fract^3# J, initial energy of one egg # this includes the residual yolk, which is eaten upon hatching
svl_met<-11 # mm, snout vent length at metamorphosis
E_m<-(p_Mref*z/kappa)/v_dotref
p_Xm<-13290#12420 # J/h.cm2, maximum intake rate when feeding
K<-0.01 # half-saturation constant
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
TA<-8816.5
TAL<-5e+04
TAH<-9e+04
TL<-14+273
TH<-40+273

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
eggmass<-3.787 # initial dry mass of an egg (g)
viviparous<-1 # 1=yes, 0=no
batch<-1 # invoke Pequerie et al.'s batch laying model?

# the following four parameters apply if batch = 1, i.e. animal mobilizes
breedrainthresh<-0 # rain dependent breeder? 0 means no, otherwise enter rainfall threshold in mm
# photoperiod response triggering ovulation, none (0), summer solstice (1), autumnal equinox (2),  
# winter solstice (3), vernal equinox (4), specified daylength thresholds (5)
photostart<- 3 # photoperiod initiating breeding
photofinish<- 2 # photoperiod terminating breeding
daylengthstart<- 12.5 # threshold daylength for initiating breeding
daylengthfinish<- 13. # threshold daylength for terminating breeding
photodirs <- 1 # is the start daylength trigger during a decrease (0) or increase (1) in day length?
photodirf <- 0 # is the finish daylength trigger during a decrease (0) or increase (1) in day length?
startday<-30*11 # make it 90 for T. rugosa loop day of year at which DEB model starts
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
# v_init<-(0.4525^3)*fract^3 #hatchling
# E_init<-E_m
# E_H_init<-E_Hb+5
# stage<-1
#v_init<-(1.271^3)*fract^3*0.85
# E_init<-E_m
# E_H_init<-E_Hp+1
# stage<-3

# mortality rates
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year
wilting<-1
ystrt<-0

setwd("c:/git/ectotherm/")
#set up call to NicheMapR function
niche<-list(wilting=wilting,ystrt=ystrt,soilmoisture=soilmoisture,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,svl_met=svl_met,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,eggmass=eggmass,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,p_Am1=p_Am1,p_AmIm=p_AmIm,arrhenius=arrhenius,disc=disc,gam=gam,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,stage=stage,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet,microin=microin,mac=mac,grasshade=grasshade)
source('NicheMapR_Setup_ecto.R')
nicheout<-NicheMapR_ecto(niche)

# retrieve output
metout<-as.data.frame(nicheout$metout)[1:(nyears*365*24),]
shadmet<-as.data.frame(nicheout$shadmet)[1:(nyears*365*24),]
soil<-as.data.frame(nicheout$soil)[1:(nyears*365*24),]
shadsoil<-as.data.frame(nicheout$shadsoil)[1:(nyears*365*24),]
rainfall<-as.data.frame(nicheout$RAINFALL)
grassgrowths<-as.data.frame(nicheout$grassgrowths)
grasstsdms<-as.data.frame(nicheout$grasstsdms)
environ<-as.data.frame(nicheout$environ[1:(365*24*nyears),])
enbal<-as.data.frame(nicheout$enbal[1:(365*24*nyears),])
masbal<-as.data.frame(nicheout$masbal[1:(365*24*nyears),])

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
plotdebout<-subset(debout,as.numeric(format(debout$dates, "%Y"))<1994)
plot(plotdebout$WETMASS~plotdebout$dates,type='l')
plot(debout$CUMBATCH~debout$dates,type='l')
points(debout$CUMREPRO~debout$dates,type='l',col='red')

subdate<-subset(environ, format(environ$dates,"%y/%m")=="00/03")
subdate<-environ
with(subdate, plot(TC~dates,ylim=c(-15,50),type = "l",col='blue'))
with(subdate, points(ACT*5~dates,type = "l",col='pink'))
with(subdate, points(SHADE/10~dates,type = "l",col='dark green'))
with(metout,points(TALOC~dates,type='l',col='green'))
with(rainfall,points(rainfall~dates,type='h',col='light blue'))
abline(20,0,col='grey',lty=2)


T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-8816.5
TAL<-50000
TAH<-90000
TL<-273+14
TH<-273+40
TC<-environ$TC
summer<-subset(environ, format(environ$dates, "%m")== "10" | format(environ$dates, "%m")== "11" | format(environ$dates, "%m")== "12" | format(environ$dates, "%m")== "01" | format(environ$dates, "%m")== "02" | format(environ$dates, "%m")== "03" )
growth<-subset(environ, format(environ$dates, "%m")== "01" | format(environ$dates, "%m")== "02" | format(environ$dates, "%m")== "03" | format(environ$dates, "%m")== "04" | format(environ$dates, "%m")== "05" | format(environ$dates, "%m")== "06" | format(environ$dates, "%m")== "07" | format(environ$dates, "%m")== "08" | format(environ$dates, "%m")== "09" | format(environ$dates, "%m")== "10" )
summer_soil<-subset(soil, format(soil$dates, "%m")== "12")
TCsumm<-summer$TC
TCgrowth<-growth$TC
TCsoil<-summer_soil$D30cm

TC<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TC)))/(1+exp(TAL*(1/(273+TC)-1/TL))+exp(TAH*(1/TH-1/(273+TC)))))
plot(TC~dates)
mean(TC)

Tb<-20.5# put your guess in here and then run the next line to see what TC that implies
exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))

TC<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TCsumm)))/(1+exp(TAL*(1/(273+TCsumm)-1/TL))+exp(TAH*(1/TH-1/(273+TCsumm)))))
plot(TC~summer$dates)
mean(TC)

Tb<-25 # put your guess in here and then run the next line to see what TC that implies
exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))

TC<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TCsoil)))/(1+exp(TAL*(1/(273+TCsoil)-1/TL))+exp(TAH*(1/TH-1/(273+TCsoil)))))
plot(TC~summer$dates)
mean(TC)

Tb<-27.2 # put your guess in here and then run the next line to see what TC that implies
exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))

TC<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+TCgrowth)))/(1+exp(TAL*(1/(273+TCgrowth)-1/TL))+exp(TAH*(1/TH-1/(273+TCgrowth)))))
plot(TC~summer$dates)
mean(TC)

Tb<-27.2 # put your guess in here and then run the next line to see what TC that implies
exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))
addTrans <- function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}


setwd("c:/NicheMapR_Working/projects/pitgrid/") # go to the folder with the pitgrid dataset
pitgrid_raw<-read.csv('pitgrid_raw.csv') # read in raw pitgrid dataset
colnames(pitgrid_raw)<-c('Survey','Date','Species.Code','Site.Pit','Mark','Recapture','SSR','Age','Sex','SVLmm','Tailmm','Regenerated','Weightgm','Comments','Recapture.Comments','Counter','Valid.Data')
pitgrid_raw$Date<-strptime(pitgrid_raw$Date, "%d/%m/%Y")
pitgrid_raw$Date<-format(pitgrid_raw$Date, "%d/%m/%Y")


trim <- function (x) gsub("^\\s+|\\s+$", "", x)
pitgrid_raw$Species.Code <- gsub('/', '_', pitgrid_raw$Species.Code) # remove slashes
pitgrid_raw$Species.Code <- gsub(' ', '_', pitgrid_raw$Species.Code) # remove spaces
pitgrid_raw$Species.Code <- gsub('sp.', 'sp', pitgrid_raw$Species.Code) # remove full stops
pitgrid_raw$Species.Code<-trim(pitgrid_raw$Species.Code) #get rid of leading and trailing white space in species names
species_list<-unique(pitgrid_raw$Species.Code) # get the list of species in the data set

species<-"Rhynchoedura_ornata"
data<-pitgrid_raw
a<-(pitgrid_raw$Species.Code==species)==TRUE
a[a==TRUE]<-1
a[a==FALSE]<-0
data<-cbind(pitgrid_raw,a)
data$dates<-as.POSIXct(as.Date(data$Date,"%d/%m/%Y"))-3600*6-3600*24
data<-data[,18:19]
data_count<-aggregate(data$a,by=list(data$dates),sum)


colnames(data_count)<-c("dates","count")
plotenviron<-as.data.frame(environ)
plotenviron1<-merge(data_count,plotenviron,all.y = TRUE)

for(i in 1991:2009){
yeartoplot<-as.character(i)
plotenviron2 <-subset(plotenviron1, format(plotenviron$dates, "%Y/%m")==paste(yeartoplot,"/11",sep=""))
plotenviron_night<-subset(metout,ZEN!=90)
plotenviron_night$TIME<-plotenviron_night$TIME/60
plotenviron_night$DAY<-plotenviron_night$JULDAY+(as.numeric(format(plotenviron_night$dates, '%Y'))-1990)*365
plotenviron_night<-subset(plotenviron_night,format(plotenviron_night$dates, "%Y/%m")==paste(yeartoplot,"/11",sep=""))
plotenviron_bask <- subset(plotenviron2,  subset=(ACT>=1 & TC>=TMINPR))
plotenviron_forage <- subset(plotenviron2,  subset=(ACT>1))
plotenviron_trap <- subset(plotenviron2,  subset=(ACT>=1 & SHADE==0 & TC>=TMINPR))
plotenviron_presence<-subset(plotenviron2,plotenviron2$count>0)
plotenviron_absence<-subset(plotenviron2,plotenviron2$count==0)

with(plotenviron_night, {plot(TIME~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col=addTrans("dark grey",50),pch=16,main=i)})
with(plotenviron_trap, {points((TIME-1)~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='black',pch=15)})
with(plotenviron_presence, {points((TIME-1)~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='blue',pch=15)})
with(plotenviron_absence, {points((TIME-1)~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='red',pch=15)})

plotrainfall <- subset(rainfall,format(dates, "%Y")==yeartoplot)
doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
plotrainfall<-cbind(plotrainfall,doy2)
plotrainfall$DAY<-plotrainfall$doy2+(as.numeric(format(plotrainfall$dates, '%Y'))-1990)*365

with(plotrainfall, {points(rainfall~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",col='light blue',type='h')})
with(plotenviron_presence, {points(count~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",type='l',col='green')})

}

act<-aggregate(plotenviron1$ACT,by=list(format(plotenviron1$dates, "%y/%m/%d")),sum)
act2<-aggregate(plotenviron1$count,by=list(format(plotenviron1$dates, "%y/%m/%d")),max,na.rm=TRUE)
act3<-cbind(act$Group.1,act$x,act2$x)
act3<-as.data.frame(act3)
act3$V1<-as.Date(act3$V1,"%y/%m/%d")
act4<-subset(act3,act3$V3!="-Inf")
act4$V2<-as.numeric(as.character(act4$V2))
act4$V3<-as.numeric(as.character(act4$V3))
colnames(act4)<-c('date','pred','obs')
plot(act4$pred~act4$obs)
#dailymin<-aggregate(metout$TALOC,by=list(format(metout$dates, "%y/%m/%d")),min)
#aa<-subset(dailymin,x>26)
#plot(aa$x~as.Date(aa$Group.1,"%y/%m/%d",ylim=c(26,35),xlim=c(min(metout$dates),max(metout$dates))),type='p')


speciestocheck<-"Menetia greyii"

# temp of day before

# histogram of max air temps for all captures
alldata<-read.csv('c:/NicheMapR_Working/projects/pitgrid/cause_of_death.csv')
alldata<-subset(alldata,Species.Code==speciestocheck)
max_air<-aggregate(metout$TAREF,by=list(format(dates,"%y/%m/%d")),max)
max_air$Group.1<-as.Date(format(as.Date(max_air$Group.1,"%y/%m/%d"),"%d/%m/%Y"),"%d/%m/%Y")
colnames(max_air)<-c("Date","Tmax")
max_air$Date<-max_air$Date-1 # get day before
max_air$Date<-format(max_air$Date,"%d/%m/%Y")
alldata_temperature<-merge(max_air,alldata)
hist(alldata_temperature$Tmax,main=paste(speciestocheck,"Tmax day before"))
histo<-hist(alldata_temperature$Tmax,plot=FALSE)

# histogram of max air temps for all deaths
alldata<-read.csv('c:/NicheMapR_Working/projects/pitgrid/cause_of_death.csv')
alldata<-alldata[1:518,]
alldata<-subset(alldata,cause.of.death=="unknown")
alldata<-subset(alldata,Species.Code==speciestocheck)
max_air<-aggregate(metout$TAREF,by=list(format(dates,"%y/%m/%d")),max)
max_air$Group.1<-as.Date(format(as.Date(max_air$Group.1,"%y/%m/%d"),"%d/%m/%Y"),"%d/%m/%Y")
colnames(max_air)<-c("Date","Tmax")
max_air$Date<-max_air$Date-1 # get day before
max_air$Date<-format(max_air$Date,"%d/%m/%Y")
alldata_temperature<-merge(max_air,alldata)
hist(alldata_temperature$Tmax,add=TRUE,col='red',breaks=histo$breaks)


# temp of day trap checked

# histogram of max air temps for all captures
alldata<-read.csv('c:/NicheMapR_Working/projects/pitgrid/cause_of_death.csv')
alldata<-subset(alldata,Species.Code==speciestocheck)
max_air<-aggregate(metout$TAREF,by=list(format(dates,"%y/%m/%d")),max)
max_air$Group.1<-as.Date(format(as.Date(max_air$Group.1,"%y/%m/%d"),"%d/%m/%Y"),"%d/%m/%Y")
colnames(max_air)<-c("Date","Tmax")
max_air$Date<-max_air$Date # get day before
max_air$Date<-format(max_air$Date,"%d/%m/%Y")
alldata_temperature<-merge(max_air,alldata)
hist(alldata_temperature$Tmax,main=paste(speciestocheck,"Tmax day checked"))
histo<-hist(alldata_temperature$Tmax,plot=FALSE)

# histogram of max air temps for all deaths
alldata<-read.csv('c:/NicheMapR_Working/projects/pitgrid/cause_of_death.csv')
alldata<-alldata[1:518,]
alldata<-subset(alldata,cause.of.death=="unknown")
alldata<-subset(alldata,Species.Code==speciestocheck)
max_air<-aggregate(metout$TAREF,by=list(format(dates,"%y/%m/%d")),max)
max_air$Group.1<-as.Date(format(as.Date(max_air$Group.1,"%y/%m/%d"),"%d/%m/%Y"),"%d/%m/%Y")
colnames(max_air)<-c("Date","Tmax")
max_air$Date<-max_air$Date # get day before
max_air$Date<-format(max_air$Date,"%d/%m/%Y")
alldata_temperature<-merge(max_air,alldata)
hist(alldata_temperature$Tmax,add=TRUE,col='red',breaks=histo$breaks)

