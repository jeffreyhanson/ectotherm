############# ectotherm model parameters ################################

setwd("source/") # set the working directory where the fortran program is
cmnd<- "rcmd SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
#R CMD SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

# get input microclimate files
file.copy('/git/micro_australia/metout.csv','metout.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadmet.csv','shadmet.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soil.csv','soil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadsoil.csv','shadsoil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soilmoist.csv','shadmoist.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soilpot.csv','soilpot.csv',overwrite=TRUE)
file.copy('/git/micro_australia/rainfall.csv','rainfall.csv',overwrite=TRUE)
file.copy('/git/micro_australia/ectoin.csv','ectoin.csv',overwrite=TRUE)
file.copy('/git/micro_australia/DEP.csv','DEP.csv',overwrite=TRUE)
file.copy('/git/micro_australia/MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)

# file.copy('/git/micro_global/metout.csv','metout.csv',overwrite=TRUE)
# file.copy('/git/micro_global/shadmet.csv','shadmet.csv',overwrite=TRUE)
# file.copy('/git/micro_global/soil.csv','soil.csv',overwrite=TRUE)
# file.copy('/git/micro_global/shadsoil.csv','shadsoil.csv',overwrite=TRUE)
# file.copy('/git/micro_global/rainfall.csv','rainfall.csv',overwrite=TRUE)
# file.copy('/git/micro_global/ectoin.csv','ectoin.csv',overwrite=TRUE)
# file.copy('/git/micro_global/DEP.csv','DEP.csv',overwrite=TRUE)
# file.copy('/git/micro_global/MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)

microin<-"" # subfolder containing the microclimate input data

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism) or dead animal?
enberr<-0.0002 # tolerance for energy balance
timeinterval<-365 # number of time intervals in a year
ystart<-read.csv('ectoin.csv')[7,2]
yfinish<-read.csv('ectoin.csv')[8,2]
nyears<-ceiling(nrow(read.csv('rainfall.csv'))/365) # number of years the simulation runs for (work out from input data)
write_input<-1 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv('ectoin.csv')[3,2],read.csv('ectoin.csv')[4,2])
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
ctkill<-0 #if 1, animal dies when it hits critical thermal limits
TPREF<-33.5 # preferred body temperature (animal will attempt to regulate as close to this value as possible)
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.19 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
minwater<-10 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-5. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
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
soilmoisture<-1 # run the soil moisture model? (models near-surface soil moisture rather than a pond as a function of field capacity and wilting point)

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
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-7130
TAL<-5.305e+04
TAH<-9.076e+04
TL<-288.
TH<-315.

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
startday<-1 # make it 90 for T. rugosa loop day of year at which DEB model starts
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
v_init<-(7.063^3)*fract^3*0.85
E_init<-E_m
E_H_init<-E_Hp+1
stage<-3

# mortality rates
ma<-1e-4  # hourly active mortality rate (probability of mortality per hour)
mi<-0  # hourly inactive mortality rate (probability of mortality per hour)
mh<-0.5   # survivorship of hatchling in first year

ystrt<-0 # year to start the simulation (if zero, starts at first year, but if greater than 1, runs at year ystart+1 and then loops back to the rest after)
#set up call to NicheMapR function
niche<-list(clutch_ab=clutch_ab,wilting=wilting,ystrt=ystrt,soilmoisture=soilmoisture,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,p_Am1=p_Am1,p_AmIm=p_AmIm,arrhenius=arrhenius,disc=disc,gam=gam,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,stage=stage,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet,microin=microin,mac=mac,grasshade=grasshade)
source('NicheMapR_Setup_ecto.R')
nicheout<-NicheMapR_ecto(niche)

# retrieve output
metout<-as.data.frame(nicheout$metout)[1:(nyears*365*24),]
shadmet<-as.data.frame(nicheout$shadmet)[1:(nyears*365*24),]
soil<-as.data.frame(nicheout$soil)[1:(nyears*365*24),]
shadsoil<-as.data.frame(nicheout$shadsoil)[1:(nyears*365*24),]
rainfall<-as.data.frame(nicheout$RAINFALL)
grassgrowths<-as.data.frame(nicheout$grassgrowths)[1:(nyears*365),]
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


#setwd('/NicheMapR_Working/projects/Sleepy Lizards/')
# environ_write<-cbind(environ,debout[,6:21])
# if(raindrink==0){
#   write.csv(environ_write,'environ_drink.csv')
# }else{
#   write.csv(environ_write,'environ_nodrink.csv')
# }


plotrainfall <- subset(rainfall,format(dates, "%y")>0)
#with(plotrainfall,plot(rainfall~dates,type='l',col='blue'))

plotmetout2<-subset(metout,format(metout$dates, "%y")>0)


plotenviron<-as.data.frame(environ)
plotenviron2 <-subset(plotenviron, format(plotenviron$dates, "%y")>0)
plotmasbal <-subset(masbal, format(masbal$dates, "%y")>0)


plotenviron_night<-subset(metout,ZEN==90)
plotenviron_night$TIME<-plotenviron_night$TIME/60-1
plotenviron_night$JULDAY<-plotenviron_night$JULDAY+(as.numeric(format(plotenviron_night$dates, '%Y'))-2009)*365
plotenviron_bask <- subset(plotenviron2,  subset=(ACT>=1 & TC>=TMINPR))
plotenviron_forage <- subset(plotenviron2,  subset=(ACT>1))
plotenviron_trap <- subset(plotenviron2,  subset=(ACT>=1 & SHADE==0 & TC>=TMINPR))


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

with(debout, {plot(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass",ylim=c(0,1200))})
points(grassgrowths*100~dates2,type='l',col='green')
points(rainfall$rainfall*10~dates2,type='h',col='blue')

with(debout, {plot((WETMASS-WETMASS*(Body_cond/100))~dates,type = "l",xlab = "day of year",ylab = "wet mass (g)",col='blue')})

with(debout, {points(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass growth")})
# grass presence vector
grass<-metout$SOILMOIST3
grassthresh<-as.single(read.csv('ectoin.csv')[6,2])/2+1
grass[grass<=grassthresh]<-0
grass2<-grass
grass2[grass2>0]<-1
points(grass*10+600~dates,type='l',col='dark green')
with(rainfall, {points(rainfall*10~dates,type='h',col='dark blue')})
#  with(environ, {points(CONDEP*30~dates,type='l',col='light blue')})

with(environ, {xyplot(TC+ACT*5+SHADE/10+DEP/10~dates,type = "l")})

# write.csv(metout,'/NicheMapR_Working/projects/sleepy_ibm_transient/metout.csv')
# write.csv(soil,'/NicheMapR_Working/projects/sleepy_ibm_transient/soil.csv')
# write.csv(shadmet,'/NicheMapR_Working/projects/sleepy_ibm_transient/shadmet.csv')
# write.csv(shadsoil,'/NicheMapR_Working/projects/sleepy_ibm_transient/shadsoil.csv')


################### analysis and plots ################################

merge<-as.data.frame(paste(with(plotenviron2,as.numeric(format(dates, "%Y"))),"_",with(plotenviron2,as.numeric(format(dates, "%m"))),"_",with(plotenviron2,as.numeric(format(dates, "%d"))),"_",plotenviron2$TIME-1,sep=""))
colnames(merge)<-'merge'
plotenviron2<-cbind(plotenviron2,merge)
library(mda)
plotenviron3<-plotenviron2
for(l in 1:nrow(plotenviron2)){ # make values average of prev hour to account for 1/2 time diff in SA
  if(l==nrow(plotenviron2)){
    plotenviron3[l,6]<-plotenviron2[l,6]
  }else{
    plotenviron3[l,6]<-plotenviron3[l,6]+(plotenviron2[l+1,6]-plotenviron2[l,6])/2
  }
}

waddlefiles<-read.csv("/NicheMapR_Working/projects/Sleepy Lizards/waddle_files_all.csv")
sex<-read.csv("/NicheMapR_Working/projects/Sleepy Lizards/Waddle/sex.csv")

threshold.act<-50
curyear<-2009
plotenviron_bask <- subset(plotenviron3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
plotenviron_night<- subset(metout,  subset=(ZEN==90))
rbPal <- colorRampPalette(c('turquoise','gold1'))

plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
plotenviron_all <- subset(plotenviron3,  subset=(ACT>=0 & substr(dates,1,4)==curyear))
#plotmetout<-cbind(metout,grass)
plotmetout <- subset(metout,  subset=(substr(dates,1,4)==curyear))
Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotenviron_bask$TC))))
Tbs<-as.data.frame(t(Tbs))
colnames(Tbs)<-'Tb'
Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
plotenviron_night<- subset(metout,  subset=(ZEN==90))
plotenviron_night$TIME<-plotenviron_night$TIME/60-1
startdy<-1
finishdy<-365
desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
desic<-as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)<-c('day','desic')


with(plotenviron_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})
with(plotenviron_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=plotenviron_bask$Col,pch=15)})
points(grassgrowths[366:730], type = "l",col='green')
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})

with(plotenviron_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})
with(plotenviron_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=plotenviron_bask$Col,pch=15)})
points(grassgrowths[366:730], type = "l",col='green')
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})


################### plot all lizards ################################

# analyse mass change in 2009 during drought period

# first with all lizards
massdata<-read.csv('C:/NicheMapR_Working/projects/Sleepy Lizards/Waddle/Church_site_mass_length.csv')
massdata2009<-subset(massdata,year==2009)
massagg<-aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry<-aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid<-aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all<-as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)<-c('dry','id','mass')
massagg1<-subset(massagg_all,dry==1)
massagg0<-subset(massagg_all,dry==0)
massagg_merge<-merge(massagg1,massagg0,by='id')
diff<-massagg_merge$mass.x-massagg_merge$mass.y
pctdes<-(diff*-1)/massagg_merge$mass.y*100
pctdesic<-subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge<-cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s<-seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

# second excluding animals that remained active
activelizards<-c(9532,9372,9364,9363,9310,40044,40012,12847,12434,11885,11505,10509)

massdata2009<-subset(massdata,year==2009 & !(lizard_ID%in%activelizards))
massagg<-aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry<-aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid<-aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all<-as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)<-c('dry','id','mass')
massagg1<-subset(massagg_all,dry==1)
massagg0<-subset(massagg_all,dry==0)
massagg_merge<-merge(massagg1,massagg0,by='id')
diff<-massagg_merge$mass.x-massagg_merge$mass.y
pctdes<-(diff*-1)/massagg_merge$mass.y*100
pctdesic<-subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge<-cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s<-seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

# third, only including animals that remained active
massdata2009<-subset(massdata,year==2009 & lizard_ID%in%activelizards)
massagg<-aggregate(massdata2009$wgt,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massdry<-aggregate(massdata2009$dryperiod,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massid<-aggregate(massdata2009$lizard_ID,by=list(paste(massdata2009$lizard_ID,massdata2009$dryperiod,sep="_")),mean)
massagg_all<-as.data.frame(cbind(massdry$x,massid$x,massagg$x))
colnames(massagg_all)<-c('dry','id','mass')
massagg1<-subset(massagg_all,dry==1)
massagg0<-subset(massagg_all,dry==0)
massagg_merge<-merge(massagg1,massagg0,by='id')
diff<-massagg_merge$mass.x-massagg_merge$mass.y
pctdes<-(diff*-1)/massagg_merge$mass.y*100
pctdesic<-subset(pctdes,pctdes>0)
mean(pctdesic,na.rm=TRUE)
massagg_merge<-cbind(massagg_merge,diff,pctdes)
with(massagg_merge,{t.test(mass.x,mass.y,paired=TRUE)})

s<-seq(length(massagg_merge$mass.x))
par(bty="l")
boxplot(massagg_merge$mass.x,massagg_merge$mass.y,xlab="Time",ylab="Measure",names=c("desiccated","hydrated"),col=c("lightblue","lightgreen"),ylim=c(400,1000))
stripchart(list(massagg_merge$mass.x,massagg_merge$mass.y),vertical=T,pch=16,cex=0.5,add=T)
segments(rep(0.95,length(massagg_merge$mass.x))[s],massagg_merge$mass.x[s],rep(2,length(massagg_merge$mass.x))[s],massagg_merge$mass.y[s],col='light grey',lwd=0.5)

yearstodo<-c(2009,2010)
for(m in 1:2){
yeartodo<-yearstodo[m]
k<-0
# use individual 9 (burrows) or 115 (dam)
for(i in 1:121){
  
  sleepy_id<-waddlefiles[i,2]
  sexliz<-subset(sex,Liz==sleepy_id)
  sexliz<-sexliz[2]
  sexlizard<-as.character(sexliz)
  if(sexlizard=="integer(0)"){
    sexlizard<-'unknown'
  }
  if(sexlizard==2){
    sexlizard<-'F'
  }
  if(sexlizard==3){
    sexlizard<-'M'
  }
  
  
  #sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  if(is.null(sleepy$Hours)){
    Hours<-as.numeric(substr(sleepy$Time,1,2))
    Minutes<-as.numeric(substr(sleepy$Time,4,5))
    sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear<-max(sleepy$Year,na.rm=TRUE)
  if(curyear==yeartodo){
    k<-k+1
    plotrainfall <- subset(rainfall,substr(dates,1,4)==curyear)
    plotenviron_bask <- subset(plotenviron3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
    plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
    plotenviron_night<- subset(metout,  subset=(ZEN==90))
    plotenviron_night$TIME<-plotenviron_night$TIME/60-1
    date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle<-date_waddle1+60*60 # adjust for Central Time
    doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy<-cbind(date_waddle,doy,sleepy)
    plotrainfall2<-cbind(doy2,plotrainfall)
    #plotgrass<-cbind(doy2,plotgrass)
    colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
    desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
    desic<-as.data.frame(cbind(desic[,2],desic[,20]))
    colnames(desic)<-c('day','desic')
    
    date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    
    
    
    # round the times to get rid of occasional 1 min addons
    x<-sleepy$date_waddle
    r <-  60*60
    
    H <- as.integer(format(x, "%H"))
    M <- as.integer(format(x, "%M"))
    S <- as.integer(format(x, "%S"))
    D <- format(x, "%Y-%m-%d")
    secs <- 3600*H + 60*M + S
    x<-as.POSIXct(round(secs/r)*r, origin=D)-11*3600
    sleepy$date_waddle<-x
    # end rounding times
    
    
    sleepy2<-as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
    colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
    
    sleepy2$Temperature<-as.factor(sleepy$Temperature)
    
    correl<-merge(sleepy2,plotenviron3,by='merge')
    correl_day<-subset(correl,ZEN!=90)
    c.doy<-as.numeric(levels(correl$doy))[correl$doy]
    c.year<-as.numeric(levels(correl$Year))[correl$Year]
    c.month<-as.numeric(levels(correl$Month))[correl$Month]
    c.day<-as.numeric(levels(correl$Day))[correl$Day]
    c.hour<-as.data.frame(correl$TIME)
    c.Tb_obs<-as.numeric(levels(correl$Temperature))[correl$Temperature]
    c.steps<-as.numeric(levels(correl$Steps))[correl$Steps]
    c.Tb_pred<-as.data.frame(correl$TC)
    c.act<-as.data.frame(correl$ACT)
    c.shade<-as.data.frame(correl$SHADE)
    correl2<-cbind(correl$dates,c.doy,c.year,c.month,c.day,c.hour,c.Tb_obs,c.steps,c.Tb_pred,c.act,c.shade)
    colnames(correl2)<-c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
    
    c.doy.day<-as.numeric(levels(correl_day$doy))[correl_day$doy]
    c.year.day<-as.numeric(levels(correl_day$Year))[correl_day$Year]
    c.month.day<-as.numeric(levels(correl_day$Month))[correl_day$Month]
    c.day.day<-as.numeric(levels(correl_day$Day))[correl_day$Day]
    c.hour.day<-as.data.frame(correl_day$TIME)
    c.Tb_obs.day<-as.numeric(levels(correl_day$Temperature))[correl_day$Temperature]
    c.steps.day<-as.numeric(levels(correl_day$Steps))[correl_day$Steps]
    c.Tb_pred.day<-as.data.frame(correl_day$TC)
    c.act.day<-as.data.frame(correl_day$ACT)
    c.shade.day<-as.data.frame(correl_day$SHADE)
    correl2.day<-cbind(correl_day$dates,c.doy.day,c.year.day,c.month.day,c.day.day,c.hour.day,c.Tb_obs.day,c.steps.day,c.Tb_pred.day,c.act.day,c.shade.day)
    colnames(correl2.day)<-c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
    
    mean_Tb_obs<-mean(correl2$Tb_obs,na.rm=TRUE)
    max_Tb_obs<-max(correl2$Tb_obs,na.rm=TRUE)
    min_Tb_obs<-min(correl2$Tb_obs,na.rm=TRUE)
    med_Tb_obs<-median(correl2$Tb_obs,na.rm=TRUE)
    mean_Tb_pred<-mean(correl2$Tb_pred,na.rm=TRUE)
    max_Tb_pred<-max(correl2$Tb_pred,na.rm=TRUE)
    min_Tb_pred<-min(correl2$Tb_pred,na.rm=TRUE)
    med_Tb_pred<-median(correl2$Tb_pred,na.rm=TRUE)
    
    
    lm_Tb<-with(correl2,(lm(Tb_pred~Tb_obs)))
    #with(correl2,(plot(Tb_pred~Tb_obs)))
    #abline(1,1)
    lm_Tb_R2<-summary(lm_Tb)$r.squared
    lm_Tb_rmsd<-sqrt(mean(((correl2$Tb_obs-correl2$Tb_pred)^2),na.rm=TRUE))
    #text(11,40,paste("r2=",round(lm_Tb_R2,2),"\n","rmsd=",round(lm_Tb_rmsd,2),sep=""))
    act.obs<-ifelse(correl2$steps>threshold.act,1,0)
    act.pred<-ifelse(correl2$act>0,1,0)
    #confus<-confusion(act.pred,act.obs)
    confus<-confusion(act.obs,act.pred)
    
    error<-attributes(confus)
    confusion<-as.data.frame(t(as.data.frame(c(confus[1:4],error$error))))
    rownames(confusion)<-NULL
    colnames(confusion)<-c('true-','false-','true+','false+','err')
    
    act.obs.day<-ifelse(correl2.day$steps>threshold.act,1,0)
    act.pred.day<-ifelse(correl2.day$act>0,1,0)
    #confus.day<-confusion(act.pred.day,act.obs.day)
    confus.day<-confusion(act.obs.day,act.pred.day)
    
    error.day<-attributes(confus.day)
    confusion.day<-as.data.frame(t(as.data.frame(c(confus.day[1:4],error.day$error))))
    rownames(confusion.day)<-NULL
    colnames(confusion.day)<-c('true-','false-','true+','false+','err')
    
    
    #sleepy$date_waddle<-as.POSIXct(as.numeric(date_waddle[,2]+9.5*60*60),tz=tzone,origin="1970-01-01")
    plotlizard_forage <- subset(sleepy,  subset=(Steps>threshold.act))
    plotlizard_noforage <- subset(sleepy,  subset=(Steps>=0))
    if(k==1){
      allforage<-cbind(plotlizard_forage,sexlizard)
      allnoforage<-plotlizard_noforage
    }else{
      allforage<-rbind(allforage,cbind(plotlizard_forage,sexlizard))
      allnoforage<-rbind(allnoforage,plotlizard_noforage)
      
    }
    
    #   
    if(gutfill>100){
      filename<-paste("c:/NicheMapR_Working/projects/Sleepy Lizards/plots_alwaysforage/",sleepy_id,"_",sexliz$Sex,"_",curyear,"_nodrink.pdf",sep="") 
    }else{
      filename<-paste("c:/NicheMapR_Working/projects/Sleepy Lizards/plots/",sleepy_id,"_",sexliz$Sex,"_",curyear,"_nodrink.pdf",sep="") 
    } 
    pdf(filename,paper="A4r",width=15,height=11) # doing this means you're going to make a pdf - comment this line out if you want to see them in R
    par(mfrow = c(2,2)) # set up for 5 plots in 2 columns
    par(oma = c(2,2,2,2) + 0.1) # margin spacing stuff
    par(mar = c(3,3,1,1) + 0.1) # margin spacing stuff 
    par(mgp = c(3,1,0) ) # margin spacing stuff 
    # layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),
    #        widths = c(lcm(10),lcm(10)), heights = c(lcm(8),lcm(8)))
    startdy<-240
    finishdy<-365
    
    with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,pch=15,col='ivory 4')})
    with(plotenviron_night, {points(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})
    
    rbPal <- colorRampPalette(c('turquoise','gold1'))
    
    #This adds a column of color values
    # based on the y values
    Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotlizard_forage$Temperature))))
    Tbs<-as.data.frame(t(Tbs))
    colnames(Tbs)<-'Tb'
    
    Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
    Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
    plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
    plotenviron_bask$Col <- rbPal(10)[as.numeric(cut(plotenviron_bask$TC,breaks = 10))]
    #with(plotenviron_forage, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=plotenviron_bask$Col,pch=15)})
    with(plotenviron_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=plotenviron_bask$Col,pch=15)})
    
    with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
    with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
    with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})
    
    
    
    
    daystart<-paste(substr(curyear,3,4),'/08/28',sep="") # y/m/d
    dayfin<-paste(substr(curyear,3,4),'/12/31',sep="") # y/m/d
    #daystart<-paste(substr(curyear,3,4),'/10/15',sep="") # y/m/d
    #dayfin<-paste(substr(curyear,3,4),'/10/30',sep="") # y/m/d
    plotlizard<-subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
    plotlizard <- plotlizard[order(plotlizard$date_waddle),] 
    #plotlizard$date_waddle<-plotlizard$date_waddle-60*60*8
    plotpred<-subset(plotenviron3, format(environ$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
    plotdeb<-subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
    plotmetout<-subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
    with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,40))})
    #with(plotmetout, {points(TAREF~dates,type = "l",col='grey')})
    plotlizard2<-plotlizard
    colnames(plotlizard2)[1] <- "dates"
    plotlizard2<-merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
    with(plotlizard2, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
    with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
    with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
    with(plotenviron, {points(CONDEP~dates, type = "l",col='light blue')})
    
    #with(plotgrass, {points(growth~dates, type = "l",col='dark green')})
    with(plotdeb,{plot(WETMASS~dates,type='l',ylim=c(400,1000))})
    
    massobs<-subset(massdata,lizard_ID==sleepy_id)
    date_mass<-as.POSIXct(with(massobs,ISOdatetime(year,month,day,0,0,0)))
    doymass<-strptime(format(date_mass, "%y/%m/%d"), "%y/%m/%d")$yday+1
    massobs<-cbind(massobs,date_mass,doymass)
    with(massobs,{points(wgt~date_mass,type='p',col='red')})
    
    startdy<-298
    finishdy<-334
    
    # with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=2,pch=15,col='ivory 4')})
    # with(plotenviron_night, {points(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.5,col=addTrans("dark grey",50),pch=16)})
    
    rbPal <- colorRampPalette(c('turquoise','gold1'))
    
    #This adds a column of color values
    # based on the y values
    Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotlizard_forage$Temperature))))
    Tbs<-as.data.frame(t(Tbs))
    colnames(Tbs)<-'Tb'
    Tbs$Col<-rbPal(20)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
    plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
    #plotenviron_bask$Col <- rbPal(10)[as.numeric(cut(plotenviron_bask$TC,breaks = 10))]
    # with(plotenviron_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=2,col=plotenviron_bask$Col,pch=15)})
    # with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
    # with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
    # with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})
    
    
    # daystart<-paste(substr(curyear,3,4),'/09/14',sep="") # y/m/d
    # dayfin<-paste(substr(curyear,3,4),'/12/31',sep="") # y/m/d
    daystart<-paste(substr(curyear,3,4),'/10/25',sep="") # y/m/d
    dayfin<-paste(substr(curyear,3,4),'/11/30',sep="") # y/m/d
    plotlizard<-subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
    plotlizard <- plotlizard[order(plotlizard$date_waddle),] 
    #plotlizard$date_waddle<-plotlizard$date_waddle-60*60*8
    plotpred<-subset(environ, format(environ$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
    plotdeb<-subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
    plotmetout<-subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
#    with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,40))})
    #with(plotmetout, {points(TAREF~dates,type = "l",col='grey')})
    plotlizard2<-plotlizard
    colnames(plotlizard2)[1] <- "dates"
    plotlizard2<-merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
#    with(plotlizard2, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
#    with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
#    with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
    #with(plotgrass, {points(growth~dates, type = "l",col='dark green')})
#    with(plotenviron, {points(CONDEP~dates, type = "l",col='light blue')})
    
    with(correl2,(plot(Tb_pred~Tb_obs,ylim=c(0,40),xlim=c(0,40))))
    abline(1,1)
    #lm_Tb_R2<-summary(lm_Tb)$r.squared
    #lm_Tb_rmsd<-sqrt(mean(((correl2$Tb_obs-correl2$Tb_pred)^2),na.rm=TRUE))
    text(11,40,paste("r2=",round(lm_Tb_R2,2),"\n","rmsd=",round(lm_Tb_rmsd,2),sep=""))
    act.obs<-ifelse(correl2$steps>threshold.act,1,0)
    act.pred<-ifelse(correl2$act>0,1,0)  
    title(main=paste(sleepy_id," sex=",sexliz$Sex," ",curyear,sep=" "),outer=T)
    dev.off()
    
    if(k==1){
      summary<-as.data.frame(cbind(as.numeric(sleepy_id),sexlizard,curyear,mean_Tb_obs,mean_Tb_pred,max_Tb_obs,max_Tb_pred,min_Tb_obs,min_Tb_pred,med_Tb_obs,med_Tb_pred,lm_Tb_R2,lm_Tb_rmsd,confusion,confusion.day))
    }else{
      summary<-rbind(summary, as.data.frame(cbind(as.numeric(sleepy_id),sexlizard,curyear,mean_Tb_obs,mean_Tb_pred,max_Tb_obs,max_Tb_pred,min_Tb_obs,min_Tb_pred,med_Tb_obs,med_Tb_pred,lm_Tb_R2,lm_Tb_rmsd,confusion,confusion.day)))
    } 
  }
} #end loop through lizards

colnames(summary)<-c('id','sex','year','mean_Tb_obs','mean_Tb_pred','max_Tb_obs','max_Tb_pred','min_Tb_obs','min_Tb_pred','med_Tb_obs','med_Tb_pred','Tb_R2','Tb_rmsd','true-','false-','true+','false+','err','day.true-','day.false-','day.true+','day.false+','day.err')

if(yeartodo==2009){
  if(raindrink==0){
    write.csv(summary,'sleepy/summary_drink_2009_2.csv')
  }else{
    write.csv(summary,'sleepy/summary_nodrink_2009_2.csv')
  }
}else{
  if(raindrink==0){
    write.csv(summary,'sleepy/summary_drink_2010_2.csv')
  }else{
    write.csv(summary,'sleepy/summary_nodrink_2010_2.csv')
  }    
}   
}

######################### mean activity plots ######################################

k<-0
liz2do<-1
for(i in 1:121){
  #for(i in 1:121){
  sleepy_id<-waddlefiles[i,2]
  sexliz<-subset(sex,Liz==sleepy_id)
  sexliz<-sexliz[2]
  sexlizard<-as.character(sexliz)
  if(sexlizard=="integer(0)"){
    sexlizard<-'unknown'
  }
  if(sexlizard==2){
    sexlizard<-'F'
  }
  if(sexlizard==3){
    sexlizard<-'M'
  }
  #sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  if(is.null(sleepy$Hours)){
    Hours<-as.numeric(substr(sleepy$Time,1,2))
    Minutes<-as.numeric(substr(sleepy$Time,4,5))
    sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear<-max(sleepy$Year,na.rm=TRUE)
  if(curyear==2009){
    k<-k+1
    plotrainfall <- subset(rainfall,substr(dates,1,4)==curyear)
    plotenviron_bask <- subset(plotenviron3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
    plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
    plotenviron_night<- subset(metout,  subset=(ZEN==90))
    plotenviron_night$TIME<-plotenviron_night$TIME/60-1
    date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle<-date_waddle1+60*30 # adjust for Central Time
    doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy<-cbind(date_waddle,doy,sleepy)
    plotrainfall2<-cbind(doy2,plotrainfall)
    #plotgrass<-cbind(doy2,plotgrass)
    colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
    desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
    desic<-as.data.frame(cbind(desic[,2],desic[,20]))
    colnames(desic)<-c('day','desic')
    
    date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy<-as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    sleepy2<-as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1))
    colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
    
    sleepy$date_waddle<-as.POSIXct(as.numeric(date_waddle[,2]+9.5*60*60),tz=tzone,origin="1970-01-01")
    plotlizard_forage <- subset(sleepy,  subset=(Steps>threshold.act))
    plotlizard_noforage <- subset(sleepy,  subset=(Steps>=0))
    if(k==1){
      allforage<-cbind(plotlizard_forage,sexlizard)
      allnoforage<-plotlizard_noforage
      allwaddleobs<-sleepy
    }else{
      allforage<-rbind(allforage,cbind(plotlizard_forage,sexlizard))
      allnoforage<-rbind(allnoforage,plotlizard_noforage)
      allwaddleobs<-rbind(allwaddleobs,sleepy)
    }
  }
} #end loop through lizards

startdy<-250
finishdy<-355

#with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,pch=15,col='ivory 4')})
with(plotenviron_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})

rbPal <- colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotlizard_forage$Temperature))))
Tbs<-as.data.frame(t(Tbs))
colnames(Tbs)<-'Tb'
Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
#plotenviron_bask$Col <- rbPal(10)[as.numeric(cut(plotenviron_bask$TC,breaks = 10))]
with(plotenviron_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1.,col=plotenviron_bask$Col,pch=15)})
#with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})




meanwaddle<-aggregate(allforage$Steps,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),sum)
meanhour<-aggregate(allforage$Hours,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),mean)
meandoy<-aggregate(allforage$doy,by=list(paste(allforage$Year,"_",allforage$Month,"_",allforage$Day,"_",allforage$Hours,sep="")),mean)
meanwaddle<-cbind(meanwaddle,meanhour[,2],meandoy[,2])
colnames(meanwaddle)<-c('date','Steps','Hours','doy')
#meanwaddle_female<-meanwaddle
with(meanwaddle, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=meanwaddle$Steps/30000,pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF



startdy<-1
finishdy<-365

#with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,pch=15,col='ivory 4')})
with(plotenviron_night, {plot(TIME+2~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})

rbPal <- colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotlizard_forage$Temperature))))
Tbs<-as.data.frame(t(Tbs))
colnames(Tbs)<-'Tb'
Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
#plotenviron_bask$Col <- rbPal(10)[as.numeric(cut(plotenviron_bask$TC,breaks = 10))]
with(plotenviron_bask, {points(TIME~JULDAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=.75,col=plotenviron_bask$Col,pch=15)})
#with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
with(desic, {points(desic~day, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~JULDAY, type = "h",col='blue')})




######################### Tbs specific period and individual, average per hour ######################################


liz2do<-9

sleepy_id<-waddlefiles[liz2do,2]
sexliz<-subset(sex,Liz==sleepy_id)
sexliz<-sexliz[2]
sexlizard<-as.character(sexliz)
if(sexlizard=="integer(0)"){
  sexlizard<-'unknown'
}
if(sexlizard==2){
  sexlizard<-'F'
}
if(sexlizard==3){
  sexlizard<-'M'
}
#sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[liz2do,3],sep=""), sep = ",", head=TRUE))
if(is.null(sleepy$Hours)){
  Hours<-as.numeric(substr(sleepy$Time,1,2))
  Minutes<-as.numeric(substr(sleepy$Time,4,5))
  sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
  write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[liz2do,3],sep=""),row.names=F,sep=",") 
}
curyear<-max(sleepy$Year,na.rm=TRUE)


plotrainfall <- subset(rainfall,substr(dates,1,4)==curyear)

date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
date_waddle<-date_waddle1+60*60 # adjust for Central Time
doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
sleepy<-cbind(date_waddle,doy,sleepy)
plotrainfall2<-cbind(doy2,plotrainfall)
#plotgrass<-cbind(doy2,plotgrass)
colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
desic<-as.data.frame(cbind(desic[,2],desic[,20]))
colnames(desic)<-c('day','desic')


date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
sleepy<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
sleepy2<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1))
colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')


daystart<-paste(substr(curyear,3,4),'/09/25',sep="") # y/m/d
dayfin<-paste(substr(curyear,3,4),'/12/30',sep="") # y/m/d
plotpred<-subset(environ, format(environ$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
plotdeb<-subset(debout, format(debout$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
plotmetout<-subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
plotlizard<-subset(sleepy, format(sleepy$date_waddle, "%y/%m/%d")>= daystart & format(sleepy$date_waddle, "%y/%m/%d")<=dayfin)
plotlizard <- plotlizard[order(plotlizard$date_waddle),] 
plotlizard2<-plotlizard
colnames(plotlizard2)[1] <- "dates"

# round the times to get rid of occasional 1 min addons
x<-plotlizard2$dates
r <-  60*60

H <- as.integer(format(x, "%H"))
M <- as.integer(format(x, "%M"))
S <- as.integer(format(x, "%S"))
D <- format(x, "%Y-%m-%d")
secs <- 3600*H + 60*M + S
x<-as.POSIXct(round(secs/r)*r, origin=D)-11*3600
plotlizard2$dates<-x
# end rounding times

plotlizard2<-merge(plotlizard2,plotpred,all=TRUE) # merge to avoid continuous line between sample gaps
plotlizard3<-plotlizard2
for(i in 1:nrow(plotlizard2)){ # make values average of prev hour to account for 1/2 time diff in SA
  if(i==1){
    plotlizard3[i,3]<-plotlizard2[i,3]
  }else{
    plotlizard3[i,3]<-(plotlizard2[i,3]+plotlizard2[i-1,3])/2
  }
}

#plotlizard2$dates<-plotlizard2$dates+60*30 # adjust for Central Time   #-60*60*8

with(plotpred, {plot(TC~dates,type = "l",ylim=c(0,40))})
#with(plotmetout, {points(TAREF~dates,type = "l",col='grey')})

with(plotlizard3, {points(Temperature~dates,type = "l",col=addTrans("red",150))})
with(plotdeb, {points(Body_cond~dates,type = "l",col='blue',lty=2,lwd=2)})
with(plotrainfall2, {points(RAINFALL~dates, type = "h",col='blue')})
#with(plotgrass, {points(growth~dates, type = "l",col='dark green')})

plotlizard_corr2<-na.omit(plotlizard2)
plotlizard_corr3<-na.omit(plotlizard3)
lm_2<-with(plotlizard_corr2,(lm(TC~Temperature)))
lm2_R2<-summary(lm_2)$r.squared
lm2_rmsd<-sqrt(mean(((plotlizard_corr2$Temperature-plotlizard_corr2$TC)^2),na.rm=TRUE))
lm2_R2
lm2_rmsd
plot(plotlizard_corr2$Temperature~plotlizard_corr2$TC)
abline(1,1)
lm_3<-with(plotlizard_corr3,(lm(TC~Temperature)))
lm3_R2<-summary(lm_3)$r.squared
lm3_rmsd<-sqrt(mean(((plotlizard_corr3$Temperature-plotlizard_corr3$TC)^2),na.rm=TRUE))
lm3_R2
lm3_rmsd
plot(plotlizard_corr3$Temperature~plotlizard_corr3$TC)
abline(1,1)




# ######## comparison with Fig. 5 Kerr 2004 #############
plotmasbal <- subset(masbal, format(masbal$dates, "%y")=="02"  | format(masbal$dates, "%y")=="03")
plotdebout <- subset(debout, format(debout$dates, "%y")=="02"  | format(debout$dates, "%y")=="03")
plotenviron <- subset(environ, format(environ$dates, "%y")=="02"  | format(environ$dates, "%y")=="03")
plotrainfall <- subset(rainfall,format(dates, "%y")=="02"  | format(dates, "%y")=="03")

with(plotdebout, {plot(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass growth",ylim=c(400,950),groups=Stage,auto.key=list(columns = 5))})
Kerr_fig5<-read.csv("c:/NicheMapR_Working/projects/Sleepy Lizards/Kerr_Fig5.csv")
date_Kerr<-with(Kerr_fig5,ISOdatetime(Year,Month,Day,0,0,0))
date_Kerr<-date_Kerr
Kerr_fig5<-cbind(date_Kerr,Kerr_fig5)
with(plotdebout, {plot(WETMASS_STD~dates,type = "l",ylab = "wet mass (g)/grass growth",ylim=c(400,950),groups=Stage,auto.key=list(columns = 5))})

with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='blue'))
with(plotdebout, {plot(Body_cond~dates,type = "l",ylim=c(-2,50),xlab = "year", ylab = "% desiccated",col='dark grey')})
with(Kerr_fig5,points(Mass/20~date_Kerr,type='p',col='blue'))
with(plotenviron, {xyplot(TC+ACT*10+SHADE/10+DEP/10~dates,type = "l")})
plotenviron_bask <- subset(plotenviron,  subset=(ACT>=1 & TC>=TMINPR))
plotenviron_forage <- subset(plotenviron,  subset=(ACT>1))
plotenviron_trap <- subset(plotenviron,  subset=(ACT>=1 & SHADE==0 & TC>=TMINPR))
with(plotenviron_bask, {plot(TIME~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='gray',pch=15)})
with(plotenviron_trap, {points(TIME~DAY, ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,col='black',pch=15)})

wetgut<-as.data.frame(((plotdebout$MASS_GUT/mu_E)*23.9)/d_V)
wetgut<-cbind(plotdebout$dates,wetgut)
colnames(wetgut)<-c("dates","wetgut")
with(wetgut,plot(wetgut~dates,type='l',ylim=c(-.25,50)))
#with(plotgrass,points(growth*50~dates,type='l',col='grey'))

#average male/female
with(plotdebout, {plot(((WETMASS-WETMASS*(Body_cond/100))+(WETMASS_STD-WETMASS_STD*(Body_cond/100)))/2~dates,ylim=c(400,850),type = "l",xlab = "day of year",ylab = "wet mass (g)")})
with(plotdebout, {points(((WETMASS)+(WETMASS_STD))/2~dates,type = "l",col='4',ylim=c(400,650))})
with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='red'))
#male
with(plotdebout, {plot((WETMASS_STD-WETMASS_STD*(Body_cond/100))~dates,ylim=c(400,850),type = "l",xlab = "day of year",ylab = "wet mass (g)")})
with(plotdebout, {points(WETMASS_STD~dates,type = "l",col='4',ylim=c(400,650))})
with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='red'))

#female
with(plotdebout, {plot((WETMASS-WETMASS*(Body_cond/100))~dates,ylim=c(400,850),type = "l",xlab = "day of year",ylab = "wet mass (g)")})
with(plotdebout, {points(WETMASS~dates,type = "l",col='4',ylim=c(400,650))})
with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='red'))

with(Kerr_fig5,points(Mass~date_Kerr,type='p',col='red'))
with(plotrainfall,points(rainfall*100~dates,type='l',col='light blue'))
with(plotgrass,points(growth*100~dates,type='l',col='dark green'))


curyear<-2002

plotenviron_bask <- subset(environ,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)>=curyear))
plotenviron_forage <- subset(environ,  subset=(ACT>1 & substr(dates,1,4)>=curyear))
DAY<-environ$DAY
plotenviron_night<-cbind(metout,DAY)
plotenviron_night<- subset(plotenviron_night,  subset=(ZEN==90))

plotenviron_night$TIME<-plotenviron_night$TIME/60-1
doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
plotrainfall2<-as.data.frame(cbind(seq(1,365*3,1),rainfall$rainfall))
#plotgrass<-cbind(doy2,plotgrass)
colnames(plotrainfall2)<-c("DAY","RAINFALL")
desic<-subset(debout,TIME==24 & substr(dates,1,4)>=curyear)
#desic<-as.data.frame(cbind(desic[,2],desic[,20]))
#colnames(desic)<-c('day','desic')
startdy<-366
finishdy<-366+720


#with(plotlizard_noforage, {plot(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=1,pch=15,col='ivory 4')})
with(plotenviron_night, {plot(TIME+2~DAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=0.5,col=addTrans("dark grey",50),pch=16)})

rbPal <- colorRampPalette(c('turquoise','gold1'))

#This adds a column of color values
# based on the y values
Tbs<-t(as.numeric(as.matrix(c(plotenviron_bask$TC,plotlizard_forage$Temperature))))
Tbs<-as.data.frame(t(Tbs))
colnames(Tbs)<-'Tb'
Tbs$Col<-rbPal(23)[as.numeric(cut(Tbs$Tb, breaks=seq(18, 37,1), include.lowest=TRUE))]
plotenviron_bask$Col<-Tbs[1:nrow(plotenviron_bask),2]
#plotenviron_bask$Col <- rbPal(10)[as.numeric(cut(plotenviron_bask$TC,breaks = 10))]
with(plotenviron_bask, {points(TIME~DAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=.75,col=plotenviron_bask$Col,pch=15)})
#with(plotlizard_forage, {points(Hours+1~doy, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",cex=log(plotlizard_forage$Steps/300),pch=15,col=addTrans("red",100))}) #col="#0000FF96" #0000FFFF
with(desic, {points(Body_cond~DAY, xlim=c(startdy,finishdy),ylim=c(0,25),xlab = "day of year",ylab = "hour of day",lwd=2,pch=15,col='blue',type='l',lty=2)})
with(plotrainfall2, {points(RAINFALL~DAY, type = "h",col='blue')})





######## average Tb obs vs Tb pred #############
yearstodo<-c(2009,2010)
for(m in 1:2){
yeartodo<-yearstodo[m]
k<-0
# use individual 9 (burrows) or 115 (dam)
for(i in 1:121){
  sleepy_id<-waddlefiles[i,2]
  sexliz<-subset(sex,Liz==sleepy_id)
  sexliz<-sexliz[2]
  sexlizard<-as.character(sexliz)
  if(sexlizard=="integer(0)"){
    sexlizard<-'unknown'
  }
  if(sexlizard==2){
    sexlizard<-'F'
  }
  if(sexlizard==3){
    sexlizard<-'M'
  }
  #sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  if(is.null(sleepy$Hours)){
    Hours<-as.numeric(substr(sleepy$Time,1,2))
    Minutes<-as.numeric(substr(sleepy$Time,4,5))
    sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear<-max(sleepy$Year,na.rm=TRUE)
  if(curyear==yeartodo){
    k<-k+1
    plotrainfall <- subset(rainfall,substr(dates,1,4)==curyear)
    plotenviron_bask <- subset(plotenviron3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
    plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
    plotenviron_night<- subset(metout,  subset=(ZEN==90))
    plotenviron_night$TIME<-plotenviron_night$TIME/60-1
    date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle<-date_waddle1+60*60 # adjust for Central Time
    doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy<-cbind(date_waddle,doy,sleepy)
    plotrainfall2<-cbind(doy2,plotrainfall)
    #plotgrass<-cbind(doy2,plotgrass)
    colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
    desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
    desic<-as.data.frame(cbind(desic[,2],desic[,20]))
    colnames(desic)<-c('day','desic')
    
    date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    
    
    
    # round the times to get rid of occasional 1 min addons
    x<-sleepy$date_waddle
    r <-  60*60
    
    H <- as.integer(format(x, "%H"))
    M <- as.integer(format(x, "%M"))
    S <- as.integer(format(x, "%S"))
    D <- format(x, "%Y-%m-%d")
    secs <- 3600*H + 60*M + S
    x<-as.POSIXct(round(secs/r)*r, origin=D)-11*3600
    sleepy$date_waddle<-x
    # end rounding times
    
    
    sleepy2<-as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
    colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
    
    sleepy2$Temperature<-as.factor(sleepy$Temperature)
    
    correl<-merge(sleepy2,plotenviron3,by='merge')
    correl_day<-subset(correl,ZEN!=90)
    c.doy<-as.numeric(levels(correl$doy))[correl$doy]
    c.year<-as.numeric(levels(correl$Year))[correl$Year]
    c.month<-as.numeric(levels(correl$Month))[correl$Month]
    c.day<-as.numeric(levels(correl$Day))[correl$Day]
    c.hour<-as.data.frame(correl$TIME)
    c.Tb_obs<-as.numeric(levels(correl$Temperature))[correl$Temperature]
    c.steps<-as.numeric(levels(correl$Steps))[correl$Steps]
    c.Tb_pred<-as.data.frame(correl$TC)
    c.act<-as.data.frame(correl$ACT)
    c.shade<-as.data.frame(correl$SHADE)
    correl2<-cbind(correl$dates,c.doy,c.year,c.month,c.day,c.hour,c.Tb_obs,c.steps,c.Tb_pred,c.act,c.shade)
    colnames(correl2)<-c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
    
    c.doy.day<-as.numeric(levels(correl_day$doy))[correl_day$doy]
    c.year.day<-as.numeric(levels(correl_day$Year))[correl_day$Year]
    c.month.day<-as.numeric(levels(correl_day$Month))[correl_day$Month]
    c.day.day<-as.numeric(levels(correl_day$Day))[correl_day$Day]
    c.hour.day<-as.data.frame(correl_day$TIME)
    c.Tb_obs.day<-as.numeric(levels(correl_day$Temperature))[correl_day$Temperature]
    c.steps.day<-as.numeric(levels(correl_day$Steps))[correl_day$Steps]
    c.Tb_pred.day<-as.data.frame(correl_day$TC)
    c.act.day<-as.data.frame(correl_day$ACT)
    c.shade.day<-as.data.frame(correl_day$SHADE)
    correl2.day<-cbind(correl_day$dates,c.doy.day,c.year.day,c.month.day,c.day.day,c.hour.day,c.Tb_obs.day,c.steps.day,c.Tb_pred.day,c.act.day,c.shade.day)
    colnames(correl2.day)<-c('date','doy','year','month','day','hour','Tb_obs','steps','Tb_pred','act','shade')
    
    mean_Tb_obs<-mean(correl2$Tb_obs,na.rm=TRUE)
    max_Tb_obs<-max(correl2$Tb_obs,na.rm=TRUE)
    min_Tb_obs<-min(correl2$Tb_obs,na.rm=TRUE)
    med_Tb_obs<-median(correl2$Tb_obs,na.rm=TRUE)
    mean_Tb_pred<-mean(correl2$Tb_pred,na.rm=TRUE)
    max_Tb_pred<-max(correl2$Tb_pred,na.rm=TRUE)
    min_Tb_pred<-min(correl2$Tb_pred,na.rm=TRUE)
    med_Tb_pred<-median(correl2$Tb_pred,na.rm=TRUE)
    
    
    lm_Tb<-with(correl2,(lm(Tb_pred~Tb_obs)))
    if(k==1){
      #with(correl2,(plot(Tb_pred~Tb_obs,cex=0.1,ylim=c(0,45),xlim=c(0,45),xlab='observed Tb',ylab='predicted Tb')))
      correl_all<-correl2
    }else{
      #with(correl2,(points(Tb_pred~Tb_obs,cex=0.1)))
      correl_all<-rbind(correl_all,correl2)
    }
  }
} 
library(aqfig)
with(correl_all,scatterplot.density(Tb_obs,Tb_pred,ylim=c(0,50),xlim=c(0,40),main=yeartodo,xlab='Tb observed',ylab='Tb predicted',col.regression.line=2, col.one.to.one.line=1))


abline(1,1,col='blue')

if(yeartodo==2010){
correl_all_2010<-correl_all
lm_Tb_2010<-with(correl_all_2010,(lm(Tb_pred~Tb_obs)))
lm_Tb_R2_2010<-summary(lm_Tb)$r.squared
lm_Tb_rmsd_2010<-sqrt(mean(((correl_all_2010$Tb_obs-correl_all_2010$Tb_pred)^2),na.rm=TRUE))
write.table(correl_all_2010,'c:/NicheMapR_Working/projects/Sleepy Lizards/correl_all_2010.csv')
}else{
  correl_all_2009<-correl_all
lm_Tb_2009<-with(correl_all_2009,(lm(Tb_pred~Tb_obs)))
lm_Tb_R2_2009<-summary(lm_Tb)$r.squared
lm_Tb_rmsd_2009<-sqrt(mean(((correl_all_2009$Tb_obs-correl_all_2009$Tb_pred)^2),na.rm=TRUE))
write.table(correl_all_2009,'c:/NicheMapR_Working/projects/Sleepy Lizards/correl_all_2009.csv')
}
} # end loop through two years



# plot observed and predicted Tb (and Tair) vs time for Sept-Nov


yearstodo<-c(2009,2010)
for(m in 1:2){
yeartodo<-yearstodo[m]
k<-0
# use individual 9 (burrows) or 115 (dam)
for(i in 1:121){
  
  sleepy_id<-waddlefiles[i,2]
  sexliz<-subset(sex,Liz==sleepy_id)
  sexliz<-sexliz[2]
  sexlizard<-as.character(sexliz)
  if(sexlizard=="integer(0)"){
    sexlizard<-'unknown'
  }
  if(sexlizard==2){
    sexlizard<-'F'
  }
  if(sexlizard==3){
    sexlizard<-'M'
  }
  
  
  #sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',sleepy_id,'_2009_ALL.csv',sep=""), sep = ",", head=TRUE))
  sleepy<-as.data.frame(read.table(file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""), sep = ",", head=TRUE))
  if(is.null(sleepy$Hours)){
    Hours<-as.numeric(substr(sleepy$Time,1,2))
    Minutes<-as.numeric(substr(sleepy$Time,4,5))
    sleepy<-as.data.frame(cbind(sleepy,Hours,Minutes))
    write.table(sleepy,file = paste('/NicheMapR_Working/projects/sleepy lizards/waddle/',waddlefiles[i,3],sep=""),row.names=F,sep=",") 
  }
  curyear<-max(sleepy$Year,na.rm=TRUE)
  if(curyear==yeartodo){
   
    plotrainfall <- subset(rainfall,substr(dates,1,4)==curyear)
    plotenviron_bask <- subset(plotenviron3,  subset=(ACT>=1 & TC>=TBASK & substr(dates,1,4)==curyear))
    plotenviron_forage <- subset(plotenviron3,  subset=(ACT>1 & substr(dates,1,4)==curyear))
    plotenviron_night<- subset(metout,  subset=(ZEN==90))
    plotenviron_night$TIME<-plotenviron_night$TIME/60-1
    date_waddle1<-with(sleepy,ISOdatetime(Year,Month,Day,Hours,Minutes,0))
    date_waddle<-date_waddle1+60*60 # adjust for Central Time
    doy<-strptime(format(date_waddle, "%y/%m/%d"), "%y/%m/%d")$yday+1
    doy2<-strptime(format(plotrainfall$dates, "%y/%m/%d"), "%y/%m/%d")$yday+1
    sleepy<-cbind(date_waddle,doy,sleepy)
    plotrainfall2<-cbind(doy2,plotrainfall)
    #plotgrass<-cbind(doy2,plotgrass)
    colnames(plotrainfall2)<-c("JULDAY","dates","RAINFALL")
    desic<-subset(debout,TIME==24 & substr(dates,1,4)==curyear)
    desic<-as.data.frame(cbind(desic[,2],desic[,20]))
    colnames(desic)<-c('day','desic')
    
    date_waddle<-aggregate(sleepy$date_waddle,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),min)
    doy<-aggregate(sleepy$doy,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    Tb<-aggregate(sleepy$Temperature,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),mean)
    steps<-aggregate(sleepy$Steps,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),sum)
    year<-aggregate(sleepy$Year,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    month<-aggregate(sleepy$Month,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    day<-aggregate(sleepy$Day,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    hours<-aggregate(sleepy$Hours,by=list(paste(sleepy$Year,"_",sleepy$Month,"_",sleepy$Day,"_",sleepy$Hours,sep="")),max)
    sleepy<-as.data.frame(cbind(as.data.frame(date_waddle[,2]),doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2]))
    colnames(sleepy)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours')
    
    
    
    # round the times to get rid of occasional 1 min addons
    x<-sleepy$date_waddle
    r <-  60*60
    
    H <- as.integer(format(x, "%H"))
    M <- as.integer(format(x, "%M"))
    S <- as.integer(format(x, "%S"))
    D <- format(x, "%Y-%m-%d")
    secs <- 3600*H + 60*M + S
    x<-as.POSIXct(round(secs/r)*r, origin=D)-11*3600
    sleepy$date_waddle<-x
    # end rounding times
    

    
    sleepy2<-as.data.frame(as.data.frame(cbind(date_waddle[,2],doy[,2],Tb[,2],steps[,2],year[,2],month[,2],day[,2],hours[,2],date_waddle$Group.1)))
    colnames(sleepy2)<-c('date_waddle','doy','Temperature','Steps','Year','Month','Day','Hours','merge')
    
        sleepy3<-subset(sleepy,sleepy$Month<11)
    if(k==0){
      if(curyear==2009){
    with(sleepy3,plot(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,50),xlim=c(1251727200,1257080400)))
      }else{
    with(sleepy3,plot(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,50),xlim=c(1283349600,1288620000.00)))
      }
    }else{
        with(sleepy3,points(Temperature~date_waddle,col='red',cex=0.2,ylim=c(0,50)))
    }
    
     k<-k+1
  }
} #end loop through lizards
  k<-0
  #points(metout$TAREF~environ$dates,cex=0.2,pch=16,col='black',type='l')
points(environ$TC~environ$dates,cex=0.5,pch=16)
}
