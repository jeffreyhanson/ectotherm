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
file.copy('/git/micro_australia/soilpot.csv','soilpot.csv',overwrite=TRUE)
file.copy('/git/micro_australia/humid.csv','humid.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soilmoist.csv','soilmoist.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadpot.csv','shadpot.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadhumid.csv','shadhumid.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadpot.csv','shadpot.csv',overwrite=TRUE)

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
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv('ectoin.csv')[3,2],read.csv('ectoin.csv')[4,2])
grasshade<-0 # use grass shade values from microclimate model as min shade values (1) or not (0)? (simulates effect of grass growth on shading, as a function of soil moisture)

# habitat settings
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
soilnode<-4. # soil node at which eggs are laid (overridden if frogbreed is 1)
minshade<-0. # minimum available shade (percent)
maxshade<-70. # maximum available shade (percent)
REFL<-rep(0.18,timeinterval*nyears) # substrate reflectances 

# morphological traits
rinsul<-0. # m, insulative fat layer thickness
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
# 5=custom (cylinder geometry is automatically invoked when container model operates)
lometry<-1 # organism shape (see above)
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
ptcond<-0.25 # decimal % of surface contacting the substrate
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
TMAXPR<-43. # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection)
TMINPR<-29. # degrees C, voluntary thermal minimum (lower body temperature for foraging)
TBASK<-25. # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-15. # degrees C, temperature at which animal will move to a basking site
ctmax<-47  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-1 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
ctkill<-1 #if 1, animal dies when it hits critical thermal limits
TPREF<-37 # preferred body temperature (animal will attempt to regulate as close to this value as possible)
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.2 # estimated from data in Bently 1959 at 23 degrees and 34.5 degrees #0.2#0.35 # %, of surface area acting like a free water surface (e.g. most frogs are 100% wet, many lizards less than 5% wet)
extref<-20. # %, oxygen extraction efficiency (need to check, but based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
PTUREA<-0. # %, water in excreted nitrogenous waste
FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
raindrink<-0. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
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
DEB<-0 # run the DEB model (1) or just heat balance, using allometric respiration below (0)

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-0.25 # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
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
wilting<-1 # redundant
ystrt<-0

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



# grass presence vector
soilpot<-read.csv(file=paste(microin,'soilpot.csv',sep=""),sep=",")
soilmoist<-read.csv(file=paste(microin,'soilmoist.csv',sep=""),sep=",")
grass<-soilpot$PT5cm
grassthresh<--1500
grass[grass<=grassthresh]<-0
grass2<-grass
grass2[grass2>grassthresh & grass2<0]<-1

with(soilmoist, {plot(WC3cm~dates,type='l',col='light blue')})
with(environ, {xyplot(TC+ACT*5+SHADE/10+DEP/10~dates,ylim=c(-15,50),type = "l")})
plot(grass2~metout$dates,type='l',col='dark green')

recover<-7 # time locust needs to build up resources to lay first batch
ovidates<-as.data.frame(cbind(grass2[1:(length(grass2)-1)],grass2[2:length(grass2)]))
ovidates<-cbind(dates[2:length(dates)],ovidates)
ovdiates2<-subset(ovidates, V1-V2==-1)
ovirows_start<-cbind(1,as.numeric(rownames(ovdiates2))) # rows at which grass growth started
ovdiates2<-subset(ovidates, V2-V1==-1)
ovirows_finish<-cbind(0,as.numeric(rownames(ovdiates2))) # rows at which grass growth finish
ovirows<-rbind(ovirows_start,ovirows_finish)
ovirows<-rbind(c(0,recover),ovirows) # add first day of sim as an oviposition date

########### chortoicetes oviposition model##########

# to do, for now assume on first and last days of grass growth periods

############### chortoicetes egg model #############

DATA1<-cbind(soil[,1:7],metout[,13:14],environ[,17],soilmoist[,4]*100)
colnames(DATA1)<-c('DATE','JULDAY','TIME','D0cm','D2_5cm','D5cm','D10cm','ZEN','SOLR','PHOTO','MOIST')

# if photo period (DATA%PHOTO) < 13 and decreasing during egg lay, then make diapause egg 
# (lay at 2.5cm and stop development at 45% under ideal conditions)
egglay <- function(photoperiod,previous_photoperiod, SHALtemp, DEEPtemp, SHALmoist){
  if(photoperiod<photo_thresh && photoperiod<=previous_photoperiod){
    #set diapause potential
    out.diapause_pot <-TRUE
    out.temp <- SHALtemp
    out.moist <- SHALmoist
    out.diapause_egg <-TRUE
  }else{
    out.diapause_pot <- FALSE
    out.temp <- DEEPtemp
    out.moist<- SHALmoist
    out.diapause_egg <-FALSE
  }
  out.diapause_hours <- 0
  out.cold_hours     <- 0
  out.dev            <- 0
  out.dry_hours      <- 0
  out.in_diapause    <- FALSE
  
  list(moist = out.moist                  ,
       temp = out.temp                    ,
       diapause_pot = out.diapause_pot    ,
       in_diapause  = out.in_diapause     ,
       diapause_hours = out.diapause_hours,
       cold_hours   = out.cold_hours      ,
       dry_hours    = out.dry_hours       ,
       diapause_egg = out.diapause_egg    ,
       dev = out.dev)
}

# Temp dependent function for C. terminifera egg dev rate fitted from Gregg (1984) egg dev. rates 
devrate <- function(temp){
  if(temp<=32){
    y = -12334*(1/(temp+273)) + 38.027 
    return(exp(y)/24) # divide by 24 to get units in 1/h
  } else{
    return(devrate(32))
  }
}

photo_thresh <- 13 # h, below this threshold, eggs are laid with diapause potential
dry_thresh   <- 9  # %, below this threshold, the low soil moisture may trigger quiescence or desiccation
dry_hours_thresh <- 365*24 # h,  above this threshold, egg dessicates 
cold_thresh  <- 15 # C, below this threshold, total cold hours accumulate
cold_hours_thresh <- 60*24 # h, above this threshold of cumulative cold hours, diapause potential is lost
diapause_hours_thresh <- 7*7*24 # h,  above this threshold of cumulative diapause hours, diapause potential is lost 
DATA<-DATA1

gethatch<-function(ovirows, stagefreq1){
  n<-0 
  p<-0
  for(m in 1:nrow(ovirows)){ #loop through oviposition dates and test for successful development
    DATA<-DATA1
    row.names(DATA) <- NULL 
    dev  <- rep(0,nrow(DATA))
    # set developmental thresholds
    # create vector for previous day photoperiod
    DATA$PHOTOPREV <- DATA$PHOTO
    DATA$PHOTOPREV[25:nrow(DATA)]<-DATA$PHOTO[1:(nrow(DATA)-24)] 
    
    #egg <- egglay(DATA$PHOTO[2],DATA$PHOTOPREV[1], DATA$D5cm, DATA$D10cm, DATA$MOIST)
    egg <- egglay(DATA$PHOTO[ovirows[m,2]],DATA$PHOTOPREV[ovirows[m,2]-1], DATA$D5cm, DATA$D10cm, DATA$MOIST)
    # make counter for total number of consecutive egg hatches
    egg_gen <- 1
    # make empty vector for development 
    dev  <- rep(0,nrow(DATA))
    # make empty vectore for diapause potential
    dp   <- rep(0,nrow(DATA))
    # make empty state variable
    state <- rep("empty",nrow(DATA))
    # make empty variable for diapause egg (TRUE\FALSE) 
    d_e <- rep(FALSE,nrow(DATA))
    if(ovirows[m,2]+recover*24<=length(grass2) & ((ovirows[m,1]==1 & sum(grass2[ovirows[m,2]:(ovirows[m,2]+recover*24)])>=24*recover) | (ovirows[m,1]==0  ))){ # grass present for at least time to first lay
      ############## START HERE ################# 
      n<-n+1
      #DATA<-DATA_orig
      #row.names(DATA) <- NULL 
      
      
      
      
      # loop through each date and update egg development vector (dev)
      if(ovirows[m,1]==1){ # check if arriving for first time or if last clutch at end of grass growth
        start<-ovirows[m,2]+recover*24
      }else{
        start<-ovirows[m,2]
      }
      for(i in start:length(dev)){
        # if cold, increment cold hours 
        if(egg$temp[i]<cold_thresh){
          egg$cold_hours <- egg$cold_hours + 1  
        }
        # if dry, increment dry hours, if moist, reset dry hours
        if(egg$moist[i]<dry_thresh){
          egg$dry_hours <- egg$dry_hours + 1  
        }else{egg$dry_hours <- 0}  
        #if too long in cold conditions or in diapause, avert diapause potential 
        if(egg$cold_hours>cold_hours_thresh|| egg$diapause_hours>diapause_hours_thresh){
          egg$diapause_pot <- FALSE
        }
        
        if(egg$diapause_pot){
          dp[i]<-1
        }  else{ dp[i] <- 0
        }
        
        # If egg has diapause potential and at 45% dev, then egg is in diapause
        if(dev[i-1]>0.45 && dev[i-1]<0.46 && egg$diapause_pot == TRUE){ 
          egg$diapause_hours  <- egg$diapause_hours + 1
          egg$in_diapause <- TRUE
        } else{ egg$in_diapause <- FALSE
        }
        # if not quiescent (Q1:dry and 30% dev)
        if(egg$moist[i]<dry_thresh && dev[i-1]>0.30 && dev[i-1]<0.31){
          egg$Q1 <- TRUE
          egg$Q2 <- FALSE
          state[i]<- "Q1"
          stagefreq1[i,Q2col]<-stagefreq1[i, Q2col]+1
          # else is not Q2:not diapause, dry, 45% dev  
        }else if(!egg$in_diapause && egg$moist[i]<dry_thresh && dev[i-1]>0.45 && dev[i-1]<0.46){
          egg$Q1 <- FALSE 
          egg$Q2 <- TRUE
          state[i] <- "Q2"
          stagefreq1[i,Q2col]<-stagefreq1[i,Q2col]+1
        }else{egg$Q1 <- FALSE
              egg$Q2 <- FALSE
              state[i]<- "diapause"
              stagefreq1[i,Diapausecol]<-stagefreq1[i,Diapausecol]+1
        }
        # nor in diapause  (cold_hours <720, diapuase = 1, and 45% dev) 
        if(!(egg$Q1||egg$Q2||egg$in_diapause)){
          # increment development at a 0.08 increase rate per day at 32C or use temp correct factor
          egg$dev <- dev[i-1] + devrate(egg$temp[i]) 
          state[i]<-"dev"
          stagefreq1[i,Eggdevcol]<-stagefreq1[i,Eggdevcol]+1
          # else in diapause/quiescent, so do not increment 
        } else{  egg$dev <- dev[i-1]
        }  
        
        # if development complete, record hatch date  
        if(egg$dev>0.99){
          #egg <- egglay(DATA$PHOTO[i],DATA$PHOTOPREV[i-1], DATA$D5cm, DATA$D10cm, DATA$MOIST)
          egg_gen <- egg_gen + 1
          state[i]<-"fin"
          hatchdate<-i
          if(n==1){
            hatchdates<-hatchdate
          }else{
            hatchdates<-c(hatchdates,hatchdate)
          }
          break
          # if consecutive dry hours is more than 6 months, terminate egg and start again   
        }else if(egg$dry_hours>dry_hours_thresh){
          #egg <- egglay(DATA$PHOTO[i],DATA$PHOTOPREV[i-1], DATA$D5cm, DATA$D10cm, DATA$MOIST)
          stagefreq1[i,Deathcol]<-stagefreq1[i,Deathcol]+1
          break
        }
        
        # update egg development vector and diapause egg vector
        dev[i]   <- egg$dev
        d_e[i]   <- egg$diapause_egg
      }
      
    } # end check if within recovery time
    
    dev1<-as.data.frame(dev)
    dev1<-as.data.frame(cbind(metout$dates,dev1, DATA$D10cm, DATA$MOIST))
    colnames(dev1)<-c('dates','dev1','temp', 'moist')
    # update temp to account for diapause egg depths
    dev1$temp[d_e]<-DATA$D5cm[d_e]
    dev1$moist[d_e]<-DATA$MOIST[d_e]
    if(ovirows[m,1]!=1){
      p<-p+1
    }
    
    #   plot(SUM111~as.Date(DATE111,origin="1970-01-01"),type='s',xlab="date",ylab="total egg gens.",col="white",ylim=c(0,3))
    #   points(SUM111~as.Date(DATE111,origin="1970-01-01"),type='s',xlab="date",ylab="total egg gens.")
    #   
    #   title(main=paste("ovi_day ",m,sep=""))
    #   if(n==1){
    #   plot(grass/100~dev1$dates,type='l',col='green',ylim=c(0,1))
    #   points(dev1$dev1~dev1$dates,type='l',col='blue')
    #   }else{
    #     if(p==1){
    #       plot(grass/100~dev1$dates,type='l',col='green',ylim=c(0,1))
    #     }
    #       points(dev1$dev1~dev1$dates,type='l',col='blue')
    #   }
     if(n==1){
      devs<-subset(dev1,dev>0)
     }else{
      devs<-rbind(devs,dev1)
     }
    
  } # end loop through ovip dates
  return(list(hatchdates=hatchdates,devs=devs, stagefreq=stagefreq1))
}


# 
# 
# 
# 
# 
# 
# # write date to csv
# setwd("C:\\Users\\Jamos\\Dropbox\\My Manuscripts\\Plague Locust\\R scripts\\chortoicetes")
# system("rcmd start beep.wav")
# csveggdata = as.data.frame(cbind(as.character(as.Date(DATE111)),SUM111))
# colnames(csveggdata)<-c("Dates", "Egg gens in period")
# write.csv(csveggdata,file = paste(loc,"egg_gens.csv"))


pars_grow<-read.csv('chortoicetes/growth_coeff.csv')[,2]

dmass <- function(pars_grow,mass, temp){ #units of mg dry weight/day!
  if(temp>39){
    temp<-39
  }
  if(temp<25.9 & temp>20){
    temp<-25.9
  }
  # change in mass, as a function of fitted parameters, mass, and temp C
  dm<- mass*(pars_grow[2]+pars_grow[3]*temp**1+
               pars_grow[4]*temp**2+
               pars_grow[5]*temp**3+
               pars_grow[6]*temp**4+
               pars_grow[7]*temp**5)
  if(temp<=20.0){
    dm<-0
  }
  return(dm)
}

pars_survive<-read.csv('chortoicetes/survive_coeff.csv')[,2]

dsurvival<-function(pars_survive,survival, temp){ # units suvival prob/day
  # change in proportion surviving as a function of fitted pars, proportion surviving, and temp C  
  if(temp>39){
    temp<-39
  }
  if(temp<25.9){
    temp<-25.9
  }
  dy <- -survival*(      pars_survive[2]*temp**1+
                           pars_survive[3]*temp**2+
                           pars_survive[4]*temp**3+
                           pars_survive[5]*temp**4)
  
  return(dy)}

getinstarfrommass<-function(mass){
  if(mass<2.88){
    instar<-"N1"
  }else if(mass<5.56){
    instar<-"N2"
  }else if(mass<11.71){
    instar<-"N3"
  }else if(mass<25.53){
    instar<-"N4"
  }else if(mass<55.){
    instar<-"N5"
  }else if(mass>=55.){
    instar<-"Adult"
  }
  return(instar)
}

getgen<-function(hatchdates, stagefreq1){
  n<-0 
  p<-0
  starve<-24 # hrs locusts can go without food
  for(m in 1:length(hatchdates)){ #loop through hatch dates and test for successful reproduction
    row.names(DATA) <- NULL 
    masses  <- rep(0,nrow(DATA))
    n<-n+1
    ############## START HERE ################# 
    nograss<-0
    start<-hatchdates[m]
    drymass<-exp(pars_grow[1]) # fitted initial mass of 1 mg (egg mass)
    for(i in (start+1):length(masses)){
      if(i==start){
        masses[i]<-exp(pars_grow[1])
      }
      ddrymass<-dmass(pars_grow,drymass, environ[i,6])
      masses[i]<-masses[i-1]+ddrymass/24.
      instar<-getinstarfrommass(masses[i])
      # increment frequency of stage in stagefreq data frame
      eval(parse(text=paste("stagefreq1[",i,",",instar,"col]<-stagefreq1[",i,",",instar,"col]+1", sep="")))
      drymass<-masses[i]
      if(nograss>0){ # reset 'nograss' if there's been some growth
        if(grass2[i]==1){
          nograss<-0
        }
      }
      if(grass2[i]==0){
        nograss<-nograss+1
      }
      if(nograss<starve){
        if(masses[i]>55){ # if development complete, record hatch date 
          reprodate<-i
          p<-p+1
          if(p==1){
            reprodates<-c(reprodate,reprodate+7*24,reprodate+14*24) # three pods
            #reprodates<-c(reprodate) # one pod
          }else{
            reprodates<-c(reprodates,c(reprodate,reprodate+7*24,reprodate+14*24)) # three pods
            #reprodates<-c(reprodates,reprodate) # one pod
          }
          break
        }
      }else{
        stagefreq1[i,Deathcol]<- stagefreq1[i,Deathcol] + 1
        break
      }
    } # end loop through hatch dates 
    
    #   plot(SUM111~as.Date(DATE111,origin="1970-01-01"),type='s',xlab="date",ylab="total egg gens.",col="white",ylim=c(0,3))
    #   points(SUM111~as.Date(DATE111,origin="1970-01-01"),type='s',xlab="date",ylab="total egg gens.")
    #   
    #   title(main=paste("ovi_day ",m,sep=""))
    #   if(n==1){
    #     plot(environ$TC~dev1$dates,type='l',col='orange',ylim=c(0,60))
    #     points(grass~dev1$dates,type='l',col='green')
    #     points(masses~dev1$dates,type='l',col='blue')
    #   }else{
    #     points(masses~dev1$dates,type='l',col='blue')
    #   }
    devs1<-as.data.frame(cbind(as.data.frame(environ$dates),masses))
    colnames(devs1)<-c('dates','mass')
    devs<-subset(devs1,mass>0)
    if(n==1){
      gens<-devs
    }else{
      gens<-rbind(gens,devs)
    }
    
  }
  if(exists("reprodates")==FALSE){
    reprodates<-NULL
  }
  return(list(reprodates=reprodates,gens=gens, stagefreq = stagefreq1))
}


# make matrix for frequency of different stages through time
stagefreqnames<-c('Eggdev', 'Q1', 'Q2', 'Diapause', 'N1','N2','N3','N4','N5','Adult','Death' )
for(name in stagefreqnames){ # make variables to easily access matrix column by name
  eval(parse(text=paste(name,"col<-",which(stagefreqnames==name), sep = ""))) 
}
stagefreq<-rep(0,nrow(DATA1)*length(stagefreqnames))
dim(stagefreq)<-c(nrow(DATA1),length(stagefreqnames))


for(g in 1:100){
  if(g==1){
    hatchings<-gethatch(ovirows, stagefreq)
    hatchdates<-hatchings$hatchdates 
    generations<-getgen(hatchdates, hatchings$stagefreq)
    reprodates<-generations$reprodates
    reprodates1<-reprodates
    gens<-generations$gens
  }else{
    if(length(reprodates)>0){
      hatchings<-gethatch(cbind(0,reprodates),stagefreq)
      hatchdates<-hatchings$hatchdates
      generations<-getgen(hatchdates, hatchings$stagefreq)
      reprodates<-generations$reprodates
      gens<-generations$gens
    }else{
      break
    }
  }
  if(length(reprodates)>0){
  if(g==1){
    allgens<-gens
  }else{
    if(sum(reprodates1)!=sum(reprodates)){
    allgens<-rbind(allgens,gens)
    reprodates1<-reprodates
    }else{
      break
    }
  }
  cat('gen ',g,'\n')
  #plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
  #points(grass~environ$dates,type='l',col='green')
  #points(allgens$mass~allgens$dates,type='p',col='blue',cex=0.1)
  }else{
    break
  }
}
      
plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
points(grass2~environ$dates,type='l',col='green')
points(allgens$mass~allgens$dates,type='p',col='blue',cex=0.1)

success<-as.data.frame(subset(allgens,mass>55))
success$dates

# plot individual gens by running each generation manually
hatchings<-gethatch(ovirows, stagefreq)
hatchdates<-hatchings$hatchdates
dev1<-as.data.frame(hatchings$devs)
plot(grass2/10~environ$dates,type='l',col='green',ylim=c(0,1))
points(dev1$dev1~dev1$dates,type='p',col='blue',cex=0.1)
generations<-getgen(hatchdates, hatchings$stagefreq)
reprodates1<-generations$reprodates
gens1<-generations$gens
plot(grass2/10~environ$dates,type='l',col='green',ylim=c(0,1))
points(gens1$mass/55~gens1$dates,type='p',col='orange',cex=0.1)

hatchings2<-gethatch(cbind(0,reprodates1), generations$stagefreq)
hatchdates2<-hatchings2$hatchdates
dev2<-as.data.frame(hatchings2$devs)
#points(dev2$dev1~dev2$dates,type='p',col='light blue',cex=0.1)
generations<-getgen(hatchdates2,hatchings2$stagefreq)
reprodates2<-generations$reprodates
gens2<-generations$gens
points(gens2$mass/55~gens2$dates,type='p',col='red',cex=0.1)

hatchings3<-gethatch(cbind(0,reprodates2), generations$stagefreq)
hatchdates3<-hatchings3$hatchdates
dev3<-as.data.frame(hatchings3$devs)
#points(dev3$dev1~dev2$dates,type='p',col='light blue',cex=0.1)
generations<-getgen(hatchdates3,hatchings3$stagefreq)
reprodates3<-generations$reprodates
gens3<-generations$gens
points(gens3$mass/55~gens3$dates,type='p',col='red',cex=0.1)

hatchings4<-gethatch(cbind(0,reprodates3), generations$stagefreq)
hatchdates4<-hatchings4$hatchdates
dev4<-as.data.frame(hatchings4$devs)
#points(dev3$dev1~dev2$dates,type='p',col='light blue',cex=0.1)
generations<-getgen(hatchdates4,hatchings4$stagefreq)
gens4<-generations$gens
points(gens4$mass/55~gens4$dates,type='p',col='red',cex=0.1)


colors<-rainbow(6)
stagefreqDF<-as.data.frame(generations$stagefreq)

stagefreqDF<-cbind(dates,longlat[1],longlat[2],stagefreqDF)
names(stagefreqDF)<-c('date','long','lat',stagefreqnames)

stagefreqDF_agg<-aggregate(stagefreqDF[,2:14],by=list(format(dates,'%Y-%m-%d')),max)

eggs<-rowSums(generations$stagefreq[,Eggdevcol:Diapausecol]) # summ all nymphs to get total nymphs
nymphs <- rowSums(generations$stagefreq[,N1col:N5col]) # summ all egg stages to get all eggs
nymphs1 <- generations$stagefreq[,N1col] # summ all egg stages to get all eggs
nymphs2 <- generations$stagefreq[,N2col] # summ all egg stages to get all eggs
nymphs3 <- generations$stagefreq[,N3col] # summ all egg stages to get all eggs
nymphs4 <- generations$stagefreq[,N4col] # summ all egg stages to get all eggs
nymphs5 <- generations$stagefreq[,N5col] # summ all egg stages to get all eggs
adults <- generations$stagefreq[,Adultcol] # summ all egg stages to get all eggs
diapause <- generations$stagefreq[,Diapausecol] # summ all egg stages to get all eggs
plot(grass2~environ$dates,type='h',col='green',ylim=c(0,max(c((grass/10),max(nymphs1)))),ylab='count',xlab='')
lines( DATA1$DATE, nymphs1, 'l', col = colors[1])
plot(grass2~environ$dates,type='h',col='green',ylim=c(0,max(c((grass/10),max(nymphs2)))),ylab='count',xlab='')
lines(DATA1$DATE, nymphs2  , 'l', col = colors[1])
lines( DATA1$DATE, nymphs3, 'l', col = colors[3])
lines(DATA1$DATE, nymphs4  , 'l', col = colors[4])
lines( DATA1$DATE, nymphs5, 'l', col = colors[5])
plot(grass2~environ$dates,type='h',col='green',ylim=c(0,max(c((grass/10),max(adults)))),ylab='count',xlab='')
lines( DATA1$DATE, adults, 'l', col = colors[6])
lines(DATA1$DATE, diapause  , 'l', col = colors[3])
lines(DATA1$DATE, eggs  , 'l', col = colors[3])
lines( DATA1$DATE, nymphs, 'l', col = colors[1])

legend("topleft",c('eggs','grass','nymphs'), , col = colors, lty = 1, bg = NULL, bty = 'n', title = "instar")





hatchings<-gethatch(ovirows)
hatchdates<-hatchings$hatchdates
dev1<-as.data.frame(hatchings$devs)
#plot(grass/100~environ$dates,type='l',col='green',ylim=c(0,1))
#points(dev1$dev1~dev1$dates,type='p',col='blue',cex=0.1)

generations<-getgen(hatchdates)
reprodates1<-generations$reprodates
gens1<-generations$gens
#points(gens1$mass/55~gens1$dates,type='p',col='orange',cex=0.1)

hatchings2<-gethatch(cbind(0,reprodates1))
hatchdates2<-hatchings2$hatchdates
dev2<-as.data.frame(hatchings2$devs)
#points(dev2$dev1~dev2$dates,type='p',col='light blue',cex=0.1)

generations<-getgen(hatchdates2)
reprodates2<-generations$reprodates
gens2<-generations$gens
#points(gens2$mass/55~gens2$dates,type='p',col='red',cex=0.1)

hatchings3<-gethatch(cbind(0,reprodates2))
hatchdates3<-hatchings3$hatchdates
dev3<-as.data.frame(hatchings3$devs)
#points(dev3$dev1~dev2$dates,type='p',col='light blue',cex=0.1)
generations<-getgen(hatchdates3)
gens3<-generations$gens
#points(gens3$mass/55~gens2$dates,type='p',col='red',cex=0.1)


gens<-rbind(gens1,gens2,gens3)




plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
points(grass~environ$dates,type='l',col='green')
points(gens$mass~gens$dates,type='p',col='blue',cex=0.1)
success<-as.data.frame(subset(gens,mass>55))
success$dates

plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
points(grass~environ$dates,type='l',col='green')
points(gens1$mass~gens1$dates,type='p',col='blue',cex=0.1)
plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
points(grass~environ$dates,type='l',col='green')
points(gens2$mass~gens2$dates,type='p',col='blue',cex=0.1)
plot(environ$TC~environ$dates,type='l',col='orange',ylim=c(0,60))
points(grass~environ$dates,type='l',col='green')
points(gens3$mass~gens3$dates,type='p',col='blue',cex=0.1)
