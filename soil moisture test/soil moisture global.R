############# ectotherm model parameters ################################

setwd("source/") # set the working directory where the fortran program is
cmnd<- "rcmd SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
#R CMD SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

##### loop through soil moisture observations ##########
ystart<-2009
yfinish<-2009
basedir<-getwd()

## start loop for all sites
workdir<-"C:/Users/mrke/My Dropbox/Soil moisture data/oznet/"
sitedata<-read.csv(paste(workdir,"oznetsiteinfo.csv",sep=""))
allsitenames<-as.character(sitedata$name)
# delete site k14
allsitenames<-allsitenames[allsitenames!="k14"]
isite<-37
for(isite in 1:nrow(sitedata)){
  # load oznet data 
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
  
  ########################## run microclimate model #########################################################
    
  setwd("/git/micro_australia/")
  cmd<-paste("R --no-save --args ",longlat[1], " ",longlat[2]," ",ystart," ",yfinish," < runmicro.R",sep="")
  system(cmd)
  setwd(basedir)

  ########################## start run ectotherm model to get soil moisture #################################
  
  microin<-"soil moisture test" # subfolder containing the microclimate input data
  # get input microclimate files
  file.copy('/git/micro_global/metout.csv',paste(microin,'/metout.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/shadmet.csv',paste(microin,'/shadmet.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/soil.csv',paste(microin,'/soil.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/shadsoil.csv',paste(microin,'/shadsoil.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/rainfall.csv',paste(microin,'/rainfall.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/ectoin.csv',paste(microin,'/ectoin.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/DEP.csv',paste(microin,'/DEP.csv',sep=""),overwrite=TRUE)
  file.copy('/git/micro_global/MAXSHADES.csv',paste(microin,'/MAXSHADES.csv',sep=""),overwrite=TRUE)
  
  # simulation settings
  live<-1 # live (metabolism) or dead animal?
  enberr<-0.0002 # tolerance for energy balance
  timeinterval<-365 # number of time intervals in a year
  ystart<-read.csv(paste(microin,'/ectoin.csv',sep=""))[7,2]
  yfinish<-read.csv(paste(microin,'/ectoin.csv',sep=""))[8,2]
  nyears<-ceiling(nrow(read.csv(paste(microin,'/rainfall.csv',sep="")))/365) # number of years the simulation runs for (work out from input data)
  write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
  longlat<-c(read.csv(paste(microin,'/ectoin.csv',sep=""))[3,2],read.csv(paste(microin,'/ectoin.csv',sep=""))[4,2])
  
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
  ABSMAX<-0.71 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
  ABSMIN<-0.71 # ** decimal %, maximum solar absorptivity (Christian, K.A., Bedford, G.S. & Shannahan, S.T. (1996) Solar absorptance of some Australian lizards and its relationship to temperature. Australian Journal of Zoology, 44.)
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
  TMAXPR<-39 #34 ** degrees C, voluntary thermal maximum (upper body temperature for foraging) Pamula 1997 - where frequency dropped substantially, rather than extreme (Fig. 3.42)
  TMINPR<-26.0 #26.0 # ** degrees C, voluntary thermal minimum (lower body temperature for foraging) Pamula 1997 (Fig. 3.42)
  TBASK<-23.1#19#26.#23.1 # degrees C, minimum basking temperature Pamula Table 3.14
  TEMERGE<-8.5#8.5 # degrees C, temperature at which animal will move to a basking site *based on Kerr and Bull 2004
  ctmax<-43.0 # ** degrees C, critical thermal maximum (used by program to determine depth selected when inactive and burrowing) (43.0, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.)
  ctmin<-3.5 # ** degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing) (3.5, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.)
  ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
  ctkill<-0 #if 1, animal dies when it hits critical thermal limits
  TPREF<-33.5 # ** preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
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
  burrow<-0 # shelter in burrow allowed (1) or not (0)?
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
  conth<-2.5 # cylindrical container/pond height (cm)
  contw<-50. # cylindrical container/pond diameter (cm)
  contype<-0 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
  rainmult<-1 # rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
  continit<-0 # initial container water level (cm)
  conthole<- 0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
  contonly<-1 # just run the container model and quit?
  contwet<-80 # percent wet value for container
  wetmod<-0 # run the wetland model?
  soilmoisture<-1 # run the soil moisture model? (models near-surface soil moisture rather than a pond as a function of field capacity and wilting point)
  
  # which energy budget model to use? 
  DEB<-0 # run the DEB model (1) or just heat balance, using allometric respiration below (0)
  
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
  
  #set up call to NicheMapR function
  niche<-list(microin=microin,soilmoisture=soilmoisture,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,FLTYPE=FLTYPE,SUBTK=SUBTK,soilnode=soilnode,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,fosorial=fosorial,rainact=rainact,actrainthresh=actrainthresh,container=container,conth=conth,contw=contw,rainmult=rainmult,andens_deb=andens_deb,d_V=d_V,d_E=d_E,eggdryfrac=eggdryfrac,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,kappa_X_P=kappa_X_P,mu_X=mu_X,mu_E=mu_E,mu_V=mu_V,mu_P=mu_P,nX=nX,nE=nE,nV=nV,nP=nP,N_waste=N_waste,T_REF=T_REF,TA=TA,TAL=TAL,TAH=TAH,TL=TL,TH=TH,z=z,kappa=kappa,kappa_X=kappa_X,p_Mref=p_Mref,v_dotref=v_dotref,E_G=E_G,k_R=k_R,MsM=MsM,delta=delta,h_aref=h_aref,viviparous=viviparous,k_J=k_J,E_Hb=E_Hb,E_Hj=E_Hj,E_Hp=E_Hp,svl_met=svl_met,frogbreed=frogbreed,frogstage=frogstage,clutchsize=clutchsize,v_init=v_init,E_init=E_init,E_H_init=E_H_init,eggmass=eggmass,batch=batch,breedrainthresh=breedrainthresh,daylengthstart=daylengthstart,daylenghtfinish=daylengthfinish,photodirs=photodirs,photodirf=photodirf,photostart=photostart,photofinish=photofinish,amass=amass,customallom=customallom,E_Egg=E_Egg,PTUREA=PTUREA,PFEWAT=PFEWAT,FoodWater=FoodWater,DEB=DEB,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,f=f,minwater=minwater,s_G=s_G,K=K,X=X,flyer=flyer,flyspeed=flyspeed,maxdepth=maxdepth,mindepth=mindepth,ctminthresh=ctminthresh,ctkill=ctkill,metab_mode=metab_mode,stages=stages,p_Am1=p_Am1,p_AmIm=p_AmIm,arrhenius=arrhenius,disc=disc,gam=gam,startday=startday,raindrink=raindrink,reset=reset,gutfill=gutfill,TBASK=TBASK,TEMERGE=TEMERGE,p_Xm=p_Xm,flymetab=flymetab,live=live,continit=continit,wetmod=wetmod,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,stage=stage,ma=ma,mi=mi,mh=mh,aestivate=aestivate,depress=depress,contype=contype,rainmult=rainmult,conthole=conthole,contonly=contonly,contwet=contwet)
  source('NicheMapR_Setup_ecto.R')
  nicheout<-NicheMapR_ecto(niche)
  
  # retrieve output
  metout<-as.data.frame(read.table(file=paste(microin,'/metout.csv',sep=""),sep=",",header=TRUE))[,-1]
  shadmet<-as.data.frame(read.table(paste(microin,'/shadmet.csv',sep=""),sep=",",header=TRUE))[,-1]
  soil<-as.data.frame(read.table(paste(microin,'/soil.csv',sep=""),sep=",",header=TRUE))[,-1]
  shadsoil<-as.data.frame(read.table(paste(microin,'/shadsoil.csv',sep=""),sep=",",header=TRUE))[,-1]
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

  ########################## end run ectotherm model to get soil moisture #################################


  daystart<-paste('07/01/21',sep="") # y/m/d
  dayfin<-paste('07/02/21',sep="") # y/m/d
  plotmetout<-subset(metout, format(metout$dates, "%y/%m/%d")>= daystart & format(metout$dates, "%y/%m/%d")<=dayfin)
  plotsoil<-subset(soil, format(soil$dates, "%y/%m/%d")>= daystart & format(soil$dates, "%y/%m/%d")<=dayfin)
  plotenviron<-subset(environ, format(environ$dates, "%y/%m/%d")>= daystart & format(environ$dates, "%y/%m/%d")<=dayfin)
  
#with(environ[environ$DAY<20,],plot(WATERTEMP~dates,type='l'))
#  plot(plotenviron$CONDEP~plotenviron$dates,type='l') 
#plot(plotenviron$WATERTEMP~plotenviron$dates,type='l')
  
  
  d1<-as.data.frame(as.Date(metout$dates-3600*10, format="%Y-%m-%d"))
  colnames(d1)<-'dates'
  datestart<-paste("1/1/",ystart,sep="") # day, month, year
  datefinish<-paste("31/12/",yfinish,sep="") # day, month, year
  datestart<-strptime(datestart, "%d/%m/%Y") # convert to date format
  datefinish<-strptime(datefinish, "%d/%m/%Y") # convert to date format
  dates<-seq(datestart,datefinish,"days")
  dates_noleap<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
  soilmdates<-as.Date(dates_noleap+3600*11)
  
  soilmoisturep <- environ$CONDEP
  #grass<-as.data.frame(nicheout$grassgrowths)
  #grass<-cbind(soilmdates,grass)
  #colnames(grass)<-c('dates','grassgrowth')
  #g<-merge(d1,grass)
  #library('geosphere')
  #PHOTO<-daylength(longlat[,2],metout$JULDAY)
  
  DATA_orig<-cbind(soil[,1:7],metout[,13:14],environ[,17],soilmoisturep)
  colnames(DATA_orig)<-c('DATE','JULDAY','TIME','D0cm','D2_5cm','D5cm','D10cm','ZEN','SOLR','PHOTO','MOIST')
  DATA <- DATA_orig
  
  # take only hourly data (data in 20min intervals)
  XLdat <- XLdat[seq(1,nrow(XLdat),60/oznetdatafreq),]
  
  
  XLdat1 <- subset(XLdat,  DATE > as.Date(paste('01/01/',ystart, sep = ""), "%d/%m/%Y"))
  XLdat1 <- subset(XLdat1, DATE < as.Date(paste('01/01/',yfinish+1, sep = ""), "%d/%m/%Y")) 
  
  
  #setwd(paste(workdir,"plots/",sep=""))
  png(filename=paste(oznetsite,".png",sep=""), width=1000,height=400)
  plot(XLdat1$DATE, XLdat1$SM_0_5cm, 'l', ylim = c(0,50), col = "red",
       xlab="date", ylab="moisture (% vol)")
  lines(as.Date(DATA$DATE), DATA$MOIST, 'l')
  #with(rainfall,points(as.Date(dates),rainfall,type='h',col='light blue'))
  
#   MOISTDEEP <- t(moists[1,]*100) 
#   MDdates <-DATA$DATE[seq(1,length(DATA$DATE),24)]
#   lines(as.Date(MDdates),MOISTDEEP, 'l', col="green")
#   MOISTDEEPER <- t(moists[3,]*100) 
#   lines(as.Date(MDdates),MOISTDEEPER, 'l', col="blue")
  title(main=oznetsite)
  legend("topleft", inset=.05,
         c("data","predict"), 
         fill=c("red","black"),bty="n", 
         horiz=TRUE, bg=NULL, cex=0.8)
abline(0,5)
  dev.off()
  
  ## end loop for all sites
}



