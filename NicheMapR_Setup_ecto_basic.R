NicheMapR_ecto <- function(niche) {
  
enberr<-0.0002 # tolerance for energy balance solution
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[3,2],read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[4,2]) # get longitude and latitude from microclimate output
nyears<-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[8,2]-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[7,2]+1 # number of years the simulation runs for 
RAINFALL<-as.numeric(as.matrix(read.csv(file=paste(microin,'rainfall.csv',sep=""),sep=","))[,2])
timeinterval<-nrow(as.data.frame(RAINFALL))/nyears  
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
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
  # 5=custom (cylinder geometry is automatically invoked when container model operates)
EMISAN<-0.95 # emissivity of animal, usually close to 1
FATOSK<-0.4 # configuration factor to sky
FATOSB<-0.4 # configuration factor to substrate
rinsul<-0. # m, insulative fat layer thickness
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
extref<-20. # %, oxygen extraction efficiency (based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)
  
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
  
  
  PFEWAT<-73. # %, fecal water (from Shine's thesis, mixed diet 75% clover, 25% mealworms)
  PTUREA<-0. # %, water in excreted nitrogenous waste
  FoodWater<-82#82 # 82%, water content of food (from Shine's thesis, clover)
  minwater<-15 # %, minimum tolerated dehydration (% of wet mass) - prohibits foraging if greater than this
  raindrink<-0. # daily rainfall (mm) required for animal to rehydrate from drinking (zero means standing water always available)
  gutfill<-75. # % gut fill at which satiation occurs - if greater than 100%, animal always tries to forage
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
metab_mode<-0 # 0 = off, 1 = hemimetabolus model (to do), 2 = holometabolous model
stages<-7 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
y_EV_l<-0.95 # mol/mol, yield of imago reserve on larval structure
S_instar<-c(2.660,2.310,1.916,0) # -, stress at instar n: L_n^2/ L_n-1^2
s_j<-0.999 # -, reprod buffer/structure at pupation as fraction of max

  
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
  ctkill<-0 #if 1, animal dies when it hits critical thermal limits
  grasshade<-0
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
  
  if(lometry==3){
    shape_a<-1.
    shape_b<-1.
    shape_c<-4.
  }
  if(lometry==4){
    shape_a<-1.
    shape_b<-1.
    shape_c<-0.5
  }
  
  #turn on container model if aquatic egg/larval phase
  if(frogbreed==1 | frogbreed==2){
    container<-1
  }
  if(frogbreed==3){
    container<-0
  }
  
  # container/pond initial conditons
  contlast<-0.
  templast<-7.
  
  iyear<-0 #initializing year counter
  countday<-1 #initializing day counter
  
  wetlandTemps=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  wetlandDepths=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  
  cat('reading microclimate input \n')
  metout<-read.csv(file=paste(microin,'metout.csv',sep=""),sep=",")[,-1]
  shadmet<-read.csv(file=paste(microin,'shadmet.csv',sep=""),sep=",")[,-1]
  soil<-read.csv(file=paste(microin,'soil.csv',sep=""),sep=",")[,-1]
  shadsoil<-read.csv(file=paste(microin,'shadsoil.csv',sep=""),sep=",")[,-1]
  if(soilmoisture==1){
  soilpot<-read.csv(file=paste(microin,'soilpot.csv',sep=""),sep=",")[,-1]
  soilmoist<-read.csv(file=paste(microin,'soilmoist.csv',sep=""),sep=",")[,-1]
  shadpot<-read.csv(file=paste(microin,'shadpot.csv',sep=""),sep=",")[,-1]
  shadmoist<-read.csv(file=paste(microin,'shadmoist.csv',sep=""),sep=",")[,-1]
  humid<-read.csv(file=paste(microin,'humid.csv',sep=""),sep=",")[,-1]
  shadhumid<-read.csv(file=paste(microin,'shadhumid.csv',sep=""),sep=",")[,-1]  
  }else{
  soilpot<-soil
  soilmoist<-soil
  shadpot<-soil
  shadmoist<-soil
  humid<-soil
  shadhumid<-soil
  soilpot[,3:12]<-0
  soilmoist[,3:12]<-0.5
  shadpot[,3:12]<-0
  shadmoist[,3:12]<-0.5
  humid[,3:12]<-0.99
  shadhumid[,3:12]<-0.99
  }
  metout<-as.matrix(metout)
  shadmet<-as.matrix(shadmet)
  shadsoil<-as.matrix(shadsoil)
  soil<-as.matrix(soil)
  soilmoist<-as.matrix(soilmoist)
  shadmoist<-as.matrix(shadmoist)  
  soilpot<-as.matrix(soilpot)
  shadpot<-as.matrix(shadpot) 
  humid<-as.matrix(humid)
  shadhumid<-as.matrix(shadhumid)
  

  

  REFL<-rep(0.18,timeinterval*nyears) # substrate reflectances 

  ectoin<-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[,-1]
  DEP<-as.matrix(read.csv(file=paste(microin,'DEP.csv',sep=""),sep=","))[,2]
  MAXSHADES<-as.matrix(read.csv(file=paste(microin,'MAXSHADES.csv',sep=""),sep=","))[,2]
  
  metout2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 18) 
  soil2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)
  shadmet2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 18)
  shadsoil2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)
  soilmoist2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)  
  shadmoist2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)  
  soilpot2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)  
  shadpot2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)  
  humid2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)  
  shadhumid2=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 12)    
  wetlandTemps=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  wetlandDepths=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  metout2[1:nrow(metout),]<-metout
  shadmet2[1:nrow(metout),]<-shadmet
  soil2[1:nrow(metout),]<-soil
  shadsoil2[1:nrow(metout),]<-shadsoil
  soilmoist2[1:nrow(metout),]<-soilmoist
  shadmoist2[1:nrow(metout),]<-shadmoist
  soilpot2[1:nrow(metout),]<-soilpot
  shadpot2[1:nrow(metout),]<-shadpot
  humid2[1:nrow(metout),]<-humid
  shadhumid2[1:nrow(metout),]<-shadhumid
  metout<-metout2
  shadmet<-shadmet2
  soil<-soil2
  shadsoil<-shadsoil2
  soilmoist<-soilmoist2
  shadmoist<-shadmoist2
  soilpot<-soilpot2
  shadpot<-shadpot2
  humid<-humid2
  shadhumid<-shadhumid2    
  metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","TS","SOILMOIST","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
  colnames(metout)<-metout.names
  colnames(shadmet)<-metout.names
  soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
  colnames(soil)<-soil.names
  colnames(shadsoil)<-soil.names
  moist.names<-c("JULDAY","TIME",paste("WC",DEP,"cm", sep = ""))
  colnames(soilmoist)<-moist.names
  colnames(shadmoist)<-moist.names
  pot.names<-c("JULDAY","TIME",paste("PT",DEP,"cm", sep = ""))
  colnames(soilpot)<-pot.names
  colnames(shadpot)<-pot.names  
  hum.names<-c("JULDAY","TIME",paste("RH",DEP,"cm", sep = ""))
  colnames(humid)<-hum.names
  colnames(shadhumid)<-hum.names 
  
  # habitat
  ALT<-ectoin[1] # altitude (m)
  OBJDIS<-1.0 # distance from object (e.g. bush)
  OBJL<-0.0001
  PCTDIF<-0.1 # percent of sunlight that is diffuse (decimal %)
  EMISSK<-1.0 # emissivity of the sky (decimal %)
  EMISSB<-1.0 # emissivity of the substrate (decimal %)
  ABSSB<-1-ectoin[2] # solar absorbtivity of the substrate (decimal %)
  shade<-minshade # shade (%)
  
  # animal properties
  AMASS<-amass/1000 # animal mass (kg)
  absan<-ABSMAX # animal solar absorbtivity
  RQ<-0.8 # respiratory quotient
  
  FATOBJ<-0.
  #  if(container==1){
  #    live<-0}else{live<-1
  #  }
  #live<-1
  TIMBAS<-1.
  #  if(container==1){
  #    SKINW<-100.}else{
  SKINW<-skinwet
  #    }
  skint<-0.
  O2gas<-20.95
  CO2gas<-0.03
  N2gas<-79.02
  gas<-c(O2gas,CO2gas,N2gas)
  #  if(container==1){
  #    transt<-1
  #  }else{
  transt<-0
  #  }
  tranin<-1
  tcinit<-metout[1,"TALOC"]
  
  ACTLVL<-1
  nodnum<-10
  spec<-0. # spectacle covering eye surface? (adds to water loss for lizard/frog/turtle geometry)
  xbas<-1.
  nofood<-0 
  tdigpr<-TPREF 
  o2max<-extref
  #  if(container==1){
  #  maxshd<-1.
  #  minshd<-0.
  #  }else{
  maxshd<-maxshade
  minshd<-minshade
  #  }
  behav=c(dayact,nocturn,crepus,rainact,burrow,CkGrShad,climb,fosorial,nofood)
  julday<-1
  
  # DEB model initial conditions
  V_init_baby<-3e-9
  E_init_baby<-E_Egg/V_init_baby
  E_baby_init<-E_init_baby
  V_baby_init<-V_init_baby
  ms_init<-0.
  cumrepro_init<-0.
  q_init<-0.
  hs_init<-0.
  cumbatch_init<-0.
  pregnant<-0
  E_m<-(p_Mref*z/kappa)/v_dotref
  
  # conversions from percent to proportion
  PTUREA1<-PTUREA/100
  PFEWAT1<-PFEWAT/100
  FoodWater1<-FoodWater/100
  water_stages[,3]<-water_stages[,3]/100
  water_stages[,4]<-water_stages[,4]/100
  water_stages[,5]<-water_stages[,5]/100
  eggmass<-0 # initial dry mass of an egg (g) - no longer used so delete
  
  #DEB mass balance calculations
  nO<-cbind(nX,nV,nE,nP) # matrix of composition of organics, i.e. food, structure, reserve and faeces
  CHON<-c(12,1,16,14)
  wO<-CHON%*%nO
  w_V=wO[3]
  M_V<-d_V/w_V
  yEX<-kappa_X*mu_X/mu_E # yield of reserve on food
  yXE<-1/yEX # yield of food on reserve
  yVE<-mu_E*M_V/E_G  # yield of structure on reserve
  yPX<-kappa_X_P*mu_X/mu_P # yield of faeces on food
  yXP<-1/yPX # yield of food on faeces
  yPE<-yPX/yEX # yield of faeces on reserve  0.143382353
  nM<-matrix(c(1,0,2,0,0,2,1,0,0,0,2,0,N_waste),nrow=4)
  N_waste_inv<-c(-1*N_waste[1]/N_waste[4],(-1*N_waste[2])/(2*N_waste[4]),(4*N_waste[1]+N_waste[2]-2*N_waste[3])/(4*N_waste[4]),1/N_waste[4])
  nM_inv<-matrix(c(1,0,-1,0,0,1/2,-1/4,0,0,0,1/2,0,N_waste_inv),nrow=4)
  JM_JO<--1*nM_inv%*%nO
  etaO<-matrix(c(yXE/mu_E*-1,0,1/mu_E,yPE/mu_E,0,0,-1/mu_E,0,0,yVE/mu_E,-1/mu_E,0),nrow=4)
  w_N<-CHON%*%N_waste
  
  lat<-ectoin[4]
  grassgrowths<-rep(X,timeinterval*nyears)
  grasstsdms<-rep(X,timeinterval*nyears)
  julstart<-metout[1,2]
  tannul<-as.numeric(metout[1,11])
  monthly<-0
  tester<-0
  microyear<-1
  FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
  soilnode<-4. # soil node at which eggs are laid (overridden if frogbreed is 1)
  ctminthresh<-12 #number of consecutive hours below CTmin that leads to death
  clutch_ab<-c(0.1696,16.855) # paramters for relationship between length and clutch size: clutch size = a*SVL-b
  # bucket model for soil moisture
  fieldcap<-ectoin[5]# %vol, water content at 0.1ba = 10kPa
  wilting<-ectoin[6] # %vol, water content at 15ba = 1500kPa (wiki for thresholds)
  fieldcap<-30 # field capacity, m3/m3*100
  if(soilmoisture==1){
    conth<-fieldcap/10 # containter height, cm
    contw<-100
    contype<-1 # is 'containter' sitting on the surface, like a bucket (0) or sunk into the ground like a pond (1)
    rainmult<-0.3 # !!!!!!!!!!!!!!rainfall multiplier to reflect catchment (don't make this zero unless you want a drought!)
    continit<-0 # initial container water level (cm)
    conthole<-0#2.8 # daily loss of height (mm) due to 'hole' in container (e.g. infiltration to soil, drawdown from water tank)
    contwet<- 2 # percent wet value for container
  }
  ectoinput<-as.matrix(c(ALT,FLTYPE,OBJDIS,OBJL,PCTDIF,EMISSK,EMISSB,ABSSB,shade,enberr,AMASS,EMISAN,absan,RQ,rinsul,lometry,live,TIMBAS,Flshcond,Spheat,Andens,ABSMAX,ABSMIN,FATOSK,FATOSB,FATOBJ,TMAXPR,TMINPR,DELTAR,SKINW,spec,xbas,extref,TPREF,ptcond,skint,gas,transt,soilnode,o2max,ACTLVL,tannul,nodnum,tdigpr,maxshd,minshd,ctmax,ctmin,behav,julday,actrainthresh,viviparous,pregnant,conth,contw,contlast,tranin,tcinit,nyears,lat,rainmult,julstart,monthly,customallom,MR_1,MR_2,MR_3,DEB,tester,rho1_3,trans1,aref,bref,cref,phi,wings,phimax,phimin,shape_a,shape_b,shape_c,minwater,microyear,container,flyer,flyspeed,timeinterval,maxdepth,ctminthresh,ctkill,gutfill,mindepth,TBASK,TEMERGE,p_Xm,SUBTK,flymetab,continit,wetmod,contonly,conthole,contype,shdburrow,breedtempthresh,breedtempcum,contwet,fieldcap,wilting,soilmoisture,grasshade))
  debmod<-c(clutchsize,andens_deb,d_V,eggdryfrac,mu_X,mu_E,mu_V,mu_P,T_REF,z,kappa,kappa_X,p_Mref,v_dotref,E_G,k_R,MsM,delta,h_aref,V_init_baby,E_init_baby,k_J,E_Hb,E_Hj,E_Hp,clutch_ab[2],batch,breedrainthresh,photostart,photofinish,daylengthstart,daylengthfinish,photodirs,photodirf,clutch_ab[1],frogbreed,frogstage,etaO,JM_JO,E_Egg,kappa_X_P,PTUREA1,PFEWAT1,wO,w_N,FoodWater1,f,s_G,K,X,metab_mode,stages,y_EV_l,s_j,startday,raindrink,reset,ma,mi,mh,aestivate,depress)
  deblast<-c(iyear,countday,v_init,E_init,ms_init,cumrepro_init,q_init,hs_init,cumbatch_init,V_baby_init,E_baby_init,E_H_init,stage)
  
  if(write_input==1){
    cat('writing input csv files \n')
    write.csv(ectoinput, file = "csv input/ectoinput.csv")
    write.csv(debmod, file = "csv input/debmod.csv")
    write.csv(deblast, file = "csv input/deblast.csv")
    write.csv(as.numeric(RAINFALL), file = "csv input/rainfall.csv")
    write.csv(DEP, file = "csv input/dep.csv")
    write.csv(grassgrowths, file = "csv input/grassgrowths.csv")
    write.csv(grasstsdms, file = "csv input/grasstsdms.csv")
    write.csv(wetlandTemps, file = "csv input/wetlandTemps.csv")
    write.csv(wetlandDepths, file = "csv input/wetlandDepths.csv")
    write.csv(arrhenius, file = "csv input/arrhenius.csv")
    write.csv(thermal_stages, file = "csv input/thermal_stages.csv")
    write.csv(behav_stages, file = "csv input/behav_stages.csv")
    write.csv(water_stages, file = "csv input/water_stages.csv")
    write.csv(MAXSHADES, file = "csv input/Maxshades.csv")
    write.csv(S_instar, file = "csv input/S_instar.csv")
    write.table(metout2[(seq(1,nyears*timeinterval*24)),], file = "csv input/metout.csv",sep=",",row.names=FALSE)
    write.table(shadmet2[(seq(1,nyears*timeinterval*24)),], file = "csv input/shadmet.csv",sep=",",row.names=FALSE)
    write.table(soil2[(seq(1,nyears*timeinterval*24)),], file = "csv input/soil.csv",sep=",",row.names=FALSE)
    write.table(shadsoil2[(seq(1,nyears*timeinterval*24)),], file = "csv input/shadsoil.csv",sep=",",row.names=FALSE)
    write.table(soilmoist2[(seq(1,nyears*timeinterval*24)),], file = "csv input/soilmoist.csv",sep=",",row.names=FALSE)
    write.table(shadmoist2[(seq(1,nyears*timeinterval*24)),], file = "csv input/shadmoist.csv",sep=",",row.names=FALSE)
    write.table(soilpot2[(seq(1,nyears*timeinterval*24)),], file = "csv input/soilpot.csv",sep=",",row.names=FALSE)
    write.table(shadpot2[(seq(1,nyears*timeinterval*24)),], file = "csv input/shadpot.csv",sep=",",row.names=FALSE)
    write.table(humid2[(seq(1,nyears*timeinterval*24)),], file = "csv input/humid.csv",sep=",",row.names=FALSE)
    write.table(shadhumid2[(seq(1,nyears*timeinterval*24)),], file = "csv input/shadhumid.csv",sep=",",row.names=FALSE)
  }
  
  ecto<-list(ectoinput=ectoinput,metout=metout,shadmet=shadmet,soil=soil,shadsoil=shadsoil,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,DEP=DEP,RAINFALL=RAINFALL,iyear=iyear,countday=countday,debmod=debmod,deblast=deblast,grassgrowths=grassgrowths,grasstsdms=grasstsdms,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,arrhenius=arrhenius,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,MAXSHADES=MAXSHADES,S_instar=S_instar)
  if(mac==1){
    source('NicheMapR_ecto_mac.R') 
  }else{
    source('NicheMapR_ecto.R') 
  }
  cat('running ectotherm model ... \n')
  
  ptm <- proc.time() # Start timing
  ectout<-ectotherm(ecto)
  print(proc.time() - ptm) # Stop the clock
 
  
  environ<-ectout$environ[1:(365*24*nyears),]
  enbal<-ectout$enbal[1:(365*24*nyears),]
  masbal<-ectout$masbal[1:(365*24*nyears),]
  debout<-ectout$debout[1:(365*24*nyears),]
  yearout<-ectout$yearout
  yearsout<-ectout$yearsout[1:nyears,]
  
  if(DEB==0){
    return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,RAINFALL=RAINFALL,enbal=enbal,environ=environ,masbal=masbal,yearout=yearout,yearsout=yearsout,grassgrowths=grassgrowths,grasstsdms=grasstsdms,nyears=nyears,timeinterval=timeinterval))   
  }else{
    return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,soilmoist=soilmoist,shadmoist=shadmoist,soilpot=soilpot,shadpot=shadpot,humid=humid,shadhumid=shadhumid,RAINFALL=RAINFALL,enbal=enbal,masbal=masbal,environ=environ,debout=debout,yearout=yearout,yearsout=yearsout,grassgrowths=grassgrowths,grasstsdms=grasstsdms,nyears=nyears,timeinterval=timeinterval))
  }
  
}