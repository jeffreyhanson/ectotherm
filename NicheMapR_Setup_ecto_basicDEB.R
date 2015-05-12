NicheMapR_ecto <- function(niche) {

  
enberr<-0.0002 # tolerance for energy balance solution
#timeinterval<-1 # number of time intervals computed in a year (min of 12, i.e. monthly, max of 365, make the same as was set for the microclimate run)
nyears<-read.csv('ectoin.csv')[8,2]-read.csv('ectoin.csv')[7,2]+1 # number of years the simulation runs for 
write_input<-0# write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[3,2],read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[4,2]) # get longitude and latitude from microclimate output
nyears<-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[8,2]-read.csv(file=paste(microin,'ectoin.csv',sep=""),sep=",")[7,2]+1 # number of years the simulation runs for 
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
FLTYPE<-0.0  # fluid type 0.0=air, 1.0=water 
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
rinsul<-0. # m, insulative fat layer thickness
water_stages[,2]<-extref
    RAINFALL<-as.matrix(read.csv(file=paste(microin,'rainfall.csv',sep=""),sep=","))[,2]
  timeinterval<-nrow(as.data.frame(RAINFALL))/nyears
  vlsci<-0
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
  
#   #turn on container model if aquatic egg/larval phase
#   if(frogbreed==1 | frogbreed==2){
#     container<-1
#   }
#   if(frogbreed==3){
#     container<-0
#   }
  
  # container/pond initial conditons
  contlast<-0.
  templast<-7.
  
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
# for insect model
metab_mode<-0 # 0 = off, 1 = hemimetabolus model (to do), 2 = holometabolous model
stages<-7 # number of stages (max = 8) = number of instars plus 1 for egg + 1 for pupa + 1 for imago
y_EV_l<-0.95 # mol/mol, yield of imago reserve on larval structure
S_instar<-c(2.660,2.310,1.916,0) # -, stress at instar n: L_n^2/ L_n-1^2
s_j<-0.999 # -, reprod buffer/structure at pupation as fraction of max

grasshade<-0
  
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
  behav_stages[,10]<-fosorial
behav_stages[,11]<-rainact
behav_stages[,12]<-actrainthresh
behav_stages[,13]<-breedactthresh
behav_stages[,14]<-flyer
  wilting<-1
ystrt<-0
soilmoisture<-0
  
  iyear<-0 #initializing year counter
  countday<-1 #initializing day counter
  
  wetlandTemps=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  wetlandDepths=matrix(data = 0., nrow = 24*nyears*timeinterval, ncol = 1)
  if(vlsci==0){
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
  metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","SOILMOIST3","POOLDEP","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
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
  } # end vlsci check
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
  
    #wilting<-ectoin[6] # %vol, water content at 15ba = 1500kPa (wiki for thresholds)

  lat<-ectoin[4]
  if(soilmoisture==1){
 

  grassgrowths<-as.data.frame(soilpot)
  soilmoist2<-as.data.frame(soilmoist)
  soilmoist2<-subset(soilmoist2,TIME==720)
  grassgrowths<-subset(grassgrowths,TIME==720)
  grassgrowths<-grassgrowths$PT10cm
    
    grow<-grassgrowths
    grow<-grassgrowths
    grow[grow>-1500]<-1
    grow[grow<=-1500]<-0
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
     grow3[grow3<7]<-0
     grow3[grow3>0]<-1
    
  soilmoist2<-soilmoist2$WC10cm
  grassgrowths<-as.data.frame(cbind(grassgrowths,soilmoist2))
  colnames(grassgrowths)<-c('pot','moist')
  grassgrowths$pot[grassgrowths$pot>-200]<-FoodWater
  grassgrowths$moist<-grassgrowths$moist*100
  potmult<-grassgrowths$pot
  potmult[potmult!=82]<-0
  potmult[potmult!=0]<-1  
  wilting<-subset(grassgrowths,pot==FoodWater)
  wilting<-min(wilting$moist)
  grassgrowths<-grassgrowths$moist
  grassgrowths[grassgrowths>wilting]<-FoodWater
  minmoist<-min(grassgrowths[grassgrowths<FoodWater])
  grassgrowths[grassgrowths<FoodWater]<-(grassgrowths[grassgrowths<FoodWater]-minmoist)/(wilting-minmoist)*FoodWater
  grassgrowths<-grassgrowths/100*grow3
  grasstsdms<-grassgrowths
  #minmoist<-min(grassgrowths)
  #grassgrowths<-(as.numeric(grassgrowths)-minmoist)
  #maxmoist<-max(grassgrowths)
  #grassgrowths[grassgrowths<wilting]<-0
  #grassgrowths<-grassgrowths/maxmoist*X#rep(X,timeinterval*nyears)#
  #grasstsdms<-grassgrowths/maxmoist*X#rep(X,timeinterval*nyears)#
  }else{
    grassgrowths<-rep(FoodWater,nrow(metout))
    grasstsdms<-grassgrowths
  }
  julstart<-metout[1,2]
  tannul<-as.numeric(metout[1,11])
  monthly<-0
  tester<-0
  microyear<-1
  
  # bucket model for soil moisture
  fieldcap<-ectoin[5]# %vol, water content at 0.1ba = 10kPa
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
  

  
  ectoinput<-c(ALT,FLTYPE,OBJDIS,OBJL,PCTDIF,EMISSK,EMISSB,ABSSB,shade,enberr,AMASS,EMISAN,absan,RQ,rinsul,lometry,live,TIMBAS,Flshcond,Spheat,Andens,ABSMAX,ABSMIN,FATOSK,FATOSB,FATOBJ,TMAXPR,TMINPR,DELTAR,SKINW,spec,xbas,extref,TPREF,ptcond,skint,gas,transt,soilnode,o2max,ACTLVL,tannul,nodnum,tdigpr,maxshd,minshd,ctmax,ctmin,behav,julday,actrainthresh,viviparous,pregnant,conth,contw,contlast,tranin,tcinit,nyears,lat,rainmult,julstart,monthly,customallom,MR_1,MR_2,MR_3,DEB,tester,rho1_3,trans1,aref,bref,cref,phi,wings,phimax,phimin,shape_a,shape_b,shape_c,minwater,microyear,container,flyer,flyspeed,timeinterval,maxdepth,ctminthresh,ctkill,gutfill,mindepth,TBASK,TEMERGE,p_Xm,SUBTK,flymetab,continit,wetmod,contonly,conthole,contype,shdburrow,breedtempthresh,breedtempcum,contwet,fieldcap,wilting,soilmoisture,grasshade)
  debmod<-c(clutchsize,andens_deb,d_V,eggdryfrac,mu_X,mu_E,mu_V,mu_P,T_REF,z,kappa,kappa_X,p_Mref,v_dotref,E_G,k_R,MsM,delta,h_aref,V_init_baby,E_init_baby,k_J,E_Hb,E_Hj,E_Hp,clutch_ab[2],batch,breedrainthresh,photostart,photofinish,daylengthstart,daylengthfinish,photodirs,photodirf,clutch_ab[1],frogbreed,frogstage,etaO,JM_JO,E_Egg,kappa_X_P,PTUREA1,PFEWAT1,wO,w_N,FoodWater1,f,s_G,K,X,metab_mode,stages,y_EV_l,s_j,startday,raindrink,reset,ma,mi,mh,aestivate,depress)

  deblast<-c(iyear,countday,v_init,E_init,ms_init,cumrepro_init,q_init,hs_init,cumbatch_init,V_baby_init,E_baby_init,E_H_init,stage)
  
  origjulday<-metout[,1]
  if(ystrt>0){
    metout<-rbind(metout[((ystrt)*365*24+1):(nyears*timeinterval*24),],metout[1:((ystrt)*365*24),])
    shadmet<-rbind(shadmet[((ystrt)*365*24+1):(nyears*timeinterval*24),],shadmet[1:((ystrt)*365*24),])
    soil<-rbind(soil[((ystrt)*365*24+1):(nyears*timeinterval*24),],soil[1:((ystrt)*365*24),])
    shadsoil<-rbind(shadsoil[((ystrt)*365*24+1):(nyears*timeinterval*24),],shadsoil[1:((ystrt)*365*24),])
    MAXSHADES<-c(MAXSHADES[((ystrt)*365+1):(nyears*timeinterval)],MAXSHADES[1:((ystrt)*365)])
    RAINFALL<-c(RAINFALL[((ystrt)*365+1):(nyears*timeinterval)],RAINFALL[1:((ystrt)*365)])
    grassgrowths<-c(grassgrowths[((ystrt)*365+1):(nyears*timeinterval)],grassgrowths[1:((ystrt)*365)])
  }
  metout[,1]<-origjulday
  shadmet[,1]<-origjulday
  soil[,1]<-origjulday
  shadsoil[,1]<-origjulday
  
  if(write_input==1){
    cat('writing input csv files \n')
    write.csv(ectoinput, file = "csv input/ectoinput.csv")
    write.csv(debmod, file = "csv input/debmod.csv")
    write.csv(deblast, file = "csv input/deblast.csv")
    write.csv(RAINFALL, file = "csv input/rainfall.csv")
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
 if(vlsci==1){
    setwd("/vlsci/VR0212/shared/NicheMapR_Working/ectotherm")
    source('NicheMapR_ecto.R')
  }else{
  if(mac==1){
    source('NicheMapR_ecto_mac.R') 
  }else{
    source('NicheMapR_ecto.R') 
  }
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