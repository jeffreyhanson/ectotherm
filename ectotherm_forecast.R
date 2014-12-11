############# ectotherm model parameters ################################

# copy microclimate model outputs to current directory
file.copy('/git/micro_australia/metout.csv','metout.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadmet.csv','shadmet.csv',overwrite=TRUE)
file.copy('/git/micro_australia/soil.csv','soil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/shadsoil.csv','shadsoil.csv',overwrite=TRUE)
file.copy('/git/micro_australia/rainfall.csv','rainfall.csv',overwrite=TRUE)
file.copy('/git/micro_australia/ectoin.csv','ectoin.csv',overwrite=TRUE)
file.copy('/git/micro_australia/DEP.csv','DEP.csv',overwrite=TRUE)
file.copy('/git/micro_australia/MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)
file.copy('/git/micro_australia/forecast.csv','forecast.csv',overwrite=TRUE)

microin<-"" # directory where the microclimate model outputs are (empty if in present directory)

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism/behavoiur) or dead animal?
enberr<-0.0002 # tolerance for energy balance solution
timeinterval<-12 # number of time intervals computed in a year (min of 12, i.e. monthly, max of 365, make the same as was set for the microclimate run)
nyears<-1 # number of years the simulation runs for 
write_input<-0 # write input into 'csv input' folder? (1 yes, 0 no)
longlat<-c(read.csv('ectoin.csv')[3,2],read.csv('ectoin.csv')[4,2]) # get longitude and latitude from microclimate output

# habitat settings
SUBTK<-2.79 # substrate thermal conductivity (W/mC)
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
ABSMAX<-0.866 # decimal %, maximum solar absorptivity
ABSMIN<-0.866 # decimal %, maximum solar absorptivity
EMISAN<-0.95 # emissivity of animal, usually close to 1
ptcond<-0.25 # decimal % of surface contacting the substrate
FATOSK<-0.4 # configuration factor to sky
FATOSB<-0.4 # configuration factor to substrate

# physiological traits
TMAXPR<-41 # degrees C, voluntary thermal maximum (upper body temperature for foraging)
TMINPR<-33.0 # degrees C, voluntary thermal minimum (lower body temperature for foraging)
TBASK<-28.# degrees C, minimum basking temperature 
TEMERGE<-15.0 # degrees C, temperature at which animal will move to a basking site 
ctmax<-45.0 # degrees C, critical thermal maximum (used by program to determine depth selected when inactive and burrowing)
ctmin<-3.5 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
TPREF<-37 # preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
DELTAR<-0.1 # degrees C, temperature difference between expired and inspired air
skinwet<-0.2 # %, percentage of total surface area acting like a free water surface for evaporation 
extref<-20. # %, oxygen extraction efficiency (based on 35 deg C for a number of reptiles, from Perry, S.F., 1992. Gas exchange strategies in reptiles and the origin of the avian lung. In: Wood, S.C., Weber, R.E., Hargens, A.R., Millard, R.W. (Eds.), Physiological Adaptations in Vertebrates: Respiration, Circulation, andMetabo -  lism. Marcel Dekker, Inc., New York, pp. 149-167.)

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


# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-30. # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

#set up call to NicheMapR function
niche<-list(mac=mac,microin=microin,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,SUBTK=SUBTK,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,extref=extref,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,amass=amass,customallom=customallom,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,maxdepth=maxdepth,mindepth=mindepth,TBASK=TBASK,TEMERGE=TEMERGE)
source('NicheMapR_Setup_ecto_basic.R')
nicheout<-NicheMapR_ecto(niche)

# retrieve output
metout<-as.data.frame(read.table(file='metout.csv',sep=",",header=TRUE))[,-1]
shadmet<-as.data.frame(read.table('shadmet.csv',sep=",",header=TRUE))[,-1]
soil<-as.data.frame(read.table('soil.csv',sep=",",header=TRUE))[,-1]
shadsoil<-as.data.frame(read.table('shadsoil.csv',sep=",",header=TRUE))[,-1]
bomforecast<-as.data.frame(read.table('forecast.csv',sep=",",header=TRUE))[,-1]
rainfall<-as.data.frame(nicheout$RAINFALL)
environ<-as.data.frame(nicheout$environ[1:(2*24),])
enbal<-as.data.frame(nicheout$enbal[1:(2*24),])
masbal<-as.data.frame(nicheout$masbal[1:(2*24),])

dates<-as.POSIXct(bomforecast$times,format="%Y-%m-%d %H:%M:%S")

environ<-cbind(dates,environ)
masbal<-cbind(dates,masbal)
enbal<-cbind(dates,enbal)
soil<-cbind(dates,soil)
metout<-cbind(dates,metout)
shadsoil<-cbind(dates,shadsoil)
shadmet<-cbind(dates,shadmet)

with(environ, plot(TC~dates,ylim=c(-15,70),type = "l"))
with(environ, points(ACT*5~dates,type = "l",col="orange"))
with(environ, points(SHADE/10~dates,type = "l",col="green"))
with(environ, points(DEP/10~dates,type = "l",col="brown"))
#with(metout, points(TAREF~dates,type = "l",col="blue"))
abline(TMAXPR,0,lty=2,col='red')
abline(TMINPR,0,lty=2,col='blue')
abline(TBASK,0,lty=2,col='light blue')

forecast<-cbind(bomforecast[,3:4],environ[,1],environ[,6:7],environ[,9:10],bomforecast[,10:12],metout[,4:9],metout[,13:17],soil[,4:13])
colnames(forecast)<-c("lon","lat","date","bodytemp","shade","depth","activity","atmos_pressure","precip","cloud_cover","Tair_local"," Tair_1.2m","RH_local","RH_1.2m","wind_local","wind_1.2m","solar_angle","solar.Wm2","Tsky","DEW","FROST","Soil0cm","Soil2.5cm","Soil5cm","Soil10cm","Soil15cm","Soil20cm","Soil30cm","Soil50cm","Soil100cm","Soil200cm")
write.csv(forecast,"animal_forecast.csv")


