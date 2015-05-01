############# ectotherm model parameters ################################

setwd("source/") # set the working directory where the fortran program is
cmnd<- "rcmd SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f "
#      R CMD SHLIB ectotherm.f Aboveground.f Allom.f ANCORR.f Belowground.f BURROWIN.f COND.f CONFAC.f Deb_baby.f DRYAIR.f Dsub.f Fun.f Funskin.f Gear.f JAC.f Met.f Osub.f RADIN.f RADOUT.f Resp.f Seldep.f Sevap.f SHADEADJUST.f Solar.f Thermo~1.f Timcon.f Traphr.f VAPPRS.f WATER.f WETAIR.f ZBRAC.f ZBRENT.f CONV.f Breed.f DEVRESET.f Wings.f Trapzd.f Wing_Solar.f Rytrec.f QTRAP.f Qromb.f Polint.f Parect.f Func.f Btrflalom.f Adjrec.f funwing.f ZBRACwing.f ZBRENTwing.f Deb_insect.f Deb.f
system(cmnd) # run the compilation
file.copy('ectotherm.dll','../ectotherm.dll',overwrite=TRUE)
setwd("..")

# copy microclimate model outputs to current directory
file.copy('/git/micro_global/metout.csv','metout.csv',overwrite=TRUE)
file.copy('/git/micro_global/shadmet.csv','shadmet.csv',overwrite=TRUE)
file.copy('/git/micro_global/soil.csv','soil.csv',overwrite=TRUE)
file.copy('/git/micro_global/shadsoil.csv','shadsoil.csv',overwrite=TRUE)
file.copy('/git/micro_global/soilmoist.csv','soilmoist.csv',overwrite=TRUE)
file.copy('/git/micro_global/shadmoist.csv','shadmoist.csv',overwrite=TRUE)
file.copy('/git/micro_global/soilpot.csv','soilpot.csv',overwrite=TRUE)
file.copy('/git/micro_global/shadpot.csv','shadpot.csv',overwrite=TRUE)
file.copy('/git/micro_global/humid.csv','humid.csv',overwrite=TRUE)
file.copy('/git/micro_global/shadhumid.csv','shadhumid.csv',overwrite=TRUE)
file.copy('/git/micro_global/rainfall.csv','rainfall.csv',overwrite=TRUE)
file.copy('/git/micro_global/ectoin.csv','ectoin.csv',overwrite=TRUE)
file.copy('/git/micro_global/DEP.csv','DEP.csv',overwrite=TRUE)
file.copy('/git/micro_global/MAXSHADES.csv','MAXSHADES.csv',overwrite=TRUE)

microin<-"" # directory where the microclimate model outputs are (empty if in present directory)

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism/behavoiur) or dead animal?

# habitat settings
minshade<-0. # minimum available shade (percent)
maxshade<-90. # maximum available shade (percent)

# morphological traits
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
lometry<-3 # organism shape (see above)
ABSMAX<-0.866 # decimal %, maximum solar absorptivity
ABSMIN<-0.866 # decimal %, maximum solar absorptivity
ptcond<-0.15 # decimal % of surface contacting the substrate

# physiological traits
TMAXPR<-35.5 # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection) # Licht 1966 thermal gradient
TMINPR<-12.15 # degrees C, voluntary thermal minimum (lower body temperature for foraging) # Kearney Obs (PhD field trip)
TBASK<-TMINPR#5 # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-9.2 # degrees C, temperature at which animal will move to a basking site
ctmax<-38.6  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-10 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
TPREF<-31.4 # preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
skinwet<-0.229 # %, percentage of total surface area acting like a free water surface for evaporation 

# behavioural traits
dayact<-0 # diurnal activity allowed (1) or not (0)?
nocturn<-1 # nocturnal activity allowed (1) or not (0)?
crepus<-1 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-0 # choose if the animal's retreat is in the shade (1) or in the open (0)
mindepth<-2 # minimum depth (soil node) to which animal can retreat if burrowing
maxdepth<-10 # maximum depth (soil node) to which animal can retreat if burrowing
CkGrShad<-1 # shade seeking allowed (1) or not (0)?
climb<-0 # climbing to seek cooler habitats allowed (1) or not (0)?

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
amass<-4 # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-0.013
MR_2<-0.8
MR_3<-0.038

#set up call to NicheMapR function
niche<-list(mac=mac,microin=microin,minshade=minshade,maxshade=maxshade,lometry=lometry,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,skinwet=skinwet,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,amass=amass,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,maxdepth=maxdepth,mindepth=mindepth,TBASK=TBASK,TEMERGE=TEMERGE)
source('NicheMapR_Setup_ecto_basic.R')
nicheout<-NicheMapR_ecto(niche)

# retrieve output
metout<-as.data.frame(read.table(file='metout.csv',sep=",",header=TRUE))[,-1]
shadmet<-as.data.frame(read.table('shadmet.csv',sep=",",header=TRUE))[,-1]
soil<-as.data.frame(read.table('soil.csv',sep=",",header=TRUE))[,-1]
shadsoil<-as.data.frame(read.table('shadsoil.csv',sep=",",header=TRUE))[,-1]
rainfall<-as.data.frame(nicheout$RAINFALL)
nyears<-nicheout$nyears
timeinterval<-nicheout$timeinterval
environ<-as.data.frame(nicheout$environ[1:(timeinterval*24*nyears),])
enbal<-as.data.frame(nicheout$enbal[1:(timeinterval*24*nyears),])
masbal<-as.data.frame(nicheout$masbal[1:(timeinterval*24*nyears),])

# append dates
if(timeinterval==365){
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(2014,1,1,tz=tzone)-3600*12, ISOdate((2014+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
dates2<-seq(ISOdate(2014,1,1,tz=tzone)-3600*12, ISOdate((2014+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
}else{
  dates<-environ$DAY+environ$TIME/24-1
  dates2<-seq(1,timeinterval,1)
}
environ<-cbind(dates,environ)
masbal<-cbind(dates,masbal)
enbal<-cbind(dates,enbal)
soil<-cbind(dates,soil)
metout<-cbind(dates,metout)
shadsoil<-cbind(dates,shadsoil)
shadmet<-cbind(dates,shadmet)
rainfall<-as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)<-c("dates","rainfall")

############### plot results ######################
library(lattice) # package used for 'xyplot'
juldays<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.) # middle day of each month

with(environ, plot(TC~dates,ylim=c(-20,70),type = "l"))
with(environ, points(ACT*5~dates,type = "l",col="orange"))
with(environ, points(SHADE/10~dates,type = "l",col="green"))
with(environ, points(DEP/10~dates,type = "l",col="brown"))
with(metout, points(TAREF~dates,type = "l",col="light blue"))
abline(TMAXPR,0,lty=2,col='red')
abline(TMINPR,0,lty=2,col='blue')

forage<-subset(environ,ACT==2)
bask<-subset(environ,ACT==1)
night<-subset(metout,ZEN==90)
day<-subset(metout,ZEN!=90)
with(night,plot(TIME/60~JULDAY,pch=15,cex=2,col='dark blue'))
with(forage,points((TIME-1)~JULDAY,pch=15,cex=2,col='orange'))
with(bask,points((TIME-1)~JULDAY,pch=15,cex=2,col='light blue'))

# code to get the 'constant temperature equivalent' (CTE)

# Arrenius response parameter
T_REF<-20 # degrees C, reference temperature - correction factor is 1 for this temperature
TA<-10191
TAL<-50000
TAH<-90000
TL<-273+10
TH<-273+37

plot(environ$TC,type='l') # plot of body temperatures across all hours
TempCorr<-as.numeric(exp(TA*(1/(273+T_REF)-1/(273+environ$TC)))/(1+exp(TAL*(1/(273+environ$TC)-1/TL))+exp(TAH*(1/TH-1/(273+environ$TC))))) # convert Tb each hour to temperature correction factor
plot(TempCorr,type='l') # plot of temperature correction across all hours
TempCorr_mean<-mean(TempCorr) # get mean temperature correction factor
TempCorr_mean # report value to console
getTb<-function(Tb){ # function finding the difference between a temperature correction factor for a specified Tb compared to the mean calculated one (aim to make this zero)
      x<-exp(TA*(1/(273+T_REF)-1/(273+Tb)))/(1+exp(TAL*(1/(273+Tb)-1/TL))+exp(TAH*(1/TH-1/(273+Tb))))-TempCorr_mean
   }
CTE<-uniroot(f=getTb,c(TL-273,TH-273),check.conv=TRUE)$root # search for a Tb (CTE) that gives the same temperature correction factor as the mean of the simulated temperature corrections
mean(environ$TC) # report mean Tb to screen
CTE # report constant temperature equivalent to screen


# run again to get Te in sun
live<-0 # live (metabolism/behavoiur) or dead animal?
#set up call to NicheMapR function
niche<-list(mac=mac,microin=microin,write_input=write_input,minshade=minshade,maxshade=maxshade,REFL=REFL,nyears=nyears,enberr=enberr,SUBTK=SUBTK,rinsul=rinsul,lometry=lometry,Flshcond=Flshcond,Spheat=Spheat,Andens=Andens,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,DELTAR=DELTAR,skinwet=skinwet,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,amass=amass,customallom=customallom,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,EMISAN=EMISAN,FATOSK=FATOSK,FATOSB=FATOSB,maxdepth=maxdepth,mindepth=mindepth,TBASK=TBASK,TEMERGE=TEMERGE)
source('NicheMapR_Setup_ecto_basic.R')
nicheout<-NicheMapR_ecto(niche)
environ_Tesun<-as.data.frame(nicheout$environ[1:(timeinterval*24*nyears),])
environ_Tesun<-cbind(dates,environ_Tesun)

month<-1 # choose month of year (1-12)
for(month in 1:12){
plotenviron<-subset(environ,JULDAY==juldays[month])
plotmetout<-subset(metout,JULDAY==juldays[month])
hours<-seq(0,23,1)
with(plotenviron, plot(TC~hours,ylim=c(-15,70),type = "l"))
with(plotenviron, points(ACT*5~hours,type = "l",col="orange"))
with(plotenviron, points(SHADE/10~hours,type = "l",col="green"))
with(plotenviron, points(DEP/10~hours,type = "l",col="brown"))
with(plotmetout, points(TAREF~hours,type = "l",col="light blue"))
abline(TMAXPR,0,lty=2,col='red')
abline(TMINPR,0,lty=2,col='blue')
plotenviron_Tesun<-subset(environ_Tesun,JULDAY==juldays[month])
with(plotenviron_Tesun, points(TC~hours,ylim=c(-15,50),type = "l",lty=2))
title(main=paste("month ",month,sep=""))
text("TMAXPR",col='red',x=1,y=70)
text("TMINPR",col='blue',x=3.5,y=70)
text("Tbody",col='black',x=5.5,y=70)
text("Tair",col='light blue',x=7,y=70)
text("Te",col='black',x=8,y=70)
text("Shade(%/10)",col='green',x=10.2,y=70)
text("Depth",col='brown',x=13,y=70)
text("Active",col='orange',x=15,y=70)
}


