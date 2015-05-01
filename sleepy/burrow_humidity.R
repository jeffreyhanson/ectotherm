

ALT<-0 # altitude, m
BP<-101325. # baro pressure, Pa
DB<- 27 # dry bulb tunnel temp, deg C
  
x<-seq(10,1) # depth, m
y<-seq(900,1900,100) # vp ambient, Pa
z<--1403.775 + 100.521*x+ 3.842*y-7.466*x^2+ 0.037*x*y-0.001*y^2

TSTD=273.15   
PSTD=101325.  
PATMOS=PSTD*((1.-(.0065*ALT/288.))^(1./.190284)) 
if(BP < 0.){BP=PATMOS}   
DENSTY=BP/(287.04*(DB+TSTD))  
VISNOT=1.8325E-5  
TNOT=296.16   
C=120.
VISDYN=(VISNOT*((TNOT+C)/(DB+TSTD+C)))*(((DB+TSTD)/TNOT)^1.5)
VISKIN=VISDYN/DENSTY  
DIFVPR=2.26E-5*(((DB+TSTD)/TSTD)^1.81)*(1.E5/BP) 
THCOND=.02425+(7.038E-5*DB)   
HTOVPR=2.5012E6-2.3787E3*DB   
TCOEFF=1./(DB+TSTD)   
GGROUP=.0980616*TCOEFF/(VISKIN*VISKIN)

t=DB+273.16 
if(t<=273.16){
  loge=-9.09718*(273.16/t-1.)-3.56654*log10(273.16/t)+.876793*(1.-t/273.16)+log10(6.1071)   
  estar=(10.^loge)*100.
}else{
  loge=-7.90298*(373.16/t-1.)+5.02808*log10(373.16/t)-1.3816E-07*(10.^(11.344*(1.-t/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/t-1.))-1.)+log10(1013.246)
  estar=(10.^loge)*100  
}
esat=estar   

rh_ambient = (y / esat) * 100
rh_burrow = (z / esat) * 100
points(rh_burrow~rh_ambient,ylim=c(0,100),xlim=c(0,100))
abline(1,1)