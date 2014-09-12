      SUBROUTINE WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,  
     *                      CP,WTRPOT) 
C*********************************************************************  

C SUBROUTINE WETAIR CALCULATES SEVERAL PROPERTIES OF HUMID AIR SHOWN AS 
C OUTPUT VARIABLES BELOW.  THE PROGRAM IS BASED ON EQUATIONS FROM LIST, 
C R. J. 1971.  SMITHSONIAN METEOROLOGICAL TABLES. SMITHSONIAN   
C INSTITUTION PRESS. WASHINGTON, DC.  WETAIR MUST BE USED IN CONJUNCTION
C WITH FUNCTION VAPPRS. 

C INPUT VARIABLES ARE SHOWN BELOW.  THE USER MUST SUPPLY KNOWN VALUES   
C FOR DB AND BP (BP AT ONE STANDARD ATMOSPHERE IS 101 325 PASCALS). 
C VALUES FOR THE REMAINING VARIABLES ARE DETERMINED BY WHETHER THE USER 
C HAS EITHER (1) PSYCHROMETRIC DATA (WB OR RH), OR (2) HYGROMETRIC DATA 
C (DP). 

C (1) PSYCHROMETRIC DATA:   
C IF WB IS KNOWN BUT NOT RH THEN SET RH = -1. AND DP = 999.     
C IF RH IS KNOWN BUT NOT WB THEN SET WB = 0. AND DP = 999.  

C (2) HYGROMETRIC DATA: 
C IF DP IS KNOWN THEN SET WB = 0. AND RH = 0.   

C************************* INPUT VARIABLES ***************************  

C DB = DRY BULB TEMPERATURE (DEGREE CELSIUS)
C WB = WET BULB TEMPERATURE (DEGREE CELSIUS)
C RH = RELATIVE HUMIDITY (PER CENT) 
C DP = DEW POINT TEMPERATURE (DEGREE CELSIUS)   
C BP = BAROMETRIC PRESSURE (PASCAL) 

C************************* OUTPUT VARIABLES **************************  

C E =      VAPOR PRESSURE (PASCAL)  
C ESAT =   SATURATION VAPOR PRESSURE (PASCAL)   
C VD =     VAPOR DENSITY (KILOGRAM PER CUBIC METRE) 
C RW =     MIXING RATIO (KILOGRAM PER KILOGRAM) 
C TVIR =   VIRTUAL TEMPERATURE (KELVIN) 
C TVINC =  VIRTUAL TEMPERATURE INCREMENT (KELVIN)   
C DENAIR = DENSITY OF HUMID AIR (KILOGRAM PER CUBIC METRE)  
C CP =     SPECIFIC HEAT OF AIR AT CONSTANT PRESSURE (JOULE PER 
C            KILOGRAM KELVIN)   
C WTRPOT = WATER POTENTIAL (PASCAL) 
    
C*********************************************************************  

c  subroutine wetair calculates several properties of humid air.  this version  
c  was taken from "properties of air: a manual for use in biophysical ecology   
c  third edition.

      Implicit None

      Real bp,cp,db,denair,dltae,dp,e,esat
      Real rh,rw,tk,tvinc,tvir,vapprs,vd,wb,wbd,wbsat,wtrpot
   
      tk  = db + 273.15 
      esat = vapprs(db) 
      if (dp .lt. 999.0) go to 100  
      if (rh .gt. -1.0) go to 200   
      wbd = db - wb 
      wbsat = vapprs(wb)
      dltae = 0.000660 * (1.0 + 0.00115 * wb) * bp * wbd
      e = wbsat - dltae 
      go to 300 
100   e = vapprs(db)
      go to 300 
200   e = esat * rh / 100.   
      go to 400 
300   rh = (e / esat) * 100. 
400   rw = ((0.62197 * 1.0053 * e) / (bp - 1.0053 * e)) 
      vd = e * 0.018016 / (0.998 * 8.31434 * tk)
      tvir = tk * ((1.0 + rw / (18.016 / 28.966)) / (1.0 + rw)) 
      tvinc = tvir - tk 
      denair = 0.0034838 * bp / (0.999 * tvir)  
      cp = (1004.84 + (rw * 1846.40)) / (1.0 + rw)  
      if (rh .le. 0.0) go to 500
      wtrpot = 4.615e+5 * tk * alog(rh / 100.0) 
      go to 600 
500   wtrpot = -999 
600   return
      end