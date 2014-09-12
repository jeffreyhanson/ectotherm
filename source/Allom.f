       SUBROUTINE ALLOM  
C    COPYRIGHT 1998 WARREN P. PORTER
C     version 7 August, 1998  set up with Charles Curtin's box turtle data   

      Implicit None

      REAL A,ABSAN,ABSMAX,ABSMIN,ABSSB,Aeff,AEYES,AHEIT,AL,ALENTH,ALT
      Real AMASS,ANDENS,Area,ASILN
      Real ASILP,ATOT,ATOTAL,AV,AWIDTH,B,BP,C,CUTFA,D,DEPSUB
      Real EMISAN,EMISSB,EMISSK,Fatcond
      Real FATOBJ,FATOSB,FATOSK,Flshcond,Fluid
      Real G,GMASS,HD,HDforc,HDfree,O2MAX,O2MIN
      REAL PCTDIF,PCTEYE,PCTN,PCTP,PI,PTCOND
      Real QSOLR,R,RELHUM,Rinsul,R1
      Real SIG,SkinW,SPHEAT,SUBTK
      Real TA,Tskin,TOBJ,TR,TSKY,TSUBST,TOTLEN,VEL,VOL
      Real WC,WCUT,WEVAP,WEYES,WRESP,ZEN
      REAL ASEMAJR,BSEMINR,CSEMINR,conthole
      Real Skint,AT,rainfall,H2O_BalPast,F12,F32,F42,F52,P

      real CONTH,CONTW,CONTVOL,CONTDEP,CONTDEPTH,contlast
      real customallom,shp,HC,convar 
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &,phimin,phimax,twing,contwet,continit,rainmult
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26

      INTEGER IHOUR,IMODEL,LIVE,Lometry,MICRO,NM,Nodnum,NumFed,NumHrs
      integer wingmod,wingcalc,wetmod,contonly,pond,contype

      CHARACTER*1 SPEC
      DIMENSION customallom(8),shp(3)

      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW
     &,SkinT,HC,convar 
      common/evap2/HDfree,HDforc
      common/evap3/spec
      COMMON/WCOND/TOTLEN,AV,AT
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp
      COMMON/MODEL/IMODEL
      COMMON/ELLIPS/ASEMAJR,BSEMINR,CSEMINR 
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      Common/Dimens/ALENTH,AWIDTH,AHEIT
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,wetmod,contonly,conthole
     &    ,contype,contwet,continit,contlast
      COMMON/CONTDEPTH/CONTDEPTH
      Common/Rainfall/Rainfall,rainmult
      common/pondtest/pond

      Data PI/3.14159/

      VOL = AMASS/ANDENS   
      R = (0.75 * VOL/PI) **.333  
      R1 = R - Rinsul 
      D=2*R1
      AEYES=0.

C     JOHN MITCHEL'S CHARACTERISTIC DIMENSION FOR CONVECTION (1976)  
      AL = VOL ** .333 
      GMASS = AMASS*1000.

      IF((CONTH .gt. 0.).and.(pond.eq.1))THEN
c    container model running
       R = CONTW/2/100
       R1 = R - Rinsul
       D=2*R1
c      Make sure container doesn't overfill
       IF(CONTDEP .gt. CONTH*10)THEN
        CONTDEP = CONTH*10
       ENDIF
       VOL = pi*r1**2*(contdep/1000)
       AMASS = VOL*ANDENS
       if(AMASS .lt. 0)then
        AMASS = 0
        contdep=0
       endif

       if(contype.eq.1)then
c     container is sunk into the ground so only one of the circles of the cylinder exposed to convection/evap
        AREA = 2*pi*r1**2+2*pi*r1*(contdep/1000)
        AWIDTH = 2.*R1
        ALENTH = contdep/1000
        ASILN = PI * R1**2
        ASILP = AWIDTH * ALENTH
        Av = pi*r1**2+2*pi*r1*(contdep/1000)
        ATOT = AREA 
        At = Area * SkinT
        if(contdep.le.0.01)then
         aeff=0
        else
         Aeff = PI * R1**2*(SkinW/100.)
        endif
        AL=contdep/1000
        GMASS = AMASS*1000.
        AEYES = 0.
        lometry=1
        FATOSK=pi*r1**2/AREA
        FATOSB=0
        FATOBJ=0
        GO TO 999
       endif
       if(contype.eq.0)then
c     container is sitting on surface so sides are exposed too
        AREA = 2*pi*r1**2+2*pi*r1*(contdep/1000)
        AWIDTH = 2.*R1
        ALENTH = contdep/1000
        ASILN = PI * R1**2
        ASILP = AWIDTH * ALENTH
        Av = pi*r1**2
        ATOT = AREA 
        At = Area * SkinT
        if(contdep.le.0.01)then
         aeff=0
        else
         Aeff = PI * R1**2*(SkinW/100.)
        endif
        AL=contdep/1000
        GMASS = AMASS*1000.
        AEYES = 0.
        lometry=1
        FATOSK=pi*r1**2/AREA
        FATOSB=2*pi*r1*(contdep/1000)/AREA
        FATOBJ=0
        GO TO 999
       endif
      ENDIF

      if (wingmod .GE. 1) then
c    wing model running
       if(wingcalc.eq.1)then
        ALENTH = bref/100
        AWIDTH = cref/100
        AHEIT =  0.01/100       
        ATOT = ALENTH * AWIDTH * 2. + ALENTH * AHEIT * 2. 
     &   + AWIDTH * AHEIT * 2.
        AREA = ATOT
        ASILN = ALENTH * AWIDTH
        ASILP = AWIDTH * AHEIT
        AL = ALENTH
        R = ALENTH/2.
        R1=R
        D=2*R
        VOL = ALENTH * AWIDTH * AHEIT
        Av = Atot/2
        Aeff = 0.
        GO TO 999
       else
        A = ((3./4.)*VOL/(PI*shp(2)*shp(3)))**0.333
        ALENTH = A
        B = A*shp(2) 
        C = A*shp(3)
        P = 1.6075
        AREA = (4*pi*(((A**P*B**P+A**P*C**P+B**P*C**P))/3)**(1/P))
        Av = Area * Ptcond
        ATOT = AREA 
        Aeff = (SkinW/100.) * (area - Av)    
        ASILN = PI * A * C
        ASILP = PI * B * C
        ASEMAJR = A
        BSEMINR = B
        CSEMINR = C
c      R = aref/2/100
c      R1 = R - Rinsul
c      D=2*R1
c      VOL = pi*r1**2*(bref/100)
c      AMASS = VOL*ANDENS
c        AREA = 2*pi*r1**2+2*pi*r1*(bref/100)
c        AWIDTH = 2.*R1
c        ALENTH = bref/100
c        ASILN = AWIDTH * ALENTH
c        ASILP = PI * R1**2
c      Av = pi*r1**2
c      ATOT = AREA 
c      At = Area * SkinT
c        Aeff = PI * R1**2*(SkinW/100.)
c      AL=(2*(2/1000000)/(pi*AWIDTH))**(1/2)
        GO TO 999
       endif
      ENDIF 

C     FLAT PLATE 
      if (Lometry .EQ. 0) then
C       Assume a cube for the moment
        ALENTH = (VOL/(shp(2)*shp(3)))**(1./3.)
        AWIDTH = ALENTH*shp(2)
        AHEIT =  ALENTH*shp(3)      
        ATOT = ALENTH * AWIDTH * 2. + ALENTH * AHEIT * 2. 
     &   + AWIDTH * AHEIT * 2.
        AREA = ATOT
        ASILN = ALENTH * AWIDTH
        ASILP = AWIDTH * AHEIT
        AL=AHEIT
        if(AWIDTH.le.ALENTH)then
        AL=AWIDTH
        else
        AL=ALENTH
        endif
        R = ALENTH/2.
        Av = Atot * Ptcond
        Aeff = (SkinW/100.) * (area - Av)
        GO TO 999
      ENDIF 

C     CYLINDER      
      IF (Lometry .EQ. 1) THEN
        R1=(VOL/(pi*shp(2)))**(1./3.)
        ALENTH=R1*shp(2)
        Area = 2*pi*r1**2+2*pi*r1*ALENTH
        VOL = AMASS/ANDENS
        AWIDTH = 2.*R1
        ASILN = AWIDTH * ALENTH
        ASILP = PI * R1**2
        Av = Area * Ptcond
        ATOT = AREA 
        Aeff = (SkinW/100.) * (area - Av)
        AL=ALENTH
        TOTLEN=ALENTH
        GO TO 999
      ENDIF 
      
      IF (Lometry .EQ. 2) THEN
c    Ellipsoid
        A = ((3./4.)*VOL/(PI*shp(2)*shp(3)))**0.333   
        B = A*shp(2) 
        C = A*shp(3)
        P = 1.6075
        AREA = (4*pi*(((A**P*B**P+A**P*C**P+B**P*C**P))/3)**(1/P))
        Av = Area * Ptcond
        ATOT = AREA 
        Aeff = (SkinW/100.) * (area - Av)    
        ASILN = PI * A * C
        ASILP = PI * B * C
        if(ASILN.lt.ASILP)then
         ASILN = PI * B * C
         ASILP = PI * A * C
        endif       
        ASEMAJR = A
        BSEMINR = B
        CSEMINR = C
        GO TO 999      
      ENDIF                     

      If (Lometry .eq. 3) then
C      Lizard - need to add correct equation here
C       User must define total and ventral area (m2) - Dipsosaurus dorsalis          
        ATOTAL = (10.4713*GMASS**.688)/10000.
        AV = (0.425*GMASS**.85)/10000.   
        ATOT = ATOTAL
        VOL = AMASS/ANDENS  
C       Conduction - radiation, etc dimension. Assume L=2D=4R1; 
C       Then Vol=PI*R1**2*L = 4*PI*R1**3
        R1 = (Vol/(4.*Pi))**0.333 
C       NORMAL AND POINTING @ SUN SILHOUETTE AREA: PORTER & TRACY 1984   
C       Max. silhouette area (normal to the sun)
        ASILN = (3.798*GMASS**.683)/10000.
C       Min. silhouette area (pointing toward the sun)         
        ASILP = (0.694*GMASS**.743)/10000.
        Area = Atot
        Av = Area * Ptcond
        At = Area * SkinT
        Aeff = ((SkinW/100.) * area) - ((SkinW/100.) * area) * Ptcond 
     & - ((SkinW/100.) * area) * SkinT
      Endif

      if(Lometry.eq.4) then

C       AREA OF LEOPARD FROG (C.R. TRACY 1976 ECOL. MONOG.)
        ATOTAL = (12.79*GMASS**.606)/10000.  
        AV = (0.425*GMASS**.85)/10000.   
        ATOT = ATOTAL    

C       NORMAL AND POINTING @ SUN SILHOUETTE AREA: EQ'N 11 TRACY 1976
        ZEN = 0.
        PCTN = 1.38171E-06*ZEN**4 - 1.93335E-04*ZEN**3 + 
     &  4.75761E-03*ZEN**2 - 0.167912*ZEN + 45.8228  
        ASILN = PCTN * ATOT/100. 

        ZEN = 90. 
        PCTP = 1.38171E-06*ZEN**4 - 1.93335E-04*ZEN**3 + 
     &  4.75761E-03*ZEN**2 - 0.167912*ZEN + 45.8228  
        ASILP = PCTP * ATOT/100.
        Area = Atot 
        Av = Area * Ptcond
            At = Area * SkinT
        Aeff = ((SkinW/100.) * area) - ((SkinW/100.) * area) * Ptcond 
     & - ((SkinW/100.) * area) * SkinT
      endif


      if(Lometry.eq.5) then
C       user defined allometry
        ATOTAL = (customallom(1)*GMASS**customallom(2))/10000.  
        AV = (customallom(3)*GMASS**customallom(4))/10000.   
        ATOT = ATOTAL    
        VOL = AMASS/ANDENS  
C       Conduction - radiation, etc dimension. Assume L=2D=4R1; 
C       Then Vol=PI*R1**2*L = 4*PI*R1**3
        R1 = (Vol/(4.*Pi))**0.333 
C       NORMAL AND POINTING @ SUN SILHOUETTE AREA: PORTER & TRACY 1984   
C       User must define Max. silhouette area (normal to the sun)
        ASILN = (customallom(5)*GMASS**customallom(6))/10000.
C       User must define Min. silhouette area (pointing toward the sun)         
        ASILP = (customallom(7)*GMASS**customallom(8))/10000.
        Area = Atot
        Av = Area * Ptcond
        At = Area * SkinT
        Aeff = ((SkinW/100.) * area) - ((SkinW/100.) * area) * Ptcond 
     & - ((SkinW/100.) * area) * SkinT
      endif

999   Continue
c    write(*,*)area    

      RETURN   
      END  



