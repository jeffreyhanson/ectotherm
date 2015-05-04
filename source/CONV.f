      SUBROUTINE CONV 

C     COPYRIGHT (C) 1988, W. P. PORTER, ALL RIGHTS RESERVED

C     ALL UNITS SI  (M,KG,S,C,K,J,PA)                                   
C     ALL EQUATIONS IN CONVEC AND FORCED DOUBLE CHECKED IN SOURCES  
C     AND CHECKED BY HAND CALCULATION JAN. '82; DEC '83 BY WPP  
C     COMBINED FREE AND FORCED SUBROUTINE CREATED 19 DEC '83 (WPP)  

      Implicit None

      REAL AEFF,AEYES,AL,ALT,AMASS,ANU,ANUfre,ATOT,AV
      Real BETA,BP,convar,CP,CUTFA,D,DB,DELTAT
      Real DENSTY,DEPSUB,DIFVPR,EMISAN
      Real FATOSK,FATOSB,Flshcond,FLTYPE   
      Real G,GGROUP,GR,GRRE2
      Real HC,HD,HDfree,HDforc,HTOVPR,PATMOS,PCTEYE,PR,PTCOND
      Real QCOND,QCONV,Qforced,Qfree
      Real QIRIN,QIROUT,QMETAB,Qresp,QSEVAP,QSOLAR
      Real R,Raylei,RE,RELHUM,SC,SHfree,SHforc,SIG,SkinW,SUBTK     
      Real TA,TCOEFF,THCOND,TOTLEN,Tskin,TR,TSUBST
      Real VEL,VISDYN,VISKIN 
      Real WCUT,WEVAP,WEYES,WRESP,H2O_BalPast
      Real SkinT,AT
      real customallom,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real ALENTH,AWIDTH,AHEIT,pi

      INTEGER Lometry,Nodnum,NumFed,NumHrs,wingmod,wingcalc
      
      CHARACTER*1 SPEC
      DIMENSION customallom(8),shp(3)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW
     &,SkinT,HC,convar 
      common/evap2/HDfree,HDforc
      common/evap3/spec
      COMMON/WCONV/FLTYPE
      COMMON/WCOND/TOTLEN,AV,AT
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp
      Common/Dimens/ALENTH,AWIDTH,AHEIT 

      Data PI/3.14159/

c    Setting the characteristic dimension for Nusselt-Reynolds correlations
      D = AL
      if(wingmod.gt.0)then
c    make the characteristic dimension increse with how open the wings are
c    which is worked out as A4 = D * cref/100, with A4 the area of imaginary surface
c    spanning the wings, bref the body length
       if(wingcalc.eq.1)then
c      if(phi.eq.180)then
c       D=cref/100*2+aref/100
c      else
         D=cref/100
c      endif
       else
c     if(phi.eq.180)then
c      D=ALENTH
c     else
        
        D=AL
c    D=AL
c     endif
c    D=ALENTH
c    D = a4/(bref/100.)
c     if(D.lt.AL)then
c      D=AL
c     endif
       endif
      endif

C       ALT = ALTITUDE  
C       ANU = NUSSELT NUMBER
C       ATOT = TOTAL AREA   
C       BETA = COEFFICIENT OF THERMAL EXPANSION AT CONSTANT DENSITY (1/K)
C     (USED IN GRASHOF NUMBER (1/FLUID TEMP (C), i.e. air or water) 
C       BP = BAROMETRIC PRESSURE
C       CP = SPECIFIC HEAT OF DRY AIR (J/KG-C)  
C       D = CHARACTERISTIC DIMENSION FOR CONVECTION 
C       DB = DRY BULB TEMPERATURE (C)   
C       DELTAT = TEMPERATURE DIFFERENCE 
C       DENSTY = DENSITY OF AIR 
C       DIFVPR = DIFFUSIVITY OF WATER VAPOR 
C    FLTYPE = FLUID TYPE (0.0 = AIR; 1.0 = WATER)
C       G = ACCELERATION DUE TO GRAVITY 
C       GGROUP = GROUP OF GRASHOF VARIABLES 
C       GR = GRASHOF NUMBER 
C       GRRE2 = GRASHOF/REYNOLDS NUMBER SQUARED 
C       HC = HEAT TRANSFER COEFFICIENT 
c      HD = mass transfer coefficient, forced convection
c      HDfree = mass transfer coefficient, free convection 
C       HTOVPR = WATER VAPOR PRESSURE 
C      Igeom = chooser for convection equations actually used  
C       Lometry = NUMBER SPECIFYING Allometry (GEOMETRY) TO BE USED:
C            1 = CYLINDER, 2 = SPHERE, 3 = Ellipsoid,
C           4 = lizard, 99 = ANIMAL -user supplied subroutine ANCORR
C       PATMOS = ATMOSPHERIC PRESSURE   
C       PR = PRANDTL NUMBER 
C       QCONV = HEAT LOSS BY CONVECTION 
C       RE = REYNOLD'S NUMBER   
C    SC = SCHMIDT NUMBER
C    SH = SHERWOOD NUMBER
C       TA = AIR TEMPERATURE (C)
C       TCOEFF = TEMPERATURE COEFFICIENT OF EXPANSION OF AIR
C       Raylei = Rayleigh Number = GRASHOF-PRANDTL PRODUCT  
C       THCOND = THERMAL CONDUCTIVITY OF AIR
C       VEL = AIR VELOCITY  
C       VISDYN = DYNAMIC VISCOSITY OF AIR   
C       VISKIN = KINEMATIC VISCOSITY OF AIR 
C       Tskin = CURRENT GUESS OF OBJECT SURFACE TEMPERATURE 


      BETA=1./(TA+273.)
      CP=1.0057E+3
      G=9.80665 
c    Convective area calculation = total area - ventral area in contact with substrate
      if(wingmod.gt.0)then
c     reduce surface area if wings are closed (phi<=90) or flat on substrate (phi>180)
c     might want to alter this to account for butterflies perching on veg vs. on a substrate
       if(wingcalc.eq.1)then
c      if((phi.le.90).or.(phi.gt.180))then
c       convar = atot/2
c        else
         convar = atot
c      endif
       else
         convar=pi*AL*ALENTH
         convar = atot
c        convar = atot - av - at
       endif
      else
      convar = atot - av - at
      endif

C    USING ALTITUDE TO COMPUTE BP (SEE DRYAIR LISTING)
      BP=0.0
      DB=TA 

      CALL DRYAIR(DB,BP,ALT,PATMOS,DENSTY,VISDYN,VISKIN,DIFVPR, 
     *THCOND,HTOVPR,TCOEFF,GGROUP)  

C     CHECKING TO SEE IF THE FLUID IS WATER, NOT AIR
      IF (FLTYPE .EQ. 1.0) THEN
          CALL WATER(TA,BETA,CP,DENSTY,THCOND,VISDYN)
      ENDIF

      PR=CP*VISDYN/THCOND   
      IF (FLTYPE .EQ. 0.00) THEN
C      AIR
        SC=VISDYN/(DENSTY*DIFVPR)
       ELSE
C      WATER; NO MEANING
        SC=1.0
      ENDIF

      DELTAT=Tskin-TA 
      if(deltat.eq.0.0000000)then
        deltat = 0.01
      endif  
      GR=((DENSTY**2)*BETA*G*(D**3)*DELTAT)/(VISDYN**2) 
C     CORRECTING IF NEGATIVE DELTAT 
      GR = ABS(GR)  

C       AVOIDING DIVIDE BY ZERO IN FREE VS FORCED Raylei  
      IF (VEL .LE. 0.000000000000) VEL = 0.0001 
      RE = DENSTY*VEL*D/VISDYN 
      
C     CHOOSING FREE OR FORCED CONVECTION
C     SIGNIFICANT FREE CONVECTION IF GR/RE**2 .GE. 20.0     
C               KREITH (1965) P. 358

      GRRE2 = GR/(RE**2)
      
C     *********************  FREE CONVECTION  ********************  
c    if(wingmod.eq.1)then
c    goto 20
c    endif

      if(lometry.eq.0)then
       Raylei=GR*PR
      ANUfre=0.55*raylei**0.25
      endif

      if((Lometry.eq.1).OR.(LOMETRY.EQ.3).or.(LOMETRY.eq.5))then
C        FREE CONVECTION OF A CYLINDER ******DOUBLE CHECK THEN BREAKPOINTS HERE!!!! 
C        FROM P.334 KREITH (1965): MC ADAM'S 1954 RECOMMENDED COORDINATES   
        Raylei=GR*PR
        IF(Raylei.lt.1.0e-05)then  
          ANUfre=0.4
          GO TO 15 
        endif
        if(Raylei.lt.0.1)then   
          ANUfre=0.976*Raylei**0.0784
          GO TO 15
        endif
        if(Raylei.le.100)then  
          ANUfre=1.1173*Raylei**0.1344   
          GO TO 15
        endif  
        IF(Raylei.lt.10000.)then  
          ANUfre=0.7455*Raylei**0.2167   
          GO TO 15
        endif  
        IF(Raylei.lt.1.0e+09)then  
          ANUfre=0.5168*Raylei**0.2501
          GO TO 15
        endif 
        IF(Raylei.lt.1.0e+12)then  
          ANUfre=0.5168*Raylei**0.2501
          GO TO 15
        endif 
      endif 

      if((Lometry.eq.2).or.(Lometry.eq.4))then
C       SPHERE FREE CONVECTION   
C       FROM P.413 BIRD ET AL (1960) TRANSPORT PHENOMENA)  
        Raylei=(GR**.25)*(PR**.333) 
        ANUfre=2.+0.60*Raylei  
        IF(Raylei .LT. 200.) THEN   
          GO TO 20  
         ELSE   
cc          WRITE(6,13) Raylei  
c   13     FORMAT(1X,'(GR**.25)*(PR**.333) ',1E10.4, 
c     *    ' IS TOO LARGE FOR CORREL.')
        endif  
      ENDIF 
      GO TO 20 

C    Forced CONVECTION FOR ANIMAL

   15    CONTINUE 
      IF((LOMETRY.EQ.3).OR.(LOMETRY.EQ.4).OR.(LOMETRY.EQ.5))THEN 
C      CALCULATE FORCED CONVECTION FOR LIZARDS, FROGS OR TURTLES
        CALL ANCORR (RE,ANU) 
      ENDIF  
      
   20 CONTINUE

      
      if(wingcalc.eq.1)then

       Raylei=GR*PR
      ANUfre=0.55*raylei**0.25
c      Raylei=GR*PR
c      IF(Raylei.lt.1.0e-09)then 
c      ANUfre=0.59*Raylei**0.25
c      else
c      ANUfre=0.1*Raylei**0.33
c      endif
      endif          

C     Calculating the free convection heat transfer coefficient, Hc  (Nu=Hc*D/Kair)
      HC=(ANUfre*THCOND)/D
c    cap at min value for air of 5 W/m2k http://www.engineeringtoolbox.com/convective-heat-transfer-d_430.html
       if(HC.lt.5)then
      HC=5
      endif
C     Calculating the Sherwood Number from the Colburn Analogy
C     (Bird, Stewart & Lightfoot, 1960. Transport Phenomena. Wiley.       
      SHfree = ANUfre * (SC/PR)**.333 
C     Calculating the mass transfer coefficient from the Sherwood Number      
      HDfree=SHfree*DIFVPR/D 
C     Calculating the convective heat loss at the skin      
      Qfree = HC * convar * (Tskin - Ta)  

C     *******************  FORCED CONVECTION  ********************* 
      if(wingmod.gt.0)then
      goto 35
      endif

      if(lometry.eq.0)then
      ANU=0.102*RE**0.675*PR**(1./3.)
      endif

      IF (Lometry.eq.1)then 
C       FORCED CONVECTION OF A CYLINDER    
C       ADJUSTING NU - RE CORRELATION FOR RE NUMBER (P. 260 MCADAMS,1954) 
        IF(RE.lt.4.)then 
          ANU=.891*RE**.33  
          GO TO 40
        endif  
        IF(RE.lt.40.)then 
          ANU=.821*RE**.385 
          GO TO 40 
        endif 
        IF(RE.lt.4000.)then  
          ANU=.615*RE**.466 
          GO TO 40
        endif  
        IF(RE.lt.40000.)then  
          ANU=.174*RE**.618 
          GO TO 40
        endif 
        if(Re.lt.400000.)then 
          ANU=.0239*RE**.805
          GO TO 40 
        endif
      endif 

      if((Lometry.eq.2).or.(Lometry.eq.4))then 
C       FORCED CONVECTION IN SPHERE 
        ANU=0.34*RE**0.24 
        GO TO 40 
      endif 

C    FORCED CONVECTION FOR ANIMAL  
   35 CONTINUE  
      if(wingmod.gt.0)then
       if(wingcalc.eq.1)then
c      ANU=0.102*RE**0.675*PR**(1./3.)
        ANU = 1.18*RE**0.5*PR**(1./3.)
       else      
c    Gates, eq 9.90
c     ANU=1.06*RE**0.5
c      below, 0 degrees orientation is first, then 90 degrees for females then for males
        if(phi.le.100)then
c       ANU=1.19539103625877*RE**00.144422857142857
         ANU=7.16966163653326*RE**0.0262414285714286
c       ANU=3.89748195383297*RE**0.106584444444444
c      else
c       ANU=1.37246587891911*RE**0.274448275862069 
c       ANU=0.688540698381432*RE**0.274448275862069
        endif
        if((phi.gt.100).and.(phi.le.125))then
c       ANU=0.88859300382332*RE**0.0.154377
         ANU=1.04562044604698*RE**0.2694125
c       ANU=1.30627835844513*RE**0.2535075
        endif
        if((phi.gt.125).and.(phi.le.140))then
c       ANU=1.11583401778839*RE**0.111997142857143
         ANU=0.998755268878792*RE**0.302962857142857
c       ANU=1.5694687891936*RE**0.178315
        endif
        if((phi.gt.140).and.(phi.le.160))then
c       ANU=0.911333044934478*RE**0.16073
         ANU=0.635917304973853*RE**0.348638571428571
c       ANU=1.21602514608032*RE**0.284633333333333
        endif
        if(phi.gt.160)then
c       ANU=1.25216541223225*RE**0.169857625
         ANU=1.28899681613556*RE**0.29015125
c       ANU=1.51577496883412*RE**0.2596425
        endif
c        ANU=0.6885*RE**0.2744
       endif
      endif
C  **************************************************************************   
   40 CONTINUE
      HC=ANU*THCOND/D   
      SHforc = ANU * (SC/PR)**.333
      HDforc=SHforc*DIFVPR/D
      Qforced = HC * convar * (Tskin - TA) 
      Qconv = Qfree + Qforced 

      END     
