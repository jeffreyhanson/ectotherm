      SUBROUTINE Sevap

c    This subroutine computes skin evaporation based on the mass transfer coefficient,
c    % of surface of the skin acting as a free water surface and exposed to the air,
c    and the vapor density gradient between the surface and the air, each at their own temperature.

c    Copyright 2003 Warren Porter All rights reserved.
      
         Implicit None 

      REAL ABSMAX,ABSMIN,ACTLVL,AEFF,AEYES,AL,ALT
      REAL AMASS,AMTFUD,ATOT,BP,BWT,CP,CUTFA
      REAL DENAIR,DB,DP,DELTAR,DEPSUB,E,EMISAN,ESAT,EX,EXTREF
      REAL FATOSK,FATOSB,Flshcond,GEVAP,GMASS,gwatph
      Real HD,HDforc,HDfree,HTOVPR,JULPEG
      REAL O2MAX,O2MIN,PATMOS,PCTEYE,POWER,PSTD,PTCOND
      REAL QCOND,QCONV,Qsevap,QIRIN,QIROUT,QMETAB,QRESP,QSOLAR
      REAL R,RH,RELH,RELHUM,RQ,RW,SIG,SkinW,SPHEAT,SUBTK
      REAL TA,TAIR,TC,TDIGPR,TIME,Tlung,TMAXPR,TMINPR,Tskin,Tsubst
      REAL TR,TVIR,TVINC,UCONV,V,VD,VDAIR,VDSURF,VEL,tbask,temerge
      REAL WATER,WB,WCUT,Wcut1,Wcut2,WEVAP,WEYES,WRESP,WTRPOT,Xtry
      Real AirVol,CO2MOL,H2O_BalPast
      REAL XBAS,ANDENS,ASILP,EMISSB,EMISSK,FLUID,G
      Real TPREF,HDD,SkinT,rainfall,HC,convar 
      real customallom,MR_1,MR_2,MR_3,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16

      DOUBLE PRECISION T

      INTEGER IMODEL,LIVE,Lometry,NumFed,NumHrs,nodnum,NDAY,IDAY,IHOUR
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66,DEB1
      integer wingmod,wingcalc

      CHARACTER*1 SPEC,Rainact

      DIMENSION customallom(8),shp(3)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND 
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW,
     &SkinT,HC,convar 
      common/evap2/HDfree,HDforc
      common/evap3/spec
      COMMON/MODEL/IMODEL
      COMMON/RKF45/T    
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp
      Common/Guess/Xtry
      Common/Rainact/Rainact
      Common/Treg/Tc
      Common/Rainfall/Rainfall
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66

C    DEBUGGING ONLY
      COMMON/DAYITR/NDAY,IDAY
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      
      TIME = real(T,4)
      
      TAIR = TA
      V = VEL
      HDD=HDfree+HDforc
      XTRY = TC

C     CALCULATING SKIN SURFACE SATURATION VAPOR DENSITY
      RH = 100.
C     CHECK FOR TOO LOW A SURFACE TEMPERATURE
      IF (Tskin .LT. 0.) THEN
         DB = 0.   
       ELSE
        DB = Tskin
      ENDIF

C     SETTING 3 PARAMETERS FOR WETAIR, SINCE RH IS KNOWN (SEE WETAIR LISTING)  
      WB=0.
      DP=999.  
C     BP CALCULATED FROM ALTITUDE USING THE STANDARD ATMOSPHERE
C     EQUATIONS FROM SUBROUTINE DRYAIR    (TRACY ET AL,1972)
      PSTD=101325.  
      PATMOS=PSTD*((1.-(.0065*ALT/288.))**(1./.190284)) 
      BP = PATMOS

      CALL WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,  
     * DENAIR,CP,WTRPOT)
      VDSURF = VD  

C     AIR VAPOR DENSITY    
c    Checking for rain-limited activity      
      If (((rainact .eq. 'y') .or. (rainact .eq. 'Y'))
     &    .and.(live.eq.1)) Then    
        RH = 99.
       Else
        RH = RELHUM
      Endif

      if(rainfall.gt.1)then
        RH = 99.
      endif

      DB = TAIR
      CALL WETAIR(DB,WB,RH,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,
     * DENAIR,CP,WTRPOT)
      VDAIR = VD   

C     CHECKING FOR LIVING OBJECTS
      IF (LIVE .EQ. 1) THEN
C       OCULAR WATER LOSS
C      CHECKING FOR OPEN EYES (ACTIVE)
        IF ((Xtry .GE. TBASK) .and. (Xtry .LE. TPREF)) THEN
C        CHECKING FOR PERMANENT SPECTACLE OVER EYE (MOD. BY M. Kearney)
            IF ((SPEC .eq. 'n') .or. (SPEC .eq. 'N')) THEN
c           EYES OPEN
             WEYES = HDD * AEYES * (VDSURF - VDAIR)
              ELSE
             WEYES = 0.0
            ENDIF
           ELSE
C          EYES CLOSED AND RESTING
            WEYES = 0.0
          ENDIF
        WRESP = GEVAP/1000.
       ELSE
        WEYES = 0.0
        WRESP = 0.0
      ENDIF
C     END OF LIVE VS INANIMATE

C     CHECK FOR TINY MASS (INSECTS)
c      IF (AMASS .LT. 0.001) THEN
c     WCUT = WRESP
c     WATER = WCUT + WRESP
c     GO TO 10 
c      ENDIF

C     WET OR "DRY" SURFACE WATER LOSS
      IF (Lometry .ne. 3) THEN
        WCUT = AEFF * HDD *(VDSURF - VDAIR)
C      FROG OR WET ROCK OR OTHER SURFACE
c      Free convection
c      WCUT1 = HDfree * Aeff * (VDSURF - VDAIR)
c      Forced convection
c      WCUT2 = HDforc * Aeff * (VDSURF - VDAIR)
c      Wcut = WCUT1 + WCUT2
c      IF(ABS(WCUT).LT.0.00000001)THEN
C        PREVENTING A SUPER SMALL NUMBER FROM STOPPING THE PROGRAM WITH AN UNDERFLOW ERROR
c        WCUT = 0.0
c      ENDIF     
        WATER = WEYES + WRESP + WCUT
      endif
      If(Lometry.eq.3)then
C      LIZARD OR DRY SURFACE
        IF (LIVE .EQ. 0) THEN
C        INANIMATE
c        WCUT = 0.0
          WCUT = AEFF * HDD *(VDSURF - VDAIR)
          WATER = WCUT
          GO TO 10
         ELSE
C        ANIMATE, CALCULATE BELOW
        ENDIF
          WCUT = AEFF * HDD *(VDSURF - VDAIR)
          WATER = WEYES + WRESP + WCUT
          PCTEYE = (WEYES/WATER)*100.  
c        ENDIF
C      END OF COMPUTING AEFF FOR SURFACE OR NOT
      ENDIF
C     END OF DRY VS WET SURFACE

   10 CONTINUE
      
C     FROM DRYAIR: LATENT HEAT OF VAPORIZATION 
      HTOVPR = 2.5012E+06 - 2.3787E+03 * TAIR  
      Qsevap = WATER * HTOVPR 
    
C    KG/S TO G/S 
      WEYES = WEYES * 1000.   
      WRESP = WRESP * 1000.
      WCUT  = WCUT * 1000.
      WEVAP = WATER * 1000.   

20    FORMAT(1X,7E10.4)

      RETURN   
      end