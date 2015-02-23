      SUBROUTINE SHADEADJUST
c    Copyright 2006 Warren P. Porter All rights reserved.
c    This subroutine adjusts the amount of shade.

      Implicit None

      Real Acthr,Actlvl,ACTXBAS,AL,Amtfud,Andens,Asil,Asilp
      Real Depsel,Depsub,Emissb,Emissk,Fluid,G,newdep,Ptcond
      Real Qsol,Qsolr,RH,Shade,Soil1,Soil3,Subtk
      Real Ta,Taloc,Tannul,Tc,Tcores,Tdigpr,Time,Tmaxpr,Tminpr,tbask
      Real Tobj,Tref,Tsky,TskyC,Tshsoi,Tsoil,Tsub,Tsubst
      Real Vel,Vref,Z,Zsoil
      Real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real Qsolrf,QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real Tskin,R,WEVAP,TR,ALT,BP,VEGHYT,D,MAXSHD,H2O_BalPast
      Real Tpref,xbas,temerge
      real customallom,shp,shdgrass
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,rhref

      Integer Ihour,Lometry,Micro,Nodnum,NumFed,NumHrs,wingmod,wingcalc

      CHARACTER*1 Burrow,Dayact,Climb,CkGrShad,Crepus,nofood,Nocturn 
      Dimension  Acthr(25),Depsel(25*365*20),Tcores(25)
      Dimension TSOIL(25),ZSOIL(10),TSHSOI(25)
      Dimension QSOL(25),RH(25),TskyC(25),SOIL1(25),SOIL3(25)
      Dimension Taloc(25),Time(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION customallom(8),shp(3),rhref(25),shdgrass(25)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
     & ,shdgrass
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
      COMMON/WSOLAR/ASIL,Shade
      COMMON/Behav1/Dayact,Burrow,Climb,CkGrShad,Crepus,Nocturn,nofood
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 
      Common/Behav3/Acthr,ACTXBAS
      Common/Treg/Tc
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/DEPTHS/DEPSEL,Tcores
      Common/Qsolrf/Qsolrf
      common/wundrg/newdep
      COMMON/SHADE/MAXSHD

c    Max. vegetation height (m).  This should be an input from the 
c    microclimate model for each pixel. It should be on the first line of input.
      Data Veghyt/1.0/

C    ANIMAL CHARACTERISTIC DIMENSION FROM ALLOM
      D = R*2.

C    RATIO OF VEGETATION TO ANIMAL HEIGHT LYING DOWN.  
C    USE THIS TO DETERMINE MAXIMUM SHADE FOR AN ANIMAL OF A GIVEN SIZE RELATIVE TO LOCAL 
C    VEGETATION AND TO DETERMINE THE % OF SHADMET CONDITIONS VS. LOCAL AIR TEMPERATURE
C    'BLACKBODY' ENVIRONMENT.  CURRENTLY ASSUMING IF MAXSHD = 2 -> 100% SHADE AVAILABLE.
C    MAXSHD = 1 -> 50% MAX SHADE AVAILABLE IF ON THE GROUND SURFACE.
c    MAXSHD = (VEGHYT/D)*50.
      IF(MAXSHD.GT. 100.)THEN
        MAXSHD = 100.
      ENDIF

      if(shade.lt.maxshd)then
c      increase shade, and do a solution in the new environment
c    original value for Functional Ecology MS
c      shade = shade + 3.
        shade = shade + 40.
        if(shade.gt. maxshd)then
          shade = maxshd
        ENDIF
c      get the climate for CHANGED shade 
        call aboveground
        IF(QSOL(IHOUR).GT. 0.00000)THEN
          Call Solar
        ENDIF
      ENDIF
      Call Radin

      RETURN 
      END
