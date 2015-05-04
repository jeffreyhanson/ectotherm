      SUBROUTINE RESP 

C    COPYRIGHT 1989  WARREN PORTER  ALL RIGHTS RESERVED

      Implicit None
      
      REAL ACTLVL,AIRATO,AIRML1,AIRML2,AIRVOL
      Real AL,ALT,AMASS,AMTFUD,ATOT,BP
      Real CO2GAS,CO2MOL,CP,DB,DELTAR,DENAIR,DEPSUB,DP
      Real E,EMISAN,ESAT,EVPMOL,EXTREF,FATOSK,FATOSB,Flshcond
      Real GMASS,GEVAP,gwatph,HTOVPR,KGEVAP
      Real N2GAS,N2MOL1,N2MOL2,O2GAS,O2MOL1,O2MOL2,O2MOLC,O2STP
      Real PCTCO2,PCTO2,PCTN2,PO2,PTCOND
      Real QCOND,QCONV,QIRIN,QIROUT,QMETAB,QRESP,Qsevap,QSOLAR
      Real R,REFPO2,RELHUM,RELXIT,RGC,RHSAT
      Real RPCTCO2,RPCTN2,RPCTO2,RQ,RW
      Real SIG,SUBTK
      Real VD,TA,TAIR,TC,TDIGPR,TIMBAS,Tlung
      Real TMAXPR,TMINPR,Tskin,TR,TSUBST,tbask,temerge
      Real TVIR,TVINC,VEL,VO2CON
      Real WEVAP,WTRPOT,WB,WMOL1,WMOL2,XCALC,Xtry,H2O_BalPast
      Real DE,PASTIM,PFEWAT,PTUREA
      Real XBAS,TPREF
      real customallom,MLO2,GH2OMET,debqmet,shp
      real ANDENS,ASILP,EMISSB,EMISSK,FLUID,G
      real MLO2_init,GH2OMET_init,debqmet_init,FoodWaterCur
      real T_A,TAL,TAH,TL,TH,T_ref,Tcorr,dryfood,faeces,nwaste
      real MR_1,MR_2,MR_3
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16

      integer IHOUR,DEB1
      Integer NumFed,NumHrs,Lometry,nodnum,wingmod,wingcalc
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66

      Character*1 Transt,Dlenth
      DIMENSION customallom(8),shp(3)
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &    FAECES(24),NWASTE(24)
      
      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND    
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/GITRAC/DE,PASTIM,PFEWAT,PTUREA,TIMBAS,FoodWaterCur
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp
      Common/Guess/Xtry
      Common/Airgas/O2gas,CO2gas,N2gas
      Common/Treg/Tc
      Common/Usropt/Transt,Dlenth
      COMMON/DEBRESP/MLO2,GH2OMET,debqmet,MLO2_init,GH2OMET_init,
     &    debqmet_init,dryfood,faeces,nwaste
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66

      TAIR = TA
c    Kluge for the moment to decide body-air temperature gradient and check for 
c    stability before doing Tlung - Tair,local
      Deltar = 1.0

C    DEFINING VARIABLES
C    BP = BAROMETRIC PRESSURE (PA)
C    EXTREF = EXTRACTION EFFICIENCY (PER CENT)
C    GEVAP = GRAMS OF WATER EVAPORATED FROM RESPIRATORY TRACT/S
C    QRESP = HEAT LOSS DUE TO RESPIRATORY EVAPORATION (W)
C    RGC = UNIVERSAL GAS CONSTANT (PA-M3/MOL-K) = (J/MOL-K)
C    RELHUM = RELATIVE HUMIDITY (PER CENT)
C    RQ = RESPIRATORY QUOTIENT (MOL CO2 / MOL O2)
C    TC = ANIMAL CORE TEMPERATURE(C)
C    TMAXPR = PREFERRED MAX. Tcore
C    TMINPR = PREFERRED MIN. Tcore
C    ACTLVL = ACTIVITY LEVEL ABOVE BASAL METABOLISM

C    ASSIGNING Reference VALUES TO VARIABLES
C    AIR FRACTIONS FROM SCHMIDT-NIELSEN, 2ND ED. ANIMAL PHYSIOLOGY CITING
C      OTIS, 1964
      RPCTO2 = 0.2095
      RPCTN2 = 0.7902
      RPCTCO2 = 0.0003
      PctO2 = RPCTO2
      PctN2 = RPCTN2
      PctCO2 = RPCTCO2
C    Allowing user to modify gas values for burrow, etc. conditions
      If (PctO2 .ne. O2gas/100.)then
        PctO2 = O2gas/100.
       else
        PctO2 = RPCTO2
      Endif
      If (PctN2 .ne. N2gas/100.)then
        PctN2 = N2gas/100.
       else
        PctN2 = RPCTN2
      Endif
      If (PctCO2 .ne. CO2gas/100.)then
        PctCO2 = CO2gas/100.
       else
        PctCO2 = RPCTCO2
      Endif
C     Universal gas constant (Pa - liters)/(mol - K)
      RGC = 8309.28 
C    INITIALIZING FOR SUB. WETAIR
      WB = 0.0
      DP = 999.

      PO2 = BP*PCTO2
      RefPO2 = 101325.*RpctO2
C    OXYGEN CONSUMPTION OF LIZARDS (BENNETT & DAWSON, 1976) (M3/S)
      GMASS = AMASS*1000.

      If((Transt.eq.'y').or.(Transt.eq.'Y'))then
          XCALC = TC
          TLUNG = TC
      Else
      XCALC = Xtry
      Endif

      if(DEB1.eq.1)then
       Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+XCALC)))/(1+EXP(TAL
     & *(1/(273+XCALC)-1/TL))+EXP(TAH*(1/TH-1/(273+XCALC))))
c     using DEB calcs for met water and O2 consumption (convert from ml/h to L/s), so skip the regressions!
       if(ihour.eq.1)then
        O2STP=MLO2_init/3600/1000*Tcorr
       else
        O2STP=MLO2(ihour-1)/3600/1000*Tcorr
       endif
      else
C    CHECK FOR TOO LARGE OR SMALL CORE TEMP
       IF (TC .GT. TPREF) THEN
        XCALC=TPREF
       ELSE
        IF (TC .LT. 0.0000) THEN
          XCALC=0.01
        ENDIF
       ENDIF

       O2STP = (1./3.6E+06)*(QMETAB/.0056)
      endif
 
C    CONVERTING STP -> VOL. OF O2 AT ANIMAL TCORE, ATM. PRESS.
c    Old: VO2CON = (O2STP*101325./273.15)*((XCALC+273.15)/BP)
      Tlung = Xcalc
      VO2CON = (O2STP*RefPO2/273.15)*((Tlung+273.15)/PO2)
c    Change by M. Kearney. Should this have the Timbas multiplier in it?    
c    VO2CON = QMETAB*Timbas/0.0056/1000/3600

C    n = PV/RT (IDEAL GAS LAW: NUMBER OF MOLES FROM PRESS,VOL,TEMP)
c    M. Kearney substituded BP for PO2 in this equation
      O2MOLC = BP*VO2CON/(RGC*(XCALC+273.15))

C    MOLES/s O2,N2, & dry air AT 1: (ENTRANCE) (Air flow = f(O2 consumption)
      O2MOL1 = O2MOLC/(EXTREF/100.)
      N2MOL1 = O2MOL1*(PCTN2/PCTO2)
C    Demand for air = f(%O2 in the air and elevation)
C    Note that as long as all 3 percentages add to 100%, no change in air flow,
C    unless you correct for change in %O2 in the air and elevation changes 
C    relative to sea level.
      Airato = (PCTN2+PCTO2+PCTCO2)/PCTO2
      AIRML1 = O2MOL1*Airato*(RpctO2/PctO2)*(RefPO2/PO2)
C    Air volume @ STP (liters/s)
      AirVol = (Airml1*RGC*273.15/101325.)

C     Computing the vapor pressure at saturation for the subsequent
C     calculation of actual moles of water based on actual relative
C     humidity.
      rhsat = 100.
      CALL WETAIR (Tair,WB,RELHUM,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     * CP,WTRPOT)
      WMOL1 = AIRML1*(ESAT*(RELHUM/100.))/(BP-ESAT*(RELHUM/100.))

C    MOLES AT 2: (EXIT)
      O2MOL2 = O2MOL1-O2MOLC
      N2MOL2 = N2MOL1
      CO2MOL = RQ*O2MOLC
c     Total moles of air at 2 (exit) will be approximately the same
c     as at 1, since the moles of O2 removed = approx. the # moles of CO2 
c     added.  Avogadro's # specifies the # molecules/mole.
      AIRML2 = (O2MOL2+CO2MOL)*((PCTN2+PCTO2)/PCTO2)*(RpctO2/PctO2)*
     &    (RefPO2/PO2)

C    SETTING UP CALL TO WETAIR; TEMP. OF EXHALED AIR AT BODY TEMP.
      DB = XCALC 
C    ASSUMING SATURATED AIR AT EXHALATION
      RELXIT = 100.
      CALL WETAIR (DB,WB,RELXIT,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,DENAIR,
     * CP,WTRPOT)
      WMOL2 = AIRML2*(ESAT/(BP-ESAT))
C    ENTHALPY = U2-U1, INTERNAL ENERGY ONLY, I.E. LAT. HEAT OF VAP.
C    ONLY INVOLVED, SINCE ASSUME P,V,T CONSTANT, SO NOT SIGNIFICANT
C    FLOW ENERGY, PV. (H = U + PV)

C     Moles/s lost by breathing:
      EVPMOL = WMOL2-WMOL1
C     Grams/s lost by breathing = moles lost * gram molecular weight of water:
      GEVAP = EVPMOL*18.

      KGEVAP = GEVAP/1000.
C    LATENT HEAT OF VAPORIZATION FROM SUB. DRYAIR
      HTOVPR = 2.5012E+06 - 2.3787E+03*Tlung
C     Heat loss by breathing (J/s)=(J/kg)*(kg/s)
      QRESP = HTOVPR*KGEVAP
C    ** NOTE THAT THERE IS NO RECOVERY OF HEAT OR MOISTURE ASSUMED IN 
C       THE NOSE **

c100   FORMAT(1X,7E11.3)

      RETURN 
      END
