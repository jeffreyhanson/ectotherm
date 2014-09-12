       SUBROUTINE MET 

C      THIS SUBROUTINE METABOLISM ACTLVL,AL,ALT,AMASS,AMTFUD,ANDENS,ASILP,ASQ,ATOT,
C     * B,BP,BSQ,C,CSUTINE ONLY CALLED IF THE OBJECT IS LIVE

      Implicit None
   
      REAL Acthr,ACTLVL,ACTXBAS,AL,ALT,Amass,AirVol,CO2MOL
      Real AMTFUD,ANDENS,Area,ASILP,Atot
      Real BP,DEPSUB,EMISAN,EMISSB,EMISSK
      Real Fatcond,FATOSK,FATOSB,Flshcond,FLUID,G,GMASS,gwatph
      Real ASEMAJR,BSEMINR,CSEMINR
      Real QCOND,QCONV,QIRIN,QIROUT
      Real QMETAB,Qresp,Qsevap,Qsolar,QSOLR,QSWEAT
      Real PTCOND,R,RELHUM,SIG,SUBTK
      Real TA,TC,TDIGPR,TMAXPR,TMINPR,Tskin,TOBJ,tbask,temerge
      Real TR,TSKY,TSUBST,VEL,WEVAP,Xtry,H2O_BalPast
      Real Tlung,EXTREF,RQ,GEVAP,DELTAR,R1,Rinsul,Vol,XBAS
      Real TPREF,MLO2,GH2OMET,debqmet
      real customallom,MR_1,MR_2,MR_3,shp
      real MLO2_init,GH2OMET_init,debqmet_init
      real T_A,TAL,TAH,TL,TH,T_ref,Tcorr,dryfood,faeces,nwaste
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real flytime,flyspeed,flymetab

      INTEGER IHOUR,MICRO,Lometry,Nodnum,NumFed,NumHrs,DEB1,wingcalc
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66,wingmod
      integer flight,flyer,flytest

      CHARACTER*1 BURROW,Dayact,Climb,CkGrShad,Crepus,nofood,Nocturn

      Dimension Acthr(25)
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
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/WMET/QSWEAT
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/Behav1/Dayact,Burrow,Climb,CkGrShad,Crepus,Nocturn,nofood
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 
      Common/Behav3/Acthr,ACTXBAS 
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      COMMON/ELLIPS/ASEMAJR,BSEMINR,CSEMINR
      Common/Guess/Xtry
      Common/Treg/Tc
      COMMON/DEBRESP/MLO2,GH2OMET,debqmet,MLO2_init,GH2OMET_init,
     &    debqmet_init,dryfood,faeces,nwaste
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      COMMON/fly/flytime,flight,flyer,flytest,flyspeed,flymetab

C      IF CORE TEMPERATURE IS THE GUESSED VARIABLE; GENERATION OF HEAT
C      BY METABOLISM, QMETAB, IS SPECIFIED FROM THE CORE TEMP. GUESS
C      AND THE MASS OF THE ANIMAL BY THE BENNETT & DAWSON EQUATION.
C      SURFACE TEMPERATURE, TO IS COMPUTED FROM THE REST BY THE 
C      SOLUTION OF THE EQUATION FOR HEAT GENERATION BY CYLINDRICAL 
C      SHAPES      

      GMASS=AMASS*1000.
      Tc = Xtry

      if(DEB1.eq.1)then
c    using DEB model to get QMETAB, skip the regressions!
      Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tc)))/(1+EXP(TAL
     &*(1/(273+Tc)-1/TL))+EXP(TAH*(1/TH-1/(273+Tc))))
      if(ihour.eq.1)then
      qmetab = debqmet_init*Tcorr
      else
      qmetab = debqmet(ihour-1)*Tcorr
      endif
      
      else
c    use regression
C      LIZARD REGRESSION
        if(Tc .gt. 50)then
c       cap metabolic rate equation with max of TC = 50
         QMETAB = 0.0056*10.**(MR_3*50)*MR_1*GMASS**MR_2
         goto 101
        endif
        If (Tc .le. TPREF) then
          If (Tc .ge. 1.0) then
C            Acceptable temperature range
            QMETAB = 0.0056*10.**(MR_3*TC)*MR_1*GMASS**MR_2
            QMETAB = QMETAB*XBAS
           else
            Qmetab = 0.01
           Endif
         else
C        Too hot                                     
c        Allowing Tcore to be a little higher than Tmax preferred so that thermoregulation
c        behaviors will be invoked to select more moderate microclimate conditions
          QMETAB = 0.0056*10.**(MR_3*(Tmaxpr+0.5))*MR_1*GMASS**MR_2
          QMETAB = QMETAB*XBAS
        Endif
      endif    
      
      if(flytest.eq.1)then
       QMETAB = QMETAB + flymetab*AMASS*1000
      endif

c20    FORMAT(1X,7E10.4)

101   continue
      RETURN   
      END
