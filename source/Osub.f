      Subroutine Osub(T,Y,TRANCT,NDAYY)

c    This subroutine prints output from Subroutine Gear, 
c    the numerical integrator for transients

      Implicit none

      Double Precision T,Y
      real hour
      real transient,HC,convar 
      real QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real XP,YP,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7
      Real XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      real gmass
      real TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,tbask,temerge
      real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      real Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      Real TPREF,gevap,PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,
     *SkinW,gcut,gresp,geyes,SkinT,core
      Real SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16

      Integer I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      Integer TRANCT
      Integer NDAYY,LIVE
      Integer IT,wingmod,wingcalc

      Dimension Y(10)
      Dimension transient(365*25*20)
      DIMENSION ZP1(365*25*20),ZP2(365*25*20),ZP3(365*25*20)
      DIMENSION ZP6(365*25*20),ZP7(365*25*20)
      DIMENSION ZD1(365*25*20),ZD2(365*25*20),ZD3(365*25*20)
      DIMENSION ZD6(365*25*20),ZD7(365*25*20)
      DIMENSION ZP4(365*25*20),ZP5(365*25*20)
      DIMENSION ZD4(365*25*20),ZD5(365*25*20)

      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      common/outsub/IT
      common/transtab/transient
      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/PLTDAT/XP,YP,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7
      COMMON/PLTDAT2/XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW,
     &SkinT,HC,convar 

      GMASS=AMASS*1000
      If(TRANCT .eq. NDAYY)THEN
      hour = real(T/60.,4)
      transient(IT)=real(Y(1),4)
c      If(Y(1) .gt. 50.)THEN
c              CORE = 50.
c          Else
c              CORE = real(Y(1),4)
c      Endif
      IF(LIVE .eq. 1)THEN
          QMETAB = 0.0056*10.**(0.038*(core)-1.771)*GMASS**.82
          QMETAB = QMETAB*XBAS
      ELSE
          QMETAB = 0.
      ENDIF
      zD2(it)=qmetab
      zD3(it)=wevap
      gevap=wevap*1000000
      gcut=wcut*1000000
      gresp=wresp*1000000
      geyes=weyes*1000000
      IT = IT + 1
      ENDIF
c130   FORMAT(1X,1F7.2,1F7.2,1F12.5,1F12.5,1F12.5,1F12.5,1F12.5,1F12.5)

      Return
      end