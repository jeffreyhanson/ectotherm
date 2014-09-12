      SUBROUTINE CLIMBER
      Implicit None

      Real Acthr,Actlvl,ACTXBAS,AL,Amtfud,Andens,Asil,Asilp
      Real Depsel,Depsub,Emissb,Emissk,Fluid,G,newdep,Ptcond
      Real Qsol,Qsolr,RH,Shade,Soil1,Soil3,Subtk
      Real Ta,Taloc,Tannul,Tc,Tcores,Tdigpr,Time,Tmaxpr,Tminpr,
     &tbask,temerge
      Real Tobj,Tref,Tsky,TskyC,Tshsoi,Tsoil,Tsub,Tsubst
      Real Vel,Vref,Z,Zsoil
      Real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real Qsolrf,QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real TPREF,XBAS
      real customallom,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,rhref

      Integer Ihour,Lometry,Micro,Nodnum,NumFed
     &,NumHrs,wingmod,wingcalc

      CHARACTER*1 Burrow,Dayact,Climb,CkGrShad,Crepus,nofood,Nocturn
      Dimension  Acthr(25),Depsel(25*365*20),Tcores(25)
      Dimension TSOIL(25),ZSOIL(10),TSHSOI(25)
      Dimension QSOL(25),RH(25),TskyC(25),SOIL1(25),SOIL3(25)
      Dimension Taloc(25),Time(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION customallom(8),shp(3),rhref(25)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
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

      if ((Tref(Ihour) .ge. Tminpr).and.(Tref(Ihour) .le. Tpref)) THEN
C      It's OK up high 
c      if (Tref(Ihour) .LE. TDIGPR)THEN
c        Climb to 200 cm height
          DEPSEL(IHOUR) = 200.0
c        Reference height air temperature is the same in sun or shade.
          Ta = Tref(Ihour)
          Vel = Vref(ihour)
c        No assumption of increase in shade due to climbing.  May still be in the sun.
c        endif
      endif
      RETURN 
      END
