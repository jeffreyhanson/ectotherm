      SUBROUTINE BURROWIN
C    COPYRIGHT 2006 WARREN P. PORTER  ALL RIGHTS RESERVED.

      Implicit None

      Real Acthr,Actlvl,ACTXBAS,AL,Amtfud,Andens,Asil,Asilp
      Real Depsel,Depsub,Depth,Emissb,Emissk,Fluid,G,newdep,Ptcond
      Real Qsol,Qsolr,RH,Shade,Soil1,Soil3,Subtk
      Real Ta,Taloc,Tannul,Tc,Tcores,Tdigpr,Time,Tmaxpr,Tminpr
      Real Tobj,Tref,Tsky,TskyC,Tshsoi,Tsoil,Tsub,Tsubst,tbask,temerge
      Real Vel,Vref,Z,Zsoil
      Real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real Qsolrf,QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real XBAS,TPREF,maxshd
      real customallom,shp,rhref,pond_depth,pond_env,twater
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16

      Integer Ihour,Lometry,Micro,Nodnum,NumFed,aquatic
     &,NumHrs,wingmod,wingcalc,shdburrow,inwater,feeding

      CHARACTER*1 Burrow,Dayact,Climb,CkGrShad,Crepus,nofood,Nocturn

      Dimension  Acthr(25),Depsel(25*365*20),Tcores(25)
      Dimension TSOIL(25),ZSOIL(10),TSHSOI(25)
      Dimension QSOL(25),RH(25),TskyC(25),SOIL1(25),SOIL3(25)
      Dimension Taloc(25),Time(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION customallom(8),shp(3),RHREF(25),pond_env(20,365,25,2)

      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env
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
      common/burrow/shdburrow
      COMMON/SHADE/MAXSHD

      DEPTH = NEWDEP
      If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
        if(shdburrow.eq.1)then
         shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
        DEPSEL(IHOUR) = NEWDEP *(-1.0)
c      Ta determined in Seldep, new Tcore suggested in Undrgrd 
c      Reset environmental conditions underground
        CALL BELOWGROUND 
        Call Radin
      ENDIF

      RETURN
      END
