      Subroutine Belowground

c    This subroutine sets the below ground environment when an animal goes into a burrow
C    It should be called from Sub. Seldep
c    copyright Warren P. Porter 2006 All rights reserved.

      Implicit None

      External Fun

      Real ANDENS,ASILP,EMISSB,EMISSK,Enb,Fun,FLUID,G
      Real  MICRO,QSOLR,Tc,TOBJ,TSKY
      Real AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      Real ASIL,Shade,Depsel,Tcores,TSOIL,TSHSOI,ZSOIL
      Real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real Tskin,R,WEVAP,TR,ALT,BP,node,newdep,H2O_BalPast
      Real QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF
      Real TSUB,VREF,Z,Tannul
      Real TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask,temerge
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,rhref
      real twater,pond_depth,pond_env,shdgrass

      Integer IHOUR,NON,NDAY,IDAY,wingmod,minnode,inwater,feeding
      integer aquatic

      Dimension Depsel(25*365*20),Tcores(25),TSOIL(25),TSHSOI(25)
     &,ZSOIL(10),shdgrass(25)
         Dimension node(10),pond_env(20,365,25,2)
      DIMENSION QSOL(25),RH(25),TskyC(25),SOIL1(25),SOIL3(25),rhref(25)
      DIMENSION TIME(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)

      COMMON/BUR/NON,minnode
      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WSOLAR/ASIL,Shade
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
      COMMON/DEPTHS/DEPSEL,Tcores
      Common/soln/Enb
      Common/Treg/Tc
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
     & ,shdgrass
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      common/wundrg/newdep
      COMMON/DAYITR/NDAY,IDAY
      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env

      data node/0.,2.5,5.,10.,15.,20.,30.,40.,50.,60./
                      
      Qsolr = 0.0
      Qsolar = 0.0
      VEL = 0.01
      if((newdep.gt.20).or.(pond_depth.gt.0.1))then
      Relhum = 80.
      endif

      if(newdep.GE. 200.)then
        Ta = Tannul
      endif

      Tsubst = Ta
      Tobj = Ta
      Tsky = Ta
      Tc = Ta + 0.1
      if((newdep.gt.0.0001).and.(wingmod.eq.2))then
      Twing=ta
      endif

      Return
      end
