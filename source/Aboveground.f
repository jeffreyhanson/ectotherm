      Subroutine Aboveground

c    This subroutine sets above ground conditions.
c    copyright 2006 Warren P. Porter All rights reserved.
C    Version 8/1/06
      
      Implicit none

      External Fun

      Real AMASS,Asil,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real AL,TA,VEL,Pi,PTCOND,SUBTK,DEPSUB,TSUBST
      Real Fun,WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      Real QSOL,RH,TskyC,Shade,SOIL1,SOIL3,TIME,Taloc,TREF
      Real Tsoil,TSUB,VREF,Z,Tannul,Tc,Tshsoi
      Real ANDENS,ASILP,EMISSB,EMISSK,FLUID,G
      Real MICRO,QSOLR,TOBJ,TSKY
      Real Zsoil,CONTH,CONTW,CONTVOL,CONTDEP
      Real Tshski,Tshlow,twater,pond_depth
      Real Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real flytime,flyspeed,rhref,flymetab,conthole,pond_env
      real zfact,kappa,E_G,k_R,delta_deb,E_H_start,clutcha,clutchb
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,MsM
     &,lambda,breedrainthresh,daylengthstart,daylengthfinish,lengthday
     &,prevdaylength,lat,metamorph,lengthdaydir,contwet,shdgrass
      real MSOIL,MSHSOI,PSOIL,PSHSOI,HSOIL,HSHSOI
      
      integer frogbreed,frogstage,photostart,photofinish,batch,
     &photodirs,photodirf,breedact,breedactthres


      Integer Ihour,wingmod,wingcalc,inwater,aquatic,feeding
      integer flight,flyer,flytest,wetmod,contonly,contype,pond

      DIMENSION TIME(25),QSOL(25),RH(25),TskyC(25),SOIL1(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)
      Dimension TSOIL(25),TSHSOI(25),Tshski(25),Tshlow(25),ZSOIL(10)
      Dimension RHREF(25),pond_env(20,365,25,2),SHDGRASS(25)
      DIMENSION MSOIL(25),MSHSOI(25),PSOIL(25),PSHSOI(25),HSOIL(25)
     & ,HSHSOI(25) 

      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
     & ,shdgrass
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/WSOLAR/ASIL,Shade
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL,MSOIL,MSHSOI,PSOIL,PSHSOI,HSOIL,
     & HSHSOI
c    Shade environmental variables for this hour
      common/shenv1/Tshski,Tshlow
      Common/Treg/Tc
      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,WETMOD,contonly,conthole
     &    ,contype,contwet
      COMMON/fly/flytime,flight,flyer,flytest,flyspeed,flymetab
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch
     &,MsM,lambda,breedrainthresh,daylengthstart,daylengthfinish
     &,photostart,photofinish,lengthday,photodirs,photodirf
     &,lengthdaydir,prevdaylength,lat,frogbreed,frogstage
     &,metamorph,breedactthres,clutcha,clutchb 
      common/pondtest/pond

      Data Pi/3.141592/

c    NOTE: this has to be a function of animal height/veg. height computed in SHADEADJUST.
c    NOTE: Shadmet comes from the % shade of the vegetation for the ground.  This might be 12%.  
c    However, small animals may still be able to seek 100% shade.  In that 
c    case, the radiant environment will be that of the local air temperature.

      QSOLR = QSOL(Ihour)*((100.- Shade)/100.)   
      ZEN = Z(Ihour) * PI / 180.  

      if(flytest.eq.1)then
       TA = TREF(Ihour)
       VEL = flyspeed             
       RELHUM = RHREF(Ihour)  
      else
       TA = Taloc(Ihour)*((100.-Shade)/100.) + 
     &    (Tshlow(Ihour)*(Shade/100.)) 
       VEL = VREF (Ihour)             
       RELHUM = RH(Ihour)  
      endif
      TSKY = TskyC(Ihour)*((100.-Shade)/100.) + (Ta*(Shade/100.))


c    Soil temperatures in sun & shade are this current hour from 
c    the surface (node 1) in sun to shade surface soil and to local air temperature.
      TSUBST = Tsoil(1)*((100.-Shade)/100.) + Ta*(Shade/100.)

      if(inwater.eq.1)then
       tsubst=twater
      endif

      TOBJ = TSUBST

      Return
      END