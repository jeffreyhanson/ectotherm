      program nichemapr
     
      Implicit None
             
      DOUBLE PRECISION T,hs
      
      double precision grassgrowths2,dep2,ectoinput2,yearout2,grasstsdm2
     &,wetlandTemps2,wetlandDepths2,thermal_stages2,S_instar2,Tc,
     &behav_stages2,water_stages2,yearsout2,maxshades2,arrhenius2

               
      double precision debmod2,deblast2
      double precision, DIMENSION(:,:), ALLOCATABLE :: environ2,enbal2
     & ,masbal2,debout2,metout2,shadmet2,soil22,shadsoil2,soilmoist2,
     &shadmoist2,soilpot2,shadpot2,humid2,shadhumid2
      double precision, DIMENSION(:), ALLOCATABLE :: rainfall2
      
      double precision vold,ED,V,v_init,E_init,Vold_init,Vpup_init
     &,Epup_init,s_j,L_b,cumrepro_init,cumbatch_init
      
      REAL ABSAN,ABSMAX,ABSMIN,ABSSB,Acthr,ACTLVL
      REAL ACTXBAS,AEFF,AEYES,AHEIT
      Real AMET,AEVP,AEIN,AWIN,ANRG,AWTR,AFOD,AHRS,AAIR,ADIS            
      Real AL,ALENTH,ALT,AMASS,AMTFUD,ANDENS,Area         
      Real ASIL,ASILN,ASILP,ATOT,AWIDTH,BP
      Real CRTOT,CUTFA
      Real DAIR,DCO2,DAY,DAYAIR
      Real DEIN,DEVP,Deltar,DEPSEL,DEPSUB
      Real DAYMET,DAYEVP,DAYEIN,DaJabs,DAYWIN,DAYNRG,DAWTR
     &,DAYWTR                   
      Real DMET,DNRG,DTIME,DWIN,DWTR,Damt,Davp
      Real EMISAN,EMISSB,EMISSK,Enb,Enberr
      REAL ENLOST,Enary1,Enary2,Enary3,Enary4
      REAL Enary5,Enary6,Enary7,Enary8,EXTREF
      Real Fatcond,FATOBJ,FATOSB,FATOSK
      Real Flshcond,FLTYPE,FLUID
      Real G,Gevap,gwatph,gwetfod
      Real HD,HDforc,HDfree,HOUR2
      Real newdep,O2MAX,O2MIN
      Real PCTDIF,PCTDRY,PCTEYE,PCTFAT,PCTPRO
      Real PTCOND
      Real QCONV,QCOND,QIRIN,QIROUT                       
      Real QMETAB,Qresp,Qsevap,QSOL,QSOLAR,QSOLR,Qsolrf
      Real R,Refshd,RH,RELHUM,RQ
      Real Shade,SIG,SkinW,soil2,SOIL3,SPHEAT,SUBTK
      Real TA,Taloc,Tannul,TCORES
      Real TDIGPR,TEIN
      Real TEVP,TIME,Tlung,TMAXPR,TMET,TMINPR,TNRG
      Real Tshski,Tshlow,Tskin,TOBJ,TR,TREF
      Real TSKY,TskyC,TSOIL,TSUB,TSUBST,TWIN
      Real TWTR
      Real Tprint,Tshsoi
      Real VEL,VREF
      Real WC,WCUT,WEVAP,WEYES,WRESP,WTRLOS
      Real XFAT,XP,XPROT,YP,Xtry
      Real Z,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7,ZEN,ZSOIL
      Real XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      Real Rinsul,R1,VOL
      Real DE,PASTIM,PCTCAR,PFEWAT,PTUREA,TIMBAS,AirVol,CO2MOL
      Real XBAS,FoodWaterCur
      Real Gprot,Gfat
c    100% Shade micromet variables; same order as those in the sun, but not dimensioned
      Real printT
      Real tcinit
      real ctmax,ctmin

      Real TSHOIL,TSOILS
      REAL Enary9,Enary10,Enary11,Enary12,Enary13,Enary14,Enary15
      REAL Enary16,Enary17,Enary18,Enary19,Enary20,Enary21
      REAL Enary22,Enary23,Enary24,Enary25,Enary26,Enary27
      Real Enary28,Enary29,Enary30,Enary31,Enary32,Enary33
      Real Enary34,Enary35,Enary36,Enary37,Enary38,Enary39
      Real Enary40,Enary41,Enary42,Enary43,Enary44,Enary45
      Real Enary46,Enary47,Enary48

      Real Transient,Transar
      Real TPREF
      Real dist
      Real daydis
      Real SkinT
      Real Maxshd
      REAL WETMASS,WETSTORAGE,WETGONAD
     &    ,svl
      REAL p_B_past,cumbatch,wetfood,cumrepro,ms
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      real annualact,annfood,food
      REAL tknest

      REAL O2gas,CO2gas,N2gas
      REAL rainfall,lengthday
     &,lat,lengthdaydir,prevdaylength

      REAL clutchsize,andens_deb,d_V,eggdryfrac,
     &w_E,mu_E,mu_V,w_V,T_REF,T_A,TAL,TAH,TL,TH,funct,
     &zfact,kappa,E_G,k_R,MsM,delta_deb,q,maxmass

      REAL v_baby1,e_baby1,v_baby_init,e_baby_init,EH_baby1,
     &e_init_baby,v_init_baby,v_baby,e_baby,EH_baby,
     &EH_baby_init

      real CONTH,CONTW,CONTVOL,CONTDEP,CONTDEPTH
      real e_egg
      REAL E_H
      REAL kappa_X,kappa_X_P,mu_X,mu_P,conthole

      REAL Thconw
      REAL ms_init,q_init,hs_init,E_H_init,
     &p_Mref,vdotref,h_aref,E_Hb,E_Hp,E_Hj,E_H_start
     &,k_Jref,lambda,daylengthstart,daylengthfinish,breedrainthresh
      real customallom,gutfreemass,shp,s_G,p_Xmref,potfreemass
      real etaO,JM_JO,O2FLUX,CO2FLUX,GH2OMET,MLO2,debqmet
      real MLO2_init,GH2OMET_init,debqmet_init,MR_1,MR_2,MR_3
      real w_X,w_P,w_N,dryfood,faeces,nwaste
      real H2O_BalPast,newclutch
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26,surviv
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,longev,gutfull
      real rhref,repro,raindrink,orig_clutchsize
      real y_EV_l,S_instar
      real E_Hpup_init,Vpup,Epup,
     &E_Hpup,surviv_init,halfsat,x_food,tbask,temerge,arrhenius
      real thermal_stages,stage,behav_stages,water_stages,orig_MsM
      real gutfill,contwet,shdgrass,clutcha,clutchb
      real MSOIL,MSHSOI,PSOIL,PSHSOI,HSOIL,HSHSOI,fec,surv
     
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),FAECES(24),
     &    NWASTE(24),surviv(24),thermal_stages(8,6)
     & ,thermal_stages2(8,6),behav_stages(8,14),
     &behav_stages2(8,14),water_stages(8,8),
     &water_stages2(8,8),maxshades2(7300),arrhenius(8,5)
     &,arrhenius2(8,5),fec(100),surv(100),S_instar(4),S_instar2(4)

      INTEGER DEB1,breedact,breedactthres    
      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,Iclim,I66
      INTEGER IDAY,IHOUR,IMODEL
      INTEGER J,JP,LIVE,Lometry,MICRO
      INTEGER NDAY,NM
      INTEGER nodnum,NON,NumFed,NumHrs
      integer days,nn3
      integer TRANCT,census
      integer IT,SCENAR
      integer INTNUM,wingmod,wingcalc
      integer SoilNode
      integer completion,complete
      integer goodsoil,monthly,julday,ctminthresh,ctkill
      integer IYEAR,NYEAR,countday,trans_start,viviparous
     &,pregnant,metamorph,ctmincum
      integer daycount,batch,photostart,photofinish,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage
      integer metab_mode,stages,minnode,wetmod,contonly,contype
      integer deadead,startday,reset,feed_imago

      CHARACTER*130 labloc,labshd,metsun,metshd,label
      CHARACTER*1 BURROW,Dayact,Climb,CkGrShad,Crepus,SPEC,Rainact
      Character*1 Nocturn,Fosorial,nofood
      Character*1 Hourop,Dlenth,Transt,screen
      Character*1 tranin

C   ******** IMPORTANT INFORMATION FOR LSODE NUMERICAL INTEGRATOR *****
C    Minimum SIZE OF IWORK = 20            FOR MF = 10
C    Minimum SIZE OF RWORK = 20 + 16*NEQ   FOR MF = 10 (NON-STIFF)
C      where NEQ is the number of equations

c    With multi-dimensional arrays COLUMN MAJOR ORDER is used in FORTRAN, 
c    and ROW MAJOR ORDER used in C.
c    Let's examine a small matrix:
c        A11  A12  A13
c        A21  A22  A23
c        A31  A32  A33
c    We can store the elements in memory like this:
c    A11 A21 A31 A12 A22 A32 A13 A23 A33     ---> Higher addresses
c    This is FORTRAN's Column major order, the first array index 
c    varies most rapidly, the Enary dimensioned below represents
c    a 2 column table with 175 rows.  Column 1 is time. 
c    Column 2 has the time dependent variables for a day 
c    (25 hrs*7variables=175). 

      DIMENSION TIME(25),XP(365*25*20),YP(365*25*20)
      DIMENSION ZP1(365*25*20),ZP2(365*25*20),ZP3(365*25*20)
      DIMENSION ZP6(365*25*20),ZP7(365*25*20)
      DIMENSION ZD1(365*25*20),ZD2(365*25*20),ZD3(365*25*20)
      DIMENSION ZD6(365*25*20),ZD7(365*25*20),ZD5(365*25*20)
      DIMENSION ZP4(365*25*20),ZP5(365*25*20),ZD4(365*25*20)
      DIMENSION QSOL(25),RH(25),TskyC(25),soil2(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION DAY(7300),Tshski(25),Tshlow(25)
      DIMENSION TSOIL(25),TSHSOI(25),ZSOIL(10),DEPSEL(365*25*20)
      dimension Tcores(25)
C    2 COLUMNS, 25 ROWS EACH TABLE
      DIMENSION Enary1(25),Enary2(25),Enary3(25),Enary4(25)
      DIMENSION Enary5(25),Enary6(25),Enary7(25),Enary8(25)
      DIMENSION Enary9(25),Enary10(25),Enary11(25),Enary12(25)
      DIMENSION Enary13(25),Enary14(25),Enary15(25),Enary16(25)
      DIMENSION enary17(25),enary18(25),enary19(25),enary20(25)
      DIMENSION enary21(25),enary22(25),enary23(25),enary24(25)
      DIMENSION enary25(25),enary26(25),enary27(25),enary28(25)
      DIMENSION enary29(25),enary30(25),enary31(25),enary32(25)
      DIMENSION enary33(25),enary34(25),enary35(25),enary36(25)
      DIMENSION Enary37(25),enary38(25),enary39(25),enary40(25)
      DIMENSION enary41(25),enary42(25),enary43(25),Enary44(25)
      DIMENSION enary45(25),enary46(25),enary47(25),enary48(25)
      DIMENSION MSOIL(25),MSHSOI(25),PSOIL(25),PSHSOI(25),HSOIL(25)
     & ,HSHSOI(25) 
     
      DIMENSION TSOILS(25),TSHOIL(25),shdgrass(25)
      DIMENSION TRANSIENT(365*25*20),TRANSAR(5,25)

      DIMENSION V(24),ED(24),wetmass(24),wetfood(24)
     &,wetstorage(24),svl(24),E_H(24),Vold(24),Vpup(24),Epup(24),
     &E_Hpup(24)
      DIMENSION wetgonad(24),cumrepro(24),
     &    hs(24),ms(24),cumbatch(24),q(24)
      DIMENSION repro(24),food(50)
      DIMENSION hour2(25)
      DIMENSION dep2(10)
      DIMENSION 
     &grassgrowths2(7300),grasstsdm2(7300),
     &wetlandTemps2(24*7300),wetlandDepths2(24*7300)
      DIMENSION debmod2(91)
     &,deblast2(13),v_baby1(24),e_baby1(24),ectoinput2(127),
     &yearout2(80)
      dimension yearsout2(20,45)
      DIMENSION customallom(8),etaO(4,3),JM_JO(4,4),shp(3),EH_baby1(24)    

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND 
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      Common/Dimens/ALENTH,AWIDTH,AHEIT
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW,
     &SkinT
      common/evap2/HDfree,HDforc
      common/evap3/spec
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      Common/wTrapez/Dtime
      COMMON/WCONV/FLTYPE
c    Sun environmental variables for the day
      COMMON/ENVAR1/QSOL,RH,TskyC,soil2,SOIL3,TIME,Taloc,TREF,rhref,
     &shdgrass 
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
c    Shade environmental arrays
      common/shenv1/Tshski,Tshlow
c    Other stuff
      COMMON/WSOLAR/ASIL,Shade
      COMMON/MODEL/IMODEL 
      COMMON/RKF45/T
      COMMON/PLTDAT/XP,YP,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7
      COMMON/DAYITR/NDAY,IDAY
      COMMON/TRANS/ICLIM,JP
      COMMON/DAYS/DAY
      Common/Dayint/DAYMET,DAYEVP,DAYEIN,DAYWIN,DAYNRG,DAYWTR,DAYAIR,
     &DAYDIS
      COMMON/SUM1/DMET,DEVP,DEIN,DWIN,DNRG,DWTR,DAIR,DCO2
      Common/Sum2/TMET,TEVP,TEIN,TWIN,TNRG,TWTR
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/Behav1/Dayact,Burrow,Climb,CkGrShad,Crepus,Nocturn,nofood
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 
      Common/Behav3/Acthr,ACTXBAS
      Common/Behav4/Fosorial 
      COMMON/DEPTHS/DEPSEL,Tcores     
      COMMON/WOPT/XPROT,XFAT,ENLOST,WTRLOS 
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL,MSOIL,MSHSOI,PSOIL,PSHSOI,HSOIL,
     & HSHSOI
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      common/lablts/labloc,labshd,metsun,metshd
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      COMMON/GITRAC/DE,PASTIM,PFEWAT,PTUREA,TIMBAS,FoodWaterCur    
      COMMON/FOOD1/PCTPRO,PCTFAT,PCTCAR,PctDry,CRTOT
      COMMON/FOOD2/Damt,Davp,DaJabs,DAWTR
      Common/FOODIN/gwetfod
      Common/Year2/AMET,AEVP,AEIN,AWIN,ANRG,AWTR,AFOD,AHRS,AAIR,ADIS         
      Common/soln/Enb
      Common/Guess/Xtry
      Common/Treg/Tc
      Common/Dsub1/Enary1,Enary2,Enary3,Enary4,Enary9,Enary10,Enary11,
     &    Enary12,Enary17,Enary18,Enary19,Enary20,Enary21,Enary22
     &   ,Enary23,Enary24,Enary25,Enary26,Enary27,Enary28,Enary45
     &   ,Enary46,Enary47,Enary48
      Common/Dsub2/Enary5,Enary6,Enary7,Enary8,Enary13,Enary14,Enary15
     &    ,Enary16,Enary29,Enary30,Enary31,Enary32,Enary33,Enary34
     &   ,Enary35,Enary36,Enary37,Enary38,Enary39,Enary40,Enary41
     &    ,Enary42,Enary43,Enary44
      Common/Qsolrf/Qsolrf
      common/wundrg/newdep

      COMMON/FOOD3/GPROT,GFAT
      Common/Rainfall/Rainfall
      Common/Rainact/Rainact
      Common/Hourop/Hourop,screen
      Common/Usropt/Transt,Dlenth
      Common/Usrop2/Enberr,printT,tprint

      COMMON/ENVIRS/TSOILS,TSHOIL
      COMMON/TRANSIENT/tcinit,transar
      COMMON/TRANINIT/tranin
      COMMON/TRANS/TRANCT
      common/outsub/IT
      common/scenario/scenar
      common/transtab/transient
      COMMON/PLTDAT2/XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      Common/Trav/Dist
      Common/Intnum/Intnum
      COMMON/EGGDEV/SOILNODE
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,wetmod,contonly,conthole
     &    ,contype,contwet
      COMMON/CONTDEPTH/CONTDEPTH
                         
C     NEED NON, # OF SOIL NODES,
      COMMON/BUR/NON,minnode
      COMMON/SHADE/MAXSHD
      COMMON/goodsoil/goodsoil
      COMMON/DEBMOD/V,ED,WETMASS,WETSTORAGE,WETGONAD,WETFOOD,
     &O2FLUX,CO2FLUX,CUMREPRO,HS,MS,SVL,p_B_past,CUMBATCH,Q,v_baby1,
     &e_baby1,E_H,stage,dead,EH_baby1,gutfreemass,surviv,Vold,Vpup,Epup
     &,E_Hpup,deadead,startday,raindrink,reset,census,potfreemass
      COMMON/DEBRESP/MLO2,GH2OMET,debqmet,MLO2_init,GH2OMET_init,
     &    debqmet_init,dryfood,faeces,nwaste
      COMMON/DEBMOD2/REPRO,orig_clutchsize,newclutch,orig_MsM
      COMMON/BREEDER/breeding
      common/debmass/etaO,JM_JO
      COMMON/REPYEAR/IYEAR,NYEAR
      COMMON/COUNTDAY/COUNTDAY,daycount
      COMMON/TRANSGUT/TRANS_START
      COMMON/DEBOUTT/fecundity,clutches,monrepro,svlrepro,monmature
     &,minED,annfood,food,longev,completion,complete,fec,surv
      common/gut/gutfull,gutfill
      COMMON/ANNUALACT/ANNUALACT
      COMMON/z/tknest,Thconw
      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch,MsM
     &,lambda,breedrainthresh,daylengthstart,daylengthfinish,photostart
     &,photofinish,lengthday,photodirs,photodirf,lengthdaydir
     &,prevdaylength,lat,frogbreed,frogstage,metamorph
     &,breedactthres,clutcha,clutchb      
      COMMON/DEBPAR3/metab_mode,stages,y_EV_l,S_instar
      COMMON/DEBPAR4/s_j,L_b
      COMMON/DEBINIT1/v_init,E_init,cumrepro_init,cumbatch_init,
     & Vold_init,Vpup_init,Epup_init
      COMMON/DEBINIT2/ms_init,q_init,hs_init,p_Mref,vdotref,h_aref,
     &e_baby_init,v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,
     &halfsat,x_food,E_Hpup_init,p_Xmref     
      Common/Airgas/O2gas,CO2gas,N2gas
      common/ctmaxmin/ctmax,ctmin,ctmincum,ctminthresh,ctkill
      common/julday/julday,monthly
      common/vivip/viviparous,pregnant
      common/refshade/refshd
      common/debbaby/v_baby,e_baby,EH_baby
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      common/thermal_stage/thermal_stages,behav_stages,water_stages
     &,arrhenius
      DATA hour2/0.,60.,120.,180.,240.,300.,360.,420.,480.,540.,600.,660
     &    .,720.,780.,840.,900.,960.,1020.,1080.,1140.,1200.,1260.,
     &    1320.,1380.,1440./

      OPEN(1,FILE='ectoinput.csv')
      read(1,*)LABEL
      DO 11 i=1,127
      read(1,*)label,ectoinput2(i)
11    continue
      close(1)

c    for Matt Malishev Netlogo sims
c    OPEN(1,FILE='shade.csv')
c    read(1,*)ectoinput2(47)
c    ectoinput2(48)=ectoinput2(47)

      nyear=int(ectoinput2(69))
      days=int(ectoinput2(104))
      nn3=nyear*days
c      nyear=3
      ALLOCATE ( environ2(24*nn3,20) )
      ALLOCATE ( enbal2(24*nn3,14) )
      ALLOCATE ( masbal2(24*nn3,21) )
      ALLOCATE ( debout2(24*nn3,20) )
      ALLOCATE ( rainfall2(nn3) )
      ALLOCATE ( metout2(24*nn3,18) )
      ALLOCATE ( shadmet2(24*nn3,18) )
      ALLOCATE ( soil22(24*nn3,12) )
      ALLOCATE ( shadsoil2(24*nn3,12) )
      ALLOCATE ( soilmoist2(24*nn3,12) )
      ALLOCATE ( shadmoist2(24*nn3,12) )
      ALLOCATE ( soilpot2(24*nn3,12) )
      ALLOCATE ( shadpot2(24*nn3,12) )
      ALLOCATE ( humid2(24*nn3,12) )
      ALLOCATE ( shadhumid2(24*nn3,12) )

      OPEN(1,FILE='metout.csv')
      read(1,*)LABEL
      do 13 i=1,days*24*nyear
      read(1,*) (metout2(i,j),j=1,18)
13    continue
      close(1)

      OPEN(1,FILE='shadmet.csv')
      read(1,*)LABEL
      do 14 i=1,days*24*nyear
      read(1,*) (shadmet2(i,j),j=1,18)
14    continue
      close(1)

      OPEN(1,FILE='soil.csv')
      read(1,*)LABEL
      do 15 i=1,days*24*nyear
      read(1,*) (soil22(i,j),j=1,12)
15    continue
      close(1)

      OPEN(1,FILE='shadsoil.csv')
      read(1,*)LABEL
      do 16 i=1,days*24*nyear
      read(1,*) (shadsoil2(i,j),j=1,12)
16    continue
      close(1)

      OPEN(1,FILE='dep.csv')
      read(1,*)LABEL
      do 17 i=1,10
      read(1,*)label,dep2(i)
17    continue
      close(1)

      OPEN(1,FILE='debmod.csv')
      read(1,*)LABEL
      do 20 i=1,91
      read(1,*)label,debmod2(i)
20    continue
      close(1)


      OPEN(1,FILE='deblast.csv')
      read(1,*)LABEL
      do 21 i=1,13
      read(1,*)label,deblast2(i)
21    continue
      close(1)

      OPEN(1,FILE='S_instar.csv')
      read(1,*)LABEL
      do 38 i=1,4
      read(1,*)label,S_instar2(i)
38    continue
      close(1)
      
      OPEN(1,FILE='rainfall.csv')
      read(1,*)LABEL
      do 22 i=1,days*nyear
      read(1,*)label,rainfall2(i)
22    continue
      close(1)


      OPEN(1,FILE='grassgrowths.csv')
      read(1,*)LABEL
      do 23 i=1,days*nyear
      read(1,*)label,grassgrowths2(i)
23    continue
      close(1)


      OPEN(1,FILE='grasstsdms.csv')
      read(1,*)LABEL
      do 24 i=1,days*nyear
      read(1,*)label,grasstsdm2(i)
24    continue
      close(1)

      OPEN(1,FILE='wetlandTemps.csv')
      read(1,*)LABEL
      do 25 i=1,days*24*nyear
      read(1,*)label,wetlandTemps2(i)
25    continue
      close(1)

      OPEN(1,FILE='wetlandDepths.csv')
      read(1,*)LABEL
      do 26 i=1,days*24*nyear
      read(1,*)label,wetlandDepths2(i)
26    continue
      close(1)
      
      OPEN(1,FILE='arrhenius.csv')
      read(1,*)LABEL
      do 31 i=1,8
      read(1,*)label,(arrhenius2(i,j),j=1,5)
31    continue
      close(1)

      OPEN(1,FILE='thermal_stages.csv')
      read(1,*)LABEL
      do 27 i=1,8
      read(1,*)label,(thermal_stages2(i,j),j=1,6)
27    continue
      close(1)

      OPEN(1,FILE='behav_stages.csv')
      read(1,*)LABEL
      do 28 i=1,8
      read(1,*)label,(behav_stages2(i,j),j=1,14)
28    continue
      close(1)

      OPEN(1,FILE='water_stages.csv')
      read(1,*)LABEL
      do 29 i=1,8
      read(1,*)label,(water_stages2(i,j),j=1,8)
29    continue
      close(1)

      OPEN(1,FILE='maxshades.csv')
      read(1,*)LABEL
      do 30 i=1,days*nyear
      read(1,*)label,maxshades2(i)
30    continue
      close(1)

      OPEN(1,FILE='soilmoist.csv')
      read(1,*)LABEL
      do 32 i=1,days*24*nyear
      read(1,*) (soilmoist2(i,j),j=1,12)
32    continue
      close(1)

      OPEN(1,FILE='shadmoist.csv')
      read(1,*)LABEL
      do 33 i=1,days*24*nyear
      read(1,*) (shadmoist2(i,j),j=1,12)
33    continue
      close(1)
      
      OPEN(1,FILE='soilpot.csv')
      read(1,*)LABEL
      do 34 i=1,days*24*nyear
      read(1,*) (soilpot2(i,j),j=1,12)
34    continue
      close(1)

      OPEN(1,FILE='shadpot.csv')
      read(1,*)LABEL
      do 35 i=1,days*24*nyear
      read(1,*) (shadpot2(i,j),j=1,12)
35    continue
      close(1)

      OPEN(1,FILE='humid.csv')
      read(1,*)LABEL
      do 36 i=1,days*24*nyear
      read(1,*) (humid2(i,j),j=1,12)
36    continue
      close(1)

      OPEN(1,FILE='shadhumid.csv')
      read(1,*)LABEL
      do 37 i=1,days*24*nyear
      read(1,*) (shadhumid2(i,j),j=1,12)
37    continue
      close(1)      

      goto 19

19    continue

c    rainfall2(1)=0

c    ectoinput2(109)=6
c    ectoinput2(110)=19.4
c    ectoinput2(111)=12.
c    ectoinput2(112)=1863.5/24.

      call ectotherm(nn3,ectoinput2,metout2,shadmet2,soil22,shadsoil2,
     &soilmoist2,shadmoist2,soilpot2,shadpot2,humid2,shadhumid2,
     &dep2,rainfall2,debmod2,deblast2,grassgrowths2,grasstsdm2
     &,wetlandTemps2,wetlandDepths2,arrhenius2,thermal_stages2,
     &behav_stages2,water_stages2,maxshades2,S_instar2,environ2,enbal2
     &,masbal2,debout2,yearout2,yearsout2)
      stop
      end
