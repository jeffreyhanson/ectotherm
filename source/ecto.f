      program nichemapr
     
      Implicit None
             
      DOUBLE PRECISION Y,YDOT,T,TOUT,RTOL,ATOL,RWORK,hs     
      
      double precision ALT1,FLTYPE1,OBJDIS1,OBJL1,PCTDIF1,EMISSK1,
     &EMISSB1,ABSSB1,shade1,enberr1,AMASS1,EMISAN1,absan1,RQ1,rinsul1,
     &lometry1,live1,TIMBAS1,Flshcond1,Spheat1,Andens1,ABSMAX1,ABSMIN1
     &,FATOSK1,FATOSB1,FATOBJ1,TMAXPR1,TMINPR1,DELTAR1,SKINW1,spec1,
     &xbas1,extref1,TPREF1,ptcond1,skint1,gas1,metout2,grassgrowths2,
     &shadmet2,soil22,shadsoil2,dep2,environ2,enbal2,transt1,soilnode1,
     &iday1,iyear1,countday1,o2max1,actlvl1,tannul1,nodnum1,
     &tdigpr1,maxshd1,minshd1,ctmax1,ctmin1,julday1,behav11,rainfall2,
     &viviparous1,pregnant1,ntry1,ectoinput2,yearout2,masbal2,grasstsdm2
     &,wetlandTemps2,wetlandDepths2,thermal_stages2,thermal_stages1,
     &behav_stages1,behav_stages2,water_stages1,water_stages2
               
      double precision debmod2,debout2,deblast2,rainthresh1,
     &yearsout2,yearsout1

      REAL ABSAN,ABSMAX,ABSMIN,ABSSB,ACT,Acthr,ACTLVL
      REAL ACTXBAS,AEFF,AEYES,AFOOD,AHEIT
      Real AMET,AEVP,AEIN,AWIN,ANRG,AWTR,AFOD,AHRS,AAIR,ADIS            
      Real AL,ALENTH,ALT,AMASS,AMTFUD,ANDENS,ANNUAL,Area         
      Real ASIL,ASILN,ASILP,ATOT,AWIDTH,BP
      Real Count,Critemp,CRTOT,CUTFA                
      Real DAIR,DCO2,DAY,DAYAIR,Daymon
      Real DEIN,DEVP,Deltar,DEPSEL,DEPSUB,DEPTH 
      Real DAYHRS,DAYMET,DAYEVP,DAYEIN,DaJabs,DAYWIN,DAYNRG,DAWTR
     &,DAYWTR                   
      Real degday,DMET,DNRG,DOY,DTIME,DWIN,DWTR,Damt,Davp
      Real EGDRY,EGFAT,EggFat,Egghrs,EggPro,EGGWT,EGNUMS,EGPRO,EGWATR
      Real EMISAN,EMISSB,EMISSK,Enb,Enberr
      REAL enbpast,ENLOST,Enary1,Enary2,Enary3,Enary4
      REAL Enary5,Enary6,Enary7,Enary8,ERRTES,EXTREF          
      Real Fatcond,FATOBJ,FATOSB,FATOSK
      Real Flshcond,FLTYPE,FLUID,FUN             
      Real G,GDRYAB,Gevap,GH2OND,GPRO,gwatph,gwetfod
      Real HD,HDforc,HDfree,HOUR,HrsAct,HOUR2
      Real JULPEG,newdep,O2MAX,O2MIN,OBJDIS,OBJL
      Real PCTDIF,PCTDRY,PCTEYE,PCTFAT,PCTPRO,PCTWTR
      Real PI,PTCOND,QACTIV,QAVAIL,Qrad    
      Real QCONV,QCOND,QIRIN,QIROUT                       
      Real QMETAB,Qresp,Qsevap,QSOL,QSOLAR,QSOLR,Qsolrf,QUIT
      Real R,REFLSH,Refshd,Reftol,RH,RELHUM,RQ
      Real Shade,SIG,SkinW,soil2,SOIL3,SPHEAT,SUBTK,Sumdays
      Real TA,TAIREF,Taloc,Tannul,TC,TCORES,Tdeep
      Real TDIGPR,TEIN,TESTNEW,TESTX
      Real TEVP,TIME,TIMEND,Tlung,TMAXPR,TMET,TMINPR,TNRG
      Real TIMCMN,Tshski,Tshlow,Tskin,TOBJ,TR,TREF
      Real TSKY,TskyC,TSOIL,TSUB,TSUBST,TWIN
      Real TWTR
      Real Tcnmin,Tcpast,Tfinal,Timcst,Tprint,Tshsoi
      Real VEL,VREF
      Real WAVAIL,WC,WCUT,WEVAP,WEYES,WRESP,WTGAIN,WTR4EG,WTRLOS
      Real X,X1,X2,XFAT,XP,XPROT,YP,Xtry
      Real Z,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7,ZBRENT,ZEN,ZSOIL
      Real XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      Real Rinsul,R1,VOL
      Real DE,PASTIM,PCTCAR,PFEWAT,PTUREA,TIMBAS,AirVol,CO2MOL,FoodWater
      Real XBAS,FoodWaterCur
      Real Gprot,Gfat
      Real Thconw
c    100% Shade micromet variables; same order as those in the sun, but not dimensioned
      Real vartim,Tshloc,Tshref,Shadrh,Shadv,ShdTsurf
      Real ShT2cm,ShT60cm,ShZen,ShSolr,Tshsky
      Real printT
      Real tcinit
      Real anegmult,slope
      Real delta,testy
      real ctmax,ctmin

      Real TSHOIL,TSOILS,ZSHOIL
      REAL Enary9,Enary10,Enary11,Enary12,Enary13,Enary14,Enary15
      REAL Enary16,Enary17,Enary18,Enary19,Enary20,Enary21
      REAL Enary22,Enary23,Enary24,Enary25,Enary26,Enary27
      Real Enary28,Enary29,Enary30,Enary31,Enary32,Enary33
      Real Enary34,Enary35,Enary36,Enary37,Enary38,Enary39
      Real Enary40,Enary41,Enary42,Enary43,Enary44,Enary45
      Real Enary46,Enary47,Enary48

      Real Transient,Transar,Ftransar
      Real SUNACT,SHDACT,SUNSOIL,SHDSOIL,CAVE
      Real TPREF
      Real Q10,vo2max,dist,dist1,dist2
      Real Travel,daydis,tcmax
      Real SkinT
      Real EggSoil,EggShsoi,WGS,RHO,GSMO,B,C,D,INTERP,IY,T0,INTERP2
      real depmax,depmaxmon
      REAL V,ED,WETMASS,WETSTORAGE,WETGONAD
     &    ,svl
      REAL ATSOIL,ATSHSOI,p_B_past,cumbatch,wetfood,cumrepro,ms
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      real annualact,annfood,food
      REAL TA_egg,TAL_egg,TAH_egg,TL_egg,TH_egg,k1,Trate1,Tcorr_egg5
      REAL Tcorr_egg10,Egghrs5,Egghrs10,EggDEV5,EggDEV10
      REAL act1,act2,act3,act4,act5,fec1,fec2,fec3,fec4,fec5,actt,
     &fec,lx,mx,rmax,R0,TT,tknest,lx2,mx2
      REAL act11,act12,act13,act14,act15,act16,act17,act18,act19,act20
      REAL fec11,fec12,fec13,fec14,fec15,fec16,fec17,fec18,fec19,fec20
      REAL act6,act7,act8,act9,act10,fec6,fec7,fec8,fec9,fec10

      REAL O2gas,CO2gas,N2gas
      REAL rainfall,rainthresh,contlast,mlength,flength,lengthday
     &,lat,lengthdaydir,prevdaylength,rainmult

      REAL clutchsize,andens_deb,d_V,eggdryfrac,
     &w_E,mu_E,mu_V,w_V,T_REF,T_A,TAL,TAH,TL,TH,funct,
     &zfact,kappa,E_G,k_R,MsM,delta_deb,q,maxmass,p_am,e_m

      REAL v_baby1,e_baby1,v_baby_init,e_baby_init,EH_baby1,
     &e_init_baby,v_init_baby,p_am_ref,v_baby,e_baby,EH_baby,
     &EH_baby_init

      real CONTH,CONTW,CONTVOL,CONTDEP,CONTDEPTH
      real massleft,volumeleft,e_egg,eggmass
      REAL v_init,E_init,E_H
      REAL kappa_X,kappa_X_P,mu_X,mu_P,conthole,Vb

      REAL debout,debfirst1,svl_met,yearsout
      REAL ms_init,cumrepro_init,q_init,hs_init,E_H_init,
     &cumbatch_init,p_Mref,vdotref,h_aref,E_Hb,E_Hp,E_Hj,E_H_start
     &,k_Jref,lambda,daylengthstart,daylengthfinish,breedrainthresh
      real customallom,gutfreemass,shp,s_G,p_Xmref
      real etaO,JM_JO,O2FLUX,CO2FLUX,GH2OMET,MLO2,debqmet
      real MLO2_init,GH2OMET_init,debqmet_init,MR_1,MR_2,MR_3
      real w_X,w_P,w_N,H2O_Bal,dryfood,faeces,nwaste,potfreemass
      real H2O_URINE,H2O_FREE,H2O_FAECES,H2O_BalPast,orig_MsM
      real WETFOODFLUX,WETFAECESFLUX,URINEFLUX,H2O_Bal_hr
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26,surviv
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,longev,gutfull
      real rhref,E_Hmoult1,E_Hmet,E_Hecl,gam,raindrink,orig_clutchsize
      real p_Am1,p_AmIm,T_A1,T_A2,T_A3,T_A4,T_A5,T_A6,T_A7,T_A8,disc
      real Vold_init,Vpup_init,Epup_init,E_Hpup_init,Vold,Vpup,Epup,
     &E_Hpup,surviv_init,halfsat,x_food,tbask,temerge,repro,surv
      real thermal_stages,stage,behav_stages,water_stages,newclutch
      real for1,for2,for3,for4,for5,for6,for7,for8,for9,for10,for11,
     &    for12,for13,for14,for15,for16,for17,for18,for19,for20

      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &    FAECES(24),NWASTE(24),surviv(24),thermal_stages(8,6)
     & ,thermal_stages2(8,6),behav_stages(8,14),behav_stages1(8,14),
     &behav_stages2(8,14),water_stages(8,8),water_stages1(8,8),
     &water_stages2(8,8),thermal_stages1(8,6),surv(100)

      INTEGER HRCALL,DEB1,timeinterval,breedact,breedactthres    
      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,Iclim,I66
      INTEGER IDAY,ie,ii,IHOUR,IL,IMODEL,IOPT,ISS,ist,ISTATE,ITASK
      INTEGER ITOL,IWORK,J,JP,LIVE,LIW,Lometry,LOMREF,LRW,MF,MICRO
      INTEGER NA,NALT,NDAY,NREPET,Nsimday,NEQ,NHOURS,NM,NOBEHV,NOD
      INTEGER nodnum,NON,NORYNT,NQ,NTOBJ,ntry,NTSUB,NumFed,NumHrs,NZ
      integer itest,kkk
      integer NDAYY,TRAN,TRANCT,census
      integer IT,SCENAR,SCENTOT
      integer INTNUM,wingmod,wingcalc
      integer deadegg,median,M,H,MonHatch,SoilNode
      integer years,years2,julstart,completion,complete
      integer goodsoil,monthly,julday,dayjul,ctminthresh,ctkill
      integer IYEAR,NYEAR,countday,trans_start,viviparous
     &,pregnant,pregnant_init,viviparous_init,metamorph,ctmincum
      integer micros,microf,daycount,batch,photostart,photofinish,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage
      integer metab_mode,stages,minnode,wetmod,contonly,contype
      integer deadead,startday,reset


      CHARACTER*130 labloc,labshd,metsun,metshd,label
      CHARACTER*1 ANS1,BURROW,Dayact,Climb,CkGrShad,Crepus,SPEC,Rainact
      Character*1 Nocturn,Fosorial,nofood
      LOGICAL SUCCES
      Character*1 Hourop,Dlenth,Transt,screen
      Character*1 tranin
      Character*1 inactive

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

      DIMENSION IWORK(200),RWORK(200)
      DIMENSION TIME(25),XP(365*25*20),Y(10),YDOT(10),YP(365*25*20)
      DIMENSION ZP1(365*25*20),ZP2(365*25*20),ZP3(365*25*20)
      DIMENSION ZP6(365*25*20),ZP7(365*25*20)
      DIMENSION ZD1(365*25*20),ZD2(365*25*20),ZD3(365*25*20)
      DIMENSION ZD6(365*25*20),ZD7(365*25*20),ZD5(365*25*20)
      DIMENSION ZP4(365*25*20),ZP5(365*25*20),ZD4(365*25*20)
      DIMENSION QSOL(25),RH(25),TskyC(25),soil2(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION DAY(7300),Tshski(25),Tshlow(25)
      DIMENSION TSOIL(25),TSHSOI(25),ZSOIL(10),DEPSEL(365*25*20)
      dimension Daymon(12),Tcores(25)
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

      DIMENSION TSOILS(25),TSHOIL(25),ZSHOIL(10),Acthr(52)
      DIMENSION TRANSIENT(365*25*20),TRANSAR(5,25),FTRANSAR(25)
      DIMENSION TRAVEL(25),INACTIVE(25)
      DIMENSION EggSoil(25),EggShsoi(25),WGS(25),GSMO(25),B(25),
     &C(25),D(25)
      DIMENSION INTERP(1440),IY(1440)
      DIMENSION INTERP2(1440)
c      DIMENSION SHD(25),EGGVO2(1440)
      DIMENSION depmaxmon(7300)

      DIMENSION V(24),ED(24),wetmass(24),wetfood(24)
     &,wetstorage(24),svl(24),E_H(24),Vold(24),Vpup(24),Epup(24),
     &E_Hpup(24)
      DIMENSION wetgonad(24),cumrepro(24),
     &    hs(24),ms(24),cumbatch(24),q(24)
      DIMENSION repro(24),food(50)
      DIMENSION hour2(25)
      DIMENSION ATSOIL(25,10),ATSHSOI(25,10),gas1(3)
      DIMENSION actt(100),fec(100),lx(100),mx(100),lx2(100),mx2(100)

      DIMENSION environ2(24*7300,20),enbal2(24*7300,14),dep2(10)
      DIMENSION METOUT2(24*7300,18),SHADMET2(24*7300,18),
     &masbal2(24*7300,21),grassgrowths2(7300),grasstsdm2(7300),
     &wetlandTemps2(24*7300),wetlandDepths2(24*7300),debmod2(106)
      DIMENSION SOIL22(24*7300,12),SHADSOIL2(24*7300,12)
      DIMENSION behav11(9),debout(24,9),debout2(24*7300,18)
     &,deblast2(13),v_baby1(24),e_baby1(24),ntry1(24),ectoinput2(122),
     &rainfall2(7300),yearout2(80),debfirst1(12),dayjul(24*7300)
      DIMENSION customallom(8),etaO(4,3),JM_JO(4,4),shp(3),EH_baby1(24)
      dimension yearsout(20,45),yearsout1(20,45),yearsout2(20,45)    

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
      COMMON/ENVAR1/QSOL,RH,TskyC,soil2,SOIL3,TIME,Taloc,TREF,rhref
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
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      common/lablts/labloc,labshd,metsun,metshd
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      COMMON/GITRAC/DE,PASTIM,PFEWAT,PTUREA,TIMBAS,FoodWaterCur    
      COMMON/FOOD1/PCTPRO,PCTFAT,PCTCAR,PctDry,CRTOT
      COMMON/FOOD2/Damt,Davp,DaJabs,DAWTR
      Common/FOODIN/gwetfod
c    Common/Year4/Hrsum
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
      COMMON/EGGSOIL/EGGSOIL
      COMMON/INTERP/INTERP
      COMMON/EGGDEV/SOILNODE
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,wetmod,contonly,conthole
     &    ,contype
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
      COMMON/ASOIL/ATSOIL,ATSHSOI
      COMMON/TRANSGUT/TRANS_START
      COMMON/DEBOUT/fecundity,clutches,monrepro,svlrepro,monmature
     &,minED,annfood,food,gutfull,longev,completion,complete,fec1,fec2,
     &fec3,fec4,fec5,fec6,fec7,fec8,fec9,fec10,fec11,fec12,fec13,fec14,
     &fec15,fec16,fec17,fec18,fec19,fec20,act1,act2,act3,act4,act5,act6,
     &act7,act8,act9,act10,act11,act12,act13,act14,act15,act16,act17,
     &act18,act19,act20,fec,surv,for1,for2,for3,for4,for5,for6,for7,for8
     &,for9,for10,for11,for12,for13,for14,for15,for16,for17,for18,for19,
     &for20
      COMMON/ANNUALACT/ANNUALACT
      COMMON/z/tknest,Thconw
      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch,MsM
     &,lambda,breedrainthresh,daylengthstart,daylengthfinish,photostart
     &,photofinish,lengthday,photodirs,photodirf,lengthdaydir
     &,prevdaylength,lat,svl_met,frogbreed,frogstage,metamorph
     &,breedactthres    
      COMMON/DEBPAR3/metab_mode,stages,p_Am1,p_AmIm,T_A1,T_A2,T_A3,T_A4
     &    ,T_A5,T_A6,T_A7,T_A8,disc,gam,E_Hmoult1,E_Hmet,E_Hecl,Vb
      COMMON/DEBINIT/v_init,E_init,ms_init,cumrepro_init,q_init,
     &hs_init,cumbatch_init,p_Mref,vdotref,h_aref,e_baby_init,
     &v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,halfsat,x_food,
     &Vold_init,Vpup_init,Epup_init,E_Hpup_init,p_Xmref
      Common/Airgas/O2gas,CO2gas,N2gas
      common/ctmaxmin/ctmax,ctmin,ctmincum,ctminthresh,ctkill
      common/julday/julday,monthly
      common/vivip/viviparous,pregnant
      common/refshade/refshd
      common/debbaby/v_baby,e_baby,EH_baby,eggmass
      COMMON/ARRHENIUS/T_A,TAL,TAH,TL,TH,T_ref
      common/thermal_stage/thermal_stages,behav_stages,water_stages

      DATA hour2/0.,60.,120.,180.,240.,300.,360.,420.,480.,540.,600.,660
     &    .,720.,780.,840.,900.,960.,1020.,1080.,1140.,1200.,1260.,
     &    1320.,1380.,1440./

      OPEN(1,FILE='ectoinput.csv')
      read(1,*)LABEL
      DO 11 i=1,122
      read(1,*)label,ectoinput2(i)
11    continue
      close(1)

      nyear=int(ectoinput2(69))

      OPEN(1,FILE='metout.csv')
      read(1,*)LABEL
      do 13 i=1,24*12
      read(1,*) (metout2(i,j),j=1,18)
13    continue
      close(1)

      OPEN(1,FILE='shadmet.csv')
      read(1,*)LABEL
      do 14 i=1,24*12
      read(1,*) (shadmet2(i,j),j=1,18)
14    continue
      close(1)

      OPEN(1,FILE='soil.csv')
      read(1,*)LABEL
      do 15 i=1,24*12
      read(1,*) (soil22(i,j),j=1,12)
15    continue
      close(1)

      OPEN(1,FILE='shadsoil.csv')
      read(1,*)LABEL
      do 16 i=1,24*12
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
      do 20 i=1,106
      read(1,*)label,debmod2(i)
20    continue
      close(1)


      OPEN(1,FILE='deblast.csv')
      read(1,*)LABEL
      do 21 i=1,13
      read(1,*)label,deblast2(i)
21    continue
      close(1)

      OPEN(1,FILE='rainfall.csv')
      read(1,*)LABEL
      do 22 i=1,365*nyear
      read(1,*)label,rainfall2(i)
22    continue
      close(1)


      OPEN(1,FILE='grassgrowths.csv')
      read(1,*)LABEL
      do 23 i=1,365*nyear
      read(1,*)label,grassgrowths2(i)
23    continue
      close(1)


      OPEN(1,FILE='grasstsdms.csv')
      read(1,*)LABEL
      do 24 i=1,365*nyear
      read(1,*)label,grasstsdm2(i)
24    continue
      close(1)

      OPEN(1,FILE='wetlandTemps.csv')
      read(1,*)LABEL
      do 25 i=1,365*24*nyear
      read(1,*)label,wetlandTemps2(i)
25    continue
      close(1)

      OPEN(1,FILE='wetlandDepths.csv')
      read(1,*)LABEL
      do 26 i=1,365*24*nyear
      read(1,*)label,wetlandDepths2(i)
26    continue
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

      goto 19

19    continue

c    rainfall2(1)=0

c    ectoinput2(109)=6
c    ectoinput2(110)=19.4
c    ectoinput2(111)=12.
c    ectoinput2(112)=1863.5/24.

      call ectotherm(ectoinput2,metout2,shadmet2,soil22,
     &shadsoil2,dep2,rainfall2,debmod2,deblast2,grassgrowths2,grasstsdm2
     &,wetlandTemps2,wetlandDepths2,thermal_stages2,behav_stages2,
     &water_stages2,environ2,enbal2,masbal2,debout2,yearout2,yearsout2)
      stop
      end
