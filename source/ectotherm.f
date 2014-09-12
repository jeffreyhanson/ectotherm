      subroutine ectotherm(ectoinput1,metout1,shadmet1,soil11,
     &shadsoil1,dep1,rainfall1,debmod1,deblast1,grassgrowth1,grasstsdm1,
     &wetlandTemps1,wetlandDepths1,arrhenius1,thermal_stages1,
     &behav_stages1,water_stages1,maxshades1,environ1,enbal1,masbal1,
     &debout1,yearout,yearsout1)
          
C    COPYRIGHT WARREN P. PORTER  15 April, 2003
c    Modified by Michael R. Kearney 2011 to interface with R environment, 
c    with integration of Kooijman's Dynamic Energy Budget (DEB) theory
c    Static energy budget removed - user can specify an empirical allometry of
c    O2 consumption rate with mass and temperature to get respiratory water loss
c    and metabolic heat generation or use DEB to obtain these from first principles

c    Can run the DEB model for up to 10 years or the original middle day of each month
c    for a specified body mass. Now determines geometry through user-specified proportions
c    given the mass/volume
                                                                                         
C      Energy balance for any point in time for ECTOTHERMS    
C 1.0  Bare solid surface animal or object  
C 1.2  Steady state or transient
C    Transient uses the Gear package, an Adams Predictor-Corrector
C      MKS System of Units (J, M, KG, S, C, K, Pa)  

      Implicit None

      EXTERNAL FUN
      EXTERNAL DSUB
      EXTERNAL JAC
      EXTERNAL FUNWING
             
      DOUBLE PRECISION Y,T,TOUT,RTOL,ATOL,RWORK     
      
      double precision metout1,
     &shadmet1,soil11,shadsoil1,dep1,environ1,enbal1,
     &rainfall1,ntry1,ectoinput1,yearout,masbal1,
     &grassgrowth1,grasstsdm1,wetlandTemps1,wetlandDepths1,
     &thermal_stages1,behav_stages1,water_stages1,maxshades1,arrhenius1

      double precision debmod1,debout1,deblast1,hs,
     &yearsout1

      REAL ABSAN,ABSMAX,ABSMIN,ABSSB,Acthr,ACTLVL,e
      REAL ACTXBAS,AEFF,AEYES,AHEIT
      Real AMET,AEVP,AEIN,AWIN,ANRG,AWTR,AFOD,AHRS,AAIR,ADIS            
      Real AL,ALENTH,ALT,AMASS,AMTFUD,ANDENS,ANNUAL,Area         
      Real ASIL,ASILN,ASILP,ATOT,AWIDTH,BP
      Real Count,Critemp,CRTOT,CUTFA                
      Real DAIR,DCO2,DAY,DAYAIR,Daymon
      Real DEIN,DEVP,Deltar,DEPSEL,DEPSUB,DEPTH 
      Real DAYMET,DAYEVP,DAYEIN,DaJabs,DAYWIN,DAYNRG,DAWTR
     &,DAYWTR                   
      Real degday,DMET,DNRG,DOY,DTIME,DWIN,DWTR,Damt,Davp
      Real Egghrs
      Real EMISAN,EMISSB,EMISSK,Enb,Enberr
      REAL ENLOST,Enary1,Enary2,Enary3,Enary4
      REAL Enary5,Enary6,Enary7,Enary8,ERRTES,EXTREF          
      Real Fatcond,FATOBJ,FATOSB,FATOSK
      Real Flshcond,FLTYPE,FLUID,FUN             
      Real G,Gevap,gwatph,gwetfod
      Real HD,HDforc,HDfree,HrsAct,HOUR2
      Real newdep,O2MAX,O2MIN,OBJDIS,OBJL
      Real PCTDIF,PCTDRY,PCTEYE,PCTFAT,PCTPRO
      Real PI,PTCOND,Qrad    
      Real QCONV,QCOND,QIRIN,QIROUT                       
      Real QMETAB,Qresp,Qsevap,QSOL,QSOLAR,QSOLR,Qsolrf,QUIT
      Real R,REFLSH,Refshd,Reftol,RH,RELHUM,RQ
      Real Shade,SIG,SkinW,SOIL1,SOIL3,SPHEAT,SUBTK
      Real TA,TAIREF,Taloc,Tannul,TC,TCORES,Tdeep
      Real TDIGPR,TEIN,TESTX
      Real TEVP,TIME,TIMEND,Tlung,TMAXPR,TMET,TMINPR,TNRG
      Real TIMCMN,Tshski,Tshlow,Tskin,TOBJ,TR,TREF,tbask,temerge
      Real TSKY,TskyC,TSOIL,TSUB,TSUBST,TWIN
      Real TWTR
      Real Tcnmin,Tcpast,Tfinal,Timcst,Tprint,Tshsoi
      Real VEL,VREF
      Real WC,WCUT,WEVAP,WEYES,WRESP,WTRLOS
      Real X,X1,X2,XFAT,XP,XPROT,YP,Xtry
      Real Z,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7,ZBRENT,ZEN,ZSOIL,zbrentwing
      Real XD2,YD,ZD1,ZD2,ZD3,ZD4,ZD5,ZD6,ZD7
      Real Rinsul,R1,VOL
      Real DE,PASTIM,PCTCAR,PFEWAT,PTUREA,TIMBAS,AirVol,CO2MOL
      Real XBAS,FoodWaterCur,FoodWater
      Real Gprot,Gfat
      Real minwater,pond_env,gutfill,tc_old,minwater2,minwaterlow
c    100% Shade micromet variables; same order as those in the sun, but not dimensioned
      Real Tshloc
      Real Tshsky
      Real printT
      Real tcinit
      Real anegmult,slope
      Real PctDess,Max_PctDess
      real ctmax,ctmin,maxtc,mintc,devtime,birthday,birthmass

      Real TSHOIL,TSOILS
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
      Real Q10,dist
      Real daydis
      Real SkinT
      Real EggSoil,EggShsoi,INTERP
      Real Shd,Maxshd,maxshade
      real depmax
      REAL V,ED,WETMASS,WETSTORAGE,WETGONAD,contref,contwet
     &    ,svl
      REAL ATSOIL,ATSHSOI,p_B_past,cumbatch,wetfood,cumrepro,ms
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      real annualact,annfood,food
      REAL act1,act2,act3,act4,act5,
     &fec,lx,mx,rmax,R0,TT,tknest
      REAL act6,act7,act8,act9,act10,fec6,fec7,fec8,fec9,fec10

      real fec1,fec2,fec3,fec4,fec5
      REAL act11,act12,act13,act14,act15,act16,act17,act18,act19,act20
      REAL fec11,fec12,fec13,fec14,fec15,fec16,fec17,fec18,fec19,fec20
      REAL degday1,degday2,degday3,degday4,degday5,degday6,degday7
      REAL degday8,degday9,degday10,degday11,degday12,degday13,degday14
      real degday15,degday16,degday17,degday18,degday19,degday20

      REAL O2gas,CO2gas,N2gas,stage_rec
      REAL rainfall,rainthresh,contlast,mlength,flength,lengthday
     &,lat,lengthdaydir,prevdaylength,rainmult,phi_init,dessdeath

      REAL clutchsize,andens_deb,d_V,eggdryfrac,
     &w_E,mu_E,mu_V,w_V,T_REF,T_A,TAL,TAH,TL,TH,funct,
     &zfact,kappa,E_G,k_R,MsM,delta_deb,q,maxmass,e_m

      REAL v_baby1,e_baby1,v_baby_init,e_baby_init,EH_baby1,
     &e_init_baby,v_init_baby,p_am_ref,v_baby,e_baby,EH_baby,
     &EH_baby_init,longev,surviv_init

      real CONTH,CONTW,CONTVOL,CONTDEP,CONTDEPTH
      real massleft,volumeleft,e_egg,eggmass
      REAL v_init,E_init,E_H,drunk
      REAL kappa_X,kappa_X_P,mu_X,mu_P,enberr2,pond_depth

      REAL debout,debfirst1,svl_met,E_H_start,orig_clutchsize
      REAL ms_init,cumrepro_init,q_init,hs_init,E_H_init,potfreemass,
     &cumbatch_init,p_Mref,vdotref,h_aref,E_Hb,E_Hp,E_Hj,s_G,orig_MsM
     &,k_Jref,lambda,daylengthstart,daylengthfinish,breedrainthresh
      real customallom,gutfreemass,shp,halfsat,x_food,p_Xmref
      real etaO,JM_JO,O2FLUX,CO2FLUX,GH2OMET,MLO2,debqmet
      real MLO2_init,GH2OMET_init,debqmet_init,MR_1,MR_2,MR_3
      real w_X,w_P,w_N,H2O_Bal,dryfood,faeces,nwaste,pondmax,pondmin
      real H2O_URINE,H2O_FREE,H2O_FAECES,H2O_BalPast,twater
      real WETFOODFLUX,WETFAECESFLUX,URINEFLUX,H2O_Bal_hr,depress
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26,surv,gam
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,gutfull,surviv
      real flytime,flyspeed,rhref,wingtemp,E_Hmoult1,E_Hmet,E_Hecl
      real p_Am1,p_AmIm,disc
      real Vold_init,Vpup_init,Epup_init,E_Hpup_init,Vold,Vpup,Epup,
     &    E_Hpup,breedtempthresh,deathstage,prevstage,maxshades
      real ectoinput,debfirst,rainfall2,grassgrowth,grasstsdm,flymetab
      real continit,wetlandTemps,wetlandDepths,conthole,tbs,causedeath
      real thermal_stages,stage,water_stages,behav_stages,repro,Thconw
      real arrhenius,raindrink,HC,convar,fieldcap,wilting

      real for1,for2,for3,for4,for5,for6,for7,for8,for9,for10,for11,
     &for12,for13,for14,for15,for16,for17,for18,for19,for20
      real yMaxStg,yMaxWgt,yMaxLen,yTmax,yTmin,yMinRes,
     &yMaxDess,yMinShade,yMaxShade,yMinDep,yMaxDep,yBsk,yForage
     &,yDist,yFood,yDrink,yNWaste,yFeces,yO2,yClutch,yFec
      real tMaxStg,tMaxWgt,tMaxLen,tTmax,tTmin,tMinRes
     &,tMaxDess,tMinShade,tMaxShade,tMinDep,tMaxDep,tBsk,tForage
     &,tDist,tFood,tDrink,tNWaste,tFeces,tO2,tClutch,tFec,Vb
      real tDLay,tDEgg,tDHatch,tDStg1,tDStg2,tDStg3,tDStg4,tDStg5
     &,tDStg7,tDStg8,tMStg1,tMStg2,tMStg3,tMStg4,tMStg5,tMStg6,tMStg7,
     &tMStg8,tsurv,tovipsurv,tfit,newclutch,tDStg6
      real yDLay,yDEgg,yDHatch,yDStg1,yDStg2,yDStg3,yDStg4,yDStg5
     &,yDStg7,yDStg8,yMStg1,yMStg2,yMStg3,yMStg4,yMStg5,yMStg6,yMStg7,
     &yMStg8,ysurv,yovipsurv,yfit,mi,ma,mh,yearfract,meanf,yDStg6

      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &FAECES(24),NWASTE(24),surviv(24),grassgrowth(7300)
     &,grasstsdm(7300),wetlandTemps(24*7300),wetlandDepths(24*7300)
     &,tbs(24*7300),thermal_stages1(8,6),thermal_stages(8,6),
     &behav_stages(8,14),water_stages(8,8),behav_stages1(8,14)
     &,water_stages1(8,8),stage_rec(25),maxshades1(7300),
     &maxshades(7300),arrhenius(8,5),arrhenius1(8,5)

      INTEGER HRCALL,DEB1,timeinterval,writecsv,II7
      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,Iclim,I66
      INTEGER IDAY,IHOUR,IMODEL,IOPT,ISS,ISTATE,ITASK
      INTEGER ITOL,IWORK,J,JP,LIVE,LIW,Lometry,LOMREF,LRW,MF,MICRO    
      INTEGER NA,NALT,NDAY,NREPET,Nsimday,NEQ,NHOURS,NM,NOBEHV,NOD
      INTEGER nodnum,NON,NORYNT,NQ,NTOBJ,ntry,NTSUB,NumFed,NumHrs,NZ
      integer itest,kkk,breedact,breedactthres    
      integer NDAYY,TRAN,TRANCT
      integer IT,SCENAR,SCENTOT,breedtempcum,census
      integer INTNUM,minnode,completion,complete
      integer SoilNode
      integer julstart,hct,aestivate,aest
      integer goodsoil,monthly,julday,dayjul,feeding
      integer IYEAR,NYEAR,countday,trans_start,viviparous
     &,pregnant,metamorph,inwater
      integer micros,microf,daycount,batch,photostart,photofinish,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage,micros2
     &,microf2,II1,II2,II3,II4,II5
      integer tester,wingmod,wingcalc,birth,microyear,aquatic,pond
      integer flight,flyer,flytest,ctmincum,ctminthresh,ctkill
      integer metab_mode,stages,deadead,startday,reset,forage
      integer hourcount,wetmod,contonly,contype,shdburrow,stage3,tranny

      integer dehydrated,f1count,counter,soilmoisture

      CHARACTER*130 labloc,labshd,metsun,metshd
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
      DIMENSION TIME(25),XP(365*25*20),Y(10),YP(365*25*20)
      DIMENSION ZP1(365*25*20),ZP2(365*25*20),ZP3(365*25*20)
      DIMENSION ZP6(365*25*20),ZP7(365*25*20)
      DIMENSION ZD1(365*25*20),ZD2(365*25*20),ZD3(365*25*20)
      DIMENSION ZD6(365*25*20),ZD7(365*25*20),ZD5(365*25*20)
      DIMENSION ZP4(365*25*20),ZP5(365*25*20),ZD4(365*25*20)
      DIMENSION QSOL(25),RH(25),TskyC(25),SOIL1(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION DAY(7300),Tshski(25),Tshlow(25)
      DIMENSION TSOIL(25),TSHSOI(25),ZSOIL(10),DEPSEL(20*25*365)
      dimension Daymon(12),Tcores(25)
      Dimension surv(100)
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

      DIMENSION TSOILS(25),TSHOIL(25)
      DIMENSION TRANSIENT(365*25*20),TRANSAR(5,25),FTRANSAR(25)
      DIMENSION INACTIVE(25)
      DIMENSION EggSoil(25),EggShsoi(25)
      DIMENSION INTERP(1440)
      DIMENSION SHD(25)

      DIMENSION V(24),ED(24),wetmass(24),wetfood(24)
     &,wetstorage(24),svl(24),E_H(24),Vold(24),Vpup(24),Epup(24),
     &E_Hpup(24)
      DIMENSION wetgonad(24),cumrepro(24),
     &    hs(24),ms(24),cumbatch(24),q(24)
      DIMENSION repro(24),food(50),Acthr(52)
      DIMENSION hour2(25),ectoinput1(126)
      DIMENSION ATSOIL(25,10),ATSHSOI(25,10)
      DIMENSION fec(100),lx(100),mx(100)

      DIMENSION environ1(24*7300,20),enbal1(24*7300,14),dep1(10)
      DIMENSION METOUT1(24*7300,18),SHADMET1(24*7300,18),
     &masbal1(24*7300,21),pond_env(20,365,25,2),debmod1(93)
      DIMENSION SOIL11(24*7300,12),SHADSOIL1(24*7300,12)
      DIMENSION debout(24,9),debout1(24*7300,20)
     &,deblast1(13),v_baby1(24),e_baby1(24),ntry1(24),
     &rainfall1(7300),yearout(80),debfirst1(12),dayjul(24*7300),
     &grassgrowth1(7300),wetlandTemps1(24*7300)
     &,wetlandDepths1(24*7300),yearsout1(20,45)
      DIMENSION customallom(8),etaO(4,3),JM_JO(4,4),shp(3),EH_baby1(24)
      dimension rainfall2(7300),debfirst(13),ectoinput(126)    

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
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW
     &,SkinT,HC,convar 
      common/evap2/HDfree,HDforc
      common/evap3/spec
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      Common/wTrapez/Dtime
      COMMON/WCONV/FLTYPE
c    Sun environmental variables for the day
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
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
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
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
      COMMON/EGGSOIL/EGGSOIL
      COMMON/INTERP/INTERP
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
      COMMON/DEBPAR3/metab_mode,stages,p_Am1,p_AmIm
     &,disc,gam,E_Hmoult1,E_Hmet,E_Hecl,Vb
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
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env
      COMMON/fly/flytime,flight,flyer,flytest,flyspeed,flymetab
      common/debinput/debfirst,ectoinput,rainfall2,grassgrowth,
     &grasstsdm,wetlandTemps,wetlandDepths
      common/pondtest/pond
      common/bodytemp/tbs,breedtempcum,breedtempthresh
      common/thermal_stage/thermal_stages,behav_stages,water_stages
     &,arrhenius
      common/death/causedeath,deathstage
      common/stage_r/stage_rec,f1count,counter,meanf
      common/metdep/depress,aestivate,aest
      common/soilmoist/fieldcap,wilting,soilmoisture

      writecsv=0

      write(*,*) writecsv
      prevstage=0

      aest=0
      ectoinput=real(ectoinput1)
c    debfirst=real(debfirst1)

      do 987 i=1,13
       debfirst(i)=real(deblast1(i),4)
987   continue
      rainfall2=real(rainfall1)
      grassgrowth=real(grassgrowth1)
      grasstsdm=real(grasstsdm1)
      wetlandTemps=real(wetlandTemps1)
      wetlandDepths=real(wetlandDepths1)
      thermal_stages=real(thermal_stages1)
      behav_stages=int(behav_stages1)
      water_stages=real(water_stages1)
      arrhenius=real(arrhenius1)

      maxshades=real(maxshades1)
      hourcount=0
      E_H_start=0
      breedact=0
      dessdeath=35.

      causedeath=0.

      deathstage=-1

      dehydrated=0
      II1=1
      II2=2
      II3=3
      II4=4
      II5=5
      if(writecsv.eq.2)then
      OPEN (II1, FILE = 'environ.csv')
      OPEN (II2, FILE = 'masbal.csv')
      OPEN (II3, FILE = 'enbal.csv')
      OPEN (II4, FILE = 'debout.csv')

      write(II1,1111) "JULDAY",",","YEAR",",","DAY",",","TIME",",","TC"
     &,",","SHADE",",","ORIENT",",","DEP",",","ACT",",","TA",",","VEL"
     &,",","RELHUM",",","ZEN",",","CONDEP",",","WATERTEMP",",","DAYLENGT
     &H",",","WINGANGLE",",","WINGTEMP",",","FLYING",",","FLYTIME"
      write(II2,1112) "JULDAY",",","YEAR",",","DAY",",","TIME",",","TC"
     &,",","O2_ml",",","CO2_ml",",","NWASTE_g",",","H2OFree_g",",","H2OM
     &et_g",",","DryFood_g",",","WetFood_g",",","DryFaeces_g",",","WetFa
     &eces_G",",","Urine_g",",","H2OResp_g",",","H2OCut_g",",","H2OEvap_
     &g",",","H2OBal_g",",","H2OCumBal_g",",","GutFreeMass_g"
      write(II3,1113) "JULDAY",",","YEAR",",","DAY",",","TIME",",","TC"
     &,",","QSOL",",","QIRIN",",","QMET",",","QEVAP",",","QIROUT",",","Q
     &CONV",",","QCOND",",","ENB",",","NTRY"
      write(II4,1114) "JULDAY",",","YEAR",",","DAY",",","TIME",",","WETM
     &ASS",",","RESERVE_DENS",",","CUMREPRO",",","HS",",","MASS_GUT",","
     &,"SVL",",","V",",","E_H",",","CUMBATCH",",","V_baby",",","E_baby"
     &,",","Pregnant",",","Stage",",","WETMASS_STD",",","Body_cond",","
     &,"Surviv_Prob"
      endif
      
      if(writecsv.gt.0)then
      OPEN (II5, FILE = 'yearout.csv')
      write(II5,1115) "DEVTIME",",","BIRTHDAY",",","BIRTHMASS",",","MONM
     &ATURE",",","MONREPRO",",","SVLREPRO",",","FECUNDITY",",","CLUTCHES
     &",",","ANNUALACT",",","MINRESERVE",",","LASTFOOD",",","TOTFOOD","
     &,","FEC1",",","FEC2",",","FEC3",",","FEC4",",","FEC5",",","FEC6","
     &,","FEC7",",","FEC8",",","FEC9",",","FEC10",",","FEC11",",","FEC12
     &",",","FEC13",",","FEC14",",","FEC15",",","FEC16",",","FEC17",","
     &,"FEC18",",","FEC19",",","FEC20",",","ACT1",",","ACT2",",","ACT3",
     &",","ACT4",",","ACT5",",","ACT6",",","ACT7",",","ACT8",",","ACT9",
     &",","ACT10",",","ACT11",",","ACT12",",","ACT13",",","ACT14",",","A
     &CT15",",","ACT16",",","ACT17",",","ACT18",",","ACT19",",","ACT20",
     &",","MINTB",",","MAXTB",",","Pct_Dess",",","LifeSpan",",","GenTime
     &",",","R0",",","rmax",",","SVL"
      endif

1111  format(19(A8,A1),A8)
1112  format(20(A13,A1),A13)
1113  format(13(A8,A1),A8)
1114  format(19(A13,A1),A13)
1115  format(59(A13,A1),A13)

c    ectoinput1(116)=1

      nyear=int(ectoinput1(69))
c    Setting print interval(min.)
      Tprint = 60.

c    Setting a negative multiplier
      Data anegmult/-1.0/ 
c    Setting final time for integrator for a day(min.)
      Data Tfinal/1440./
c    Number of hours in a day + 1
      DATA NHOURS/25/
      DATA DAYMON/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA hour2/0.,60.,120.,180.,240.,300.,360.,420.,480.,540.,600.,660
     &    .,720.,780.,840.,900.,960.,1020.,1080.,1140.,1200.,1260.,
     &    1320.,1380.,1440./
      Data e/2.71828182845904/
c    data DAYJUL/15,46,74,105,135,166,196,227,258,288,319,349/
c    setting counter for year's total hours above 
c    critical (minimum) egg development temp. in soil
      data degday/0/
c    Setting the number of simulation days, Nsimday, to get output.  
c    Normally this value is 12 for the average day for each month 
c    of the year, but smaller values will work.


c    first check if containter model is on (ectoinput(101)), then check if WET0D is being used (ectoinput(116)
      if(ectoinput1(101).eq.1)then
       aquatic=1
       if(ectoinput1(116).eq.1)then
        pond=0
       else
        pond=1
       endif
      endif

      
5501  continue

c    zeroing annual outputs of fecundity and activity
      do 5500 i=1,nyear
       fec(i)=0
       mx(i)=0.
       lx(i)=0.
       surv(i)=0.
5500  continue
      completion=0
      complete=0
      dead=0
      deadead=0
      act1=0
      act2=0
      act3=0
      act4=0
      act5=0
      act6=0
      act7=0
      act8=0
      act9=0
      act10=0
      act11=0
      act12=0
      act13=0
      act14=0
      act15=0
      act16=0
      act17=0
      act18=0
      act19=0
      act20=0
      for1=0
      for2=0
      for3=0
      for4=0
      for5=0
      for6=0
      for7=0
      for8=0
      for9=0
      for10=0
      for11=0
      for12=0
      for13=0
      for14=0
      for15=0
      for16=0
      for17=0
      for18=0
      for19=0
      for20=0
      degday1=0
      degday2=0
      degday3=0
      degday4=0
      degday5=0
      degday6=0
      degday7=0
      degday8=0
      degday9=0
      degday10=0
      degday11=0
      degday12=0
      degday13=0
      degday14=0
      degday15=0
      degday16=0
      degday17=0
      degday18=0
      degday19=0
      degday20=0
      birth=0
      birthday=0
      maxtc=0
      mintc=0
      devtime=0
      birthmass=0
      longev=0
      r0=0
      rmax=0
      TT=0
      annualact=0
      fecundity=0
      clutches=0
      breeding=0
      max_pctdess=0
      monmature=0
          
      daycount=1
      iyear=1
      countday=1
      metamorph=0

2000  continue

      if(daycount.gt.1)then
       if(metamorph.eq.1)then
        transt='n'
        FLTYPE=ectoinput(2)
        OBJDIS=ectoinput(3)
        OBJL=ectoinput(4)
        PCTDIF=ectoinput(5)
        EMISSK=ectoinput(6)
        EMISSB=ectoinput(7)
        ABSSB=ectoinput(8)
        shade=ectoinput(9)
        enberr2=ectoinput(10)
        EMISAN=ectoinput(12)
        absan=ectoinput(13)
        RQ=ectoinput(14)
        rinsul=ectoinput(15)
        lometry=int(ectoinput(16))
        live=int(ectoinput(17))
        TIMBAS=ectoinput(18)
        Flshcond=ectoinput(19)
        Spheat=ectoinput(20)
        Andens=ectoinput(21)
        ABSMAX=ectoinput(22)
        ABSMIN=ectoinput(23)
        FATOSK=ectoinput(24)
        FATOSB=ectoinput(25)
        FATOBJ=ectoinput(26)
        TMAXPR=ectoinput(27)
        TMINPR=ectoinput(28)
        DELTAR=ectoinput(29)
        SKINW=ectoinput(30)
        if(ectoinput(31).eq.0)then
         spec = 'n'
        endif
        if(ectoinput(31).eq.1)then
         spec = 'y'
        endif
        xbas=ectoinput(32)
        extref=ectoinput(33)
        TPREF=ectoinput(34)
        ptcond=ectoinput(35)
        skint=ectoinput(36)
        O2gas=ectoinput(37)
        CO2gas=ectoinput(38)
        N2gas=ectoinput(39)
        soilnode=int(ectoinput(41))
        tdigpr=ectoinput(46)
        maxshd=ectoinput(47)
        refshd=ectoinput(48)
        ctmax=ectoinput(49)
        ctmin=ectoinput(50)
         if(ectoinput(51).eq.1)then
         dayact = 'y'
        else
         dayact = 'n'
        endif
        if(ectoinput(52).eq.1)then
         nocturn = 'y'
        else
         nocturn = 'n'
        endif
        if(ectoinput(53).eq.1)then
         crepus='y'
        else
         crepus='n'
        endif
        if(ectoinput(54).eq.1)then
         rainact='y'
        else
         rainact='n'
        endif
        if(ectoinput(55).eq.1)then
         burrow = 'y'
        else
         burrow = 'n'
        endif
        if(ectoinput(56).eq.1)then
         CkGrShad='Y'
        else
         CkGrShad='n'
        endif
        if(ectoinput(57).eq.1)then
         climb='y'
        else
         climb='n'
        endif
        if(ectoinput(58).eq.1)then
         Fosorial='y'
        else
         Fosorial='n'
        endif
        if(ectoinput(59).eq.1)then
         nofood='y'
        else
         nofood='n'
        endif
        Rainthresh=ectoinput(61)
        rainfall=rainfall2(daycount)
       endif

       if(dead.eq.1)then
        stage=int(deblast1(13))
        v_init=debfirst(3)
        E_init=debfirst(4)
        ms_init=debfirst(5)
        cumrepro_init=debfirst(6)
        q_init=debfirst(7)
        hs_init=debfirst(8)
        cumbatch_init=debfirst(9)
        v_baby_init=debfirst(10)
        e_baby_init=debfirst(11)
        E_H_init=debfirst(12)
        EH_baby_init=0.
        MLO2_init=0.
        GH2OMET_init=0.
        debqmet_init=0.

        if(deadead.eq.0)then
        surviv_init=1.

        endif
        Vold_init=debfirst(3)
        Vpup_init=3E-9
        Epup_init=0.
        E_Hpup_init=0.

       else
        v_init=v(24)
        E_init=ed(24)
        ms_init=ms(24)
        cumrepro_init=cumrepro(24)
        q_init=q(24)
        hs_init=real(hs(24),4)
        cumbatch_init=cumbatch(24)
        v_baby_init=v_baby1(24)
        e_baby_init=e_baby1(24)
        EH_baby_init=EH_baby1(24)
        E_H_init=E_H(24)
        MLO2_init=MLO2(24)
        GH2OMET_init= GH2OMET(24)
        debqmet_init=debqmet(24)
        surviv_init=surviv(24)
        Vold_init=Vold(24)
        Vpup_init=Vpup(24)
        Epup_init=Epup(24)
        E_Hpup_init=E_Hpup(24)
       endif

       pregnant=pregnant
       conth=conth
       contw=contw
       contlast=contdepth
       
       
       if(microyear.eq.1)then

        micros=(countday*25-24)+(iyear-1)*365*25
        microf=countday*25+(iyear-1)*365*25
        micros2=(countday*24-23)+(iyear-1)*365*24
        microf2=countday*24+(iyear-1)*365*24
        do 1804 i=micros,microf
         i2=i-micros+1
        if(i.lt.microf)then      
      TIME(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,2),4)
      TALOC(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,3),4)
      Tref(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,4),4)
      RH(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,5),4)
      RHREF(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,6),4)
      VREF(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,7),4)
      SOIL1(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,9),4)
      SOIL3(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,10),4)
      Z(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,12),4)
      TSKYC(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,14),4)
      TSHLOW(i-(countday-1)*25-(iyear-1)*365*25)=
     &    real(shadmet1(micros2+i2-1,4),4)
      TSHSKI(i-(countday-1)*25-(iyear-1)*365*25)=
     &    real(shadmet1(micros2+i2-1,14),4)
      QSOL(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-1,13),4)
        else
      TIME(i-(countday-1)*25-(iyear-1)*365*25)=1440
      TALOC(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,3),4)
      Tref(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,4),4)
      RH(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,5),4)
      RHREF(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,6),4)
      VREF(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,7),4)
      SOIL1(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,9),4)
      SOIL3(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,10),4)
      Z(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,12),4)
      TSKYC(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,14),4)
      TSHLOW(i-(countday-1)*25-(iyear-1)*365*25)=
     &    real(shadmet1(micros2+i2-2,4),4)
      TSHSKI(i-(countday-1)*25-(iyear-1)*365*25)=
     &    real(shadmet1(micros2+i2-2,14),4)
      QSOL(i-(countday-1)*25-(iyear-1)*365*25)=
     &real(metout1(micros2+i2-2,13),4)
        endif
1804    continue
        do 1805 j=3,12
         do 1806 i=micros,microf
         i2=i-micros+1
         if(i.lt.microf)then
         ATSOIL(i-(countday-1)*25-(iyear-1)*365*25,j-2)=
     &       real(soil11(micros2+i2-1,j),4)
         ATSHSOI(i-(countday-1)*25-(iyear-1)*365*25,j-2)=
     &       real(shadsoil1(micros2+i2-1,j),4)
         else
         ATSOIL(i-(countday-1)*25-(iyear-1)*365*25,j-2)=
     &       real(soil11(micros2+i2-2,j),4)
         ATSHSOI(i-(countday-1)*25-(iyear-1)*365*25,j-2)=
     &       real(shadsoil1(micros2+i2-2,j),4)
         endif
1806    continue
1805    continue
       rainfall=real(rainfall1(countday+(iyear-1)*365),4)
       dayjul(countday)=int(metout1(countday*24,1),4)

       if(dayjul(countday).eq.0)then

        goto 200

       endif
       julday=dayjul(countday)     
       else
       
        micros=countday*25-24
        microf=countday*25
        micros2=countday*24-23
        microf2=countday*24    
        do 1801 i=micros,microf
         i2=i-micros+1
        if(i.lt.microf)then      
        TIME(i-(countday-1)*25)=real(metout1(micros2+i2-1,2),4)
        TALOC(i-(countday-1)*25)=real(metout1(micros2+i2-1,3),4)
        Tref(i-(countday-1)*25)=real(metout1(micros2+i2-1,4),4)
        RH(i-(countday-1)*25)=real(metout1(micros2+i2-1,5),4)
        VREF(i-(countday-1)*25)=real(metout1(micros2+i2-1,7),4)
        SOIL1(i-(countday-1)*25)=real(metout1(micros2+i2-1,9),4)
        SOIL3(i-(countday-1)*25)=real(metout1(micros2+i2-1,10),4)
        Z(i-(countday-1)*25)=real(metout1(micros2+i2-1,12),4)
        TSKYC(i-(countday-1)*25)=real(metout1(micros2+i2-1,14),4)
        TSHLOW(i-(countday-1)*25)=real(shadmet1(micros2+i2-1,4),4)
        TSHSKI(i-(countday-1)*25)=real(shadmet1(micros2+i2-1,14),4)
        QSOL(i-(countday-1)*25)=real(metout1(micros2+i2-1,13),4)
        else
        TIME(i-(countday-1)*25)=1440
        TALOC(i-(countday-1)*25)=real(metout1(micros2+i2-2,3),4)
        Tref(i-(countday-1)*25)=real(metout1(micros2+i2-2,4),4)
        RH(i-(countday-1)*25)=real(metout1(micros2+i2-2,5),4)
        VREF(i-(countday-1)*25)=real(metout1(micros2+i2-2,7),4)
        SOIL1(i-(countday-1)*25)=real(metout1(micros2+i2-2,9),4)
        SOIL3(i-(countday-1)*25)=real(metout1(micros2+i2-2,10),4)
        Z(i-(countday-1)*25)=real(metout1(micros2+i2-2,12),4)
        TSKYC(i-(countday-1)*25)=real(metout1(micros2+i2-2,14),4)
        TSHLOW(i-(countday-1)*25)=real(shadmet1(micros2+i2-2,4),4)
        TSHSKI(i-(countday-1)*25)=real(shadmet1(micros2+i2-2,14),4)
        QSOL(i-(countday-1)*25)=real(metout1(micros2+i2-2,13),4)
        endif
1801    continue
        do 1802 j=3,12
         do 1803 i=micros,microf
         i2=i-micros+1
         if(i.lt.microf)then
         ATSOIL(i-(countday-1)*25,j-2)=real(soil11(micros2+i2-1,j),4)
      ATSHSOI(i-(countday-1)*25,j-2)=real(shadsoil1(micros2+i2-1,j),4)
         else
         ATSOIL(i-(countday-1)*25,j-2)=real(soil11(micros2+i2-2,j),4)
      ATSHSOI(i-(countday-1)*25,j-2)=real(shadsoil1(micros2+i2-2,j),4)
         endif
1803    continue
1802    continue
        rainfall=real(rainfall1(daycount),4)
        dayjul(countday)=int(metout1(countday*24,1))

       if(dayjul(countday).eq.0)then

        goto 200

       endif
        julday=dayjul(countday)
c     end of check for whether microclimate input is more than one year
       endif    
      endif

      if(daycount.eq.1)then

       Nsimday = 1
       inactive = 'N'
c     this is from the original ectotherm main program    
       ICLIM = 1
       PRINTT = 60
C      DEFINING CONSTANTS:
       ANS1 = 'N'
       PI = 3.141592
       SIG = 5.669E - 08
c       NON = 10
C      ANIMAL CONSTANTS
       AHEIT = 0.035
       ALENTH = 0.07
       AWIDTH = 0.035
       ANDENS = 1000.
       EXTREF = 16.
       RQ = 0.8
       NORYNT = 1
c     minimum temperature for egg development to proceed
       Critemp = 24.
       TPREF = TDIGPR
C      NOBEHV = LOGICAL VARIABLE TO OUTPUT TO SCREEN LOCATION & Tcore
C     EVERY HOUR. 0 = NO SCREEN OUTPUT, 1 = OUTPUT TO SCREEN
       NOBEHV = 0
     
C      ENVIRONMENTAL CONSTANTS
C      FLUID TYPE; 0.0=AIR; 1.0=WATER
       OBJDIS = 1.0
       OBJL = 0.0001
       NQ = 1
       NZ = 1
       NA = 11
       NM=1 
       NTSUB = 11
       NTOBJ = 1
       NALT = 1
c    end check if julday is 1
      endif

C     DEFINING ALLOWABLE ERRORS FOR LSODE (GEAR INTEGRATOR)
      RTOL = 1.D-05
      ATOL = 1.D-07

C     SETTING OTHER PARAMETERS FOR LSODE
C     NUMBER OF DIFFERENTIAL EQUATIONS = NEQ 
      NEQ=1
      ITASK = 1
      ITOL = 1
      ISTATE =1
       IOPT = 0
      LRW = 20 + 16*NEQ
      LIW = 20
      MF = 10

C    SETTING DAY COUNTER = 1
      NDAY = 1
C    SETTING DAY OF YEAR COUNTER
      IF (JP .EQ. 0) THEN
        DOY = 0.
       ELSE
      ENDIF

c    INPUT from R***********************************************************
      hourop='y'
      neq = 1
      IDAY=1
      I10=17


      if(daycount.eq.1)then
c     OPEN (I10, FILE = 'CONTUR.OUT')
c     OPEN (I4, FILE='Hourplot.out')
       ALT=real(ectoinput1(1),4)
       BP = 101325. *((1.-(.0065*ALT/288.))**(1./.190284))
       FLTYPE=real(ectoinput1(2),4)
       OBJDIS=real(ectoinput1(3),4)
       OBJL=real(ectoinput1(4),4)
       PCTDIF=real(ectoinput1(5),4)
       EMISSK=real(ectoinput1(6),4)
       EMISSB=real(ectoinput1(7),4)
       ABSSB=real(ectoinput1(8),4)
       shade=real(ectoinput1(9),4)
       enberr2=real(ectoinput1(10),4)
       AMASS=real(ectoinput1(11),4)
       EMISAN=real(ectoinput1(12),4)
       absan=real(ectoinput1(13),4)
       RQ=real(ectoinput1(14),4)
       rinsul=real(ectoinput1(15),4)
       lometry=int(ectoinput1(16))
       live=int(ectoinput1(17))
       TIMBAS=real(ectoinput1(18),4)
       Flshcond=real(ectoinput1(19),4)
       Spheat=real(ectoinput1(20),4)
       Andens=real(ectoinput1(21),4)
       ABSMAX=real(ectoinput1(22),4)
       ABSMIN=real(ectoinput1(23),4)
       FATOSK=real(ectoinput1(24),4)
       FATOSB=real(ectoinput1(25),4)
       FATOBJ=real(ectoinput1(26),4)
       TMAXPR=real(ectoinput1(27),4)
       TMINPR=real(ectoinput1(28),4)
       DELTAR=real(ectoinput1(29),4)
       SKINW=real(ectoinput1(30),4)
       if(ectoinput1(31).eq.0)then
        spec = 'n'
       endif
       if(ectoinput1(31).eq.1)then
        spec = 'y'
       endif
       xbas=real(ectoinput1(32),4)
       extref=real(ectoinput1(33),4)
       TPREF=real(ectoinput1(34),4)
       ptcond=real(ectoinput1(35),4)
       skint=real(ectoinput1(36),4)
       O2gas=real(ectoinput1(37),4)
       CO2gas=real(ectoinput1(38),4)
       N2gas=real(ectoinput1(39),4)
       if(ectoinput1(40).eq.1)then
        TRANST = 'y'
        tranny=1
       else
        TRANST = 'n'
        tranny=0
       endif
       soilnode=int(ectoinput1(41))
       o2max=real(ectoinput1(42),4)
       ACTLVL=real(ectoinput1(43),4)
       tannul=real(ectoinput1(44),4)
       nodnum=int(ectoinput1(45))
       tdigpr=real(ectoinput1(46),4)
       maxshd=real(ectoinput1(47),4)
       refshd=real(ectoinput1(48),4)
       ctmax=real(ectoinput1(49),4)
       ctmin=real(ectoinput1(50),4)
       if(ectoinput1(51).eq.1)then
        dayact = 'y'
       else
        dayact = 'n'
       endif
       if(ectoinput1(52).eq.1)then
        nocturn = 'y'
       else
        nocturn = 'n'
       endif
       if(ectoinput1(53).eq.1)then
        crepus='y'
       else
        crepus='n'
       endif
       if(ectoinput1(54).eq.1)then
        rainact='y'
       else
        rainact='n'
       endif
       if(ectoinput1(55).eq.1)then
        burrow = 'y'
       else
        burrow = 'n'
       endif
       if(ectoinput1(56).eq.1)then
        CkGrShad='Y'
       else
        CkGrShad='n'
       endif
       if(ectoinput1(57).eq.1)then
        climb='y'
       else
        climb='n'
       endif
       if(ectoinput1(58).eq.1)then
        Fosorial='y'
       else
        Fosorial='n'
       endif
       if(ectoinput1(59).eq.1)then
        nofood='y'
       else
        nofood='n'
       endif
       Rainthresh=real(ectoinput1(61),4)
       viviparous=int(ectoinput1(62))
       pregnant=int(ectoinput1(63))

c    check if bucket model has run (or if WET0D was run) and, if so, switch off pond
       if(((aquatic.eq.1).and.(pond.eq.0))
     &.or.(ectoinput1(101).eq.0).or.(ectoinput1(116).eq.1))then
        conth=0
        contw=0
        contref=real(ectoinput1(64),4)
        contlast=0
        transt='n'
        rainmult=0
        jp=0
        DOY = 0.
       else
        conth=real(ectoinput1(64),4)
        contref=conth
        contw=real(ectoinput1(65),4)
        contlast=real(ectoinput1(66),4)
        rainmult=real(ectoinput1(71),4)
       endif
       if(ectoinput1(67).eq.1)then
        tranin = 'y'
       else
        tranin = 'n'
       endif
       tcinit=real(ectoinput1(68),4)
       nyear=int(ectoinput1(69))
       lat=real(ectoinput1(70),4)
           
       julstart=int(ectoinput1(72))    
       monthly=int(ectoinput1(73))
       customallom(1)=real(ectoinput1(74),4)
       customallom(2)=real(ectoinput1(75),4)
       customallom(3)=real(ectoinput1(76),4)
       customallom(4)=real(ectoinput1(77),4)
       customallom(5)=real(ectoinput1(78),4)
       customallom(6)=real(ectoinput1(79),4)
       customallom(7)=real(ectoinput1(80),4)
       customallom(8)=real(ectoinput1(81),4)
       MR_1=real(ectoinput1(82),4)
       MR_2=real(ectoinput1(83),4)
       MR_3=real(ectoinput1(84),4)
       DEB1=int(ectoinput1(85))
       tester=int(ectoinput1(86))
       rho1_3=real(ectoinput1(87),4)
       trans1=real(ectoinput1(88),4)
       aref=real(ectoinput1(89),4)
       bref=real(ectoinput1(90),4)
       cref=real(ectoinput1(91),4)
       phi=real(ectoinput1(92),4)
       phi_init=phi
       wingmod=int(ectoinput1(93))
       phimax=real(ectoinput1(94),4)
       phimin=real(ectoinput1(95),4)
       shp(1)=real(ectoinput1(96),4)
       shp(2)=real(ectoinput1(97),4)
       shp(3)=real(ectoinput1(98),4)
       if((shp(1).eq.shp(2)).and.(shp(2).eq.shp(3)))then
        shp(3)=shp(3)-0.0000001
       endif
       minwater=real(ectoinput1(99),4)
       microyear=int(ectoinput1(100))
       flyer=int(ectoinput1(102))
       flyspeed=real(ectoinput1(103),4)
       timeinterval=int(ectoinput1(104))
       non=int(ectoinput1(105))
       ctminthresh=int(ectoinput1(106))
       ctkill=int(ectoinput1(107))
       gutfill=real(ectoinput1(108)/100.,4)
       minnode=int(ectoinput1(109))
       rainfall=real(rainfall1(daycount),4)
       tbask=real(ectoinput1(110),4)
       temerge=real(ectoinput1(111),4)
       p_Xmref=real(ectoinput1(112),4)
       SUBTK=real(ectoinput1(113),4)
       flymetab=real(ectoinput1(114),4)
       continit=real(ectoinput1(115),4)
       wetmod=int(ectoinput1(116))
       contonly=int(ectoinput1(117))
       conthole=real(ectoinput1(118),4)
       contype=int(ectoinput1(119))
       shdburrow=int(ectoinput1(120))
       breedtempthresh=real(ectoinput1(121),4)
       breedtempcum=int(ectoinput1(122))
       contwet=real(ectoinput1(123),4)
       fieldcap=real(ectoinput1(124),4)
       wilting=real(ectoinput1(125),4)
       soilmoisture=int(ectoinput1(126))
       do 800 i=1,10
        zsoil(i)=real(dep1(i),4)  
800    continue

       do 801 i=1,25
       if(i.lt.25)then
        dayjul(i)=int(metout1(i,1))

       if(dayjul(i).eq.0)then

        goto 200

       endif
        TIME(i)=real(metout1(i,2),4)
        TALOC(i)=real(metout1(i,3),4)
        Tref(i)=real(metout1(i,4),4)
        RH(i)=real(metout1(i,5),4)
        VREF(i)=real(metout1(i,7),4)
        SOIL1(i)=real(metout1(i,9),4)
        SOIL3(i)=real(metout1(i,10),4)
        Z(i)=real(metout1(i,12),4)
        TSKYC(i)=real(metout1(i,14),4)
        TSHLOW(i)=real(shadmet1(i,4),4)
        TSHSKI(i)=real(shadmet1(i,14),4)
        QSOL(i)=real(metout1(i,13),4)
       else
        dayjul(i)=int(metout1(24,1))

       if(dayjul(i).eq.0)then

        goto 200

       endif
        TIME(i)=1440
        TALOC(i)=real(metout1(24,3),4)
        Tref(i)=real(metout1(24,4),4)
        RH(i)=real(metout1(24,5),4)
        VREF(i)=real(metout1(24,7),4)
        SOIL1(i)=real(metout1(24,9),4)
        SOIL3(i)=real(metout1(24,10),4)
        Z(i)=real(metout1(24,12),4)
        TSKYC(i)=real(metout1(24,14),4)
        TSHLOW(i)=real(shadmet1(24,4),4)
        TSHSKI(i)=real(shadmet1(24,14),4)
        QSOL(i)=real(metout1(24,13),4)
       endif
801    continue
       do 802 j=3,12
        do 803 i=1,25
         if(i.lt.25)then
         ATSOIL(i,j-2)=real(soil11(i,j),4)
         ATSHSOI(i,j-2)=real(shadsoil1(i,j),4)
         else
         ATSOIL(24,j-2)=real(soil11(24,j),4)
         ATSHSOI(24,j-2)=real(shadsoil1(24,j),4)
         endif
803     continue
802    continue

      julday=dayjul(1)

c     DEB model
       clutchsize = real(debmod1(1),4)
       andens_deb = real(debmod1(2),4)
       d_V = real(debmod1(3),4)
       eggdryfrac = real(debmod1(4),4)
       mu_X = real(debmod1(5),4)
       mu_E = real(debmod1(6),4)
       mu_V = real(debmod1(7),4)
       mu_P = real(debmod1(8),4)
        T_REF = real(debmod1(9),4)

c     primary DEB parameters
       zfact = real(debmod1(10),4)
       kappa = real(debmod1(11),4)
       kappa_X = real(debmod1(12),4)
       p_Mref = real(debmod1(13),4)
       vdotref= real(debmod1(14),4)
       E_G = real(debmod1(15),4)
       k_R = real(debmod1(16),4)
       MsM = real(debmod1(17),4)
       delta_deb = real(debmod1(18),4)
       h_aref=real(debmod1(19),4)
       V_init_baby=real(debmod1(20),4)
       E_init_baby=real(debmod1(21),4)

       v_baby=V_init_baby

       E_baby=E_init_baby
       k_Jref = real(debmod1(22),4)
       E_Hb = real(debmod1(23),4)
       E_Hj = real(debmod1(24),4)
       E_Hp = real(debmod1(25),4)
       eggmass = real(debmod1(26),4)
       batch = int(debmod1(27))
       breedrainthresh = real(debmod1(28),4)
       photostart = int(debmod1(29))
       photofinish = int(debmod1(30))
       daylengthstart=real(debmod1(31),4)
       daylengthfinish=real(debmod1(32),4)
       photodirs=int(debmod1(33))
       photodirf=int(debmod1(34))

       svl_met=real(debmod1(35),4)
       frogbreed=int(debmod1(36))
       frogstage=int(debmod1(37))
       etaO(1,1)=real(debmod1(38),4)
       etaO(2,1)=real(debmod1(39),4)
       etaO(3,1)=real(debmod1(40),4)
       etaO(4,1)=real(debmod1(41),4)
       etaO(1,2)=real(debmod1(42),4)
       etaO(2,2)=real(debmod1(43),4)
       etaO(3,2)=real(debmod1(44),4)
       etaO(4,2)=real(debmod1(45),4)
       etaO(1,3)=real(debmod1(46),4)
       etaO(2,3)=real(debmod1(47),4)
       etaO(3,3)=real(debmod1(48),4)
       etaO(4,3)=real(debmod1(49),4)
       JM_JO(1,1)=real(debmod1(50),4)
       JM_JO(2,1)=real(debmod1(51),4)
       JM_JO(3,1)=real(debmod1(52),4)
       JM_JO(4,1)=real(debmod1(53),4)
       JM_JO(1,2)=real(debmod1(54),4)
       JM_JO(2,2)=real(debmod1(55),4)
       JM_JO(3,2)=real(debmod1(56),4)
       JM_JO(4,2)=real(debmod1(57),4)
       JM_JO(1,3)=real(debmod1(58),4)
       JM_JO(2,3)=real(debmod1(59),4)
       JM_JO(3,3)=real(debmod1(60),4)
       JM_JO(4,3)=real(debmod1(61),4)
       JM_JO(1,4)=real(debmod1(62),4)
       JM_JO(2,4)=real(debmod1(63),4)
       JM_JO(3,4)=real(debmod1(64),4)
       JM_JO(4,4)=real(debmod1(65),4)
       e_egg=real(debmod1(66))
       kappa_X_P=real(debmod1(67),4)
       PTUREA=real(debmod1(68),4)
       PFEWAT=real(debmod1(69),4)
       w_X=real(debmod1(70),4)
       w_E=real(debmod1(71),4)
       w_V=real(debmod1(72),4)
       w_P=real(debmod1(73),4)
       w_N=real(debmod1(74),4)
       FoodWater=real(debmod1(75),4)
       FoodWaterCur=FoodWater
       funct=real(debmod1(76),4)
       s_G = real(debmod1(77),4)
       halfsat = real(debmod1(78),4)
       x_food = real(debmod1(79),4)
       metab_mode = int(debmod1(80))
       stages = int(debmod1(81))
       p_Am1 = real(debmod1(82),4)
       p_AmIm = real(debmod1(83),4)
        disc = real(debmod1(84),4)
       gam = real(debmod1(85),4)
       startday = int(debmod1(86))
       raindrink = int(debmod1(87))
       reset = int(debmod1(88))
       ma=real(debmod1(89),4)
       mi=real(debmod1(90),4)
       mh=real(debmod1(91),4)
       aestivate=int(debmod1(92))
       depress=real(debmod1(93),4)
       if(deb1.eq.1)then
       if(metab_mode.gt.0)then
        E_Hmoult1=E_Hj
c      E_Hmet=E_Hj
        E_Hecl=E_Hp
c      if((metab_mode.eq.1).or.(metab_mode.eq.3))then
c       E_Hb = E_Hmoult1/gam**3.
c      endif
c      if((metab_mode.eq.2).or.(metab_mode.eq.4))then
c       E_Hb = E_Hmoult1*(0.5)**3.
c      endif
       endif
       endif

c     initial conditions or values from last day
c     iyear=deblast1(1)
       countday=int(deblast1(2))
       v_init=real(deblast1(3),4)
       E_init=real(deblast1(4),4)
       ms_init=real(deblast1(5),4)
       cumrepro_init=real(deblast1(6),4)
       q_init=real(deblast1(7),4)
       hs_init=real(deblast1(8),4)
       cumbatch_init=real(deblast1(9),4)
       v_baby_init=real(deblast1(10),4)
       e_baby_init=real(deblast1(11),4)
       E_H_init=real(deblast1(12),4)

       stage=real(deblast1(13),4)
       MLO2_init=0.
       GH2OMET_init=0.
       debqmet_init=0.
       surviv_init=1.
       Vold_init=real(deblast1(3),4)
       Vpup_init=3E-9
       Epup_init=0.
       E_Hpup_init=0.
c     save initial values for resetting to egg upon death

       if((daycount.eq.1).and.(ihour.eq.0))then
        do 1101 i=1,12
         debfirst(i)=real(deblast1(i),4)
1101    continue
       endif
       p_AM_ref = p_Mref*zfact/kappa
       E_M = p_AM_ref/vdotref
       maxmass=(zfact**3)*andens_deb+((((zfact**3)*E_m)/
     & mu_E)*w_E)/d_V
      endif
c    end of check that daycount is 1

      if(conth.gt.0)then
       live=0
       transt='y'
      else
       if(ectoinput1(40).eq.1)then
        TRANST = 'y'
       else
        TRANST = 'n'
       endif
      endif

      If (live .eq. 0) then
       Dayact = 'Y'
       nocturn = 'y'
       crepus = 'y'
       Burrow = 'N'
       Climb = 'N'
       CkGrShad ='N'
       Numfed = 0
       NumHrs = 0
       TMAXPR=100.
       TMINPR=-100.
       TBASK=-100.
       TEMERGE=-100.
       TPREF =100.
      Endif

      if(conth.gt.0)then
       skinw=contwet
c       if(rainfall.eq.0)then
c        rainfall=0.01
c       endif
       if(conth.gt.0)then
        if(daycount.eq.1)then
         CONTDEP = continit*10
        else
         CONTDEP = rainfall*rainmult+contlast-conthole
        endif
        if(contdep.lt.0.01)then
         contdep=0.01
        endif
        IF(CONTDEP .gt. conth*10)THEN
          CONTDEP = conth*10
        ENDIF
       endif
      else
       rainmult=0
      endif    

C     if(soilmoisture.eq.1)then
C      if(pond.eq.1)then
C       skinw=contwet*EXP((CONTDEP-fieldcap)/(wilting))
C       if(skinw.lt.0)then
C           skinw=0
C       endif
C       if(skinw.gt.contwet)then
C           skinw=contwet
C       endif
C      else
C       SKINW=real(ectoinput1(30),4)
C      endif
C     endif
c    running bucket model so set config factor    
c    if((aquatic.eq.1).and.(pond.eq.1))then
c      FATOSK=1
c      FATOSB=0
c      FATOBJ=0
c    endif

c    setting counter for day's total hours above 
c    critical (minimum) egg development temp. in soil
      Egghrs = 0.
      Dist = 0.
c    daycount=1

C    SETTING LOGICAL FOR PLOT OUTPUT TO BE 

C    ANNUAL = 1; DAY OF YEAR VS TIME OF DAY
      ANNUAL = 1.
C    TO BE PUT IN COMMON WITH MAIN
      NREPET = 1
C     SETTING UP ENVIRONMENT VARIABLES FROM ARRAYS 
      TIMEND = 1440.
C    Initializing time (in cumulative minutes)
      T = TIME(1)
c    Hour of the day
      IHOUR = int((T/60.)) + 1

      DTIME=PRINTT*60
      INTNUM=int(1500/PRINTT)

      LOMREF = LOMETRY 

c    initialize mass
      if((deb1.eq.1).and.(live.eq.1))then
       if((daycount.eq.1).and.(ihour.eq.1))then
        AMASS=((((V_init*E_init)/mu_E)*w_E)/d_V + V_init)/1000
       endif
      endif

C       ****************** END OF I/O ********************* 

C     COMPUTING ANIMAL (OBJECT) AREAS AND CONVECTION DIMENSION
      CALL ALLOM

C    ESTABLISHING A REFERENCE FOR THE USER SUPPLIED FLESH THERMAL CONDUCTIVITY
      REFLSH = FLSHCOND
      WC = AMASS * SPHEAT

C     COMPUTING GEOMETRY OF DIFFUSE RADIATION CONFIGURATION FACTOR,F,    
C     BETWEEN ANIMAL AND LARGE NEARBY OBJECT IN THE ENVIRONMENT   
C     (OTHER THAN SKY AND GROUND)
 
      CALL CONFAC (AL,OBJDIS,OBJL,FATOSK,FATOBJ)

C     COMPUTING SOLAR AND LONG WAVE INFRARED HEAT INPUT  

C    CHOOSING NORMAL OR POINTING POSTURE
      IF (NORYNT .EQ. 1) THEN
       ASIL = ASILN
      ELSE
       ASIL = ASILP
      ENDIF 

C    Computing soil depth (m) for conduction: from surface to first node down.
      Depsub = (zsoil(2) - zsoil(1))/100.

c    Initialize HrsAct for this month
      HrsAct = 0

c    checking this month's minimum activity temperature
      if(tdigpr.lt.tminpr)then
        tminpr = tdigpr
      endif
       do 5778 i=1,24

5778   repro(i)=0

      do 5787 i=1,25
5787  stage_rec(i)=0

      IHOUR = 0


c    setting startday for photoperiod-specified breeding periods when startday set to zero by user

      if((daycount.eq.1).and.(ihour.lt.2).and.(photostart.eq.5).and.
     &    (deb1.eq.1).and.(startday.eq.0))then
       do 844 i=1,365
        mlength=1-TAN(lat*pi/180)*TAN(23.439*PI/180*
     &COS(0.0172*(i-1)))    
        if(mlength.gt.2)then
         mlength=2
        endif
        if(mlength.lt.0)then
         mlength=0
        endif
        flength=ACOS(1-mlength)/(2*PI)*360/180
        if((flength*24.gt.daylengthstart).and.(flength*24.gt.lengthday)
     &      .and.(photodirs.eq.1))then
         startday=i
        endif
        if((flength*24.gt.daylengthstart).and.(flength*24.lt.lengthday)
     &      .and.(photodirs.eq.0))then
         startday=i
        endif
        lengthday=flength*24
844    continue
      endif



      if((daycount.eq.1).and.(ihour.lt.2).and.(photostart.lt.5).and.
     &    (deb1.eq.1).and.(startday.eq.0))then
       if(lat.lt.0)then
c     southern hemisphere        
        if(photostart.eq.1)then
         startday=357
        endif
        if(photostart.eq.2)then
        startday=80
        endif
        if(photostart.eq.3)then
         startday=173
        endif
        if(photostart.eq.4)then
         startday=266
        endif
       else
c     northern hemisphere
        if(photostart.eq.1)then
         startday=173
        endif
        if(photostart.eq.2)then
        startday=266
        endif
        if(photostart.eq.3)then
         startday=357
        endif
        if(photostart.eq.4)then
         startday=80
        endif
       endif
      endif    
c    saving last day's length for check for increasing or decreasing day length
      if(daycount.gt.1)then
       prevdaylength=lengthday
      endif

c    calculated length of current day
      mlength=1-TAN(lat*pi/180)*TAN(23.439*PI/180*
     &COS(0.0172*(julday-1)))    
      if(mlength.gt.2)then
       mlength=2
      endif
      if(mlength.lt.0)then
       mlength=0
      endif
      flength=ACOS(1-mlength)/(2*PI)*360/180
      lengthday=flength*24

c    check for increasing or decreasing day length
      if(daycount.gt.1)then
       if(prevdaylength.lt.lengthday)then
        lengthdaydir=1
       else
        lengthdaydir=0
       endif
      endif

      if(daycount.eq.1)then    
      if(photostart.eq.0)then
       lambda=6./12.
      endif
      if(photostart.eq.5)then

       lambda=6./12.

      endif
c    lambda=3/12

c    getting lambda for season-specified breeding
      if((photostart.ne.0).and.(photostart.ne.5))then
       if((photostart.eq.1).and.(photofinish.eq.2))then
        lambda=3./12.
       endif
       if((photostart.eq.1).and.(photofinish.eq.3))then
        lambda=6./12.
       endif
       if((photostart.eq.1).and.(photofinish.eq.4))then
        lambda=9./12.
       endif
       if((photostart.eq.2).and.(photofinish.eq.3))then
        lambda=3./12.
       endif
       if((photostart.eq.2).and.(photofinish.eq.4))then
        lambda=6./12.
       endif
       if((photostart.eq.2).and.(photofinish.eq.1))then
        lambda=9./12.
       endif
       if((photostart.eq.3).and.(photofinish.eq.4))then
        lambda=3./12.
       endif
       if((photostart.eq.3).and.(photofinish.eq.1))then
        lambda=6./12.
       endif
       if((photostart.eq.3).and.(photofinish.eq.2))then
        lambda=9./12.
       endif
       if((photostart.eq.4).and.(photofinish.eq.1))then
        lambda=3./12.
       endif
       if((photostart.eq.4).and.(photofinish.eq.2))then
        lambda=6./12.
       endif
       if((photostart.eq.4).and.(photofinish.eq.3))then
        lambda=9./12.
       endif
      endif
      endif

C     ****************Start of 25 HOURS IN THE DAY LOOP (0 - 24)***********************
54    CONTINUE
C     START STEADY STATE LOOP

     

      maxshd=maxshades(daycount)    
      IHOUR=IHOUR+1
      if((ihour.eq.1).and.(countday.eq.1).and.(iyear.eq.1))then
      hourcount=0
      endif



c    for Matt Malishev Netlogo sims

c    if(ihour.eq.2)then

c    goto 2001

c    endif


      if(ihour.lt.25)then
      hourcount=hourcount+1
      endif



      if(ihour.eq.25)then
       do 5777 i=1,24
5777   repro(i)=0
      endif




c    startday=1
      if(ihour.eq.1)then
       if(monthly.eq.0)then
        census=startday-1
        if(census.le.0)then
         census=365
        endif
       else
        census=startday
       endif
      endif

      if((((daycount.eq.1).and.(ihour.eq.1)).or.
     &(countday.eq.startday)).and.(ihour.eq.1))then
      yMaxStg=0
      yMaxWgt=0
      yMaxLen=0
      yTmax=0
      yTmin=0
      yMinRes=0
      yMaxDess=0
      yMinShade=0
      yMaxShade=0
      yMinDep=0
      yMaxDep=0
      yBsk=0
      yForage=0
      yDist=0
      yFood=0
      yDrink=0
      yNWaste=0
      yFeces=0
      yO2=0
      yClutch=0
      yFec=0
      yDLay=0
      yDEgg=0
      yDHatch=0
      yDStg1=0
      yDStg2=0
      yDStg3=0
      yDStg4=0
      yDStg5=0
      yDStg6=0
      yDStg7=0
      yDStg8=0
      yMStg1=0
      yMStg2=0
      yMStg3=0
      yMStg4=0
      yMStg5=0
      yMStg6=0
      yMStg7=0
      yMStg8=0
      ysurv=1
      yovipsurv=1
      yfit=1
      endif



      if((daycount.eq.1).and.(ihour.eq.1))then
      tMaxStg=0
      tMaxWgt=0
      tMaxLen=0
      tTmax=0
      tTmin=0
      tMinRes=0
      tMaxDess=0
      tMinShade=0
      tMaxShade=0
      tMinDep=0
      tMaxDep=0
      tBsk=0
      tForage=0
      tDist=0
      tFood=0
      tDrink=0
      tNWaste=0
      tFeces=0
      tO2=0
      tClutch=0
      tFec=0
      tDLay=0
      tDEgg=0
      tDHatch=0
      tDStg1=0
      tDStg2=0
      tDStg3=0
      tDStg4=0
      tDStg5=0
      tDStg6=0
      tDStg7=0
      tDStg8=0
      tMStg1=0
      tMStg2=0
      tMStg3=0
      tMStg4=0
      tMStg5=0
      tMStg6=0
      tMStg7=0
      tMStg8=0
      tsurv=1
      tovipsurv=1
      tfit=1
      endif

      if((aquatic.eq.1).and.(pond.eq.0).and.(ihour.lt.25))then
c     get current pond environment, initialize max/min temps to water temp at hour 1
       if(wetmod.eq.1)then
        pond_depth=wetlandDepths(hourcount)
        twater=wetlandTemps(hourcount)
        pondmax=wetlandTemps(hourcount-ihour+1)
        pondmin=wetlandTemps(hourcount-ihour+1)
       else
        pond_depth=pond_env(iyear,countday,ihour,2)
        twater=pond_env(iyear,countday,ihour,1)
        pondmax=pond_env(iyear,countday,1,1)
        pondmin=pond_env(iyear,countday,1,1)
       endif
      endif

      flytest=0
      flight=0
      flytime=0

      if(stage.eq.8)then
      continue
      endif

      if(daycount.eq.2)then
      continue
      endif


      if((deb1.eq.1).and.(stage.le.7).and.(pond.eq.0))then
       stage3=int(stage)
       ctmin=thermal_stages(stage3+1,1)
       ctmax=thermal_stages(stage3+1,2)
       tminpr=thermal_stages(stage3+1,3)
       tmaxpr=thermal_stages(stage3+1,4)
       tbask=thermal_stages(stage3+1,5)
       tpref=thermal_stages(stage3+1,6)
       T_A=arrhenius(stage3+1,1)
       TAL=arrhenius(stage3+1,2)
       TAH=arrhenius(stage3+1,3)
       TL=arrhenius(stage3+1,4)
       TH=arrhenius(stage3+1,5)
       if(behav_stages(stage3+1,1).eq.1)then
        dayact = 'y'
       else
        dayact = 'n'
       endif
       if(behav_stages(stage3+1,2).eq.1)then
        nocturn = 'y'
       else
        nocturn = 'n'
       endif
       if(behav_stages(stage3+1,3).eq.1)then
        crepus = 'y'
       else
        crepus = 'n'
       endif
       if(behav_stages(stage3+1,4).eq.1)then
        burrow = 'y'
       else
        burrow = 'n'
       endif
      shdburrow=int(behav_stages(stage3+1,5))
      minnode=int(behav_stages(stage3+1,6))
      non=int(behav_stages(stage3+1,7))
       if(behav_stages(stage3+1,8).eq.1)then
        CkGrShad = 'y'
       else
        CkGrShad = 'n'
       endif
       if(behav_stages(stage3+1,9).eq.1)then
        climb = 'y'
       else
        climb = 'n'
       endif
       if(behav_stages(stage3+1,11).eq.1)then
        fosorial = 'y'
       else
        fosorial = 'n'
       endif
       if(behav_stages(stage3+1,11).eq.1)then
        rainact = 'y'
       else
        rainact = 'n'
       endif
      rainthresh=behav_stages(stage3+1,12)
c    breedactthresh=behav_stages(stage3+1,13)
      flyer=int(behav_stages(stage3+1,14))
      
      skinw=water_stages(stage3+1,1)
      extref=water_stages(stage3+1,2)
      PFEWAT=water_stages(stage3+1,3)
      PTUREA=water_stages(stage3+1,4)
      FoodWater=water_stages(stage3+1,5)
      minwater=water_stages(stage3+1,6)
      raindrink=water_stages(stage3+1,7)
      gutfill=water_stages(stage3+1,8)/100.
c     for great desert skink

       if(aest.eq.1)then
        dayact = 'n'
        nocturn = 'n'
        crepus = 'n'
        minnode=6
       endif
      endif


      if((deb1.eq.1).and.(pond.eq.0).and.((stage.eq.8).or.
     &    (complete.eq.1)))then
       if(complete.eq.1)then
       stage3=8
       else
       stage3=int(stage)
       endif

       ctmin=thermal_stages(stage3,1)
       ctmax=thermal_stages(stage3,2)
       tminpr=thermal_stages(stage3,3)
       tmaxpr=thermal_stages(stage3,4)
       tbask=thermal_stages(stage3,5)
       tpref=thermal_stages(stage3,6)

       if(behav_stages(stage3,1).eq.1)then
        dayact = 'y'
       else
        dayact = 'n'
       endif

       if(behav_stages(stage3,2).eq.1)then
        nocturn = 'y'
       else
        nocturn = 'n'
       endif

       if(behav_stages(stage3,3).eq.1)then
        crepus = 'y'
       else
        crepus = 'n'
       endif

       if(behav_stages(stage3,4).eq.1)then
        burrow = 'y'
       else
        burrow = 'n'
       endif

      shdburrow=int(behav_stages(stage3,5))
      minnode=int(behav_stages(stage3,6))
      non=int(behav_stages(stage3,7))

       if(behav_stages(stage3,8).eq.1)then
        CkGrShad = 'y'
       else
        CkGrShad = 'n'
       endif

       if(behav_stages(stage3,9).eq.1)then
        climb = 'y'
       else
        climb = 'n'
       endif

       if(behav_stages(stage3,11).eq.1)then
        fosorial = 'y'
       else
        fosorial = 'n'
       endif

       if(behav_stages(stage3,11).eq.1)then
        rainact = 'y'
       else
        rainact = 'n'
      endif

      rainthresh=behav_stages(stage3,12)

c    breedactthresh=behav_stages(stage3+1,13)

      flyer=int(behav_stages(stage3,14))
      skinw=water_stages(stage3,1)
      extref=water_stages(stage3,2)
      PFEWAT=water_stages(stage3,3)
      PTUREA=water_stages(stage3,4)
      FoodWater=water_stages(stage3,5)
      minwater=water_stages(stage3,6)
      raindrink=water_stages(stage3,7)
      gutfill=water_stages(stage3,8)/100.
      endif



c    if(daycount.eq.752)then

c     write(*,*) non

c    endif
c    note!!!!!!!!!! this should be specific to the last stage but had to put stage 7 for maddie because we are adding an extra stage so that the butterflies die after some repro
      if((deb1.eq.1).and.(metab_mode.gt.0).and.(pond.eq.0))then
       if(stage.lt.7)then
        wingmod=0
       else
        wingmod=int(ectoinput1(93))
       endif
      endif

      do 56 J=1,NON
       TSOIL(J)=ATSOIL(IHOUR,J)
       TSHSOI(J)=ATSHSOI(IHOUR,J)
56    continue

      EggSoil(ihour)=TSOIL(SoilNode)
      EggShSoi(ihour)=TSHSOI(SoilNode)
c    Setting two shade temperatures, sky and local air temp. for this hour from file Shadmet
      Tshsky = Tshski(Ihour)
      Tshloc = Tshlow(Ihour)
      phi=phi_init    
      forage=0

      if(((live.eq.1).and.(deb1.eq.0)).or.((live.eq.1).and.(deb1.eq.1)
     &    .and.(metab_mode.eq.0)))then
      TPREF=real(ectoinput1(34),4)
      endif

c    Initializing counter for iteration
      count = 0
c    Initializing retry counter
      ntry = 1
c    Resetting shade for any thermoregulation that may have happened in a prior hour
      shade = refshd
c    Resetting depth selection
      Depsel(Ihour) = 0.00
      newdep = 0.0
C    Setting QUIT variable for Thermoreg.for
      QUIT = 0.

c    create a solar reference value for determining activity
      Qsolrf = Qsol(Ihour)
      Qsolr = Qsolrf

C    Resetting absorptivity, flesh thermal conductivity, o2 extraction efficiency, and geometry to default values.
      ABSAN = ABSMAX
      EXTREF = O2MAX
      FLSHCOND = REFLSH
      LOMETRY = LOMREF

      CALL ALLOM

      If((Transt.eq.'y').or.(Transt.eq.'Y'))then
       GOTO 7111
      Endif


c    getting above ground conditions as a starter
      Call Aboveground
C     Setting environmental parameters depending on whether fossorial now. 
C     If Fossorial = yes(Y/y), assume in a burrow and inactive at all times.
      If ((Fosorial .eq. 'Y').or.(Fosorial .eq. 'y')) then
       if((nofood .eq. 'Y').or.(nofood .eq. 'y')) then
C     Hibernating
c     Set the microclimate variables for coolest conditions above 3 C
c     200 cm below ground surface: either 60 cm (node 10) or 200 cm.
        if(Tannul.lt.Tsoil(non)) then
         if(Tannul.ge. 3.0)then
          Ta = Tannul
          Depth = -200.
         else
c        Annual temperature < 3C; go more shallowly.
          Tdeep = Tsoil(NON)
          Depth = ZSOIL(NON)
          slope = (200. - Depth)/(Tdeep - Tannul)
          Ta = 3.0
          Depth = (slope*Ta + 200.0)*anegmult
         endif
        else
c       Tannual.ge.Tsoil(non)
         Tdeep = Tsoil(NON)
         Depth = ZSOIL(NON)
         slope = (200. - Depth)/(Tdeep - Tannul)
         Ta = 3.0
         Depth = (slope*Ta + 200.0)*anegmult
        endif
        Qsolr = 0.0000
        ZEN = 90. * PI / 180.   
        TAIREF = Ta   
        VEL = 0.01             
        RELHUM = 99. 
        TSKY = Ta
        TSUBST = Ta  
        TOBJ = TSUBST
        Go to 253
       else
c      Not hibernating, feeding
c      not going so deep, since nodnum = 10 is only 60 cm max
        Ta = Tsoil(nodnum)
        Depth = Zsoil(nodnum)
        Qsolr = 0.0000
        ZEN = 90. * PI / 180.   
        TAIREF = Ta   
        VEL = 0.01             
        RELHUM = 99. 
        TSKY = Ta
        TSUBST = Ta  
        TOBJ = TSUBST
        Go to 253
       Endif
       else
c    Not fossorial
      Endif
c    End of check to see if fossorial

c    Get this hour's environmental conditions above ground, because needed below when Ihour>0
      Call aboveground

c    create a solar reference value for determining activity
      Qsolrf = Qsolr

      if((transt.eq.'n').or.(transt.eq.'N'))then
      goto 7119
      endif

7111  continue

7119  continue

      IF (JP .GE. 25) THEN
       ISS = (JP + 1) - int(DOY*25)
      ELSE
       ISS = JP + 1
      ENDIF

      Qsolr = Qsol(Ihour)
      If (Qsolr .gt. 0.000000000) then
       CALL SOLAR
      else
C     no sun 
       Qsolar = 0.000
      Endif   

      If((Transt.eq.'y').or.(Transt.eq.'Y'))then
        GOTO 253
      Endif

c    check to see if developing egg and, if it is, put it in the ground
      if((deb1.eq.1).and.(pond.eq.0))then
       if(monthly.lt.2)then
c      if(E_H_init.lt.E_Hb)then
        if(stage.eq.0)then
        if((viviparous.eq.1).or.((reset.gt.0).and.(E_H_start.eq.0).and.
     &      (frogbreed.gt.0)))then
          amass=maxmass/1000
         else
          if(frogbreed.ne.1)then
           if(shdburrow.eq.1)then
            Ta=EggShSoi(ihour)
            shade=maxshade
           else
            Ta=Eggsoil(ihour)
           endif
           Call Belowground
           call Radin    
           depsel(ihour)=-1*zsoil(soilnode)
           goto 252
          endif
         endif
        endif
       endif
      endif

C    *********Work out initial location of animal given activity constraints********

c    check if it was in the burrow last hour and, if so, is it too cold to emerge and bask?
      if((daycount.eq.1).and.(ihour.eq.1))then
       continue
      else
       if(environ1((ihour+24*(daycount-1))-1,8).lt.
     &0)then
c      get the soil node and temperature for this hour at that depth
        do 233 i=1,NON
       if(environ1((ihour+24*(daycount-1))-1,8).eq.(zsoil(i)
     &*(-1.)))then
          depth=zsoil(i)
          Tc_old=tsoil(i)
        endif
233     continue
        if(tc_old.lt.temerge)then
         ta=tc_old
         newdep=depth
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
         DEPSEL(IHOUR) = newdep * (-1.0)
         goto 253
        else
        depth=depsel(ihour)
        endif    
       endif
      endif

c    check if aquatic stage of frog and, if so, make its environment water
      if((deb1.eq.1).and.(pond.eq.0).and.((frogbreed.eq.1).or.

     &    (frogbreed.eq.2)))then
       if(frogbreed.eq.1)then
        if(stage.le.1)then
         fltype=1
         tc=twater
         inwater=1
         DEPSEL(IHOUR) = 0
         if(stage.eq.1)then
          feeding=1
          if(gutfull.lt.gutfill)then
c     IF((PctDess.ge.minwater-1).and.(minwater.gt.0))then
c         forage=0
c     else
           forage=1
c     endif
          endif 
         else
          feeding=0
          forage=0
         endif
         goto 252
        endif
       endif
       if(frogbreed.eq.2)then
        if(stage.eq.1)then
         fltype=1
         tc=twater
         inwater=1
         feeding=1
         DEPSEL(IHOUR) = 0
         if(gutfull.lt.gutfill)then
          forage=1
         endif 
         goto 252
        endif
       endif
      endif

c    aquatic behaviour
      if((aquatic.eq.1).and.(pond.eq.0).and.(frogbreed.eq.4)

     &    .and.(ihour.lt.25))then
c     get current pond environment, initialize max/min temps to water temp at hour 1
       if(wetmod.eq.1)then
        pond_depth=wetlandDepths(hourcount)
        twater=wetlandTemps(hourcount)
        pondmax=wetlandTemps(hourcount-ihour+1)
        pondmin=wetlandTemps(hourcount-ihour+1)
       else
        pond_depth=pond_env(iyear,countday,ihour,2)
        twater=pond_env(iyear,countday,ihour,1)
        pondmax=pond_env(iyear,countday,1,1)
        pondmin=pond_env(iyear,countday,1,1)
       endif
c    aquatic animal, pond model has run so now do animal - first check for water in the pond (greater than 10% of max depth)
c     if(pond_depth.gt.(contref*10./2.))then
       if(pond_depth.gt.contref*10*0.1)then
        burrow='N'
        if(ectoinput(51).eq.1)then
         dayact = 'y'
        else
         dayact = 'n'
        endif
        if(ectoinput1(52).eq.1)then
         nocturn = 'y'
        else
         nocturn = 'n'
        endif
        if(ectoinput1(53).eq.1)then
         crepus='y'
        else
         crepus='n'
        endif
        if(ectoinput1(54).eq.1)then
         rainact='y'
        else
         rainact='n'
        endif
c    next get daily max/min of pond to check that it is suitable for the animal
        if(wetmod.eq.1)then
         do 5058 i=(hourcount-ihour+1),(hourcount-ihour+24)
          if(wetlandTemps(i).gt.pondmax)then
           pondmax=wetlandTemps(i)
          endif
          if(wetlandTemps(i).lt.pondmin)then
           pondmin=wetlandTemps(i)
          endif
5058     continue
        else
         do 5057 i=1,24
          if(pond_env(iyear,countday,i,1).gt.pondmax)then
           pondmax=pond_env(iyear,countday,i,1)
          endif
          if(pond_env(iyear,countday,i,1).lt.pondmin)then
           pondmin=pond_env(iyear,countday,i,1)
          endif
5057     continue
        endif
c    check if pond too hot - if it is, leave pond
        if(pondmax.gt.tmaxpr)then
         fltype=0
         feeding=0
         inwater=0
         burrow='Y'
         dayact='N'
         nocturn='N'
         crepus='N'
         rainact='N'
         continue
        endif
c    if pond isn't too hot, check if water temp is right for feeding    
        if((twater.gt.tminpr).and.(twater.lt.tmaxpr))then
c    if right conditions and gut is not yet full, then active and feeding in water, else bask
         if(gutfull.lt.gutfill)then
c        depsel(ihour)=0
c        fltype=1
c        tc=twater
c        inwater=1
c        forage=1
c        use this code for basking only
          depsel(ihour)=0
          fltype=0
          tc=twater
          inwater=1
          forage=1
          IF ((Dayact .EQ. 'Y') .OR. (Dayact .EQ. 'y')) THEN
C        DIURNAL  Check for sunlight
           IF (Qsolr .eq. 0.0000) THEN
            IF ((NOCTURN .eq. 'y') .or. (NOCTURN .eq. 'Y'))THEN
             IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
              if(rainfall.gt.rainthresh)then
               feeding=1
               goto 5021
              else
               feeding=0
               goto 5021          
              Endif    
             ELSE         
              feeding=1
              goto 5021
             ENDIF 
            else 
             feeding=0
             goto 5021
            endif
           else
C          Sunlight, maybe go topside; diurnal, crepuscular?
C          If crepuscular, check for direct sun; stay up if z(Ihour) = 90.0
            If (z(Ihour).eq. 90.0) then 
             If ((Crepus .eq. 'Y').or.(Crepus .eq. 'y'))then
              IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
               if(rainfall.gt.rainthresh)then
                feeding=1
                goto 5021
               else
                feeding=0
                goto 5021
              Endif          
              ELSE          
               feeding=1
               goto 5021
              endif
             else
c            No crepuscular
              feeding=0
              goto 5021
             endif
            else
C          Sun above horizon and diurnal
             IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
              if(rainfall.gt.rainthresh)then
               feeding=1
               goto 5021
              else
               feeding=0
               goto 5021
              Endif         
             else
              feeding=1
              goto 5021
             endif
            Endif
C          End zenith angle check
           Endif
C         End solar check
          Endif
c        End diurnal check
          If((Nocturn.eq.'Y').or.(Nocturn.eq.'y'))then
C         NOCTURNAL ?; up when no sunlight?; crepuscular?; diurnal
C         Check for no sunlight
           IF (Qsolrf .eq. 0.0000) THEN
C          Nighttime: Activity OK
            IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
             if(rainfall.gt.rainthresh)then
              feeding=1
              goto 5021
             else
              feeding=0
              goto 5021
             Endif          
            ELSE          
             feeding=1
             goto 5021
            ENDIF
            goto 253
           endif
C         Light, is sun over horizon? crepuscular?
           If ((Crepus .eq. 'Y').or.(Crepus .eq. 'y'))then
            If (z(ihour).eq. 90.0) then 
C          Crepuscular environment OK
             IF ((RAINACT .eq. 'y') .or. (RAINACT .eq.
     & 'Y'))THEN
              if(rainfall.gt.rainthresh)then
               feeding=1
               goto 5021
              else
               feeding=0
               goto 5021
              Endif          
             ELSE          
              feeding=1
              goto 5021
             ENDIF
             goto 253
            Endif
C          Not crepuscular, go to burrow environment, if burrow OK
            feeding=0
            goto 5021
           Endif
C         Not crepuscular, go to burrow environment, if burrow OK
           feeding=0
           goto 5021
          Endif
C        End of choices for nocturnal animal    
5021      continue
c        comment this goto out to make it always bask
c        goto 252
         else
          feeding=0
          continue
         endif
        else
c    if water temp is not right for feeding, stay in water
c         feeding=0
c        depsel(ihour)=0
c        fltype=1
c        tc=twater
c        inwater=1
c        forage=0
c       goto 252
c    if water temp is not right for feeding, try basking
         inwater=1
         feeding=0
         forage=0
         continue
        endif
       else
        fltype=0
        feeding=0
        inwater=0
        burrow='y'
        dayact='N'
        nocturn='N'
        crepus='N'
        rainact='N'
        continue
       endif
      endif

c    actpup=0
c    turn off activity if inactive pupal stage
c    if(deb1.eq.1)then
c     if((metab_mode.eq.1).or.(metab_mode.eq.3))then
c      if(stage.eq.stages-2)then
c       if(actpup.eq.0)then
c        dayact='N'
c        nocturn='N'
c        crepus='N'
c        rainact='N'
c       endif
c      endif
c     endif
c    endif

c    turn activity back on if it had an inactive pupal stage
c    if(deb1.eq.1)then
c     if((metab_mode.eq.1).or.(metab_mode.eq.3))then
c      if(stage.gt.stages-2)then
c       if(actpup.eq.0)then
c         if(ectoinput(51).eq.1)then
c       dayact = 'y'
c      else
c       dayact = 'n'
c      endif
c      if(ectoinput(52).eq.1)then
c       nocturn = 'y'
c      else
c       nocturn = 'n'
c      endif
c      if(ectoinput(53).eq.1)then
c       crepus='y'
c      else
c       crepus='n'
c      endif
c      if(ectoinput(54).eq.1)then
c       rainact='y'
c      else
c       rainact='n'
c      endif
c       endif
c      endif
c     endif
c    endif

      minwaterlow=minwater*.5
      if(dehydrated.eq.1)then
       minwater2=minwaterlow
      else
       minwater2=minwater
      endif


      if((deb1.eq.1).and.(pond.eq.0))then
       IF((PctDess.gt.minwater2).and.(minwater.gt.0))then
c     too dehydrated for activity
        dehydrated=1
        If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
         minnode=9
         shdburrow=0
         if(shdburrow.eq.1)then
          shade=maxshd
          Call Seldep (TSHSOI,ZSOIL,DEPTH)
         else
          shade=refshd
          Call Seldep (TSOIL,ZSOIL,DEPTH) 
         endif
         DEPSEL(IHOUR) = newdep * (-1.0)
         goto 253
        endif
       else
        dehydrated=0
       endif    

c    gutfill=0.75
       if(gutfull.lt.gutfill)then
c     IF((PctDess.ge.minwater-1).and.(minwater.gt.0))then
c         forage=0
c     else
           forage=1
c     endif
       endif 

       if((gutfull.ge.gutfill).and.(aquatic.eq.0))then
c     gut close to full - stay home
        If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
         tc_old=tc
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
         DEPSEL(IHOUR) = newdep * (-1.0)
c       check if too cold in retreat so the animal needs to bask
         if(TC.GE.tdigpr)then
          goto 253
         else
          tc=tc_old
          call aboveground
          call radin
          depsel(ihour)=0
          newdep=0
          depth=0
         endif
        endif
       endif
      endif
      
      IF ((Dayact .EQ. 'Y') .OR. (Dayact .EQ. 'y')) THEN
C     DIURNAL  Check for sunlight
       IF (Qsolr .eq. 0.0000) THEN
        IF ((NOCTURN .eq. 'y') .or. (NOCTURN .eq. 'Y'))THEN
         IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
          if(rainfall.gt.rainthresh)then
           Call aboveground
           goto 253
          else
c        not right rainfall, can't be active
           IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C          Put animal in burrow, it's nighttime
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
            DEPSEL(IHOUR) = newdep * (-1.0)
           else
c          No burrow allowed
            Call aboveground
           Endif
           goto 253          
          Endif    
         ELSE         
          Call aboveground
         ENDIF 
         goto 253
        else 
         IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C        Put animal in burrow, it's nighttime
          Call Seldep (TSOIL,ZSOIL,DEPTH)
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
          DEPSEL(IHOUR) = newdep * (-1.0)
         else
c        No burrow allowed
          Call aboveground
         Endif
         goto 253
        Endif
       else
C      Sunlight, maybe go topside; diurnal, crepuscular?
C      If crepuscular, check for direct sun; stay up if z(Ihour) = 90.0
        If (z(Ihour).eq. 90.0) then 
         If ((Crepus .eq. 'Y').or.(Crepus .eq. 'y'))then
          IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
           if(rainfall.gt.rainthresh)then
            Call aboveground
            goto 253
           else
            IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C          Put animal in burrow, it's nighttime
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
             DEPSEL(IHOUR) = newdep * (-1.0)
            else
c           No burrow allowed
             Call aboveground
            Endif
            goto 253
          Endif          
          ELSE          
           Call aboveground
          ENDIF
          goto 253
         else
c        No crepuscular
          if((BURROW .eq. 'Y') .or. (BURROW .eq. 'y'))Then
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
           DEPSEL(IHOUR) = newdep * (-1.0)
          else
c         No burrow allowed
           Call aboveground
          endif
          goto 253
         Endif
        else
C      Sun above horizon and diurnal
         IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
          if(rainfall.gt.rainthresh)then
           Call aboveground
           goto 253
          else
           IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C          Put animal in burrow, it's nighttime
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
            DEPSEL(IHOUR) = newdep * (-1.0)
           else
c          No burrow allowed
            Call aboveground
           Endif
           goto 253
          Endif         
         else
          call aboveground
         endif
         goto 253
        Endif
C      End zenith angle check
       Endif
C     End solar check
      Endif
c    End diurnal check

      If((Nocturn.eq.'Y').or.(Nocturn.eq.'y'))then
C     NOCTURNAL ?; up when no sunlight?; crepuscular?; diurnal
C     Check for no sunlight
       IF (Qsolrf .eq. 0.0000) THEN
C      Nighttime: Activity OK
        IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
         if(rainfall.gt.rainthresh)then
          Call aboveground
          goto 253 
         else
          IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C        Put animal in burrow, it's nighttime
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
           DEPSEL(IHOUR) = newdep * (-1.0)
          else
c         No burrow allowed
           Call aboveground
          Endif
         Endif          
        ELSE          
         Call aboveground
        ENDIF
        goto 253
       endif
C     Light, is sun over horizon? crepuscular?
       If ((Crepus .eq. 'Y').or.(Crepus .eq. 'y'))then
        If (z(ihour).eq. 90.0) then 
C      Crepuscular environment OK
         IF ((RAINACT .eq. 'y') .or. (RAINACT .eq. 'Y'))THEN
          if(rainfall.gt.rainthresh)then
           Call aboveground
           goto 253
          else
           IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
C          Put animal in burrow, it's nighttime
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
            DEPSEL(IHOUR) = newdep * (-1.0)
           else
c          No burrow allowed
            Call aboveground
           Endif
           goto 253
          Endif          
         ELSE          
          Call aboveground
         ENDIF
         goto 253
        Endif
C      Not crepuscular, go to burrow environment, if burrow OK
        IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
         DEPSEL(IHOUR) = newdep * (-1.0)
        else
C       No burrow allowed.  You are 'out there'
         Call aboveground
        Endif
        goto 253
       Endif
C     Not crepuscular, go to burrow environment, if burrow OK
       IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
        DEPSEL(IHOUR) = newdep * (-1.0)
       else
C      No burrow allowed.  You are 'out there'
        Call aboveground
       Endif
       goto 253
      Endif
C    End of choices for nocturnal animal    

      If(((Nocturn.eq.'N').or.(Nocturn.eq.'n')).and.((Dayact .EQ. 'N')
     &    .OR. (Dayact .EQ. 'n')).and.((Crepus .eq. 'N').or.
     &(Crepus .eq. 'n')))then
        IF ((BURROW .EQ. 'Y') .OR. (BURROW .EQ. 'y'))THEN
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
         DEPSEL(IHOUR) = newdep * (-1.0)
        else
C      No burrow allowed.  You are 'out there'
         Call aboveground
       Endif
       goto 253
      Endif
253   Continue
C      ******end check of initial location********

C    Incrementing time within the day loop
      T = TIME(Ihour)
      IHOUR = int((T/60.) + 1)

      If((Transt.eq.'y').or.(Transt.eq.'Y'))then
       goto 7112
      Endif     
      
C     INFRARED CALCULATIONS?
C     CHECKING TO SEE IF WATER ENVIRONMENT
      IF (FLTYPE .EQ. 0.0) THEN 
C      AIR ENVIRONMENT (Temperatures in C)
c      TSKY = TskyC(Ihour) or = Soil temperature set in Seldep
       TR = (TSUBST + TSKY)/2. 
       CALL RADIN 
      ELSE
C      WATER ENVIRONMENT
       FATOBJ = 0.00
       FATOSK = 0.00
       FATOSB = 0.00
       QIRIN = 0.00
      ENDIF

      enberr=enberr2*AMASS
c    user control of error tolerance now overrides local control 
      TESTX = Enberr
      reftol =testx
      flytest=0
      flight=0
      flytime=0

225   continue
      
      IF(depsel(ihour).LE.0.00001)THEN
       if(wingmod.eq.2)then
        wingcalc=1
C       SETTING UP FOR FINDING BRACKETING VALUES OF X, Tcore, BETWEN WHICH Y=0
       IF(NTRY.EQ.1)THEN
         x1 = 0.
         x2 = 50.
        ELSE
         X1 = TC- 5.
         X2 = TC + 5.
        ENDIF
C      GUESSING FOR CORE TEMPERATURE
        X = TA
        CALL ZBRACwing(FUNwing,X1,X2,SUCCES)
C       INVOKING THE ENERGY BALANCE EQUATION VIA ZBRENT AND FUN 
c      user control of error tolerance now overrides local control 
        TESTX = Enberr
        reftol =testx
        X = ZBRENTwing(FUNwing,X1,X2,Testx)
C       OUT COMES THE GUESSED VARIABLE VALUE (X) THAT SATISFIES  
C       THE ENERGY BALANCE EQUATION   
C       DEPENDENT VARIABLE GUESSED FOR EITHER CORE TEMP. OR METABOLISM
C      GUESS FOR CORE TEMPERATURE
        TWING = x
        wingcalc=0   
       endif
      endif
c    end of wing temp calc
      wingtemp=twing
      
      call allom

c    call RADIN again now in case wing model was run
      IF (FLTYPE .EQ. 0.0) THEN 
C      AIR ENVIRONMENT (Temperatures in C)
c      TSKY = TskyC(Ihour) or = Soil temperature set in Seldep
       TR = (TSUBST + TSKY)/2. 
       CALL RADIN 
      ELSE
C      WATER ENVIRONMENT
       FATOBJ = 0.00
       FATOSK = 0.00
       FATOSB = 0.00
       QIRIN = 0.00
      ENDIF


C     SETTING UP FOR FINDING BRACKETING VALUES OF X, Tcore, BETWEEN WHICH Y=0
      IF(NTRY.EQ.1)THEN
       x1 = 0.
       x2 = 50.
       if((fosorial.eq.'Y').or.(fosorial.eq.'y'))then
c      Thermoregulate in burrow or not?  Is it too cold to do that?
        if(TSHSOI(IHOUR).LT.TBASK)THEN
         DEPSEL(IHOUR) = 200.*(-1.0)
         NEWDEP = 200.
         CALL BELOWGROUND
         CALL RADIN 
         X1 = TA - 5.
         X2 = TA + 5.
        ENDIF
       ENDIF
      ELSE
       X1 = TC- 5.
       X2 = TC + 5.
      ENDIF
C    GUESSING FOR CORE TEMPERATURE
      IF(depsel(ihour).LT.0.)THEN
       call belowground
      else
       call aboveground
      endif

      X = TA
      
      if(amass.gt.0.000001)then
      CALL ZBRAC(FUN,X1,X2,SUCCES)

C     INVOKING THE ENERGY BALANCE EQUATION VIA ZBRENT AND FUN 
c    user control of error tolerance now overrides local control 
      TESTX = Enberr
      reftol =testx

      X = ZBRENT(FUN,X1,X2,Testx)
      endif
      if(x.gt.80)then
      x=tsoil(1)
      endif
C     OUT COMES THE GUESSED VARIABLE VALUE (X) THAT SATISFIES  
C       THE ENERGY BALANCE EQUATION   
C     DEPENDENT VARIABLE GUESSED FOR EITHER CORE TEMP. OR METABOLISM
C    GUESS FOR CORE TEMPERATURE
      TC = X   
      
      Y(NEQ) = X
c    Initial storage for this hour's core temperature

      if((aquatic.eq.1).and.(pond.eq.0).and.(frogbreed.eq.4))then
       if(pond_depth.gt.contref*10*0.1)then
        if(twater.gt.tc)then
         tc=twater
         inwater=1
         goto 252
        endif
       endif
      endif


      if((tc.gt.tmaxpr).and.(shade.ge.maxshd))then

      continue

      endif


      if(depsel(ihour).lt.0)then
c    has decided to burrow, so don't bother thermoregulating if within CTmin and VTmax to save cpu time
       if(depsel(ihour).eq.(-1*zsoil(non)))then
C      NO THERMOREGULATION NEEDED because nowhere to go!
        ERRTES = ABS(ENB)
        IF((ERRTES .LE. TESTX).OR.(QUIT.GT. 0.0))THEN
C       A SOLUTION! OR A BUST: GET OUT AND GO TO NEXT HOUR
         GO TO 252
        endif
       endif
       IF((TC .GE. CTMIN).AND.(TC .LE. TMAXPR))THEN
C      NO THERMOREGULATION NEEDED, JUST ADJUST GAINS OR LOSSES A BIT
        ERRTES = ABS(ENB)
        IF((ERRTES .LE. TESTX).OR.(QUIT.GT. 0.0))THEN
C        A SOLUTION! OR A BUST: GET OUT AND GO TO NEXT HOUR
          GO TO 252
        endif
       endif
      endif

      if(((pond.eq.0).and.(flyer.eq.1).and.(deb1.eq.0)).or.

     &    ((pond.eq.0).and.(flyer.eq.1).and
     &    .(deb1.eq.1).and.(stage.gt.stages-2)))then
       if(flytest.eq.1)then
        IF((TC .GE. TMINPR).AND.(TC .LE. TMAXPR).and.(z(ihour).lt.90)
     &      )THEN
         flight=1
c       first formula is females, second is males
         flytime=-0.0264*TC**2+1.4118*TC-4.456
         flytime=-0.04195*TC**2+2.028902*TC-5.85991
         if(flytime.lt.0)then
          flytime=0
         endif
         GO TO 252
        else
         flytest=2
         flytime=0
         flight=0
         phi=phimin
         call aboveground

         goto 225

        endif
       endif
      endif       

C    SKIP OVER CHECKS IF CORE TEMPERATURE OK & SOLUTION WITHIN TOLERANCE
      IF((TC .GE. TMINPR).AND.(TC .LE. TPREF))THEN
       if((wingmod.gt.0).and.(tc.lt.tpref).and.(shade.eq.0))then
        call thermoreg(quit)
        IF((QUIT.EQ. 1.).or.(phi.eq.phimax))THEN
      if(((pond.eq.0).and.(flyer.eq.1).and.(deb1.eq.0).and.(tdigpr.lt.
     &tpref).and.(z(ihour).lt.90)).or.((pond.eq.0).and.(flyer.eq.1).and
     &.(deb1.eq.1).and.(stage.gt.stages-2).and.(tdigpr.lt.tpref)

     &.and.(z(ihour).lt.90)))then
          if(flytest.eq.0)then
c         hasn't yet tried flying
           flytest=1
           phi=180
           call aboveground

           goto 225
          endif
         endif
         GO TO 252
        ENDIF
         go to 225
        endif
      if(((pond.eq.0).and.(flyer.eq.1).and.(deb1.eq.0).and.
     &(z(ihour).lt.90)).or.((pond.eq.0).and.(flyer.eq.1).and.(deb1.eq.1)
     &.and.(stage.gt.stages-2).and.(z(ihour).lt.90).and.depsel(ihour)
     &.ge.0))then
        if(flytest.eq.0)then
c     hasn't yet tried flying
         flytest=1
         phi=180
         call aboveground

         goto 225
        endif
       endif
C     NO THERMOREGULATION NEEDED, JUST ADJUST GAINS OR LOSSES A BIT
       ERRTES = ABS(ENB)
       IF((ERRTES .LE. TESTX).OR.(QUIT.GT. 0.0))THEN
C      A SOLUTION! OR A BUST: GET OUT AND GO TO NEXT HOUR
        GO TO 252
       ELSE
C      TCORE OK, NOW NEED SMALL ENVIRONMENT ADJUSTMENTS TO GET WITHIN TOLERANCE
C      DROP DOWN TO THE TESTS BELOW
        IF(TC.GT.TMINPR)THEN
         IF(ABSAN.GT.ABSMIN)THEN
C        USE MIN VALUE
          ABSAN = ABSMIN

          GO TO 225
         ENDIF
        ENDIF
        IF(ENB.LT.0.000)THEN
C       REDUCE HEAT LOSSES BY WIND A LITTLE BY SEEKING SHELTER
C       CHANGE POSTURE ONCE/HR TO MINIMIZE HEAT LOSS - ASSUME A SPHERICAL GEOMETRY IF A FROG, A CYLINDER IF A LIZARD
         IF(IHOUR.NE.HRCALL)THEN
c        Posture changes are very potent.  Consider grading this toward % cylinder instead of one completely.
c        It provides good solution accuracy for very little cost.
          IF(LOMETRY.EQ.3)THEN
C         LIZARD -> CYLINDER GEOMETRY. 
           LOMETRY = 1
          ENDIF
          IF(LOMETRY.EQ.4)THEN
C         FROG -> ellipse
          LOMETRY = 2
          ENDIF
          HRCALL = IHOUR
          CALL ALLOM
         ELSE
C        REDUCE EFFECTIVE WET SURFACE AREA
          AEFF = AEFF - AEFF * 0.01
          IF(AEFF.LE. 0.1)THEN
           AEFF = 0.1
C         SET EXIT CODE ON NEXT ITERATION
           QUIT = 1.
          ENDIF
         ENDIF

c    write(*,*) ihour," ",wetmass(ihour)," ",ANDENS_deb
         GO TO 225
        ELSE
C       ENERGY BALANCE ERROR IS POSITIVE. Increase TPREF towards TMAXPR
         IF(TPREF .le. TMAXPR)THEN
          TPREF=TPREF+1.
          If(TPREF .gt. TMAXPR)THEN
           TPREF = TMAXPR
          ELSE
           GO TO 225
          ENDIF
         ENDIF
C       ENERGY BALANCE ERROR IS POSITIVE. REDUCE GAINS BY ADDING SHADE
         IF(SHADE.LT. maxshd)THEN
          CALL SHADEADJUST
          GO TO 225
         ELSE
          SHADE=maxshd
C        CALL THERMOREG TO DO BEHAVIORAL OR PHYSIOLOGICAL ADJUSTMENTS
          CALL THERMOREG(QUIT)
          IF(QUIT.GT. 0.0)THEN
C         ALL BEHAVIORAL AND PHYSIOLOGICAL OPTIONS EXHAUSTED. QUIT WITH THE CURRENT LEVEL OF ERROR.
           GO TO 252
          ENDIF
         ENDIF
         GO TO 225
        ENDIF
       ENDIF
      ENDIF

C    Alive & maybe active?  Fossorial changes each month.
      if((fosorial.eq.'Y').or.(fosorial.eq.'y'))then
c     Thermoregulate in burrow or not?  Is it too cold to do that?
       if(TSHSOI(IHOUR).LT.TBASK)THEN
        DEPSEL(IHOUR) = 200.*(-1.0)
        NEWDEP = 200.
        Acthr(Ihour) = 0.0
        CALL BELOWGROUND
        CALL RADIN
        X1 = TANNUL - 2.
        X2 = TANNUL +2.
        X = ZBRENT(FUN,X1,X2,Testx)
c      skip the thermoregulatory checks
        go to 252
       ELSE
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
        DEPSEL(IHOUR) = newdep *(-1.0)
        Acthr(Ihour) = 0.0
       CALL BELOWGROUND
        CALL RADIN
        X1 = TSHSOI(IHOUR) - 5.
        X2 = TSHSOI(IHOUR) + 5.
        X = ZBRENT(FUN,X1,X2,Testx)
c      skip the thermoregulatory checks
        go to 252
       ENDIF
      ELSE
C      NOT FOSSORIAL RIGHT NOW, BUT IT MIGHT BE TOO COLD OR TOO HOT. SEE BELOW WHERE THESE ARE CONSIDERED
      endif

c    NOTE that if enb is negative, the losses are greatest.  Cut wind speed to reduce losses.
c    if enb is positive, gains are greatest, reduce those (shade, more wind)
c    this should help to guide the behavioral choices to get better convergence.
c    Need an algorithm to reduce wind if active and high wind.
      IF((TC.LT.TBASK).OR.(TC.GT.TPREF))THEN
       CALL THERMOREG(QUIT)
       IF(QUIT.EQ. 1.)THEN
        GO TO 252
       ENDIF
       if(ntry.lt.300)then
c      force another iterative try
        ntry = ntry + 1
        if(ihour.lt.25)then
         ntry1(ihour)=1
        endif
        GO TO 225
       else
c      no chance the environment is too harsh: bail
        ntry = 1
        go to 252
       endif
      ENDIF

c    IF GET THIS FAR, SOLUTION ISN'T SUITABLE YET, BUT BODY TEMPERATURE 
C    ABOVE MINIMUM FOR ACTIVITY AND BELOW MAX PREFERRED
      if(ERRTES.GT.testx)then
       if(Tc.GE.Tminpr)then
c      IF(TC.LE.TPREF)THEN
C      IN A GOOD BODY TEMPERATURE RANGE...WHAT TO DO??? 
        CALL THERMOREG(QUIT)
        IF(QUIT.EQ. 1.)THEN
         GO TO 252
        ENDIF
        go to 225
       endif
      endif

      if(ERRTES.le.enberr)then
c     a suitable solution.  The core temperature may be outside desired bounds, but if in burrow, quit.
       if(depsel(ihour).lt.0.00000)then
c      Animal below ground and inactive
        Acthr(Ihour) = 0.0
        go to 252
       endif  
c      Any behavioral options?
        if(((Burrow.eq.'N').or.(Burrow.eq.'n')).and.
     &    ((Climb.eq.'N').or.(Climb.eq.'n')).and.
     &     ((CkGrShad.eq.'N').or.(CkGrShad.eq.'n').or.
     & (WINGMOD.eq.0)))then
c       no thermoregulatory options -> quit
         go to 252
        endif
      endif

      if(Tc .ge. TPREF)then
c     *********** Set a different physical environment, 
c     including depth, but not activity, which is determined below
       if(((CkGrShad.eq.'N').or.(CkGrShad.eq.'n')).or.
     &  (shade.eq.maxshd))then
        if((climb.eq.'N').or.(climb.eq.'n'))then
         if((burrow.eq.'N').or.(burrow.eq.'n'))then
c        no more options, have to go with current Tc
          if(ntry.lt.300)then
c         force another iterative try
           ntry = ntry + 1
           if(ihour.lt.25)then
            ntry1(ihour)=1
           endif
           go to 225
          else
c        no chance the environment is too harsh: bail
           ntry = 1
          endif
         else
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
          DEPSEL(IHOUR) = newdep * (-1.0)
          go to 252
         endif
        else
         IF(DEPSEL(IHOUR).ne.200.0)THEN
C        Try climbing
          if ((Tref(Ihour).ge.Tminpr).and.(Tref(Ihour).le.Tpref))THEN
C         It's OK up high 
c         Climb to 200 cm height
           DEPSEL(IHOUR) = 200.0
c         Reference height air temperature is the same in sun or shade.
           Ta = Tref(Ihour)
           Vel = Vref(ihour)
           go to 252
c         No assumption of increase in shade due to climbing.  May still be in the sun.
          endif
         ENDIF
        endif
       endif
c     Depth is needed since Thermoreg may call Seldep, which calls Belowground, 
c     which must have depth to assign temperature
       DEPSEL(IHOUR) = newdep * (-1.0)
       Call Thermoreg(QUIT)
       IF(QUIT.EQ. 1.)THEN
        GO TO 252
       ENDIF
c     Now rerun the numerical guessing with the new environment
       go to 225
      Endif

      If ((Tc .lt. TBASK) .or. (Tc .gt. TPREF)) then
C     Outside preferred temperature range. 
c     *********** Set a different physical environment, 
c     including depth, but not activity, which is determined below
       if(((CkGrShad.eq.'N').or.(CkGrShad.eq.'n')).or.
     &     (shade.eq.maxshd))then
        if((climb.eq.'N').or.(climb.eq.'n'))then
         if((burrow.eq.'N').or.(burrow.eq.'n'))then
c        no more options, have to go with current Tc
          go to 252
         else
c        Burrow OK, if gone underground and enb within tolerance,
c        no other options.  Quit
          if((enb.le.enberr).and.(depsel(Ihour).lt.0.000000))then
           DEPSEL(IHOUR) = newdep * (-1.0)
           go to 252
          endif
        if(shdburrow.eq.1)then
      shade=maxshd
         Call Seldep (TSHSOI,ZSOIL,DEPTH)
        else
      shade=refshd
         Call Seldep (TSOIL,ZSOIL,DEPTH) 
        endif
          DEPSEL(IHOUR) = newdep * (-1.0)
          go to 252
         endif
        else
         IF(DEPSEL(IHOUR).ne.200.0)THEN
C        Try climbing
          if ((Tref(Ihour).ge.Tminpr).and.(Tref(Ihour).le.Tpref))THEN
C         It's OK up high 
c         Climb to 200 cm height
           DEPSEL(IHOUR) = 200.0
c         Reference height air temperature is the same in sun or shade.
           Ta = Tref(Ihour)
           Vel = Vref(ihour)
           go to 252
c         No assumption of increase in shade due to climbing.  May still be in the sun.
          endif
         endif
        endif
       endif
       Call Thermoreg(QUIT)
       DEPSEL(IHOUR) = newdep * (-1.0)
c     Now rerun the numerical guessing with the new environment
       IF(QUIT.EQ. 1.)THEN
        GO TO 252
       ENDIF
       go to 225
      Endif

      if((wingmod.gt.0).and.(tc.lt.tpref))then
      call thermoreg(quit)
       IF((QUIT.EQ. 1.).or.(phi.eq.phimax))THEN
        GO TO 252
       ENDIF
       go to 225
      endif

252   Continue

      if(((Depsel(ihour).ge. 0).and.(deb1.eq.0).and.(pond.eq.0)).or.
     &    ((Depsel(ihour).ge.0).and.(deb1.eq.1).and.(stage.gt.0).and.

     &(pond.eq.0)))then
       if((Tc .ge. tbask) .and. (Tc .le. tmaxpr))then
        if(((rainfall.le.rainthresh).and.((RAINACT .eq. 'n') .or. 
     &(RAINACT .eq. 'N').or.(inwater.eq.1))).or.((rainfall.gt.rainthresh
     &).and.((RAINACT.eq. 'Y') .or.(RAINACT .eq. 'y'))).or.((RAINACT 
     &.eq. 'N') .or.(RAINACT .eq. 'n')))then
         if(deb1.eq.1)then
          if((aquatic.eq.1).and.(frogbreed.eq.4))then
           if((tc.ge.tminpr).and.(forage.eq.1).and.(inwater.eq.1))then
            Acthr(Ihour) = 2.0
           else
            Acthr(Ihour) = 1.0
           endif
          else
           if((tc.ge.tminpr).and.(forage.eq.1))then
            Acthr(Ihour) = 2.0
           else
            Acthr(Ihour) = 1.0
           endif
          endif
         else
          if((aquatic.eq.1).and.(frogbreed.eq.4))then
           if((tc.ge.tminpr).and.(forage.eq.1).and.(inwater.eq.1))then
            Acthr(Ihour) = 2.0
           else
            Acthr(Ihour) = 1.0
           endif
          else
           if(tc.ge.tminpr)then
            Acthr(Ihour) = 2.0
           else
            Acthr(Ihour) = 1.0
           endif
          endif
         endif

         if((dead.eq.0).and.(deadead.eq.0))then
         annualact=annualact+1
         if(iyear.eq.1)then
         act1=act1+1
          for1=for1+acthr(ihour)-1
         endif
         if(iyear.eq.2)then
          act2=act2+1
          for2=for2+acthr(ihour)-1
         endif
         if(iyear.eq.3)then
          act3=act3+1
          for3=for3+acthr(ihour)-1
         endif
         if(iyear.eq.4)then
          act4=act4+1
          for4=for4+acthr(ihour)-1
         endif
         if(iyear.eq.5)then
          act5=act5+1
          for5=for5+acthr(ihour)-1
         endif
         if(iyear.eq.6)then
          act6=act6+1
          for6=for6+acthr(ihour)-1
         endif
         if(iyear.eq.7)then
          act7=act7+1
          for7=for7+acthr(ihour)-1
         endif
         if(iyear.eq.8)then
          act8=act8+1
          for8=for8+acthr(ihour)-1
         endif
         if(iyear.eq.9)then
          act9=act9+1
          for9=for9+acthr(ihour)-1
         endif
         if(iyear.eq.10)then
          act10=act10+1
          for10=for10+acthr(ihour)-1
         endif
         if(iyear.eq.11)then
         act11=act11+1
          for11=for11+acthr(ihour)-1
         endif
         if(iyear.eq.12)then
          act12=act12+1
          for12=for12+acthr(ihour)-1
         endif
         if(iyear.eq.13)then
          act13=act13+1
          for13=for13+acthr(ihour)-1
         endif
         if(iyear.eq.14)then
          act14=act14+1
          for14=for14+acthr(ihour)-1
         endif
         if(iyear.eq.15)then
          act15=act15+1
          for15=for15+acthr(ihour)-1
         endif
         if(iyear.eq.16)then
          act16=act16+1
          for16=for16+acthr(ihour)-1
         endif
         if(iyear.eq.17)then
          act17=act17+1
          for17=for17+acthr(ihour)-1
         endif
         if(iyear.eq.18)then
          act18=act18+1
          for18=for18+acthr(ihour)-1
         endif
         if(iyear.eq.19)then
          act19=act19+1
          for19=for19+acthr(ihour)-1
         endif
         if(iyear.eq.20)then
          act20=act20+1
          for20=for20+acthr(ihour)-1
         endif

         endif
         if((dayact .eq. 'n') .or. (dayact .eq. 'N'))then
          if(z(ihour).lt. 90)then
           Acthr(Ihour) = 0.0
          endif
         endif
         if((nocturn .eq. 'n') .or. (nocturn .eq. 'N'))then
          if(qsolr .eq. 0.000)then
           if(z(ihour) .eq. 90)then
            Acthr(Ihour) = 0.0
           endif
          endif
         endif      
         if((crepus .eq. 'n') .or. (crepus .eq. 'N'))then
          if(qsolr .gt. 0.000)then
           if(z(ihour) .eq. 90)then
            Acthr(Ihour) = 0.0
           endif
          endif
         endif
         else
          Acthr(Ihour) = 0.0
         endif
        else
         Acthr(Ihour) = 0.0
        endif
       else
        Acthr(Ihour) = 0.0
      endif


c     for WST - get time within narrower thermal window
      if((Depsel(ihour).ge. 0).and.(inwater.eq.1))then
       if((Tc .ge. tbask) .and. (Tc .le. tmaxpr))then
        if(((rainfall.le.rainthresh).and.((RAINACT .eq. 'n') .or. 
     &(RAINACT .eq. 'N'))).or.((rainfall.gt.rainthresh).and.((RAINACT 
     &.eq. 'Y') .or.(RAINACT .eq. 'y'))).or.((RAINACT 
     &.eq. 'n') .or.(RAINACT .eq. 'n')))then

         if(iyear.eq.1)then
         degday1=degday1+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.2)then
          degday2=degday2+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.3)then
          degday3=degday3+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.4)then
          degday4=degday4+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.5)then
          degday5=degday5+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.6)then
          degday6=degday6+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.7)then
          degday7=degday7+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.8)then
          degday8=degday8+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.9)then
          degday9=degday9+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.10)then
          degday10=degday10+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.11)then
         degday11=degday11+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.12)then
          degday12=degday12+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.13)then
          degday13=degday13+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.14)then
          degday14=degday14+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.15)then
          degday15=degday15+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.16)then
          degday16=degday16+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.17)then
          degday17=degday17+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.18)then
          degday18=degday18+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.19)then
          degday19=degday19+max(0.,(Tc-tbask)/24.)
         endif
         if(iyear.eq.20)then
          degday20=degday20+max(0.,(Tc-tbask)/24.)
         endif
        endif
       endif
      endif

      Tcores(Ihour) = Tc

7112  continue

      if(ihour.lt.25)then
      Tbs((daycount-1)*24+ihour) = Tc
      endif
      
c    have TC, now call DEB model to get next hour's V, E, mass and repro (but don't call if it is hour 25, to avoid having an extra hour)
      if(live.eq.1)then
      if(deb1.eq.1)then

      if(ihour.lt.25)then
       if(metab_mode.gt.0)then

        call DEB_INSECT(ihour)
       else
        call DEB(ihour)
       endif
       if(PTUREA.eq.0)then
        H2O_URINE=0
        URINEFLUX=NWASTE(ihour)
       else
        H2O_URINE=NWASTE(ihour)/PTUREA-NWASTE(ihour)
        URINEFLUX=NWASTE(ihour)/PTUREA
       endif
       if(grassgrowth(daycount).eq.0)then
        FoodWaterCur=0.
       else
        FoodWaterCur=FoodWater
       endif
       if(FoodWaterCur.eq.0)then
        H2O_FREE=0.
        WETFOODFLUX=dryfood(ihour)
       else
        H2O_FREE=dryfood(ihour)/(1-FoodWaterCur)-dryfood(ihour)
        WETFOODFLUX=dryfood(ihour)/(1-FoodWaterCur)
       endif
       if(PFEWAT.eq.0)then
        H2O_FAECES = 0
        WETFAECESFLUX=FAECES(ihour)
       else
        H2O_FAECES=FAECES(ihour)/(1-PFEWAT)-FAECES(ihour)
        WETFAECESFLUX=FAECES(ihour)/(1-PFEWAT)
       endif
       if(E_H(ihour).le.E_Hb*1.01)then
        H2O_Bal=0
        H2O_BalPast=0
        H2O_Bal_hr=0
        PctDess=0
        if(deb1.eq.1)then
         WEVAP=0
         WCUT=0
         WRESP=0
        endif
       else
        if((rainfall.ge.raindrink).and.(Tc .ge. ctmin))then

         drunk=abs(H2O_Bal)
         H2O_Bal=0
        else
        H2O_Bal=H2O_FREE+GH2OMET(ihour)-H2O_URINE-H2O_FAECES-
     &WEVAP*3600+H2O_BalPast
        endif
        if(H2O_bal.lt.0)then
        PctDess=-1*H2O_Bal/potfreemass*100

        if(PctDess.gt.dessdeath)then
           if((stage.gt.deathstage).and.(v(ihour).ne.0))then
            causedeath=3.
            deathstage=stage
            if(reset.eq.0)then
             surviv(ihour)=0.49
             longev=(daycount+ihour/24.)/365.
             nyear=iyear
             census=countday
            endif
           endif
         if(reset.gt.0)then
          dead=1
         else
          dead=1
          deadead=1
          surv(iyear)=surviv(ihour)
        endif
        endif
        else
        PctDess=0
        endif
c      if(PctDess.gt.minwater)then
c       funct=0
c      endif
        if(PctDess.gt.Max_PctDess)then
         Max_PctDess=PctDess
        endif
        if(H2O_bal.gt.0)then
         H2O_bal=0.
        endif
        H2O_Bal_hr=H2O_FREE+GH2OMET(ihour)-H2O_URINE-H2O_FAECES-WEVAP*
     &  3600
       endif
       H2O_BalPast=H2O_Bal

c     stop the simulation in the year that the animal dies
c     if(hs(countday+(iyear-1)*365,ihour).ge.1)then
c     nyear=iyear
c     endif
c    end of call to deb
      endif

      else
      O2FLUX = 10.**(MR_3*TC)*MR_1*(AMASS*1000)**MR_2
      endif
      endif


      goodsoil = 0
      Q10=2

      SHD(IHOUR)=SHADE


      If((Transt.eq.'y').or.(Transt.eq.'Y'))then
C    Storing in array for each hour (Time Value) for 8 key env. variables set by 
c    animal behavior FOR USE IN TRANSIENT. THE time each hour is in a data statement
C    in DSUB.

c    Full sun
      Enary1(Ihour)= Qsol(Ihour)
      Enary2(Ihour)=Z(Ihour)
      Enary3(Ihour)=Taloc(Ihour)
      Enary4(Ihour)=Vref(Ihour)
      Enary5(Ihour)=RH(Ihour)
      Enary6(Ihour)=TskyC(Ihour)

      if((aquatic.eq.1).and.(pond.eq.1))then

c    had this at Tshsoi(4) for westerns swamp tortoise
       Enary7(Ihour) = Tshsoi(2)
      else
       Enary7(Ihour)=Tsoil(1)
      endif
      if((aquatic.eq.1).and.(pond.eq.1))then

c    had this at 40. for westerns swamp tortoise
       Enary8(Ihour)= 0.0
      else
       Enary8(Ihour)= 0.0
      endif

c    Full shade
      Enary9(Ihour)= Qsol(Ihour)
      Enary10(Ihour)=Z(Ihour)
      Enary11(Ihour)=Tshlow(Ihour)
      Enary12(Ihour)=Vref(Ihour)
      Enary13(Ihour)=RH(Ihour)
      Enary14(Ihour)=Tshski(Ihour)
      Enary15(Ihour)=Tshsoi(1)
      Enary16(Ihour)= 100.0

c    Full sun soil 30 cm
      Enary17(Ihour)= 0.
      Enary18(Ihour)= 90.
      Enary19(Ihour)=Tsoil(5)
      Enary20(Ihour)= 0.01
      Enary21(Ihour)= 99
      Enary22(Ihour)=Tsoil(5)
      Enary23(Ihour)=Tsoil(5)
      Enary24(Ihour)= 0.0

c    Shaded soil 30 cm
      Enary25(Ihour)= 0.
      Enary26(Ihour)= 90.
      Enary27(Ihour)=Tshsoi(5)
      Enary28(Ihour)=0.01
      Enary29(Ihour)=99
      Enary30(Ihour)=Tshsoi(5)
      Enary31(Ihour)=Tshsoi(5)
      Enary32(Ihour)= 100.0

c    Cave/Deep underground
      Enary33(Ihour)= 0.
      Enary34(Ihour)= 90.
      Enary35(Ihour)=Tsoil(10)
      Enary36(Ihour)=0.01
      Enary37(Ihour)=99
      Enary38(Ihour)=Tsoil(10)
      Enary39(Ihour)=Tsoil(10)
      Enary40(Ihour)= 100.0

c    Final array of environmental conditions for Dsub, to be written later
      Enary41(Ihour)= Qsol(Ihour)
      Enary42(Ihour)=Z(Ihour)
      Enary43(Ihour)=Taloc(Ihour)
      Enary44(Ihour)=Vref(Ihour)
      Enary45(Ihour)=RH(Ihour)
      Enary46(Ihour)=TskyC(Ihour)
      Enary47(Ihour)=Tsoil(1)
      Enary48(Ihour)= 0.0
      
      Endif



c    Almost end of hours loop

      if((hourop .eq. 'Y').or. (hourop .eq. 'y'))then
c       WRITE(I2,115)Ihour,Tc,QSOLAR,QIRIN,QMETAB,Qsevap,
c     *  QIROUT,QCONV,QCOND,Wevap,shade,Enb
       IF ((Transt.eq.'N').or.(Transt.eq.'n')) THEN
        if(Ihour.lt.25)then
         hct=ihour+24*(daycount-1)
         enbal1(hct,1)=julday
         enbal1(hct,2)=iyear
         enbal1(hct,3)=daycount
         enbal1(hct,4)=Ihour
         enbal1(hct,5)=Tc
         enbal1(hct,6)=QSOLAR
         enbal1(hct,7)=QIRIN
         enbal1(hct,8)=QMETAB
         enbal1(hct,9)=QSEVAP
         enbal1(hct,10)=QIROUT
         enbal1(hct,11)=QCONV
         enbal1(hct,12)=QCOND
         enbal1(hct,13)=Enb
         enbal1(hct,14)=ntry
      if(writecsv.eq.2)then
      write(II3,113)julday,",",iyear,",",daycount,",",Ihour
     &,",",Tc,",",QSOLAR,",",QIRIN,",",QMETAB,",",QSEVAP,",",Q
     &IROUT,",",QCONV,",",QCOND,",",Enb,",",ntry
      endif
         if((iyear.eq.1).and.(daycount.eq.1).and.(ihour.eq.1))then
          mintc=tc
          maxtc=tc
         endif
         if(tc.lt.mintc)then
          mintc=tc
         endif
         if(tc.gt.maxtc)then
          maxtc=tc
         endif
        endif
       endif
      endif

      if((hourop .eq. 'Y').or. (hourop .eq. 'y'))then
       IF ((Transt.eq.'N').or.(Transt.eq.'n')) THEN
        if(Ihour.lt.25)then
         hct=ihour+24*(daycount-1)
        masbal1(hct,1)=julday
         masbal1(hct,2)=iyear
         masbal1(hct,3)=daycount
         masbal1(hct,4)=Ihour
         masbal1(hct,5)=Tc
         masbal1(hct,6)=O2Flux
         if(deb1.eq.1)then
         masbal1(hct,7)=CO2Flux
         masbal1(hct,8)=NWASTE(ihour)
         masbal1(hct,9)=H2O_FREE
         masbal1(hct,10)=GH2OMET(ihour)
         masbal1(hct,11)=dryfood(ihour)
         masbal1(hct,12)=WETFOODFLUX
         masbal1(hct,13)=FAECES(ihour)
         masbal1(hct,14)=WETFAECESFLUX
         masbal1(hct,15)=URINEFLUX
         masbal1(hct,19)=H2O_Bal_hr
         masbal1(hct,20)=H2O_Bal
         masbal1(hct,21)=gutfreemass
         else
         masbal1(hct,7)=0
         masbal1(hct,8)=0
         masbal1(hct,9)=0
         masbal1(hct,10)=0
         masbal1(hct,11)=0
         masbal1(hct,12)=0
         masbal1(hct,13)=0
         masbal1(hct,14)=0
         masbal1(hct,15)=0
         masbal1(hct,19)=0
         masbal1(hct,20)=0
         masbal1(hct,21)=0
         endif
         masbal1(hct,16)=WRESP*3600
         masbal1(hct,17)=WCUT*3600
         masbal1(hct,18)=WEVAP*3600
      if(writecsv.eq.2)then
      write(II2,112)masbal1(hct,1),",",masba
     &l1(hct,2),",",masbal1(hct,3),",",masba
     &l1(hct,4),",",masbal1(hct,5),",",masba
     &l1(hct,6),",",masbal1(hct,7),",",masba
     &l1(hct,8),",",masbal1(hct,9),",",masba
     &l1(hct,10),",",masbal1(hct,11),",",mas
     &bal1(hct,12),",",masbal1(hct,13),",",m
     &asbal1(hct,14),",",masbal1(hct,15),","
     &,masbal1(hct,16),",",masbal1(hct,17),"
     &,",masbal1(hct,18),",",masbal1(hct,19)
     &,",",masbal1(hct,20),",",masbal1(hct,2
     &1)  
      endif
        endif
       endif
      endif

      if((hourop .eq. 'Y').or.(hourop .eq. 'y'))then
       IF ((Transt.eq.'N').or.(Transt.eq.'n')) THEN
        if(Ihour.lt.25)then
         hct=ihour+24*(daycount-1)
         environ1(hct,1)=julday
         environ1(hct,2)=iyear
         environ1(hct,3)=daycount
         environ1(hct,4)=Ihour
         environ1(hct,5)=Tc
         environ1(hct,6)=shade
         environ1(hct,7)=norynt
         environ1(hct,8)=depsel(ihour)
         environ1(hct,9)=acthr(ihour)
         environ1(hct,10)=Ta
         environ1(hct,11)=Vel
         environ1(hct,12)=Relhum
         environ1(hct,13)=z(ihour)
         if((aquatic.eq.1).and.(pond.eq.0))then
          if(wetmod.eq.1)then
           environ1(hct,14)=
     &       wetlandDepths(hourcount)
           environ1(hct,15)=
     &       wetlandTemps(hourcount)
          else
           environ1(hct,14)=
     &       pond_env(iyear,countday,ihour,2)
           environ1(hct,15)=
     &       pond_env(iyear,countday,ihour,1)
          endif
         else
          environ1(hct,14)=0
          environ1(hct,15)=0
         endif
         environ1(hct,16)=lengthday    
         environ1(hct,17)=phi
         environ1(hct,18)=wingtemp
         environ1(hct,19)=flight
         environ1(hct,20)=flytime    
      if(writecsv.eq.2)then
      write(II1,111)environ1(hct,1),',',environ1(hct,
     &2),',',environ1(hct,3),',',environ1(hct,4),',',env
     &iron1(hct,5),',',environ1(hct,6),',',environ1(hct,
     &7),',',environ1(hct,8),',',environ1(hct,9),',',envi
     &ron1(hct,10),',',environ1(hct,11),',',environ1(hct,
     &12),',',environ1(hct,13),',',environ1(hct,14),',',en
     &viron1(hct,15),',',environ1(hct,16),',',environ1(hct
     &,17),',',environ1(hct,18),',',environ1(hct,19),',',
     &environ1(hct,20)
      endif
        endif
       endif
      endif


      if((hourop .eq. 'Y').or.(hourop .eq. 'y'))then
       IF ((Transt.eq.'N').or.(Transt.eq.'n')) THEN
        if(complete.eq.0)then
         if(ihour.lt.25)then
c    ********** begin summary per year *****
          if(stage.gt.yMaxStg)then
           yMaxStg=stage
          endif
          if(wetmass(ihour).gt.yMaxWgt)then
           yMaxWgt=wetmass(ihour)
          endif
          if(SVL(ihour).gt.yMaxLen)then
           yMaxLen=SVL(ihour)
          endif
          if((((daycount.eq.1).and.(ihour.eq.1)).or.(countday.
     &eq.startday)).and.(ihour.eq.1))then
           yTmin=tc
           yTmax=tc
           yMinRes=ed(ihour)/e_m
           if(yMinRes.gt.1)then
            yMinRes=1
           endif
          endif
          if(v(ihour).gt.0)then
           if(Tc.gt.yTmax)then
            yTmax=Tc
           endif
           if(Tc.lt.yTmin)then
            yTmin=Tc
           endif
           if(ed(ihour)/e_m.lt.yMinRes)then
            yMinRes=ed(ihour)/e_m
            if(yMinRes.gt.1)then
             yMinRes=1
            endif
           endif
           if(PctDess.ge.Max_PctDess)then
            yMaxDess=PctDess
           endif
           if(shade.lt.yMinShade)then
            yMinShade=shade
           endif
           if(shade.gt.yMaxShade)then
            yMaxShade=shade
           endif
           if(depsel(ihour).lt.0)then
            if(depsel(ihour).gt.yMinDep)then
             yMinDep=depsel(ihour)
            endif
            if(depsel(ihour).lt.yMaxDep)then
             yMaxDep=depsel(ihour)
            endif
           endif
           if(acthr(ihour).eq.1)then
            yBsk=yBsk+1
           endif
           if(acthr(ihour).eq.2)then
            yForage=yForage+1
           endif
           if(flytime*flyspeed.gt.0)then
            yDist=yDist+flytime*60.*flyspeed/1000
           endif
           yFood=yFood+dryfood(ihour)
           yDrink=yDrink+Drunk
           yNWaste=yNWaste+nwaste(ihour)
           yFeces=yFeces+faeces(ihour)
           yO2=yO2+O2flux
        
           if(repro(ihour).gt.0)then
            yClutch=yClutch+repro(ihour)
            yFec=yClutch*newclutch
            yDLay=countday
            yovipsurv=surviv(ihour)
            yfit=yovipsurv*yFec
           endif
          if(((yDEgg.eq.0).or.(prevstage.gt.stage)).and.
     &     (stage.eq.0))then
            yDEgg=countday
           endif
           if((stage.gt.prevstage).and.(stage.eq.1))then
            yDStg1=countday
            yMStg1=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.2))then
            yDStg2=countday
            yMStg2=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.3))then
            yDStg3=countday
            yMStg3=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.4))then
            yDStg4=countday
            yMStg4=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.5))then
            yDStg5=countday
            yMStg5=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.6))then
            yDStg6=countday
            yMStg6=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.7))then
            yDStg7=countday
            yMStg7=wetmass(ihour)
           endif
           if((stage.gt.prevstage).and.(stage.eq.8))then
            yDStg8=countday
            yMStg8=wetmass(ihour)
           endif
           if(surviv(ihour).lt.ysurv)then
            ysurv=surviv(ihour)
           endif
          endif
c    ********** end summary per year *****
         endif
        endif
       endif
      endif

      prevstage=stage


C     SETTING PLOT VARIABLES AND WRITING TO 3D GRID PACKAGE

C    SETTING UP OUTPUT FOR RADIUS VS TIME OF DAY (ANNUAL = 0.)
C    OR DAY OF YEAR VS TIME OF DAY (ANNUAL = 1.)
      IF (NEQ .EQ. 1) THEN
C     NO POSSIBILITY OF MULTIPLE NODES IN BODY, SO DO HOUR VS MONTH
       ANNUAL = 1.
      ELSE  
      ENDIF

      If((Transt.eq.'N').or.(Transt.eq.'n'))then
       IF (ANNUAL .EQ. 0.) THEN
        JP=JP+1
        DO 55 NOD = 1,NEQ
C      NOTE THAT THIS PART OF THE IF LOOP WILL ONLY PLOT THE LAST DAY
C         TRANSIENT OUTPUTS FILLED FOR 2D & 3D PLOTS
C      X COORDINATE (TIME IN HOURS)
             XP(JP)= int((T/60.))
C      Y COORDINATE (DIAMETER)
             YP(JP) = AWIDTH 
C      Z COORDINATE
             ZP1(JP) = int(Y(NOD))
             ZP2(JP) = QMETAB
c           Total water loss from skin & eyes based on Dip. data (Mark Weiner)
             ZP3(JP) = WEVAP
55       CONTINUE
        ELSE
         IF (NDAY .EQ. NREPET) THEN
C          TRANSIENT OUTPUTS FILLED FOR 2D & 3D PLOTS
           JP=int(DOY*25. + T/60. + 1)
C       X COORDINATE (TIME IN HOURS)
             XP(JP)= int((T/60.))
C       Y COORDINATE (DAY OF YEAR, i.e. day of simulation)
             YP(JP) = DOY + 1.
C       Z COORDINATE
             ZP1(JP) = int(Y(NEQ))
             ZP2(JP) = QMETAB
             ZP3(JP) = WEVAP
       ENDIF
      ENDIF
      Endif

      IF (daycount .EQ. 2.) THEN
       if(tester.eq.2)then
        goto 2001
       endif
      endif


c    if(pond.eq.0)then
      IF (TIME(IHOUR) .LT. 1440.) THEN
       if(tester.eq.1)then

         debout1(1,1)=julday
         debout1(1,2)=iyear
         debout1(1,3)=daycount
         debout1(1,4)=1-24*(daycount-1)
         debout1(1,5)=wetmass(1-24*(daycount-1))
         debout1(1,6)=ed(1-24*(daycount-1))
         debout1(1,7)=cumrepro(1-24*(daycount-1))
         debout1(1,8)=hs(1-24*(daycount-1))
         debout1(1,9)=ms(1-24*(daycount-1))
         debout1(1,10)=SVL(1-24*(daycount-1))
         debout1(1,11)=v(1-24*(daycount-1))
         debout1(1,12)=E_H(1-24*(daycount-1))
         debout1(1,13)=cumbatch(1-24*(daycount-1))
         debout1(1,14)=v_baby1(1-24*(daycount-1))
         debout1(1,15)=e_baby1(1-24*(daycount-1))
         debout1(1,16)=pregnant
         debout1(1,17)=stage_rec(1-24*(daycount-1))
         debout1(1,18)=wetmass(1-24*(daycount-1))-
     &    wetgonad(1-24*(daycount-1))-wetfood(1-24*(daycount-1))
c       body condition (non repro wet mass/max non repro wetmass)
c       debout1(i,19)=debout1(i,18)/((((debout1(i,11)
c     &    *E_m)/mu_E)*w_E)/d_V+debout1(i,11))
          debout1(1,19)=pctdess
          debout1(1,20)=surviv(1-24*(daycount-1))


        goto 2001
       endif
C      DO ANOTHER HOUR
        GO TO 54
       ELSE
C      LEAVE TO DO ANOTHER DAY  ***END of DAY loop ***
      ENDIF
c    endif


      If((Transt.eq.'N').or.(Transt.eq.'n'))then
c     Writing out time, Tcore and location of the animal for the day
c     to file Contur.out 
      micros=daycount*24-23
      microf=daycount*24    
      DO 989 I=micros,microf
       if(depsel(I-24*(daycount-1)).lt.depmax)then
          depmax = depsel(i-24*(daycount-1))
       endif
c       if(depsel(I-24*(daycount-1)).lt.depmaxmon(iday))then     
c          depmaxmon(iday)=depsel(i-24*(daycount-1))
c       endif
       if((hourop .eq. 'Y').or. (hourop .eq. 'y'))then
        if(monthly.eq.2)then
         do 2012 j=1,18    
          debout1(i,j)=1
2012     continue
        else
         debout1(i,1)=julday
         debout1(i,2)=iyear
         debout1(i,3)=daycount
         debout1(i,4)=i-24*(daycount-1)
         debout1(i,5)=wetmass(i-24*(daycount-1))
         debout1(i,6)=ed(i-24*(daycount-1))
         debout1(i,7)=cumrepro(i-24*(daycount-1))
         debout1(i,8)=hs(i-24*(daycount-1))
         debout1(i,9)=ms(i-24*(daycount-1))
         debout1(i,10)=SVL(i-24*(daycount-1))
         debout1(i,11)=v(i-24*(daycount-1))
         debout1(i,12)=E_H(i-24*(daycount-1))
         debout1(i,13)=cumbatch(i-24*(daycount-1))
         debout1(i,14)=v_baby1(i-24*(daycount-1))
         debout1(i,15)=e_baby1(i-24*(daycount-1))
         debout1(i,16)=pregnant
         debout1(i,17)=stage_rec(i-24*(daycount-1))
         debout1(i,18)=wetmass(i-24*(daycount-1))-wetgonad(i-
     &    24*(daycount-1))-wetfood(i-24*(daycount-1))
c       body condition (non repro wet mass/max non repro wetmass)
c       debout1(i,19)=debout1(i,18)/((((debout1(i,11)
c     &    *E_m)/mu_E)*w_E)/d_V+debout1(i,11))
          debout1(i,19)=pctdess
          debout1(i,20)=surviv(i-24*(daycount-1))
      if(writecsv.eq.2)then
      write(II4,114)debout1(i,1),',',debout1(i,2),',',deb
     &out1(i,3),',',debout1(i,4),',',debout1(i,5),',',debo
     &ut1(i,6),',',debout1(i,7),',',debout1(i,8),',',debou
     &t1(i,9),',',debout1(i,10),',',debout1(i,11),',',debo
     &ut1(i,12),',',debout1(i,13),',',debout1(i,14),',',de
     &bout1(i,15),',',debout1(i,16),',',debout1(i,17),',',
     &debout1(i,18),',',debout1(i,19),',',debout1(i,20)
      endif
         if(stage.eq.0)then
          devtime=devtime+(1./24.)
         endif
         if((stage.gt.1).and.(birth.eq.0))then
          birth=1
          birthday=julday
          birthmass=real(debout1(i,18),4)
         endif
        endif
       endif
   
989   CONTINUE
      Endif

c    test for Enary variables
      If((Transt.eq.'Y').or.(Transt.eq.'y'))then
c      WRITE(6,*)'Running transient model'
c      : inputs from S.S. behavior,',
c     &      ' then output Tcores'
c      write(i2,503)
c503     format('   Hour    Qsolr    Zen       Tair       Vel',
c     &  '      RH         Tsky      Tsubst   Shade')
        Do 501 kkk = 1,25
c        write(i2,502)KKK,Enary1(kkk),Enary2(kkk),
c     &    Enary3(kkk),Enary4(kkk),Enary5(kkk),
c     &    Enary6(kkk),Enary7(kkk),Enary8(kkk)
501     continue
      endif
c502   format(1i4,8f10.2)

       
      DOY = DOY + 1.

      IF ((Transt.eq.'N').or.(Transt.eq.'n')) THEN
C      NO TRANSIENT, WRITE OUTPUT PLOT FILE
        GO TO 101
      ENDIF
       
C     ******************* END OF LOOPS FOR THE DAY *****************

101   CONTINUE

      If((Transt.eq.'Y').or.(Transt.eq.'y'))then

      SCENAR=1
      do 902 i=1,5
       do 903 j=1,25
      transar(i,j)=0.
903   continue
902   continue
      IF(CONTW .gt. 0.)THEN
        SCENTOT=1
       ELSE
        SCENTOT=5
      ENDIF
c     ******************** Begin loop of 5 initial scenarios for transient************
      do 929 SCENAR=1,SCENTOT
      TRAN=1
      TRANCT=1
      IF((TRANIN .eq. 'n') .or. (TRANIN .eq.'N'))THEN
          NDAYY = 1
      Else
          NDAYY = 1
      ENDIF
      do 919 TRAN=1,NDAYY
c      Repeat the day using transient model & behaviorally selected env's
c      IF THE ANIMAL HAS BEEN ACTIVE ABOVE GROUND & NOT FOSSORIAL (HIBERNATING)
C      Transient: set STEP SIZE, reinitialize time for the transient.
c      All input data from Enary, which has been set up from animal 
c      behavioral selections in steady state. 
c      Time for integrator to quit = Tout 
        TOUT = 1440.
C       CORE TEMPERATURE   
C       Y(1)=X; 
C       YDOT(1)=DX/DT  COMPUTED IN DSUB
C      INITIAL TEMPERATURE CONDITIONS FOR INTEGRATOR
        Ihour = 1 
        T = Time(Ihour)


C        INITIALIZE Gear integrator
C        TCORES IS RELOADED STARTING AT '1' EVERY NEW TIME INTERVAL (DAY OF YEAR SIMULATED)
C        THE INTEGRATOR, LSODE, IS STARTED AT THE END OF EVERY DAY & RUN FOR 24 HOURS
C        IF A TRANSIENT IS INVOKED

          If((TRANIN .eq. 'n') .or. (TRANIN .eq. 'N'))then
            Y(1)=tcinit
            y(2)=tcinit
          else
            Y(1)=Taloc(1)
          Endif

          If((TRANCT .gt. 1).or.(DOY.gt.1))THEN
          Y(1)=TCPAST
          ENDIF

c        Check the time constant to see if this is reasonable
C         COMPUTING THE TIME CONSTANT (seconds)
          QRAD = QIRIN - QIROUT
          CALL TIMCON(AMASS,TIMCST,TIMCMN)
C        Converting time constant to minutes
          Tcnmin = Timcst/60.

        IF (T .EQ. 0.D0) THEN
          IT=1
          Call Osub(T,Y,TRANCT,NDAYY)
         ELSE
C        TIME NOT 0.
C        INITIALIZE Gear integrator
c          Y(1)=Taloc(1)
      ENDIF

c      The LSODE (Gear) integrator) is now only called once/day.  
c      It goes through an entire day (Tout = 1440 min) using the 
c      behaviorally selected environmental conditions stored in array Enary.
c      Internal Gear calls to the output subroutine, Osub, put output in 
c      file output at the moment.

      if(countday.eq.81)then
      continue
      endif
      if(contdep.le.0.01)then
      TRANIN='Y'
      else
       if(ectoinput1(67).eq.1)then
        tranin = 'y'
       else
        tranin = 'n'
       endif
      endif

c    write(*,*) NEQ,Y(1),T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,

c     *   IOPT,LRW,LIW,MF,TRANCT,NDAYY
        CALL LSODE(DSUB,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     *   IOPT,RWORK,LRW,IWORK,LIW,JAC,MF,TRANCT,NDAYY)
C      PRINT OUT ANSWERS AT THE END OF TIME INTERVAL, TPRINT
c      WRITE(I2,130) T,(Y(NOD),NOD=1,NEQ)
        DO 939 I=1,25
        TRANSAR(SCENAR,I)=TRANSIENT(I)
939   continue
           TRANCT = TRANCT + 1
919        tcpast=real(y(1),4)
           
c     endif
929   continue
c     ******************** End loop of 5 initial scenarios for transient************



c     ******************** Begin transient scenario analysis ************
      IF(CONTW .gt. 0.)THEN

       do 953 i=1,25
       FTRANSAR(I)=1
       DEPSEL(I)=0.
953    continue

       GOTO 952
      ENDIF    

      DO 949 I=1,25
      
       SUNACT=TRANSAR(1,I)
       SHDACT=TRANSAR(2,I)
       SUNSOIL=TRANSAR(3,I)
       SHDSOIL=TRANSAR(4,I)
       CAVE=TRANSAR(5,I)
       INACTIVE(I)='N'
      
c    Diurnally active
      IF ((Dayact .EQ. 'Y') .OR. (Dayact .EQ. 'y')) THEN
       IF(ENARY1(I) .gt. 0.)THEN
       If(ENARY2(I) .eq. 90.)then
c       GOTO 9989
       Endif    
       If((SUNACT .le. TMAXPR) .and. (SUNACT .ge. TMINPR))THEN
c      In preferred range, make animal active in full sun
        FTRANSAR(I)=1
        DEPSEL(I)=0.
        SHD(I) = 0.
        GOTO 959
       Endif

       IF(SUNACT .gt. TMAXPR)THEN
          IF(SHDACT .le. TMAXPR)THEN
            FTRANSAR(I)=2
            DEPSEL(I)=0.
            SHD(I) = 100.
          GOTO 959
          ENDIF
       ENDIF
       
       If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
       IF(SUNACT .lt. TMINPR)THEN
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I) = 0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
       ENDIF
       ENDIF
      Else
          If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
          IF((NOCTURN .eq. 'n') .or. (nocturn .eq. 'N'))THEN
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I) = 0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          Endif
          ENDIF
      Endif
      Else
9989  continue
      If ((Crepus .eq. 'Y').or.(Crepus .eq. 'y'))then
       IF(ENARY2(I) .eq. 90)THEN
       If((SUNACT .le. TMAXPR) .and. (SUNACT .ge. TMINPR))THEN
c      In preferred range, make animal active in full sun
        FTRANSAR(I)=1
        DEPSEL(I)=0.
        SHD(I) = 0.
        GOTO 959
       Endif
           IF(SUNACT .gt. TMAXPR)THEN
          IF(SHDACT .le. TMAXPR)THEN
            FTRANSAR(I)=2
            DEPSEL(I)=0.
            SHD(I) = 100.
          GOTO 959
          ENDIF
       ENDIF
       If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
       IF(SUNACT .lt. TMINPR)THEN
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I) = 0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
       ENDIF
      Endif
      Endif
      Endif
      Endif
c    Nocturnally active
      IF ((Nocturn .EQ. 'Y') .OR. (Nocturn .EQ. 'y')) THEN
       IF(ENARY1(I) .eq. 0.)THEN
       If((SHDACT .le. TMAXPR) .and. (SHDACT .ge. TMINPR))THEN
c      In preferred range, make animal active under clear sky
        FTRANSAR(I)=1
        DEPSEL(I)=0.
        SHD(I) = 0.
        GOTO 959
       Endif

       IF(SHDACT .gt. TMAXPR)THEN
          IF(SUNACT .le. TMAXPR)THEN
            FTRANSAR(I)=2
            DEPSEL(I)=0.
            SHD(I) = 100.
          GOTO 959
          ENDIF
       ENDIF
       
       If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
       IF(SHDACT .lt. TMINPR)THEN
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I) = 0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
       ENDIF
       ENDIF
      Else
          If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I) = 0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
          ENDIF
      Endif
      Endif

      If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
c    Can't be active nocturnally, diurnally or crepuscularly, put in burrow
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=3
            DEPSEL(I)=-15.
            SHD(I)=0.
            GOTO 959
          ENDIF
          IF((SUNSOIL .le. TDIGPR) .and. (SUNSOIL .ge. 1.))THEN
            FTRANSAR(I)=4
            DEPSEL(I)=-15.
            SHD(I) = 100.
            GOTO 959
          Endif
          IF(CAVE .le. TMAXPR)THEN
            FTRANSAR(I)=5
            DEPSEL(I)=-60.
            SHD(I) = 100.
            GOTO 959
          ENDIF
      Endif

c    Stuck on surface because can't burrow, or too hot in burrow, put in shade
            FTRANSAR(I)=2
            DEPSEL(I)=-0.5
            INACTIVE(I) = 'Y'
            SHD(I)=100.

959    continue
          
949   continue
952   continue
      
c    ****************Now constructin final environmental array**************
      DO 969 I=1,25
      If(FTRANSAR(I) .eq. 1)Then
      Enary41(I)=Enary1(I)
      Enary42(I)=Enary2(I)
      Enary43(I)=Enary3(I)
      Enary44(I)=Enary4(I)
      Enary45(I)=Enary5(I)
      Enary46(I)=Enary6(I)
      Enary47(I)=Enary7(I)
      Enary48(I)=Enary8(I)
      Endif
      If(FTRANSAR(I) .eq. 2)Then
      Enary41(I)=Enary9(I)
      Enary42(I)=Enary10(I)
      Enary43(I)=Enary11(I)
      Enary44(I)=Enary12(I)
      Enary45(I)=Enary13(I)
      Enary46(I)=Enary14(I)
      Enary47(I)=Enary15(I)
      Enary48(I)=Enary16(I)
      Endif
      If(FTRANSAR(I) .eq. 3)Then
      Enary41(I)=Enary17(I)
      Enary42(I)=Enary18(I)
      Enary43(I)=Enary19(I)
      Enary44(I)=Enary20(I)
      Enary45(I)=Enary21(I)
      Enary46(I)=Enary22(I)
      Enary47(I)=Enary23(I)
      Enary48(I)=Enary24(I)
      Endif
      If(FTRANSAR(I) .eq. 4)Then
      Enary41(I)=Enary25(I)
      Enary42(I)=Enary26(I)
      Enary43(I)=Enary27(I)
      Enary44(I)=Enary28(I)
      Enary45(I)=Enary29(I)
      Enary46(I)=Enary30(I)
      Enary47(I)=Enary31(I)
      Enary48(I)=Enary32(I)
      Endif
      If(FTRANSAR(I) .eq. 5)Then
      Enary41(I)=Enary33(I)
      Enary42(I)=Enary34(I)
      Enary43(I)=Enary35(I)
      Enary44(I)=Enary36(I)
      Enary45(I)=Enary37(I)
      Enary46(I)=Enary38(I)
      Enary47(I)=Enary39(I)
      Enary48(I)=Enary40(I)
      Endif
969   continue

      IF(CONTW .gt. 0.)THEN
      GOTO 980
      ENDIF

c    ***************start final transient run*************************
      TRAN=1
      TRANCT=1
      IF((TRANIN .eq. 'n') .or. (TRANIN .eq.'N'))THEN
          NDAYY = 1
      Else
          NDAYY = 3
      ENDIF
      do 979 TRAN=1,NDAYY
c      Repeat the day using transient model & behaviorally selected env's
c      IF THE ANIMAL HAS BEEN ACTIVE ABOVE GROUND & NOT FOSSORIAL (HIBERNATING)
C      Transient: set STEP SIZE, reinitialize time for the transient.
c      All input data from Enary, which has been set up from animal 
c      behavioral selections in steady state. 
c      Time for integrator to quit = Tout 
        TOUT = 1440.
C       CORE TEMPERATURE   
C       Y(1)=X; 
C       YDOT(1)=DX/DT  COMPUTED IN DSUB
C      INITIAL TEMPERATURE CONDITIONS FOR INTEGRATOR
        Ihour = 1 
        T = Time(Ihour)


C        INITIALIZE Gear integrator
C        TCORES IS RELOADED STARTING AT '1' EVERY NEW TIME INTERVAL (DAY OF YEAR SIMULATED)
C        THE INTEGRATOR, LSODE, IS STARTED AT THE END OF EVERY DAY & RUN FOR 24 HOURS
C        IF A TRANSIENT IS INVOKED

          If((TRANIN .eq. 'n') .or. (TRANIN .eq. 'N'))then
            Y(1)=tcinit
            Y(2)=tcinit
          else
            Y(1)=Taloc(1)
          Endif

          If((TRANCT .gt. 1).or.(DOY.gt.1))THEN
          Y(1)=TCPAST
          ENDIF

c        Check the time constant to see if this is reasonable
C         COMPUTING THE TIME CONSTANT (seconds)
          QRAD = QIRIN - QIROUT
          CALL TIMCON(AMASS,TIMCST,TIMCMN)
C        Converting time constant to minutes
          Tcnmin = Timcst/60.

        IF (T .EQ. 0.D0) THEN
          IT=1
          Call Osub(T,Y,TRANCT,NDAYY)
         ELSE
C        TIME NOT 0.
C        INITIALIZE Gear integrator
c          Y(1)=Taloc(1)
      ENDIF

c      The LSODE (Gear) integrator) is now only called once/day.  
c      It goes through an entire day (Tout = 1440 min) using the 
c      behaviorally selected environmental conditions stored in array Enary.
c      Internal Gear calls to the output subroutine, Osub, put output in 
c      file output at the moment.
        CALL LSODE(DSUB,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
     *   IOPT,RWORK,LRW,IWORK,LIW,JAC,MF,TRANCT,NDAYY)
C      PRINT OUT ANSWERS AT THE END OF TIME INTERVAL, TPRINT
c      WRITE(I2,130) T,(Y(NOD),NOD=1,NEQ)
           TRANCT = TRANCT + 1
979        TCPAST = Y(1)
c    ***************end of final transient run*************************
      endif

980   continue

      If((Transt.eq.'Y').or.(Transt.eq.'y'))then
c    ***************beginning of transient output*********************

      DO 889 I=1,25
      IF(FTRANSAR(I) .eq. 2)THEN
          IF((TRANSAR(1,I).gt.TDIGPR).and.(TRANSAR(2,I).le.TDIGPR))THEN
          TRANSIENT(I)=TDIGPR
          ENDIF
      ENDIF
889   continue
      
      
      Q10=2.
      J=0
      if(live.eq.1)then
      Do 771 I=1,24
      IF(INACTIVE(I) .eq. 'N')THEN
       If((FTRANSAR(I) .eq. 1) .or. (FTRANSAR(I) .eq. 2))THEN
        If((TRANSIENT(I) .ge. TMINPR) .and. (TRANSIENT(I) .le. TMAXPR))
     &      THEN
         Acthr(I)=1
         J=J+1
        Endif
       Endif
      Endif
771   Continue

      endif
c    Do 777 JP=1,25
C      X COORDINATE (TIME IN HOURS)
c             XP(JP)= (I)
C      Y COORDINATE (DIAMETER)
c             YP(JP) = AWIDTH 
C      Z COORDINATE
c             ZP1(JP) = TRANSIENT(I)
c777    continue
      JP=0
      Do 772 I=1,INTNUM
c    JP=JP+1
      ZP1(I)=ZD1(I)
      ZP2(I)=ZD2(I)
      ZP3(I)=ZD3(I)
      ZP4(I)=ZD4(I)
      ZP5(I)=ZD5(I)
      ZP6(I)=ZD6(I)
      ZP7(I)=ZD7(I)
      call traphr(I,DTIME)
772   continue    


      IF(CONTDEPTH .gt. CONTH*10)THEN
          CONTDEPTH = CONTH*10
      ENDIF

      contdep=contdepth


      DO 7891 I=1,25
      if((contdep.le.01).and.(tranny.ne.1))then
       transient(i)=atsoil(i,1)
      endif
7891  continue

      micros=daycount*24-23
      microf=daycount*24

      DO 891 I=micros,microf
      TC=transient(i-24*(daycount-1))
      if((live.eq.1).or.(contonly.eq.1).or.(tranny.eq.1))then

c    if((live.eq.1))then
       if((DEB1.eq.1).and.(metab_mode.gt.0))then
        call DEB_INSECT(i-24*(daycount-1))
       else
        call DEB(i-24*(daycount-1))
       endif

       if(monthly.eq.2)then
        do 2013 j=1,17    
         debout1(i,j)=1
2013    continue
       else
        debout1(i,1)=julday
        debout1(i,2)=iyear
        debout1(i,3)=daycount
        debout1(i,4)=i-24*(daycount-1)
        debout1(i,5)=wetmass(i-24*(daycount-1))
        debout1(i,6)=ed(i-24*(daycount-1))
        debout1(i,7)=cumrepro(i-24*(daycount-1))
        debout1(i,8)=hs(i-24*(daycount-1))
        debout1(i,9)=ms(i-24*(daycount-1))
        debout1(i,10)=SVL(i-24*(daycount-1))
        debout1(i,11)=v(i-24*(daycount-1))
        debout1(i,12)=E_H(i-24*(daycount-1))
        debout1(i,13)=cumbatch(i-24*(daycount-1))
        debout1(i,14)=v_baby1(i-24*(daycount-1))
        debout1(i,15)=e_baby1(i-24*(daycount-1))
        debout1(i,16)=pregnant
        debout1(i,17)=stage_rec(i-24*(daycount-1))
        debout1(i,18)=wetmass(i-24*(daycount-1))-wetgonad(
     &    i-24*(daycount-1))-wetfood(i-24*(daycount-1))
c    body condition (non repro wet mass/max non repro wetmass)
c    debout1(i,19)=debout1(i,18)/((((debout1(i,11)
c     &    *E_m)/mu_E)*w_E)/d_V+debout1(i,11))
        debout1(i,19)=pctdess
        debout1(i,20)=surviv(i-24*(daycount-1))
      if(writecsv.eq.2)then
      write(II4,114)debout1(i,1),',',debout1(i,2),',',debout
     &1(i,3),',',debout1(i,4),',',debout1(i,5),',',debout1(i,6
     &),',',debout1(i,7),',',debout1(i,8),',',debout1(i,9),','
     &,debout1(i,10),',',debout1(i,11),',',debout1(i,12),',',d
     &ebout1(i,13),',',debout1(i,14),',',debout1(i,15),',',deb
     &out1(i,16),',',debout1(i,17),',',debout1(i,18),',',debou
     &t1(i,19),',',debout1(i,20)
      endif
       endif
       if(stage.eq.0)then
        devtime=devtime+(1./24.)
       endif
       if((stage.gt.1).and.(birth.eq.0))then
        birth=1
        birthday=julday
        birthmass=real(debout1(i,18),4)
       endif

       enbal1(i+24*(iday-1),1)=julday
       enbal1(i+24*(iday-1),2)=iyear
       enbal1(i+24*(iday-1),3)=daycount
       enbal1(i+24*(iday-1),4)=i-24*(daycount-1)
       enbal1(i+24*(iday-1),5)=transient(i-24*(daycount-1))
       enbal1(i+24*(iday-1),6)=QSOLAR
       enbal1(i+24*(iday-1),7)=QIRIN
       enbal1(i+24*(iday-1),8)=QMETAB
       enbal1(i+24*(iday-1),9)=QSEVAP
       enbal1(i+24*(iday-1),10)=QIROUT
       enbal1(i+24*(iday-1),11)=QCONV
       enbal1(i+24*(iday-1),12)=QCOND
       enbal1(i+24*(iday-1),13)=Enb
       enbal1(i+24*(iday-1),14)=ntry
      if(writecsv.eq.2)then
      write(II3,113)julday,",",iyear,",",daycount,",",
     &enbal1(i+24*(iday-1),4),",",transient(i-24*(dayco
     &unt-1)),",",QSOLAR,",",QIRIN,",",QMETAB,",",QSEVA
     &P,",",QIROUT,",",QCONV,",",QCOND,",",Enb,",",ntry
      endif
        if((iyear.eq.1).and.(daycount.eq.1))then
         mintc=tc
         maxtc=tc
        endif
        if(tc.lt.mintc)then
         mintc=tc
        endif
        if(tc.gt.maxtc)then
         maxtc=tc
        endif
       hct=i+24*(iday-1)
       masbal1(i+24*(iday-1),1)=julday
       masbal1(i+24*(iday-1),2)=iyear
       masbal1(i+24*(iday-1),3)=daycount
       masbal1(i+24*(iday-1),4)=i-24*(daycount-1)
       masbal1(i+24*(iday-1),5)=transient(i-24*(daycount-1))
       masbal1(i+24*(iday-1),6)=QSOLAR
       masbal1(i+24*(iday-1),7)=QIRIN
       masbal1(i+24*(iday-1),8)=QMETAB
       masbal1(i+24*(iday-1),9)=QSEVAP
       masbal1(i+24*(iday-1),10)=QIROUT
       masbal1(i+24*(iday-1),11)=QCONV
       masbal1(i+24*(iday-1),12)=Wevap
       masbal1(i+24*(iday-1),13)=Enb
       masbal1(i+24*(iday-1),14)=ntry
       masbal1(i+24*(iday-1),15)=H2O_Bal 
      if(writecsv.eq.2)then
      write(II2,112)masbal1(hct,1),",",masba
     &l1(hct,2),",",masbal1(hct,3),",",masba
     &l1(hct,4),",",masbal1(hct,5),",",masba
     &l1(hct,6),",",masbal1(hct,7),",",masba
     &l1(hct,8),",",masbal1(hct,9),",",masba
     &l1(hct,10),",",masbal1(hct,11),",",mas
     &bal1(hct,12),",",masbal1(hct,13),",",m
     &asbal1(hct,14),",",masbal1(hct,15),","
     &,masbal1(hct,16),",",masbal1(hct,17),"
     &,",masbal1(hct,18),",",masbal1(hct,19)
     &,",",masbal1(hct,20),",",masbal1(hct,2
     &1)  
      endif 

       environ1(i+24*(iday-1),1)=julday
       environ1(i+24*(iday-1),2)=iyear
       environ1(i+24*(iday-1),3)=daycount
       environ1(i+24*(iday-1),4)=i-24*(daycount-1)
       environ1(i+24*(iday-1),5)=transient(i-24*(daycount-1))
       environ1(i+24*(iday-1),6)=Enary48(i-24*(daycount-1))
       environ1(i+24*(iday-1),7)=1
       environ1(i+24*(iday-1),8)=DEPSEL(I-24*(daycount-1))
       environ1(i+24*(iday-1),9)=acthr(i-24*(daycount-1))
       environ1(i+24*(iday-1),10)=Enary43(I-24*(daycount-1))
       environ1(i+24*(iday-1),11)=Enary44(I-24*(daycount-1))
       environ1(i+24*(iday-1),12)=Enary45(I-24*(daycount-1))
       environ1(i+24*(iday-1),13)=Enary42(I-24*(daycount-1))
        if((aquatic.eq.1).and.(pond.eq.0))then
          environ1(i+24*(iday-1),14)=environ1(i+24*(iday-1),14)
          environ1(i+24*(iday-1),15)=environ1(i+24*(iday-1),15)
         continue
        else
         if(conth.gt.0)then
          environ1(i+24*(iday-1),14)=contdepth
          environ1(i+24*(iday-1),15)=transient(i-24*(daycount-1))
c        pond_env(i+24*(iday-1),1)=julday
         else
          environ1(i+24*(iday-1),14)=0
          environ1(i+24*(iday-1),15)=0
         endif
        endif
       environ1(i+24*(iday-1),16)=lengthday
       environ1(i+24*(iday-1),17)=phi
      if(writecsv.eq.2)then
      write(II1,111)environ1(hct,1),',',environ1(hct,
     &2),',',environ1(hct,3),',',environ1(hct,4),',',env
     &iron1(hct,5),',',environ1(hct,6),',',environ1(hct,
     &7),',',environ1(hct,8),',',environ1(hct,9),',',envi
     &ron1(hct,10),',',environ1(hct,11),',',environ1(hct,
     &12),',',environ1(hct,13),',',environ1(hct,14),',',en
     &viron1(hct,15),',',environ1(hct,16),',',environ1(hct
     &,17),',',environ1(hct,18),',',environ1(hct,19),',',
     &environ1(hct,20)
      endif
      endif
891   continue

      if((aquatic.eq.1).and.(pond.eq.1))then
       do 5052 i=1,24
        pond_env(iyear,countday,i,1)=transient(i)

        if(contdep.eq.0.01)then
         pond_env(iyear,countday,i,2)=0.

        else

         pond_env(iyear,countday,i,2)=contdep 

        endif
5052   continue
      endif


c    ****************************transient output********************************
      Endif

      IF (Iday .EQ. 1) THEN
        if((hourop .eq. 'Y').or. (hourop .eq. 'y'))then
c         write(i10,*)'TMIN, TMAX =',TMINPR,TMAXPR  
        endif
        itest=1
      ENDIF
      
c21     FORMAT(1X,7F8.2,1F6.0,1I3)
c22     FORMAT(1X,7E11.3)

c25     FORMAT(1X,'G protein = ',1E12.3,' G fat = ',1E12.3,/1x,
c     * 'Energy lost (J) = ',1E12.3,' Water lost (g) = ',1E12.3)
c91     FORMAT(80A1)

C      FORMAT OUTPUT BY MICROMETEOROLOGY PROGRAM FROM OSUB AS 'METOUT'
c92     FORMAT(1F5.0,1F5.1,1F5.0,1E10.4,4F5.1,2E10.4)

c93     FORMAT(1X,1F5.0,1F5.1,1F5.0,1F7.3,3F5.1,1F5.1,1F7.0,1F6.1)
c94     FORMAT(' TIME TAIR   RH   VEL TSUBST ',
c     *    'TSOIL1 TSOIL3  ZEN  SOLR TskyC')
c95    FORMAT(1X,1F5.0,2F6.1,1F5.0,1F6.2,4F6.1,1E12.4,1F6.2,1F6.1,1F5.2,
c     &1F5.0)
c96    FORMAT(1X,'Hr Tc   Qsol      QIRin     Qmetab    Qsevap    '
c     &,'QIRout    Qconv     Qcond     gH2O/s   %shade   Enb')
c100    FORMAT(1X,1I5,7E12.3)
c110    FORMAT(1A)
c111    FORMAT(1A130)    
c112    FORMAT(1X,1A130)    
c113    FORMAT(20I4) 
c114    FORMAT(8F10.4)   
c115    FORMAT(1I2,1F5.1,8E10.2,1f5.0,1E11.2)   
c116    FORMAT(1X,/) 
c117    FORMAT(1X,1I4,2F10.3,1I4)          
c118    FORMAT(1X,2I4,3F10.3)
c119    FORMAT(1X,6F6.1)
c120   FORMAT('Ihour Mon Amass Qmetab   Wevap     AirVol    CO2MOL',
c     & '    Tskin Tc  KFlsh SkinW shade  Ta   Vel RH  Qsolar    z(i)',
c     & '   Enb')
c130    FORMAT(1X,1F5.0,1F7.2)
c150    FORMAT(1X,1I5,7E12.3,1I4)
c151    FORMAT(6E12.3,2E12.4,1E12.3,1E12.4)
c300    FORMAT(1X,7E12.3)
c990    FORMAT(2F10.2,1X,1F10.4)
c991    FORMAT(1F6.2,1X,1F12.3,1X,1E12.4)        
c992   FORMAT(2I3,1F8.3,4E10.3,2F5.1,1f5.2,1F6.1,2f6.1,
c     & 1F5.1,1F4.0,1E10.3,1F5.1,1e11.3)
c210    FORMAT(130A1)
c212    FORMAT(I4,I4,I4,I4,1F10.2,1F10.2,1F10.2,1F10.2,1F10.2,1F13.2,
c     & 1F13.2,1F10.4,1F10.3,1F10.3)


c    causedeath    
c    0    no death
c    1    cold
c    2    heat
c    3    dess
c    4    starve
c    5    ageing
c    6    pond dry
      if(countday.eq.census)then
       yearsout1(iyear,1)=iyear
       yearsout1(iyear,2)=yMaxStg
       yearsout1(iyear,3)=yMaxWgt
       yearsout1(iyear,4)=yMaxLen
       yearsout1(iyear,5)=yTmax
       yearsout1(iyear,6)=yTmin
       yearsout1(iyear,7)=yMinRes
       yearsout1(iyear,8)=yMaxDess
       yearsout1(iyear,9)=yMinShade
       yearsout1(iyear,10)=yMaxShade
       yearsout1(iyear,11)=yMinDep
       yearsout1(iyear,12)=yMaxDep
       yearsout1(iyear,13)=yBsk
       yearsout1(iyear,14)=yForage
       yearsout1(iyear,15)=yDist
       yearsout1(iyear,16)=yFood
       yearsout1(iyear,17)=yDrink
       yearsout1(iyear,18)=yNWaste
       yearsout1(iyear,19)=yFeces
       yearsout1(iyear,20)=yO2
       yearsout1(iyear,21)=yClutch
       yearsout1(iyear,22)=yFec
       yearsout1(iyear,23)=causedeath
       yearsout1(iyear,24)=ydLay
       yearsout1(iyear,25)=ydEgg
       yearsout1(iyear,26)=ydStg1
       yearsout1(iyear,27)=ydStg2
       yearsout1(iyear,28)=ydStg3
       yearsout1(iyear,29)=ydStg4
       yearsout1(iyear,30)=ydStg5
       yearsout1(iyear,31)=ydStg6
       yearsout1(iyear,32)=ydStg7
       yearsout1(iyear,33)=ydStg8
       yearsout1(iyear,34)=ymStg1
       yearsout1(iyear,35)=ymStg2
       yearsout1(iyear,36)=ymStg3
       yearsout1(iyear,37)=ymStg4
       yearsout1(iyear,38)=ymStg5
       yearsout1(iyear,39)=ymStg6
       yearsout1(iyear,40)=ymStg7
       yearsout1(iyear,41)=ymStg8
       yearsout1(iyear,42)=ysurv
       yearsout1(iyear,43)=yovipsurv
       yearsout1(iyear,44)=yfit
       yearsout1(iyear,45)=deathstage

      if(writecsv.ge.1)then
        if(iyear.eq.1)then
         II7=7
         OPEN (II7, FILE = 'yearout1.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
        if(iyear.eq.2)then
         II7=7
         OPEN (II7, FILE = 'yearout2.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.3)then
         II7=7
         OPEN (II7, FILE = 'yearout3.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.4)then
         II7=7
         OPEN (II7, FILE = 'yearout4.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.5)then
         II7=7
         OPEN (II7, FILE = 'yearout5.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.6)then
         II7=7
         OPEN (II7, FILE = 'yearout6.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.7)then
         II7=7
         OPEN (II7, FILE = 'yearout7.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.8)then
         II7=7
         OPEN (II7, FILE = 'yearout8.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.9)then
         II7=7
         OPEN (II7, FILE = 'yearout9.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.10)then
         II7=7
         OPEN (II7, FILE = 'yearout10.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.11)then
         II7=7
         OPEN (II7, FILE = 'yearout11.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.12)then
         II7=7
         OPEN (II7, FILE = 'yearout12.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.13)then
         II7=7
         OPEN (II7, FILE = 'yearout13.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.14)then
         II7=7
         OPEN (II7, FILE = 'yearout14.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.15)then
         II7=7
         OPEN (II7, FILE = 'yearout15.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.16)then
         II7=7
         OPEN (II7, FILE = 'yearout16.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.17)then
         II7=7
         OPEN (II7, FILE = 'yearout17.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.18)then
         II7=7
         OPEN (II7, FILE = 'yearout18.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.19)then
         II7=7
         OPEN (II7, FILE = 'yearout19.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
            if(iyear.eq.20)then
         II7=7
         OPEN (II7, FILE = 'yearout20.csv')
      write(II7,1118)"MaxStg",",","MaxWgt",",","MaxLen",",
     &","Tmax",",","Tmin",",","MinRes",",","MaxDess",",",
     &"MinShade",",","MaxShade",",","MinDep",",","MaxDep",
     &",","Bsk",",","Forage",",","Dist",",","Food",",",
     &"Drink",",","NWaste",",","Feces",",","O2",",","Clutch
     &",",","Fec",",","CauseDeath",",","tLay",",","tEgg",",    
     &","tStg1",",","tStg2",",","tStg3",",","tStg4",",",
     &"tStg5",",","tStg6",",","tStg7",",","tStg8",",","mStg1",
     &",","mStg2",",","mStg3",",","mStg4",",","mStg5",",",
     &"mStg6",",","mStg7",",","mStg8",",","surv",",",
     &"ovipsurv",",","fit",",","deathstage"
      write(II7,1117)yMaxStg,",",yMaxWgt,",",yMaxLen,",",
     &yTmax,",",yTmin,",",yMinRes,",",yMaxDess,",",yMinSh
     &ade,",",yMaxShade,",",yMinDep,",",yMaxDep,",",yBsk,
     &",",yForage,",",yDist,",",yFood,",",yDrink,",",yNWa
     &ste,",",yFeces,",",yO2,",",yClutch,",",yFec,",",caus
     &edeath,",",ydLay,",",ydEgg,",",ydStg1,",",ydStg2,","
     &,ydStg3,",",ydStg4,",",ydStg5,",",ydStg6,",",ydStg7,"
     &,",ydStg8,",",ymStg1,",",ymStg2,",",ymStg3,",",ymStg4
     &,",",ymStg5,",",ymStg6,",",ymStg7,",",ymStg8,",",ysur
     &v,",",yovipsurv,",",yfit,",",deathstage
         close(ii7)
        endif
       endif
      causedeath=0
      deathstage=-1
      ymaxdess=0.
      if(dead.eq.1)then
      max_pctdess=0.
      endif
      endif

c1117    format(1F1.0,A,4(1F8.3,A),1F20.10,A,5(1F8.3,A),2F4.0,A,6(1F8.3,A)
c     &,1F8.0,A,1F8.0)
1117  format(43(F20.10,A),1F20.10)
1118  format(43(A13,A1),A13)

      daycount=daycount+1
      countday=countday+1
      if(daycount.eq.7300)then
      continue
      endif
c    write(i4,*) countday

      if(pond.eq.0)then
       if(stage.eq.2)then
       conth=0
       contw=0
       contdep=0
       transt='n'
       metamorph=1
       endif
      endif

      if(countday.gt.365)then
      iyear=iyear+1
      countday=1
      completion=0
      endif

      if(monthly.eq.2)then
      if(countday.gt.timeinterval)then
      goto 2001
      endif
      endif


c    if(daycount.lt.365*nyear)then
      if(iyear.le.nyear)then
      goto 2000
      endif

2001  continue

      if((aquatic.eq.1).and.(pond.eq.1))then
c     check to see if just container model is running or whether to start again and do an animal
       if(contonly.eq.0)then
        pond=0
        goto 5501
       endif
      endif

c    mi=0
c    ma=1e-5

c    calculate life table and rmax
      do 1010 i=1,nyear
      if(i.lt.nyear)then
      yearfract=1
      else
      yearfract=longev-aint(longev)
      endif
c    account for non-senescent mortality via paramteres mi, ma and mh by multiplying senescence by these values
       if(i.eq.1)then
        if(for1.eq.0)then
         surv(i)=surv(i)*mh*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for1)*(1-ma)**for1*mh*yearfract
        endif
       endif
       if(i.eq.2)then
        if(for2.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for2)*(1-ma)**for2*yearfract
        endif
       endif
       if(i.eq.3)then
        if(for3.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for3)*(1-ma)**for3*yearfract
        endif
       endif
       if(i.eq.4)then
        if(for4.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for4)*(1-ma)**for4*yearfract
        endif
       endif
       if(i.eq.5)then
        if(for5.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for5)*(1-ma)**for5*yearfract
        endif
       endif
       if(i.eq.6)then
        if(for6.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for6)*(1-ma)**for6*yearfract
        endif
       endif
       if(i.eq.7)then
        if(for7.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for7)*(1-ma)**for7*yearfract
        endif
       endif
       if(i.eq.8)then
        if(for8.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for8)*(1-ma)**for8*yearfract
        endif
       endif
       if(i.eq.9)then
        if(for9.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for9)*(1-ma)**for9*yearfract
        endif
       endif
       if(i.eq.10)then
        if(for10.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for10)*(1-ma)**for10*yearfract
        endif
       endif
       if(i.eq.11)then
        if(for11.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for11)*(1-ma)**for11*yearfract
        endif
       endif
       if(i.eq.12)then
        if(for12.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for12)*(1-ma)**for12*yearfract
        endif
       endif
       if(i.eq.13)then
        if(for13.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for13)*(1-ma)**for13*yearfract
        endif
       endif
       if(i.eq.14)then
        if(for14.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for14)*(1-ma)**for14*yearfract
        endif
       endif
       if(i.eq.15)then
        if(for15.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for15)*(1-ma)**for15*yearfract
        endif
       endif
       if(i.eq.16)then
        if(for16.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for16)*(1-ma)**for16*yearfract
        endif
       endif
       if(i.eq.17)then
        if(for17.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for17)*(1-ma)**for17*yearfract
        endif
       endif
       if(i.eq.18)then
        if(for18.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for18)*(1-ma)**for18*yearfract
        endif
       endif
       if(i.eq.19)then
        if(for19.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for19)*(1-ma)**for19*yearfract
        endif
       endif
       if(i.eq.20)then
        if(for20.eq.0)then
         surv(i)=surv(i)*yearfract
        else
         surv(i)=surv(i)*(1-mi)**(8760-for20)*(1-ma)**for20*yearfract
        endif
       endif

       if(i.eq.1)then
c      the *(Thconw/1.12) part is to penalize hatchling survival proportional to z, with no penalty for the max size (1.12)
        lx(i) = surv(i)
       else
        lx(i) = surv(i)*lx(i-1)
       endif
       mx(i) = fec(i)/2.
       R0=R0+lx(i)*mx(i)*1.
       TT=TT+i*lx(i)*mx(i)*1.
1010  continue
      if(R0.gt.0)then
      TT=TT/R0
      rmax=log(R0)/TT
      else
      TT=0
      R0=0
      endif

      yearout(1)=devtime
      yearout(2)=birthday
      yearout(3)=birthmass
      yearout(4)=monmature
      yearout(5)=monrepro
      yearout(6)=svlrepro
      yearout(7)=fecundity
      yearout(8)=clutches    
      yearout(9)=annualact
      if(deb1.eq.1)then
      yearout(10)=minED
      else
      yearout(10)=0
      endif
      yearout(11)=food(nyear)/1000
      yearout(12)=annfood/1000
      if(frogbreed.lt.4)then
      yearout(13)=fec(1)
      yearout(14)=fec(2)
      yearout(15)=fec(3)
      yearout(16)=fec(4)
      yearout(17)=fec(5)
      yearout(18)=fec(6)
      yearout(19)=fec(7)
      yearout(20)=fec(8)
      yearout(21)=fec(9)
      yearout(22)=fec(10)
      yearout(23)=fec(11)
      yearout(24)=fec(12)
      yearout(25)=fec(13)
      yearout(26)=fec(14)
      yearout(27)=fec(15)
      yearout(28)=fec(16)
      yearout(29)=fec(17)
      yearout(30)=fec(18)
      yearout(31)=fec(19)
      yearout(32)=fec(20)
      else
      yearout(13)=degday1
      yearout(14)=degday2
      yearout(15)=degday3
      yearout(16)=degday4
      yearout(17)=degday5
      yearout(18)=degday6
      yearout(19)=degday7
      yearout(20)=degday8
      yearout(21)=degday9
      yearout(22)=degday10
      yearout(23)=degday11
      yearout(24)=degday12
      yearout(25)=degday13
      yearout(26)=degday14
      yearout(27)=degday15
      yearout(28)=degday16
      yearout(29)=degday17
      yearout(30)=degday18
      yearout(31)=degday19
      yearout(32)=degday20
      endif
      yearout(33)=act1
      yearout(34)=act2
      yearout(35)=act3
      yearout(36)=act4
      yearout(37)=act5
      yearout(38)=act6
      yearout(39)=act7
      yearout(40)=act8
      yearout(41)=act9
      yearout(42)=act10
      yearout(43)=act11
      yearout(44)=act12
      yearout(45)=act13
      yearout(46)=act14
      yearout(47)=act15
      yearout(48)=act16
      yearout(49)=act17
      yearout(50)=act18
      yearout(51)=act19
      yearout(52)=act20
      yearout(53)=surv(1)
      yearout(54)=surv(2)
      yearout(55)=surv(3)
      yearout(56)=surv(4)
      yearout(57)=surv(5)
      yearout(58)=surv(6)
      yearout(59)=surv(7)
      yearout(60)=surv(8)
      yearout(61)=surv(9)
      yearout(62)=surv(10)
      yearout(63)=surv(11)
      yearout(64)=surv(12)
      yearout(65)=surv(13)
      yearout(66)=surv(14)
      yearout(67)=surv(15)
      yearout(68)=surv(16)
      yearout(69)=surv(17)
      yearout(70)=surv(18)
      yearout(71)=surv(19)
      yearout(72)=surv(20)
      yearout(73)=mintc
      yearout(74)=maxtc
      yearout(75)=Max_PctDess
      yearout(76)=longev
      yearout(77)=TT
      yearout(78)=R0
      yearout(79)=rmax
      yearout(80)=debout1(nyear*24*365,10)

      if(writecsv.gt.0)then    
      write(II5,115)yearout(1),",",yearout(2),",",year
     &out(3),",",yearout(4),",",yearout(5),",",yearout(
     &6),",",yearout(7),",",yearout(8),",",yearout(9),"
     &,",yearout(10),",",yearout(11),",",yearout(12),",
     &",yearout(13),",",yearout(14),",",yearout(15),","
     &,yearout(16),",",yearout(17),",",yearout(18),",",
     &yearout(19),",",yearout(20),",",yearout(21),",",y
     &earout(22),",",yearout(23),",",yearout(24),",",ye
     &arout(25),",",yearout(26),",",yearout(27),",",yea
     &rout(28),",",yearout(29),",",yearout(30),",",year
     &out(31),",",yearout(32),",",yearout(33),",",yearo
     &ut(34),",",yearout(35),",",yearout(36),",",yearou
     &t(37),",",yearout(38),",",yearout(39),",",yearout
     &(40),",",yearout(41),",",yearout(42),",",yearout(
     &43),",",yearout(44),",",yearout(45),",",yearout(4
     &6),",",yearout(47),",",yearout(48),",",yearout(49)
     &,",",yearout(50),",",yearout(51),",",yearout(52),
     &",",yearout(53),",",yearout(54),",",yearout(55),",
     &",yearout(56),",",yearout(57),",",yearout(58),",",
     &yearout(59),",",yearout(60),",",yearout(61),",",ye

     &arout(62),",",yearout(63),",",yearout(64),",",year

     &out(65),",",yearout(66),",",yearout(67),",",yearou

     &t(68),",",yearout(69),",",yearout(70),",",yearout(

     &71),",",yearout(72),",",yearout(73),",",yearout(74

     &),",",yearout(75),",",yearout(76),",",yearout(77),

     &",",yearout(78),",",yearout(79),",",yearout(80)
      endif

111   format(19(1F8.3,A),1F8.3)
112   format(20(1F8.3,A),1F8.3)
113   format(4(I10,A),9(1F8.3,A),I10)
114   format(4(1F8.3,A),2(E8.3,A),13(1E8.3,A),1F8.3)
115   format(79(1F10.3,A),1F10.3)    
c    write(*,*)area,tc    
c    close(i10)
c    close(i4)

      if(writecsv.eq.2)then
      close (II1)
      close (II2)
      close (II3)
      close (II4)

      endif

      if(writecsv.gt.0)then
      close (II5)

      endif
200    RETURN 
      end