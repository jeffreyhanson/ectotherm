      SUBROUTINE DSUB (NEQ, T, Y, YDOT)   

C    VERSION 27 July, 2006 COPYRIGHT W. P. PORTER All Rights Reserved.
c    The call to this subroutine from the Gear integrator refers to this 
c    subroutine as Subroutine F, i.e. Call F(NEQ, TN, Y, SAVF)

      Implicit None
  
      EXTERNAL FUN,funskin    

c    This program uses the solution for uniform internal heat generation 
c    for all geometries, which yields a parabolic temperature profile internally.
c    There is no breaking the animal into internal nodes any more.
c    Average internal temperature is the average over the integral from core to skin.

C    PROGRAM FOR LSODE, AN ADAMS PREDICTOR CORRECTOR (GEAR) 
C        INTEGRATOR.
C    THIS SUBROUTINE CONTAINS TIME DEPENDENT EQUATIONS 
C    (ALGEBRAIC & DERIVATIVES) USED BY LSODE.
C    DSUB IS WHERE THE INTEGRATOR COMES TO GET ALL VALUES AS A     
C    FUNCTION OF TIME (OR OTHER INDEPENDENT VARIABLE).  EACH   
C    STEP FORWARD IN TIME REQUIRES FOUR DIFFERENT TIMES WHEN ANIMAL
C    (AND ENVIRONMENT) TIME DEPENDENT VARIABLES MUST BE OBTAINED   
C    OR COMPUTED.  THE INTEGRATOR TREATS Y'S AS VARIABLES AND  
C    YDOT'S AS FIRST ORDER DERIVATIVES.  SECOND ORDER OR HIGHER  
C    DERIVATIVES CAN BE EXPRESSED AS FIRST ORDER DERIVATIVES   
C    AS ILLUSTRATED ON P. 134 OF FORSYTHE, MALCOLM & MOLER'S 1977  
C    TEXT ON COMPUTER METHODS FOR MATHEMATICAL CALCULATIONS.   
C    (PRENTICE HALL)  

c    Neq = number of equations (nodes) for animal
c    T = time (minutes)
c    Y = Tcore (C)
c    Ydot = dTcore/dTime 

C    NOTE: VARIABLE ACTIVITY DEPENDING ON HOUR OF THE DAY NOT YET IN DSUB.FOR

      DOUBLE PRECISION  T, Y, YDOT 

      REAL ABSAN,ABSMAX,ABSMIN,ABSSB,AIND,AL,ALT,AMASS
      REAL ANDENS,ASEMAJR,ASIL,ASILN,ASILP,ASQ,ATOT
      Real ALENTH,AWIDTH,AHEIT
      Real BSEMINR,BP,BSQ,CONST,CPMIN,CSEMINR,CSQ
      REAL DELTAR,DEPSUB
      Real EMISAN,EMISSB,EMISSK,Enary1,Enary2,Enary3,Enary4
      REAL Enary5,Enary6,Enary7,Enary8
      Real EXTREF,FUN,FATOBJ,FATOSB,FATOSK,Flshcond,FLUID
      Real G,GEVAP,GMASS,GN,gwatph,O2MAX,O2MIN,PCTDIF,PTCOND
      Real QCOND,QCONV,QGENET,Qsevap,QIN,QIRIN,QIROUT
      Real QMETAB,QOUT,QRESP,QSOL,QSOLR,QSOLAR,QST
      Real R,RELHUM,RFLESH,RH,RQ
      Real S2,SHADE,SIG,SOIL1,SOIL3,SPHEAT,SUBTK
      Real TA,Taloc,Tannul,TC,Tcpast,TI,TIME,Tlung,Tskin,TOBJ
      Real TPAST,TR,TREF,TSKY,TskyC,TSUB,TSUBST,Tym
      Real VAR,VEL,VOL,VREF,WC,WCMIN,WEVAP,XBAS,XCALC,Z,ZEN
      Real Rinsul,R1,Area,Fatcond,AirVol,CO2MOL
      REAL TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,tbask,temerge,R2
      Real tcinit,pi,MLO2,GH2OMET,H2O_BalPast,TWING,massleft,volumeleft

      REAL Enary9,Enary10,Enary11,Enary12,Enary13,Enary14,Enary15
      REAL Enary16,Enary17,Enary18,Enary19,Enary20,Enary21
      REAL Enary22,Enary23,Enary24,Enary25,Enary26,Enary27
      Real Enary28,Enary29,Enary30,Enary31,Enary32,Enary33
      Real Enary34,Enary35,Enary36,Enary37,Enary38,Enary39
      Real Enary40,Enary41,Enary42,Enary43,Enary44,Enary45
      Real Enary46,Enary47,Enary48
      Real Transar
      Real TPREF,TB,j,kTC,hr
      real customallom,debqmet,MR_1,MR_2,MR_3,shp
      real MLO2_init,GH2OMET_init,debqmet_init,dryfood,faeces,nwaste
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,rhref

      real HC,Trad,HD,WB,DP,E,ESAT,VD,RW,TVIR,TVINC,TOTLEN,AV,AT,  
     * DENAIR,CP,WTRPOT,HTOVPR,CONTDEPTH,shdgrass
      real PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,AEYES,SkinW,convar
     &,SkinT,VDAIR,VDSURF,TAVE,RHUM,x1,x2,x,testx,zbrent,enberr,reftol
      real CONTH,CONTW,CONTVOL,CONTDEP,conthole,fieldcap,wilting,contwet

      INTEGER ITYM,JTYM,ICLIM,IDAY,IHOUR,JP
      INTEGER LIVE,Lometry,MICRO,NDAY,NEQ,Nodnum
      Integer NM,NumFed,NumHrs,soilmoisture,pond
      Integer IT,SCENAR,DEB1,wingmod,wingcalc,WETMOD,contonly,contype

      Character*1 tranin
      logical succes
      
      DIMENSION TIME(25),Y(10),YDOT(10)
      DIMENSION QSOL(25),RH(25),TskyC(25),SOIL1(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION TI(25),rhref(25)
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
      Dimension TRANSAR(5,25),shdgrass(25)
      DIMENSION customallom(8),shp(3)
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &    FAECES(24),NWASTE(24)

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
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/WSOLAR/ASIL,Shade
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
     & ,shdgrass
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/TRANS/ICLIM,JP
      COMMON/DAYITR/NDAY,IDAY
      Common/Dsub1/Enary1,Enary2,Enary3,Enary4,Enary9,Enary10,Enary11,
     &    Enary12,Enary17,Enary18,Enary19,Enary20,Enary21,Enary22
     &   ,Enary23,Enary24,Enary25,Enary26,Enary27,Enary28,Enary45
     &   ,Enary46,Enary47,Enary48
      Common/Dsub2/Enary5,Enary6,Enary7,Enary8,Enary13,Enary14,Enary15
     &    ,Enary16,Enary29,Enary30,Enary31,Enary32,Enary33,Enary34
     &   ,Enary35,Enary36,Enary37,Enary38,Enary39,Enary40,Enary41
     &    ,Enary42,Enary43,Enary44
      COMMON/DSUB3/AIND,VAR
      Common/Treg/Tc
      COMMON/ELLIPS/ASEMAJR,BSEMINR,CSEMINR
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/TRANSIENT/tcinit,transar
      COMMON/TRANINIT/tranin
      common/outsub/IT
      common/scenario/scenar
      COMMON/DEBRESP/MLO2,GH2OMET,debqmet,MLO2_init,GH2OMET_init,
     &    debqmet_init,dryfood,faeces,nwaste
      COMMON/EVAP1/PCTEYE,WEYES,WRESP,WCUT,AEFF,CUTFA,HD,AEYES,SkinW
     &,SkinT,HC,convar
      COMMON/WCOND/TOTLEN,AV,AT
      COMMON/CONTDEPTH/CONTDEPTH
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,WETMOD,contonly,conthole
     &,contype,contwet
      common/soilmoist/fieldcap,wilting,soilmoisture
      common/pondtest/pond

      DATA TI/0.,60.,120.,180.,240.,300.,360.,420.,480.,540.,600.,660.,
     &720.,780.,840.,900.,960.,1020.,1080.,1140.,1200.,1260.,1320.,
     &1380.,1440./

C    GRAMS OF MASS
      GMASS = AMASS*1000.
      RFLESH = R1
      PI = 3.141592
      volumeleft=vol
      massleft=amass

C     TIME DEPENDENT TEMPERATURE OR HEAT GEN./UNIT VOLUME
        If((TRANIN .eq. 'n') .or. (TRANIN .eq. 'N'))then
          TC=tcinit
         else
          TC=real(Y(1),4)
        Endif
C    GUESS FOR CORE TEMPERATURE
c    TC = X   

      IHOUR = INT((T/60.)) + 1

      If (T .GT. 0.) THEN
c      If Time > ), then do something.
        IF (ICLIM .EQ. 0) THEN
C        KEYBOARD INPUT; SENSITIVITY ANALYSIS
            TSUBST = TSUB(2)
C           INFRARED CALCULATIONS?
C           CHECKING TO SEE IF WATER ENVIRONMENT
            IF (FLUID .EQ. 0.0) THEN 
C             AIR ENVIRONMENT.  Above ground or below ground conditions should be set/stored
C            Tsky & Tsubst stored and set according to behaviors in Steady State run prior to this.
              CALL ALLOM
              CALL RADIN
              IF(QSOL(IHOUR).GT. 0.00000)THEN
                Call Solar
              ENDIF  
             ELSE
C            WATER ENVIRONMENT
              FATOBJ = 0.00
              FATOSK = 0.00
              FATOSB = 0.00
              QIRIN = 0.00
            ENDIF
         ELSE
C        MICROMET INPUT
C        GET HEAT FLUXES FOR ENERGY BALANCE EQUATION FOR NEXT TIME
C         INTERPOLATING VALUES OF DRIVING VARIABLES FOR 
C        INTERMEDIATE TIME

c        Calculating environmental conditions for the 
c        next time (increment), T, into the future as specified by the integrator.
c        Integrator is running using environmental conditions determined by
c        steady state solutions of the animal's energetics.
          Tym = real(T,4)
          if(Tym.gt.1440.)then
            Tym = 1440.
          endif
c        Doing table look up to get the environmental conditions at the time 
c        the integrator wants to use.
          AIND = TYM
      IF(SCENAR .eq. 1)THEN
           DO 100 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value 
              QSOLR =Enary1(JTYM)-CONST*(Enary1(JTYM)- Enary1(ITYM))
              ZEN =(Enary2(JTYM)*PI/180.)-CONST*((Enary2(JTYM)*PI/180.
     &)-(Enary2(ITYM))*PI/180.)
              TA =Enary3(JTYM)-CONST*(Enary3(JTYM)- Enary3(ITYM))
              VEL =Enary4(JTYM)-CONST*(Enary4(JTYM)- Enary4(ITYM))
              RELHUM =Enary5(JTYM)-CONST*(Enary5(JTYM)- Enary5(ITYM))
              TSUBST =Enary7(JTYM)-CONST*(Enary6(JTYM)- Enary6(ITYM))
              TSKY =Enary6(JTYM)-CONST*(Enary7(JTYM)- Enary7(ITYM))
              SHADE =Enary8(JTYM)-CONST*(Enary8(JTYM)- Enary8(ITYM))  
              GO TO 101
            ENDIF  
  100     CONTINUE
      Endif
      IF(SCENAR .eq. 2)THEN
C        Get the time dependent variables
          DO 110 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value
              QSOLR =Enary9(JTYM)-CONST*(Enary9(JTYM)- Enary9(ITYM))
              ZEN =(Enary10(JTYM)*PI/180.)-CONST*((Enary10(JTYM)*PI/180.
     &)-(Enary10(ITYM))*PI/180.)
              TA =Enary11(JTYM)-CONST*(Enary11(JTYM)- Enary11(ITYM))
              VEL =Enary12(JTYM)-CONST*(Enary12(JTYM)- Enary12(ITYM))
              RELHUM =Enary13(JTYM)-CONST*(Enary13(JTYM)- Enary13(ITYM))
              TSUBST =Enary15(JTYM)-CONST*(Enary14(JTYM)- Enary14(ITYM))
              TSKY =Enary14(JTYM)-CONST*(Enary15(JTYM)- Enary15(ITYM))
              SHADE =Enary16(JTYM)-CONST*(Enary16(JTYM)- Enary16(ITYM)) 
              GO TO 101
            ENDIF  
  110     CONTINUE
        ENDIF
      IF(SCENAR .eq. 3)THEN
C        Get the time dependent variables
          DO 120 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value 
              QSOLR =Enary17(JTYM)-CONST*(Enary17(JTYM)- Enary17(ITYM))
              ZEN =(Enary18(JTYM)*PI/180.)-CONST*((Enary18(JTYM)*PI/180.
     &)-(Enary18(ITYM))*PI/180.)
              TA =Enary19(JTYM)-CONST*(Enary19(JTYM)- Enary19(ITYM))
              VEL =Enary20(JTYM)-CONST*(Enary20(JTYM)- Enary20(ITYM))
              RELHUM =Enary21(JTYM)-CONST*(Enary21(JTYM)- Enary21(ITYM))
              TSUBST =Enary23(JTYM)-CONST*(Enary22(JTYM)- Enary22(ITYM))
              TSKY =Enary22(JTYM)-CONST*(Enary23(JTYM)- Enary23(ITYM))
              SHADE =Enary24(JTYM)-CONST*(Enary24(JTYM)- Enary24(ITYM))  
              GO TO 101
            ENDIF  
  120     CONTINUE
        ENDIF
      IF(SCENAR .eq. 4)THEN
C        Get the time dependent variables
          DO 130 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value 
              QSOLR =Enary25(JTYM)-CONST*(Enary25(JTYM)- Enary25(ITYM))
              ZEN =(Enary26(JTYM)*PI/180.)-CONST*((Enary26(JTYM)*PI/180.
     &)-(Enary26(ITYM))*PI/180.)
              TA =Enary27(JTYM)-CONST*(Enary27(JTYM)- Enary27(ITYM))
              VEL =Enary28(JTYM)-CONST*(Enary28(JTYM)- Enary28(ITYM))
              RELHUM =Enary29(JTYM)-CONST*(Enary29(JTYM)- Enary29(ITYM))
              TSUBST =Enary31(JTYM)-CONST*(Enary30(JTYM)- Enary30(ITYM))
              TSKY =Enary30(JTYM)-CONST*(Enary31(JTYM)- Enary31(ITYM))
              SHADE =Enary32(JTYM)-CONST*(Enary32(JTYM)- Enary32(ITYM))  
              GO TO 101
            ENDIF  
  130     CONTINUE
        ENDIF
      IF(SCENAR .eq. 5)THEN
C        Get the time dependent variables
          DO 140 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value 
              QSOLR =Enary33(JTYM)-CONST*(Enary33(JTYM)- Enary33(ITYM))
              ZEN =(Enary34(JTYM)*PI/180.)-CONST*((Enary34(JTYM)*PI/180.
     &)-(Enary34(ITYM))*PI/180.)
              TA =Enary35(JTYM)-CONST*(Enary35(JTYM)- Enary35(ITYM))
              VEL =Enary36(JTYM)-CONST*(Enary36(JTYM)- Enary36(ITYM))
              RELHUM =Enary37(JTYM)-CONST*(Enary37(JTYM)- Enary37(ITYM))
              TSUBST =Enary39(JTYM)-CONST*(Enary38(JTYM)- Enary38(ITYM))
              TSKY =Enary38(JTYM)-CONST*(Enary39(JTYM)- Enary39(ITYM))
              SHADE =Enary40(JTYM)-CONST*(Enary40(JTYM)- Enary40(ITYM))  
              GO TO 101
            ENDIF  
  140     CONTINUE
        ENDIF
          IF(SCENAR .gt. 5)THEN
C        Get the time dependent variables
          DO 150 ITYM = 2,25
c          Search the times for time greater than current integrator time
            IF(AIND.LE. TI(ITYM)) THEN 
c            Set the counter for the time value below current integrator time  
              JTYM=ITYM-1
c            Compute the location in the interval between higher and lower time
              CONST=(TI(JTYM)-AIND)/(TI(JTYM)-TI(ITYM))
c            Interpolate linearly the dependent value 
              QSOLR =Enary41(JTYM)-CONST*(Enary41(JTYM)- Enary41(ITYM))
              ZEN =(Enary42(JTYM)*PI/180.)-CONST*((Enary42(JTYM)*PI/180.
     &)-(Enary42(ITYM))*PI/180.)
              TA =Enary43(JTYM)-CONST*(Enary43(JTYM)- Enary43(ITYM))
              VEL =Enary44(JTYM)-CONST*(Enary44(JTYM)- Enary44(ITYM))
              RELHUM =Enary45(JTYM)-CONST*(Enary45(JTYM)- Enary45(ITYM))
              TSUBST =Enary47(JTYM)-CONST*(Enary46(JTYM)- Enary46(ITYM))
              TSKY =Enary46(JTYM)-CONST*(Enary47(JTYM)- Enary47(ITYM))
              SHADE =Enary48(JTYM)-CONST*(Enary48(JTYM)- Enary48(ITYM))  
              GO TO 101
            ENDIF  
  150     CONTINUE
        ENDIF
  101     CONTINUE 

c        check that temperature isn't going haywire Kearney added this during the frog workshop 17/9/2012
          if((tc.lt.-150).or.(tc.gt.150))then
           TC=Ta
           y(1)=tc
          endif


          TR = (TSUBST + TSKY)/2.

          TOBJ = TSUBST
C         INFRARED CALCULATIONS?
C         CHECKING TO SEE IF WATER ENVIRONMENT
          IF (FLUID .EQ. 0.0) THEN 
C           AIR ENVIRONMENT.  Tsky & Tsubst stored and set according to behaviors in Steady State run prior to this.
            CALL ALLOM
            CALL RADIN
            Call Solar
           ELSE
C          WATER ENVIRONMENT
            FATOBJ = 0.00
            FATOSK = 0.00
            FATOSB = 0.00
            QIRIN = 0.00
          ENDIF

C        QIN
C         GETTING HEAT FLUXES BASED ON CURRENT TEMPERATURES 
C        ONE LUMP MODEL
          QIN = QSOLAR + QIRIN
c        m**3 = kg/(kg/m**3)
        ENDIF

C      GETTING SURFACE HEAT FLUXES BASED ON CURRENT SKIN TEMP'S
C      AND GETTING RESP. EVAP LOSS BASED ON CURRENT CORE TEMP
        TC = real(Y(1),4)
c      If(SCENAR .gt. 5)Then
c       IF((TRANSAR(1,IHOUR).gt.TDIGPR).and.(TRANSAR(2,IHOUR).le.TDIGPR
c     &       ))THEN
c        TC=TDIGPR
c       Endif
c      Endif
        TB=TC
        if(live.eq.0)then
         QMETAB = 0.
         QRESP = 0.
         QGENET = 0.
         GN = 0.
        else
         IF(TC .gt. 50)THEN
          TB = 50
         ENDIF
         QMETAB = 0.0056*10.**(0.038*(TB)-1.771)*GMASS**.82
         QMETAB = QMETAB*XBAS
c       Using integrated average of body temperatures for first estimate to get Qresp
         Tlung = (Tc + Tskin)/2.
         CALL RESP
         QGENET = Qmetab-Qresp
         GN = QGENET/VOL
        endif

        IF((LOMETRY.EQ.1).OR.(LOMETRY.EQ.3).OR.(LOMETRY.EQ.5))THEN
C        USE CYLINDER: TC - TSKIN = (GENPV*R^2)/(4*FLSHCOND)
          TSKIN = TC - (GN*Rflesh**2)/(4*FLSHCOND)
          Tlung = (Gn*Rflesh**2)/(8.*Flshcond) + Tskin
        ENDIF
        IF((LOMETRY.EQ.2).OR.(LOMETRY.EQ.4))THEN
          ASQ = ASEMAJR**2
          BSQ = BSEMINR**2
          CSQ = CSEMINR**2
          Tskin = TC - (Gn/(2.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &    (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ))
          Tlung = (Gn/(4.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &    (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ)) + Tskin
        ENDIF    

C       CHECKING FOR FLUID TYPE
        IF (FLUID .EQ. 0.00) THEN
C        AIR
          Y(2) = TSKIN
          CALL CONV 
          if(live.eq.1)then
           CALL RESP
          endif
          CALL Sevap
          CALL RADOUT 
          CALL COND 
         ELSE
C        WATER
          Qsevap = 0.00
          WEVAP = 0.0
          QIROUT = 0.00
          QCOND = 0.00
        ENDIF

C      QOUT
C        ONE LUMP MODEL
          QOUT = QCONV + Qsevap + QIROUT + QRESP + QCOND

c       C/min = (J/s)*min*(s/min)/(kg/m^3*m^3*J/kg-C)
c       Integrator multiplies by number of minutes:Ydot(n) in J/min
         YDOT(1)=((QIN + QGENET - QOUT)*60.)/(AMASS*SpHeat)

      Trad=(QIRIN/(EMISAN*1*CONVAR*SIG))**(1./4.)-273.15
      HTOVPR = 2.5012E+06 - 2.3787E+03 * Ta
      Tave=(Trad+Tskin)/2.
      HR=4*EMISAN*SIG*(Tave+273)**3
C    INITIALIZING FOR SUB. WETAIR
      WB = 0.0
      DP = 999.         
         CALL WETAIR(TA,WB,RELHUM,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,  
     * DENAIR,CP,WTRPOT)
      VDAIR = VD
      RHUM=100.
         CALL WETAIR(TSKIN,WB,RHUM,DP,BP,E,ESAT,VD,RW,TVIR,TVINC,  
     * DENAIR,CP,WTRPOT)
      VDSURF = VD
      
      if((lometry.eq.4).or.(lometry.eq.2))then
      S2=((ASQ*BSQ*CSQ)/(ASQ*BSQ+ASQ*CSQ+BSQ*CSQ))    
       j=(1/(AMASS*SpHeat))*((QSOLAR+QGENET+CONVAR*(((GN*S2)/(2*
     &Flshcond))*(HC+HR)+HC*Ta+HR*Trad)+AEFF*(HD*HTOVPR*VDAIR))+
     &((SUBTK*AV)/(ALENTH/2))*((GN*S2)/(2*Flshcond)+Tsubst))
      else
      if(pond.eq.0)then
      R2=(ALENTH/2)**2
       j=(1/(AMASS*SpHeat))*((QSOLAR+QGENET+CONVAR*(((GN*R2)/(4*
     &Flshcond))*(HC+HR)+HC*Ta+HR*Trad)+AEFF*(HD*HTOVPR*VDAIR))+
     &((SUBTK*AV)/0.025)*((GN*R2)/(4*Flshcond)+Tsubst)) 
      else
      R2=(CONTDEP/1000/2)**2
      j=(1/(AMASS*SpHeat))*((QSOLAR+QGENET+CONVAR*(((GN*R2)/(4*
     &Flshcond))*(HC+HR)+HC*Ta+HR*Trad)+AEFF*(HD*HTOVPR*VDAIR))+
     &((SUBTK*AV)/0.025)*((GN*R2)/(4*Flshcond)+Tsubst)) 
      endif
      endif
      
      if(pond.eq.1)then
      kTC=(CONVAR*(TC*HC+TC*HR)+AEFF*HD*HTOVPR*VDSURF+TC*((SUBTK*AV)
     &/0.025))/(AMASS*SpHeat)
      else
      kTC=(CONVAR*(TC*HC+TC*HR)+AEFF*HD*HTOVPR*VDSURF+TC*((SUBTK*AV)
     &/(ALENTH/2)))/(AMASS*SpHeat)
      endif
      
      YDOT(1)=(j-kTc)*60
      
c     now get the skin temperature with zbrent         
         x1 = -50.
         x2 = 80.
 
C      GUESSING FOR CORE TEMPERATURE
        X = TA
        CALL ZBRAC(FUNskin,X1,X2,SUCCES)
C       INVOKING THE ENERGY BALANCE EQUATION VIA ZBRENT AND FUN 

      TESTX = Enberr
      reftol =testx
        X = ZBRENT(FUNskin,X1,X2,Testx)
C       OUT COMES THE GUESSED VARIABLE VALUE (X) THAT SATISFIES  
C       THE ENERGY BALANCE EQUATION   
C       DEPENDENT VARIABLE GUESSED FOR EITHER CORE TEMP. OR METABOLISM
C      GUESS FOR CORE TEMPERATURE
c        Tskin=Tc+0.1
         Tskin = x
        Tlung = (Tskin+Tc)/2.         
      CALL Sevap   
C       QSTORED (J/s) = kg/m^3* m^3* J/kg-C* C/(min*s/min)
         QST = AMASS*SpHeat*real(YDOT(1),4)/((real(T,4) - TPAST)*60.)


C      DEPENDENT VARIABLE GUESSED FOR: CORE TEMP. OR METABOLISM
C          IF (VARGES .EQ. 0.0) THEN
C        CORE TEMPERATURE; METABOLISM FIXED
C            YDOT(1) = (QSOLAR+QIRIN+QMETAB-Qsevap-QIROUT-
C     *      QCONV-QCOND)/WCMIN          
C        ELSE
C        HEAT GENERATION RATE/UNIT VOLUME; FIXED CORE TEMP.
C            YDOT(1) = -(QSOLAR+QIRIN-Qsevap-QIROUT-
C     *      QCONV-QCOND)/(AMASS/ANDENS)
C          ENDIF
        TPAST = real(T,4)

        IF (NDAY .GT. 1) THEN
C        DO NOT INITIALIZE, USE CURRENT VALUES AS NEXT DAY'S 
C        INITIAL CONDITIONS
          GO TO 200
        ENDIF
       else
C    ***********************************************************
c      Time = 0.0:  INITIALIZE

C       CONVERTING TO MINUTES FOR LSODE, BECAUSE ALL HEAT FLUXES 
C      IN WATTS (J/S)  
        CPMIN = SPHEAT*60.
C      ONE LUMP MODEL
        WCMIN = AMASS*CPMIN

        QSOLR=Enary1(1)
        Zen=Enary2(1)
        Ta=Enary3(1)
        Vel=Enary4(1)
        Relhum=Enary5(1)
        Tsubst=Enary7(1)
        Tsky=Enary6(1)
        Shade=Enary8(1)

C       DTEMP/DTIME; TORSO TRANSIENT ENERGY BALANCE; ACTUALLY THIS
C       IS ONLY DTEMP/1 minute; DTIME IS DETERMINED BY THE UNITS OF SPECIFIC
C      HEAT AS ILLUSTRATED FOR CPMIN

C      USING AIR TEMPERATURE TO SET INITIAL ESTIMATE OF BODY TEMPERATURE AT MIDNIGHT
c        If((TRANIN .eq. 'n') .or. (TRANIN .eq. 'N'))then
c          Y(1)=tcinit
c          Y(2)=tcinit
c          XCALC = tcinit
c         else
c          XCALC = TA
c          Y(1)= TA
c        Endif

C       DTEMP/DTIME (YDOT) AND TEMPERATURE PROFILE THROUGH THE BODY 
C      STEADY STATE INITIAL VALUES OF CORE (TC) AND SKIN (TO) DONE
        TC = real(Y(1),4)
        if(live.eq.0)then
         QMETAB = 0.
         QRESP = 0.
         QGENET = 0.
         GN = 0.
        else
         IF(TC .gt. 50)THEN
          TB = 50
         ENDIF
         QMETAB = 0.0056*10.**(0.038*(TB)-1.771)*GMASS**.82
c       Sceloporus
c       QMETAB = EXP(-10+0.51*log(+0.0001)+0.115*GMASS)
         QMETAB = QMETAB*XBAS
c       Using integrated average of body temperatures for first estimate to get Qresp
         Tlung = (Tc + Tskin)/2.
         CALL RESP
C       HEAT GENERATION PER UNIT VOLUME
         QGENET = Qmetab-Qresp
         GN = QGENET/VOL
        endif

        IF((LOMETRY.EQ.1).OR.(LOMETRY.EQ.3).OR.(LOMETRY.EQ.5))THEN
C        USE CYLINDER: TC - TSKIN = (GENPV*R^2)/(4*FLSHCOND)
          TSKIN = TC - (GN*Rflesh**2)/(4*FLSHCOND)
          Tlung = (Gn*Rflesh**2)/(8.*Flshcond) + Tskin
        ENDIF
        IF((LOMETRY.EQ.2).OR.(LOMETRY.EQ.4))THEN
          Tskin = TC - (Gn/(2.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &    (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ))
          Tlung = (Gn/(4.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &    (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ)) + Tskin
        ENDIF 
        Tpast = real(T,4)
c      End of calculations for when Time > 0.
      Endif

      TcPAST = Tc
c     wevap is g/s so convert to kg/min 
      Massleft=amass-wevap/1000.*60.
      if(massleft.le.0.)then
      massleft=0.
      contdepth=0.
      else
      volumeleft=(massleft/andens)
      contdepth=volumeleft/(pi*(contw/2./100.)**2.)
      contdepth=contdepth*1000.
      endif
      contdep=contdepth
      if(contdep.lt.0.01)then
          contdep=0.01
          skinw=0.
      endif

      if(soilmoisture.eq.1)then
       if(pond.eq.1)then
        skinw=contwet*EXP((CONTDEP-fieldcap)/(wilting))
        if(skinw.lt.0)then
            skinw=0
        endif
        if(skinw.gt.contwet)then
            skinw=contwet
        endif
       endif
      endif

c10    FORMAT('DSUB: IHOUR,T,X,YDOT(1) = ',1X,1I4,2F7.1,1E12.4)
c21    FORMAT(1X,'Tskin,TC,G,R,Flshcond = ',5E12.4)
c22    FORMAT(1X,7E11.3)
c23    FORMAT(1X,1I3,6E11.3)
c24    FORMAT(1X,7E11.3)

      if(T.gt.7)then
      continue
      endif

200   RETURN 
      END

