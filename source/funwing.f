      FUNCTION FUNWING (X) 

      Implicit None

      REAL ABSMAX,ABSMIN,ACTLVL,AL,ALT,AMASS,Amtfud
      REAL ANDENS,Area,ASIL,ASILP,ATOT
      Real BP
      Real DEPSUB,DELTAR,EMISAN,EMISSB,EMISSK,Enb,EXTREF
      Real Fatcond,FATOSB,FATOSK,Flshcond,FLTYPE,FLUID,funwing
      Real G,GEVAP,gwatph,O2MAX,O2MIN
      Real PI,PTCOND,QCOND,QCONV,QIN,QIRIN,QIROUT
      Real QMETAB,QOUT,QRESP,Qsevap,QSOLAR,QSOLR,QSWEAT
      Real R,R1,RELHUM,Rinsul,RQ
      Real shade,SIG,SPHEAT,SUBTK,TA,TC,Tlung
      Real Tdigpr,Tmaxpr,Tminpr,Tskin,TOBJ,TR,TSKY,tbask,temerge
      Real TSUBST,VEL,VOL,WEVAP,X,XBAS,Xtry,AirVol,CO2MOL
      REAL ASEMAJR,BSEMINR,CSEMINR,H2O_BalPast
      Real actxbas,acthr
      Real TPREF,A1,A2,A3,A4,A4b,A5,A6
      real customallom,MR_1,MR_2,MR_3,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f61,f13,f14,f15,f16
     &,f23,f24,f25,f26
      real alenth,awidth,aheit,av,tqsol
      real WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      real TKSKY,TKSUB,TKOBJ
      real IR1,IR2,IR3,IR4,IR5,NETQIR,f1sub,tkskysub,ir6

      INTEGER IHOUR,LIVE,Lometry,MICRO,NumFed,NumHrs,nodnum,DEB1
      integer wingmod,wingcalc

      DIMENSION customallom(8),shp(3)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WCONV/FLTYPE
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/WMET/QSWEAT
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      COMMON/ANPARMS/Rinsul,R1,Area,VOL,Fatcond
      COMMON/ELLIPS/ASEMAJR,BSEMINR,CSEMINR 
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      Common/soln/Enb
      Common/Guess/Xtry
      Common/Treg/Tc
      COMMON/WSOLAR/ASIL,Shade
      Common/Behav3/Acthr,ACTXBAS


      DATA PI/3.14159265/

C     THE GUESSED VARIABLE, X, IS CORE TEMPERATURE (C); SEE SUB. MET
C    FOR DETAILED EXPLANATION OF CALCULATION OF SURF. TEMP., Tskin, 
C     FROM TC AND MASS
c    This assumes uniform body temperature.

C    Control of body temperature guesses for stability purposes
      If (X .gt. 50.) then
c      X = 50.
       else
        If (X .lt. -3.0) then
          X = ta + 0.1
        Endif
      Endif

      Tc = X
      Xtry = X

c    ALLOM for wing

C     FLAT PLATE 
C       Assume a cube for the moment
        ALENTH = bref/100
        AWIDTH = cref/100
        AHEIT =  0.01/100       
        ATOT = ALENTH * AWIDTH * 2. + ALENTH * AHEIT * 2. 
     &   + AWIDTH * AHEIT * 2.
        AREA = ATOT
        ASILN = ALENTH * AWIDTH
        ASILP = AWIDTH * AHEIT
        AL = ALENTH
        R = ALENTH/2.
        VOL = ALENTH * AWIDTH * AHEIT
        Av = Atot/2

C     COMPUTING SURFACE TEMPERATURE AS DICTATED BY GEOMETRY

C      FLAT PLATE
c      Tskin = TC - G * R**2/(2.*Flshcond)
      Tskin=TC
  
C     Limiting skin temperature extremes               
      if (Tskin .lt. -3.0) then
        Tskin = -3.00000
      endif 

      call wings(rho1_3,absan,trans1,QSOLR,aref,bref,cref,phi,
     &F21,f31,f41,f51,f61,f12,f32,f42,f52,sidex,WQSOL,TQSOL,A1,A2,
     &A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26,asilp)

C     COMPUTING LONG WAVE INFRARED ABSORBED 
      TKSKY = Tsky + 273.15
         
      TKSUB = TSUBST + 273.15  
      TKOBJ = TC + 273.15  

c    top of wing IR
      IR1=EMISAN*SIG*(A2*F21*TKOBJ**4-A1*F12*TKOBJ**4)
      IR2=EMISAN*SIG*(A3*F31*TKOBJ**4-A1*F13*TKOBJ**4)
      IR3=EMISAN*SIG*(A4*F41*TKSKY**4-A1*F14*TKOBJ**4)
      IR4=EMISAN*SIG*(A5*F51*((TKSKY+TKSUB)/2.)**4-A1*F15*TKOBJ**4)
      IR5=EMISAN*SIG*(A6*F61*((TKSKY+TKSUB)/2.)**4-A1*F16*TKOBJ**4)

c    formula for surface of a finite rectangle tilted relative to an infinite plane
c    if phi le 90 then wing obscured from substrate by body/other wing so make f1sub=0
      if(phi.le.90)then
       f1sub=0
      else
       f1sub=(1-cos((180-phi)*pi/180))/2.
      endif

c    bottom of wing IR
      TKSKYSUB=TKSUB*f1sub+TKSKY*(1-f1sub)
      IR6=EMISAN*SIG*(A1*TKSKYSUB**4-A1*TKOBJ**4)
      NETQIR=IR1+IR2+IR3+IR4+IR5+IR6
       

      CALL CONV 
c      CALL RADOUT 

    
      QIN = WQSOL + NETQIR
      QOUT = Qconv
C    Finding the deviation from zero in guessing the solution
      Enb = QIN - QOUT   
      FUNwing = Enb  

c10    FORMAT(1X,7E10.3)
      RETURN   
      END
