      SUBROUTINE RADIN 

      Implicit None

      REAL ABSAN,ABSSB,AL,AMASS,ANDENS,ASIL,ASILN,ASILP,ATOT
      REAL DEPSUB,EMISAN,EMISSB,EMISSK
      REAL FATOBJ,FATOSB,FATOSK,Flshcond,FLUID,FSKY,G
      REAL PTCOND,PCTDIF,QCOND,QCONV,QIRIN,QIROBJ,QIROUT
      REAL QIRSKY,QIRSUB,QMETAB,Qresp,Qsevap,QSOL,QSOLAR,QSOLR,RELHUM
      Real RH,Shade,SIG,SOIL1,SOIL3,SUBTK
      REAL TA,Tannul,Taloc,Time,TKOBJ,TKSKY
      Real TKSUB,TOBJ,Tref,Tsub,TSKY,TskyC,TSUBST
      Real VEL,Vref,WC,Z,ZEN
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real ir1,ir2,ir3,ir4,ir5,ir6,rhref

      INTEGER IHOUR,MICRO,NM,wingmod,wingcalc

      DIMENSION TIME(25),QSOL(25),RH(25),TskyC(25),SOIL1(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)

      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/WSOLAR/ASIL,Shade

C     COMPUTING LONG WAVE INFRARED ABSORBED 
      TKSKY = Tsky + 273.15
         
      TKSUB = TSUBST + 273.15 

      if(wingmod.eq.2)then
c    currently assuming posture is parallel to the ground and that the view through surfaces 5 and 6 are half ground and half sky
      TKOBJ = TWING + 273.15
c    top of thorax IR
      IR1=EMISAN*SIG*A2*F12*TKOBJ**4
      IR2=EMISAN*SIG*A2*F32*TKOBJ**4
      IR3=EMISAN*SIG*A2*F42*TKSKY**4
      IR4=EMISAN*SIG*A2*F52*((TKSKY+TKSUB)/2.)**4
      IR5=EMISAN*SIG*A2*F52*((TKSKY+TKSUB)/2.)**4

c    bottom of thorax IR
      IR6=EMISAN*1*ATOT*EMISSB*SIG*TKSUB**4 

      QIRIN=IR1+IR2+IR3+IR4+IR5+IR6
      else     
      TKOBJ = TSUBST + 273.15  

      FSKY  = FATOSK - FATOBJ  
      IF (FSKY .LT. 0.000) THEN
        FSKY = 0.0
      ENDIF

      QIROBJ = EMISAN * FATOBJ * ATOT * EMISSB * SIG * TKOBJ * * 4 
      QIRSKY = EMISAN * FSKY * ATOT * EMISSK * SIG * TKSKY * * 4   
      QIRSUB = EMISAN * FATOSB * ATOT * EMISSB * SIG * TKSUB * * 4 

      QIRIN = QIRSKY + QIRSUB + QIROBJ     
      endif


      

c10    FORMAT(1X,7E10.3)

      RETURN   
      END
