      SUBROUTINE RADOUT 

      Implicit None
 
       REAL AL,ALT,AMASS,ATOT,BP,DEPSUB,EMISAN,FATOSB,FATOSK,Flshcond,
     * PTCOND,QIR2SK,QIR2SB,QCOND,QCONV,Qresp,Qsevap,QIRIN,QIROUT,
     * QMETAB,QSOLAR,R,RELHUM,SIG,SUBTK,
     * TA,TC,Tskin,TR,TSUBST,VEL,WEVAP,X,XK,H2O_BalPast
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real ir1,ir2,ir3,ir4,ir5,ir6,f23,f24,f25,f26

      integer wingmod,wingcalc

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26

      X = Tskin   
      XK = X + 273.15 

      if(wingmod.eq.2)then
      IR1=EMISAN*SIG*A2*F21*XK**4
      IR2=EMISAN*SIG*A2*F23*XK**4
      IR3=EMISAN*SIG*A2*F24*XK**4
      IR4=EMISAN*SIG*A2*F25*XK**4
      IR5=EMISAN*SIG*A2*F26*XK**4
      IR6=EMISAN*SIG*ATOT*1*XK**4
      QIROUT=IR6
      else
      QIR2SK = ATOT * FATOSK * EMISAN * SIG * XK * * 4 
      QIR2SB = ATOT * FATOSB * EMISAN * SIG * XK * * 4 
      QIROUT = QIR2SK + QIR2SB 
      endif

10    FORMAT(1X,7E10.2)

      RETURN   
      end