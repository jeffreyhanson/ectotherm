      SUBROUTINE COND 
C     SUBROUTINE FOR CALCULATING HEAT TRANSFER TO THE SUBSTRATE   
C    FOR A GARTER SNAKE, THAMNOPHIS ELEGANS

      Implicit None

      REAL AL,ALT,AMASS,ANDENS,ASILP,ATOT,AV
      REAL BP,DEPSEL,DEPSUB,EMISAN
      Real EMISSB,EMISSK,FATOSB,FATOSK,Flshcond,FLUID,G
      Real PTCOND,QCOND,QCONV,Qresp
      Real QSEVAP,QIRIN,QIROUT,QMETAB,QSOLAR
      Real R,RELHUM,SIG,SUBTK
      Real TA,TCORES,TSHSOI,Tskin,TOTLEN,TR,TSOIL,TSUBST,VEL
      Real WEVAP,Xtry,ZSOIL,AT
      real customallom,H2O_BalPast,shp,subtk2
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,twing,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16

      Integer IHOUR,NumFed,NumHrs,Lometry,nodnum,wingmod,wingcalc
       
      DIMENSION TSOIL(25),TSHSOI(25),ZSOIL(10),DEPSEL(25*365*20)
     &,Tcores(25)
      DIMENSION customallom(8),shp(3)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      COMMON/DEPTHS/DEPSEL,Tcores
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WCOND/TOTLEN,AV,AT 
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
      Common/Guess/Xtry
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp

C       Soil thermal cond. (SUBTK =0.35W/M-C) defined on line 296 in Ecto2006d. 
c      Wood also has a thermal cond. 0.10-0.35 W/M-C
C      CHANGE SUBSTRATE THERMAL CONDUCTIVITY DEPENDING ON WHETHER ON THE GROUND OR NOT.
        IF(DEPSEL(IHOUR).GT. 0.0000)THEN
C        ABOVE GROUND - USE WOOD THERMAL CONDUCTIVITY FOR SUBSTRATE
          SUBTK2 = 0.10
         ELSE
C        USE STANDARD SOIL THERMAL CONDUCTIVITY
          SUBTK2 = SUBTK
        ENDIF

        QCOND = AV * (SUBTK2/(ZSOIL(2)/100.)) * (Tskin - Tsubst)

      RETURN   
      END

