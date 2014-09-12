      FUNCTION FUN (X) 

      Implicit None

      REAL A,ABSMAX,ABSMIN,ACTLVL,AL,ALT,AMASS,Amtfud
      REAL ANDENS,Area,ASIL,ASILP,ASQ,ATOT
      Real B,BP,BSQ,C,CSQ
      Real DEPSUB,DELTAR,EMISAN,EMISSB,EMISSK,Enb,EXTREF
      Real Fatcond,FATOSB,FATOSK,Flshcond,FLTYPE,FLUID,FUN
      Real G,GEVAP,GN,gwatph,O2MAX,O2MIN
      Real PI,PTCOND,QCOND,QCONV,Qgenet,QIN,QIRIN,QIROUT
      Real QMETAB,QOUT,QRESP,Qsevap,QSOLAR,QSOLR,QSWEAT
      Real R,R1,RELHUM,Rflesh,Rinsul,RQ,Rskin
      Real S1,shade,SIG,SPHEAT,SUBTK,TA,TC,Tlung
      Real Tdigpr,Tmaxpr,Tminpr,Tskin,TOBJ,TR,TSKY,tbask,temerge
      Real TSUBST,VEL,VOL,WEVAP,X,XBAS,Xtry,AirVol,CO2MOL
      REAL ASEMAJR,BSEMINR,CSEMINR,H2O_BalPast
      Real TPREF
      real customallom,MR_1,MR_2,MR_3,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16
      real flytime,flyspeed,flymetab

      INTEGER IHOUR,LIVE,Lometry,MICRO,NumFed,NumHrs,nodnum,DEB1
      integer wingmod,wingcalc,flight,flyer,flytest

      dimension customallom(8),shp(3)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
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
      COMMON/fly/flytime,flight,flyer,flytest,flyspeed,flymetab

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

c    Get the metabolic rate 
C     CHECKING FOR INANIMATE OBJECT
      IF (LIVE .EQ. 0) THEN
C      Inanimate
        QMETAB = 0.0
        Tc = X
       ELSE 
c      Alive, but is it too cold?
        If (Tc .ge. 0.0) then           
          CALL MET 
         else
C        Too cold, super low metabolism
          Qmetab = 0.0001
          Tc = X
        Endif
      ENDIF

c    Get the respiratory water loss
C     Checking for fluid type
      if (FLTYPE .EQ. 0.00) THEN
C      AIR
C       Call for respiratory water & energy loss           
        If (Qmetab .ge. 0.000) then
          CALL RESP          
         else
c       Negative metabolic rate. No physiological meaning - dead.         
          Qresp = 0.00000
          Qmetab = 0.00000
        endif 
      endif
C    ACTIVE ONLY IF ABOVE MINIMUM PREFERRED TEMPERATURE
      IF (TC .ge. Tminpr) THEN
        If (Tc .le. TPREF) then
          QMETAB = QMETAB * ACTLVL
        ENDIF
      ENDIF

C     Net internal heat generation        
      Qgenet = Qmetab - Qresp 
C     Net internal heat generation/unit volume. Use for estimating skin temp.      
      Gn = Qgenet/Vol
        IF (LIVE .EQ. 0) THEN
          GN = 0.
        ENDIF

C     COMPUTING SURFACE TEMPERATURE AS DICTATED BY GEOMETRY
      IF (Lometry .EQ. 0) THEN
C      FLAT PLATE
        Tskin = TC - G * R**2/(2.*Flshcond)
      Endif
C     First set average body temperature for estimation of avearage lung temperature 
      IF (Lometry .eq. 1) then
C       Cylinder: From p. 270 Bird, Stewart & Lightfoot. 1960. Transport Phenomena.
C       Tave = (gR**2/(8k)) + Tskin, where Tskin = Tcore - gR**2/(4k) 
C       Note:  these should all be solved simultaneously.  This is an approximation
C       using cylinder geometry. Subcutaneous fat is allowed in cylinder & sphere 
C       calculations.
        Rflesh = R1 - Rinsul
        Tskin = TC - Gn*Rflesh**2/(4.*Flshcond)
C       Computing average torso temperature from core to skin         
        Tlung = (Gn*Rflesh**2)/(8.*Flshcond) + Tskin
      endif
      if (Lometry .eq. 2) then
C       Ellipsoid: Derived 24 October, 1993  W. Porter
        A = ASEMAJR
        B = BSEMINR
        C = CSEMINR
        ASQ = A**2
        BSQ = B**2
        CSQ = C**2                               
        Tskin = TC-(Gn/(2.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &  (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ)) 
C       Computing average torso temperature from core to skin      
        Tlung = (Gn/(4.*Flshcond)) * ((ASQ*BSQ*CSQ)/
     &  (ASQ*BSQ+ASQ*CSQ+BSQ*CSQ)) + Tskin
      endif
      if(lometry.eq.4)then
C       Sphere: 
        RFLESH = R1 - RINSUL
        RSKIN = R1 
C      FAT LAYER, IF ANY
        S1 = (QGENET/(4.*PI*Flshcond))*((RFLESH - RSKIN)/(RFLESH*RSKIN))
        TSKIN = TC - (GN*RFLESH**2)/(6.*Flshcond) + S1 
C       COMPUTING AVERAGE TORSO TEMPERATURE FROM CORE TO SKIN (12 BECAUSE TLUNG IS 1/2 THE TC-TSKIN DIFFERENCE, 6*AK1)       
        TLUNG = (GN*RFLESH**2)/(12.*Flshcond) + TSKIN 
      endif
      IF ((Lometry .eq. 3).OR.(LOMETRY.EQ.5)) then
C      Model lizard as cylinder
C       Cylinder: From p. 270 Bird, Stewart & Lightfoot. 1960. Transport Phenomena.
C       Tave = (gR**2/(8k)) + Tskin, where Tskin = Tcore - gR**2/(4k) 
C       Note:  these should all be solved simultaneously.  This is an approximation
C       using cylinder geometry. Subcutaneous fat is allowed in cylinder & sphere 
C       calculations.
        Rflesh = R1 - Rinsul
        Tskin = TC - Gn*Rflesh**2/(4.*Flshcond)
C       Computing average torso temperature from core to skin         
        Tlung = (Gn*Rflesh**2)/(8.*Flshcond) + Tskin
      endif 
C     Limiting lung temperature extremes               
      if (Tlung .gt. Tc) then
        Tlung = Tc
      endif 
      if (Tlung .lt. -3.) then
        Tlung = -3.
      endif    
      
C     Limiting skin temperature extremes               
      if (Tskin .lt. -3.0) then
        Tskin = -3.00000
      endif 

      CALL CONV 
      CALL RESP
      CALL Sevap
      CALL RADOUT 
      CALL COND 
      
      if (FLTYPE .EQ. 1.00) THEN
C      WATER Environment
        Qsevap = 0.00
        WEVAP = 0.0
        QIRIN = 0.0
        QIROUT = 0.00
        QCOND = 0.00
      ENDIF

      QIN = Qsolar + Qirin + Qmetab
      QOUT = Qresp + Qsevap + Qirout + Qconv + Qcond
C    Finding the deviation from zero in guessing the solution
      Enb = QIN - QOUT   
      FUN = Enb  

c10    FORMAT(1X,7E10.3)
      RETURN   
      END
