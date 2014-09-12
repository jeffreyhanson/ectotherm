      Subroutine Thermoreg(QUIT) 

C    Copyright Warren P. Porter 2006. All rights reserved.
C    This thermoregulation subroutine allows for burrow retreat, 
C    if allowed, and for shade seeking, either only on the 
C    ground or climbing to 2m (reference) height to seek 
C    cooler temperatures and higher wind speeds.

C    Modified by Michael Kearney Oct 2002 so that nocturnal animals
C    seek shade when too cold. Revised 7/26/2006 W. Porter
c    Physiological options added and a quit flag added.
c    A number of structural problems corrected.

      Implicit None

      Real ABSMAX,ABSMIN,Acthr,Actlvl
      REAL ACTXBAS,AL,Amtfud,Andens,Asil,Asilp
      Real DELTAR,Depsel,Depsub,Depth
      REAL Emissb,Emissk,EXTREF,Fluid,G
      Real newdep,O2MAX,O2MIN,Ptcond,Qsol,Qsolr
      REAL RH,RQ,Shade,Soil1,Soil3,SPHEAT,Subtk
      Real Ta,Taloc,Tannul,Tc,Tcores,Tdigpr,tbask,temerge
      Real Time,Tlung,Tmaxpr,Tminpr,Tobj,Tref,Tsky,TskyC,Tshsoi
      Real Tsoil,Tsub,Tsubst,Vel,Vref,Z,Zsoil
      Real AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      Real Qsolrf,QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      Real QUIT,TSHLOW,TSHSKI,TSOILS,TSHOIL
      REAL WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ
      Real Xbas,TPREF,maxshd
      Real ctmax,ctmin,refshd
      real customallom,MR_1,MR_2,MR_3,shp
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,rhref
      real flytime,flyspeed,flymetab
      real gutfull,gutfill

      Integer Ihour,LIVE,Lometry,Micro,NM,wingcalc,ctminthresh,ctkill
      INTEGER Nodnum,NumFed,NumHrs,DEB1,wingmod,ctmincum
      integer flight,flyer,flytest,shdburrow

      CHARACTER*1 Burrow,Dayact,Climb,CkGrShad,Crepus,nofood,Nocturn 

      Dimension  Acthr(25),Depsel(25*365*20),Tcores(25)
      Dimension TSOIL(25),ZSOIL(10),TSHSOI(25)
      Dimension QSOL(25),RH(25),TskyC(25),SOIL1(25),SOIL3(25)
      Dimension Taloc(25),Time(25),TREF(25),TSUB(25),VREF(25),Z(25)
      DIMENSION TSHLOW(25),TSHSKI(25),TSOILS(25),TSHOIL(25)
      DIMENSION customallom(8),shp(3),rhref(25)
      
      common/gut/gutfull,gutfill
      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/FUN6/LIVE,SPHEAT,ABSMAX,ABSMIN,O2MAX,O2MIN
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/SOIL/TSOIL,TSHSOI,ZSOIL
      COMMON/WSOLAR/ASIL,Shade
      COMMON/Behav1/Dayact,Burrow,Climb,CkGrShad,Crepus,Nocturn,nofood 
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 
      Common/Behav3/Acthr,ACTXBAS
      Common/Treg/Tc
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      COMMON/DEPTHS/DEPSEL,Tcores
      Common/Qsolrf/Qsolrf
      common/wundrg/newdep
      COMMON/ENVIRS/TSHLOW,TSHSKI,TSOILS,TSHOIL
      COMMON/SHADE/MAXSHD
      common/ctmaxmin/ctmax,ctmin,ctmincum,ctminthresh,ctkill
      common/refshade/refshd
      COMMON/fly/flytime,flight,flyer,flytest,flyspeed,flymetab
      common/burrow/shdburrow
      
C    TRY WING ANGLE (IF BUTTERFLY) OR COLOUR CHANGE FIRST
      Depth = newdep
      
      if((wingmod.gt.0).and.(flytest.ne.1))then
       if(phimin.ne.phimax)then
        if(phi.ne.phimax)then
        IF(DEPTH.le.0.0001)THEN
C       ON THE SURFACE.  WHAT'S THE CORE TEMPERATURE, TOO HOT OR TOO COLD?
         IF(TC.LT.TPREF)THEN
          if(phi.lt.phimax)then
           phi=phi+20
           call solar
c         phi=phimax
           if(phi.gt.phimax)then
            phi=phimax
           endif
           return
          endif
         endif
c       IF(TC.gt.TPREF)THEN
c        if(phi.gt.phimin)then
c         phi=phi-5
c         if(phi.lt.phimin)then
c          phi=phimin
c         endif
c         return
c        endif
c       endif
        endif
       endif
      endif  
      endif

C    CHECK REFLECTIVITY (NEED MAX/MIN VALUES) IF NO CHANGE POSSIBLE, USE SAME VALUES FOR MAX, MIN.
C    IN GENERAL, ANIMALS THAT CAN CHANGE COLOR ARE DARK BELOW THEIR MINIMUM ACTIVITY TEMPERATURE.
      IF((QSOLAR.GT.0.0000).and.(DEPTH.LE.0.00001))THEN
       IF(TC.GT.TPREF)THEN
        IF(ABSAN.GT.ABSMIN)THEN
C       USE MIN VALUE
         ABSAN = ABSAN-0.05
         if(ABSAN.lt.ABSMIN)then
          ABSAN=ABSMIN
         endif
        ENDIF
       ELSE
        ABSAN = ABSMAX
       ENDIF
      endif

      IF((DEPTH.LE.0.00001).and.(Z(ihour).lt.90.))THEN
C     ON THE SURFACE, SUN IS UP. DIURNAL BEHAVIOURAL THERMOREGULATION WHAT'S THE CORE TEMPERATURE, TOO HOT OR TOO COLD?
       IF(TC.GT.TPREF)THEN
C      NEED SOME SHADE, LESS SUN
        If ((CkGrShad .eq. 'Y') .or. (CkGrShad .eq. 'y')) then
         IF(SHADE.LT. MAXSHD)THEN
c        INCREASE THE SHADE (CALL SHADEADJUST) & RECALCULATE LOCAL ENVIRONMENT (CALL ABOVEGROUND)
          CALL SHADEADJUST
          RETURN
         ENDIF
         SHADE=maxshd
        ENDIF
C      IF GOT THIS FAR SHADE MAXED OUT. CLIMB?
        If ((Climb .eq. 'Y') .or. (Climb .eq. 'y')) then
         IF(DEPSEL(IHOUR).ne.200.0)THEN
C        Try climbing
          if ((Tref(Ihour).ge.Tminpr).and.(Tref(Ihour).le.Tpref))THEN
C         It's OK up high 
c         Climb to 200 cm height
           DEPSEL(IHOUR) = 200.0
c         Reference height air temperature is the same in sun or shade.
           Ta = Tref(Ihour)
           Vel = Vref(ihour)
           RETURN
c         No assumption of increase in shade due to climbing.  May still be in the sun.
          endif
         ENDIF
        ENDIF
        IF(TPREF .le. TMAXPR)THEN
c       original value for Functional Ecology MS
c       TPREF=TPREF+.5
         TPREF=TPREF+0.5
         If(TPREF .gt. TMAXPR)THEN
          TPREF = TMAXPR
         ELSE
          Return
         ENDIF
        ENDIF
C      IF GOT THIS FAR, THEN SHADE AND CLIMBING MAXED OUT. BURROW?
        If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
         IF(DEPSEL(IHOUR).GE.0.)THEN
           if(TC.LT.TMAXPR)then
             quit=1
             return
           else
C         CURRENTLY ABOVE GROUND, ALLOW BURROW ENTRY & RESET LOCAL ENVIRONMENT
          CALL BURROWIN
          RETURN
          endif
         ENDIF
        else
C        No burrow allowed. Must stay above ground even though
C        outside of preferred temperature range. Not active.
C        TRY A PHYSIOLOGICAL SOLUTION
        ENDIF
       ELSE
c     too cold, try burrow
        IF(DEPSEL(IHOUR).GE.0.)THEN
         If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
           if(TC.GT.CTMIN)then
             quit=1
             return
           else
C         CURRENTLY ABOVE GROUND, ALLOW BURROW ENTRY & RESET LOCAL ENVIRONMENT
          CALL BURROWIN
          RETURN
          endif
         else
C         No burrow allowed. Must stay above ground even though
C         outside of preferred temperature range. Not active.
C         TRY A PHYSIOLOGICAL SOLUTION
         endif
        ENDIF
       endif
      ENDIF

      IF((DEPTH.LE.0.00001).and.(Z(ihour).eq.90))THEN
C     ON THE SURFACE, SUN IS DOWN. NOCTURNAL BEHAVIOURAL THERMOREGULATION WHAT'S THE CORE TEMPERATURE, TOO HOT OR TOO COLD?
       IF(TC.LT.TMINPR)THEN
C      NEED SOME SHADE, LESS COLD SKY
        If ((CkGrShad .eq. 'Y') .or. (CkGrShad .eq. 'y')) then
         IF(SHADE.LT. MAXSHD)THEN
c        INCREASE THE SHADE (CALL SHADEADJUST) & RECALCULATE LOCAL ENVIRONMENT (CALL ABOVEGROUND)
          CALL SHADEADJUST
          RETURN
         ENDIF
        ENDIF
        If ((Climb .eq. 'Y') .or. (Climb .eq. 'y')) then
         IF(DEPSEL(IHOUR).ne.200.0)THEN
C        Try climbing
          if ((Tref(Ihour).ge.Tminpr).and.(Tref(Ihour).le.Tpref))THEN
C         It's OK up high 
c         Climb to 200 cm height
           DEPSEL(IHOUR) = 200.0
c         Reference height air temperature is the same in sun or shade.
           Ta = Tref(Ihour)
           Vel = Vref(ihour)
           RETURN
c         No assumption of increase in shade due to climbing.  May still be in the sun.
          endif
         ENDIF
        ENDIF
C      IT MAY STILL BE TOO COLD
        If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
         if(TC.GT.CTMIN)then
             quit=1
             return
         else
             SHADE=refshd
         CALL BURROWIN
         RETURN
         endif
        ENDIF
C       No burrow allowed. Must stay above ground even though
C       outside of preferred temperature range. Not active.
C       TRY A PHYSIOLOGICAL SOLUTION
       else
c      too hot, try burrow
        If ((Burrow .eq. 'Y') .or. (Burrow .eq. 'y')) then
          if(TC.LT.TMAXPR)then
             quit=1
             return
           else
         CALL BURROWIN
         RETURN
         endif
         else
        ENDIF
       endif
      ENDIF

C    TRY PHYSIOLOGY CHANGES
C    INCREASE THERMAL CONDUCTIVITY OF TISSUES
      IF(Flshcond .LT. 0.6)THEN
       Flshcond = 0.6
       RETURN
      ENDIF

C    PANT (REDUCE OXYGEN EXTRACTION EFFICIENCY)(NEED MAX/MIN VALUES)
c    IF(EXTREF.GT.O2MIN)THEN
c     EXTREF = O2MIN
c     RETURN
c    ENDIF
      
c    if it is a climber and the environment is still too stressful, leave it up the tree to minimize stress
      If ((Climb .eq. 'Y') .or. (Climb .eq. 'y')) then
       if((tc.gt.tmaxpr).or.(tc.lt.ctmin))then
        IF((DEPSEL(IHOUR).ne.200.0).and.(DEPSEL(IHOUR).ge.0))THEN
C       Try climbing
C       It's OK up high 
c       Climb to 200 cm height
         DEPSEL(IHOUR) = 200.0
c       Reference height air temperature is the same in sun or shade.
         Ta = Tref(Ihour)
         Vel = Vref(ihour)
         RETURN
c       No assumption of increase in shade due to climbing.  May still be in the sun.
        endif
       endif
      ENDIF

C    IF REACH HERE, OUT OF OPTIONS, SET EXIT FLAG
      QUIT = 1.

c    Return to Itaday and run another simulation with new environmental conditions
      Return
      End
