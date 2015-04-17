       SUBROUTINE SELDEP (TSOIL,ZSOIL,DEPTH)
C    COPYRIGHT WARREN P. PORTER  12 DEC, 1990   

C    If an ectotherm has to be below ground,
C    this subroutine finds the depth in the soil where it
C    can reach its preferred temperature to maximize digestion, 
C    TDIGPR, if such a temperature exists.  If it is too cold, to 
C    digest, the lowest temperature is selected in the soil.
c    This subroutine also calls Subroutine Undrgrd to reset the environmental 
c    parameters the animal experiences.

      Implicit None

      Real ALT,BP,DEPTH,QSOLR,R,slope,newdep
      Real AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      Real TC,Tdeep,TDIGPR,TNORM,TR,TSKIN,TSMAX,TSMIN,TSOIL
      Real WEVAP,ZMAX,ZMIN,ZSOIL,H2O_BalPast
      Real QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF
      Real TSUB,VREF,Z,Tannul
      Real ANDENS,ASILP,EMISSB,EMISSK,FLUID,G
      Real MICRO,TOBJ,TSKY,pond_env,shdgrass
      Real Intercept,TWING,twater,pond_depth,rhref,tbask,temerge
      Real TMAXPR,TMINPR,ACTLVL,AMTFUD,XBAS,TPREF,ctmin,ctmax

      Integer I,IDEP,Ihour,NON,goodsoil,inwater,aquatic,feeding,
     &    ctmincum,ctminthresh,ctkill,minnode

      DIMENSION TSOIL(10),ZSOIL(10),pond_env(20,365,25,2),shdgrass(25)
      DIMENSION QSOL(25),RH(25),TskyC(25),TIME(25),rhref(25),
     * SOIL1(25),SOIL3(25),Taloc(25),TREF(25),TSUB(25),VREF(25),Z(25)

C    NEED NON, # OF SOIL NODES,
      COMMON/BUR/NON,minnode
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST
      COMMON/FUN4/Tskin,R,WEVAP,TR,ALT,BP,H2O_BalPast
      Common/Treg/Tc,TWING
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref,
     &shdgrass 
      COMMON/ENVAR2/TSUB,VREF,Z,Tannul
      COMMON/WDSUB/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR,
     *  MICRO,QSOLR,TOBJ,TSKY
      common/wundrg/newdep
      common/goodsoil/goodsoil
      common/ctmaxmin/ctmax,ctmin,ctmincum,ctminthresh,ctkill
      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge


      IF (NON .GT. 10) THEN
c      WRITE(0,*)'Trouble in SUB. SELDEP. Number of soil nodes > 10'
        RETURN
       ELSE
      ENDIF
      
      if(goodsoil .eq. 1)then
      ta=ta-0.5
      goto 999
      endif

c    ctmin = 10.1
c    ctmax = 37
      
      goodsoil = 0

c    if(aquatic.eq.1)then
c     newdep=zsoil(7)
c     Ta = TSOIL(5)
c     goto 999
c    endif

C    INITIALIZING FOR FINDING SOIL TEMPERATURE MAXIMUM,MINIMUM & CORRESPONDING DEPTHS
      TSMAX = TSOIL(minnode)
      ZMAX = ZSOIL(minnode)
      TSMIN = TSOIL(minnode)
      ZMIN = ZSOIL(minnode)
      DO 1 I = minnode,NON
        IF (TSOIL(I).GT. TSMAX)THEN
          TSMAX = TSOIL(I)
          ZMAX = ZSOIL(I)
         ELSE
          IF (TSOIL(I) .LT. TSMIN)THEN
            TSMIN = TSOIL(I)
            ZMIN = ZSOIL(I)
           ELSE
          ENDIF
        ENDIF
1     CONTINUE
C    WRITE(0,*)'TSMAX,TSMIN = ',TSMAX,TSMIN
C    WRITE(0,*)'MAXDEPTH,MINDEPTH = ',ZMAX,ZMIN

      DO 13 IDEP = minnode,NON
c      ctmax burrow
c      IF ((TSOIL(IDEP) .GT. CTMIN).and.(TSOIL(IDEP) .LT. tmaxpr))THEN
      IF ((TSOIL(IDEP) .GT. CTMIN).and.(TSOIL(IDEP) .LT. 
     & (ctmax-(ctmax-tmaxpr)/2)))THEN    
    
            Ta = TSOIL(IDEP)
            newdep = ZSOIL(IDEP)
c          goodsoil = 1
            GO TO 999
      endif
13    continue

c    kearney, comment this out if allowing deep soil to be selected
c          Ta = TSOIL(IDEP-1)
c          newdep = ZSOIL(IDEP-1)
c          GO TO 999
      if(NON.eq.10)then
       Ta = Tannul
       NEWDEP = 200.
      else
       Ta = TSOIL(NON)
       NEWDEP = ZSOIL(NON)
      endif
      goto 999



C    DETERMINE WHETHER MAX. SOIL TEMPERATURE IS GREATER THAN MIN.
C    TEMPERATURE FOR DIGESTION
      IF (TSMAX .LT. 20.)THEN
C      IT'S TOO COLD TO DIGEST below ground, then go to coolest temperature,
C      but MAKE SURE YOU DON'T FREEZE THE ANIMAL
        IF (TSMIN .GT. CTMIN) THEN
          Ta = TSMIN
          NEWDEP = ZMIN
C        Check to make sure that coldest temperature is not at the surface.  If so,
C        put at first depth below the surface.
          IF (NEWDEP .eq. 0.000) THEN
            NEWDEP = zsoil(minnode)
          ENDIF
           GO TO 999
C        Done
         ELSE
C        Soil minimum less than 3C.  Go deeper to find warmer temperature.
          If (Tannul .gt. CTMIN)then
c          Interpolate assuming Tannual at 200 cm
c          Establishing temperature & depth at 60 cm (max curr. depth)          
            Tdeep = Tsoil(NON)
            NEWDEP = ZSOIL(NON)
            slope = (200. - newdep)/(Tdeep - Tannul)

            Ta = CTMIN
            NEWDEP = slope*Ta + 200.0
           else
C          Frozen tundra here.  Nowhere to go.  Better have some antifreeze.
            Ta = Tannul
            NEWDEP = 200.
          Endif
        ENDIF
        GO TO 999
C      Done
       ELSE
C      IT'S WARM ENOUGH TO DIGEST, FIND THE BEST, SHALLOWEST DEPTH in next section.      
      ENDIF

C    IT'S WARM ENOUGH TO DIGEST, FIND THE BEST, SHALLOWEST DEPTH
      DO 10 IDEP = minnode,NON
c     ctmax burrow 
c        IF (TSOIL(IDEP) .GT. tmaxpr)THEN
        IF (TSOIL(IDEP) .GT. (ctmax-(ctmax-tmaxpr)/2))THEN
C        Look deeper
C        Next bit added by M. Kearney to deal with warm deep soil temps
          If (IDEP .eq. NON) then
              If (Tannul .lt. TDIGPR)then
c              Interpolate assuming Tannual at 200 cm
c              Establishing temperature & depth at 60 cm (max curr. depth)          
                Tdeep = Tsoil(NON)
                newdep = ZSOIL(NON)
                slope = (200. - newdep)/(Tdeep - Tannul)*(-1.)
                Intercept = 200. - (slope*Tannul)
                Ta = TDIGPR
                newdep = slope*TDIGPR + Intercept
                goto 999
              else
C              Frozen tundra here.  Nowhere to go.  Better have some antifreeze.
                Ta = Tannul
                newdep = 200.
                goto 999
              Endif
          Endif
         ELSE
          IF (TSOIL(IDEP) .EQ. TDIGPR)THEN
            Ta = TSOIL(IDEP)
            newdep = ZSOIL(IDEP)
            goodsoil = 1
            GO TO 999
C          Done
           ELSE
            IF (TSOIL(IDEP) .LT. TDIGPR)THEN
C            DO A LINEAR INTERPOLATION TO GET DEPTH
C             LOOKING ABOVE, IF IDEP .GT. 1
              IF (IDEP .GT. 1) THEN
C              CONTINUE
                IF (TSOIL(IDEP-1) .GT. TDIGPR)THEN
                  Ta = TDIGPR
                  goodsoil = 1
                  TNORM = (TSOIL(IDEP-1)-TDIGPR)/
     &             (TSOIL(IDEP-1)-TSOIL(IDEP))
                  newdep = ZSOIL(IDEP-1) +
     &             (ZSOIL(IDEP)-ZSOIL(IDEP-1))*TNORM
C                WRITE(0,*)'4, TNORM,TSOIL(IDEP-1),TSOIL(IDEP) = ',
C     &             TNORM,TSOIL(IDEP-1),TSOIL(IDEP)
                    GO TO 999
                        ELSE
C                  TSOIL LESS THAN TDIGPR, KEEP LOOKING, BUT SAVE 
C                  WARMEST SOIL TEMPERATURE
                    Ta = TSMAX
                    newdep = ZMAX
                    goodsoil = 1
                     IF (newdep .eq. 0.000) THEN
C                    PUT at first node BELOW THE SURFACE
                      newdep = Zsoil(minnode)
                    ENDIF
C                Done
                ENDIF
               ELSE
C              LOOKING BELOW
                IF (IDEP .LT. NON) THEN
                  IF (TSOIL(IDEP+1) .GT. TDIGPR)THEN
                    Ta = TDIGPR
                    TNORM = (TSOIL(IDEP)-TDIGPR)/
     &              (TSOIL(IDEP)-TSOIL(IDEP+1))
                    newdep = ZSOIL(IDEP) +
     &              (ZSOIL(IDEP+1)-ZSOIL(IDEP))*TNORM
C                  WRITE(0,*)'5, TNORM,TSOIL(IDEP+1),TSOIL(IDEP) = ',
C     &              TNORM,TSOIL(IDEP+1),TSOIL(IDEP)
                    GO TO 999
                  ELSE
C                  TSOIL LESS THAN TDIGPR, KEEP LOOKING, BUT SAVE 
C                  WARMEST SOIL TEMPERATURE
                    Ta = TSMAX
                    newdep = ZMAX
                     IF (newdep .eq. 0.000) THEN
C                    PUT at first node BELOW THE SURFACE
                      newdep = Zsoil(minnode)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF    
            ENDIF
          ENDIF
        ENDIF
        IF ((QSOLR .EQ. 0.000) .AND. (newdep .EQ. 0.000))THEN
C        PUT at first node BELOW THE SURFACE
          newdep = Zsoil(minnode)
        ENDIF
10    CONTINUE

999   CONTINUE
c    Set up underground climate conditions
      Call Belowground
      call Radin
      
      RETURN
      END
