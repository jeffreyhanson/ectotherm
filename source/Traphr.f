      SUBROUTINE TRAPhr(IHOUR,DTIME)

C     THIS SUBROUTINE USES TRAPEZOIDAL INTEGRATION TO COMPUTE TOTAL
C    METABOLISM;             ZP2,DMET,TMET
C    EVAPORATIVE WATER LOSS; ZP3,DEVP,TEVP
C    ENERGY ABSORBED;        ZP4,DEIN,TEIN
C    WATER ABSORBED;         ZP5,DWIN,TWIN
C    DISCRETIONARY ENERGY;   ZP6,DNRG,TNRG
C    DISCRETIONARY WATER;    ZP7,DWTR,TWTR
C     ON A DAILY (D) AND TOTAL TIME SELECTED (T) BASIS

      IMPLICIT None

      REAL DAY,Daymet,Dayevp,Dayein,Daywin,Daynrg,Daywtr,Daydis
      Real DAIR,DAYAIR,DAYCO2,DCO2,DEIN,DEVP
      Real DMET,DNRG,DTIME,DWTR,DWIN,gwatph,DDIS
      Real OLDAIR,OLDEVP,OLDMET,OLDCO2,OLDWIN,OLDDIS
      Real TEIN,TNRG,TEVP,TMET,TWTR,TWIN
      Real XP,YP,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7
      Real Tlung,EXTREF,RQ,GEVAP,DELTAR,AirVol,CO2MOL
      Real QSOLAR,QIRIN,QMETAB,Qresp,QSEVAP,QIROUT,QCONV,QCOND
      Real DIST,MR_1,MR_2,MR_3

      INTEGER Iclim,IHOUR,JP,Intnum,DEB1

      CHARACTER*1 Transt,Dlenth

      DIMENSION DAY(7300),XP(365*25*20)
      DIMENSION ZP1(365*25*20),ZP2(365*25*20),ZP3(365*25*20)
      DIMENSION ZP6(365*25*20),ZP7(365*25*20)
      DIMENSION YP(365*25*20),ZP4(365*25*20),ZP5(365*25*20)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/SUM1/DMET,DEVP,DEIN,DWIN,DNRG,DWTR,DAIR,DCO2
      COMMON/SUM2/TMET,TEVP,TEIN,TWIN,TNRG,TWTR
      COMMON/DAYS/DAY
      COMMON/TRANS/ICLIM,JP
      COMMON/PLTDAT/XP,YP,ZP1,ZP2,ZP3,ZP4,ZP5,ZP6,ZP7
      Common/Dayint/DAYMET,DAYEVP,DAYEIN,DAYWIN,DAYNRG,DAYWTR,DAYAIR,
     &DAYDIS
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/REVAP2/GEVAP,AirVol,CO2MOL,gwatph
      Common/Usropt/Transt,Dlenth
      Common/Trav/Dist
      Common/Intnum/Intnum
C     We must save the values of these variables between calls.
      SAVE OLDMET,OLDEVP,OLDWIN,OLDAIR,OLDCO2,OLDDIS

      If((Transt.eq.'Y').or.(Transt.eq.'y'))then
      JP=JP+1
      Endif

C     HOURLY INTEGRATION FOR A DAY
      IF(IHOUR .EQ. 1) THEN         
C    INITIALIZE VARIABLES: all variable quantities/s
        OLDMET = ZP2(JP)    
        OLDEVP = ZP3(JP)
        OLDWIN = ZP5(JP)
        OLDAIR = AirVol 
        OLDCO2 = CO2MOL  
        OLDDIS = DIST 
        DMET = 0.     
        DEVP = 0.
        DEIN = 0.     
        DWIN = 0.
        DNRG = 0.     
        DWTR = 0.
        DAIR = 0.
        DCO2 = 0.
        DDis = 0.
        Dist = 0.
        Daymet = 0.
        Dayevp = 0.
        Dayein = 0.
        Daywin = 0.
        Daynrg = 0.
        Daywtr = 0.
        Dayair = 0.
        Daydis = 0.
       ELSE
C    DO TRAPEZOIDAL INTEGRATION
        DMET=((ZP2(JP)+OLDMET)/2.)*DTIME+DMET    
        DEVP=((ZP3(JP)+OLDEVP)/2.)*DTIME+DEVP
c      metabolic water production
        DWIN=((ZP5(JP)+OLDWIN)/2.)*DTIME+DWIN
        DAIR=((AirVol+OLDAIR)/2.)*DTIME+DAIR
        DCO2=((CO2MOL+OLDCO2)/2.)*DTIME+DCO2
        DDis=(Dist+OLDDIS)
C    SET CURRENT VALUE TO OLD VALUE FOR NEXT INTEGRATION
        OLDMET = ZP2(JP)    
        OLDEVP = ZP3(JP)
        OLDWIN = ZP5(JP)
        OLDAIR = AirVol
        OLDCO2 = CO2MOL 
        OLDDIS = DDis   
      ENDIF

      IF (IHOUR .EQ. INTNUM) THEN
C      Daily totals

        DAYMET = DMET
        DAYEVP = DEVP
        DAYWIN = DWIN
        DAYAIR = DAIR
        DAYCO2 = DCO2
        DAYDIS = DDis
        DMET = 0.     
        DEVP = 0
        DWIN = 0.
        DAIR = 0.
        DCO2 = 0.
      ENDIF
      
      RETURN
      END   

