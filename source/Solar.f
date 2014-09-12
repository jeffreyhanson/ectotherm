      SUBROUTINE SOLAR 

      Implicit None

      REAL ABSSB,ABSAN,AL,AMASS,ANDENS,ASIL,ASILN,ASILP,ATOT
      Real DEPSUB,EMISAN,EMISSB,EMISSK,FATOSB,FATOSK   
      Real FATOBJ,Flshcond,FLUID,G,PI,PTCOND,PCTDIF 
      Real QCOND,QCONV,QIRIN,QIROUT,QMETAB,Qnorm,Qresp,Qsevap 
      Real QSDIFF,QSDIR,QSOBJ,QSOLAR,QSOLR,QSRSB,QSSKY 
      Real RELHUM,Shade,SIG,SUBTK
      Real TA,TOBJ,TSKY,TSUBST,VEL,WC,ZEN,Zenith
      REAL rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &,TQSOL,phimin,phimax,twing,F12,F32,F42,F52,f61,f13,f14,f15,f16
      Real A1,A2,A3,A4,A4b,A5,A6,f23,f24,f25,f26

      INTEGER IHOUR,MICRO,NM,wingmod,wingcalc

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND  
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/FUN3/AL,TA,VEL,PTCOND,SUBTK,DEPSUB,TSUBST 
      COMMON/FUN5/WC,ZEN,PCTDIF,ABSSB,ABSAN,ASILN,FATOBJ,NM
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/WSOLAR/ASIL,Shade
      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
      COMMON/WDSUB2/MICRO,QSOLR,TOBJ,TSKY
      Data PI/3.14159/
      
C       COMPUTING SOLAR ENERGY ABSORBED:
      if(qsolr.gt.0)then
      continue
      endif
C    Checking for scattered skylight only when sun below horizon
      Zenith = Zen*180./PI
C    DIRECT BEAM COMPONENT
      If (Zenith .lt. 90.00) then
C       DIRECT BEAM (normal to the direct beam)
        Qnorm = (QSOLR / COS(ZEN))
        if(qnorm.gt.1367.)then
c        making sure that low sun angles don't lead to solar values
c        greater than the solar constant
          qnorm = 1367.
        endif
        if(zenith.ge.90.)then
          qnorm = 0.000
        endif
        QSDIR = ABSAN * ASILN * (1.00 - PCTDIF) * Qnorm *
     &      (1.0 -(shade/100.))   
       else
        Qsdir = 0.00
        qnorm = 0.00
      endif

      if(wingmod.eq.2)then
      call wings(rho1_3,absan,trans1,QSOLR,aref,bref,cref,phi,
     &F21,f31,f41,f51,f61,f12,f32,f42,f52,sidex,WQSOL,TQSOL,A1,A2,
     &A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26,asilp)
       QSRSB = ABSAN * 1 * ATOT/2 * (1.0 - ABSSB) * QSOLR
     &   * (1.0 -(shade/100.))  
       QSOLAR = QSRSB + TQSOL         
      else
C      DIFFUSE COMPONENTS (SKY AND SUBSTRATE)  
       QSOBJ = ABSAN * FATOBJ* ATOT * PCTDIF * Qnorm
       if(wingmod.eq.1)then
        if(phi.lt.90)then
         QSSKY = 0.
        else        
       QSSKY = ABSAN * FATOSK * ATOT * PCTDIF * Qnorm *
     &      (1.0 -(shade/100.))  
        endif
       else
       QSSKY = ABSAN * FATOSK * ATOT * PCTDIF * Qnorm *
     &      (1.0 -(shade/100.))  
       endif  
       QSRSB = ABSAN * FATOSB * ATOT * (1.0 - ABSSB) * QSOLR
     &   * (1.0 -(shade/100.))   

       QSDIFF = QSSKY + QSRSB + QSOBJ  
       if(wingmod.eq.1)then
        if(phi.lt.90)then
         QSOLAR=QSDIFF
        else
         QSOLAR = QSDIR + QSDIFF 
        endif
        else
        QSOLAR = QSDIR + QSDIFF 
       endif
      endif
      
c115   FORMAT(1X,7E10.3)
      RETURN  
      END
