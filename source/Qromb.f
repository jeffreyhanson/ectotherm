      SUBROUTINE qromb(func,a,b,ss)
c    Romberg integration from Numerical Recipes
      implicit none
      INTEGER j,JMAX,K,KM
      REAL a,b,func,ss,EPS,dss
      EXTERNAL func
c      PARAMETER (EPS=1.e-6, JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1)

CU    USES polint,trapzd

      REAL h(21),s(21)
      Data eps/0.000001/
      Data jmax/20/
      Data k/5/
      Data km/4/
 
      h(1)=1.
      do 11 j=1,JMAX
        call trapzd(a,b,s(j),j)
        if (j.ge.K) then
          call polint(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
c      pause 'too many steps in qromb'
      END
C  (C) Copr. 1986-92 Numerical Recipes Software |2j.012u2.
