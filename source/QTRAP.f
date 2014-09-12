      SUBROUTINE qtrap(a,b,s)
      INTEGER JMAX
      REAL a,b,s,EPS
      PARAMETER (EPS=1.e-6, JMAX=20)
CU    USES trapzd
      INTEGER j
      REAL olds
      olds=-1.e30
      do 11 j=1,JMAX
        call trapzd(a,b,s,j)
        if (abs(s-olds).lt.EPS*abs(olds)) return
        olds=s
11    continue
c      pause 'too many steps in qtrap'
      END
C  (C) Copr. 1986-92 Numerical Recipes Software |2j.012u2.
