      SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NRPD)
C    A DUMMY SUBROUTINE FOR LSODE WHEN A NON-STIFF PROBLEM
      DOUBLE PRECISION PD, T, Y
      DIMENSION Y(1), PD(NRPD,1)
      INTEGER ML,MU,NEQ,NRPD
      RETURN
      end