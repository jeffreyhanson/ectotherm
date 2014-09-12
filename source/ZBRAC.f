      SUBROUTINE ZBRAC(FUN,X1,X2,SUCCES)
      REAL FACTOR,X1,X2,F1,FUN,F2
      INTEGER J,NTRY

      LOGICAL SUCCES
      FACTOR=1.6
      NTRY=50
c      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      F1=FUN(X1)
      F2=FUN(X2)
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          F1=FUN(X1)
        ELSE
          X2=X2+FACTOR*(X2-X1)
          F2=FUN(X2)
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      end