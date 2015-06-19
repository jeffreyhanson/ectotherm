      FUNCTION CONTD5(II,X,CON,ICOMP,ND)
C ----------------------------------------------------------
C     THIS FUNCTION CAN BE USED FOR CONTINUOUS OUTPUT IN CONNECTION
C     WITH THE OUTPUT-SUBROUTINE FOR DOPRI5. IT PROVIDES AN
C     APPROXIMATION TO THE II-TH COMPONENT OF THE SOLUTION AT X.
C ----------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION CON(5*ND),ICOMP(ND)
      COMMON /CONDO5/XOLD,H
C ----- COMPUTE PLACE OF II-TH COMPONENT 
      I=0 
      DO 5 J=1,ND 
      IF (ICOMP(J).EQ.II) I=J
   5  CONTINUE
      IF (I.EQ.0) THEN
         WRITE (6,*) ' NO DENSE OUTPUT AVAILABLE FOR COMP.',II 
         RETURN
      END IF  
      THETA=(X-XOLD)/H
      THETA1=1.D0-THETA
      CONTD5=CON(I)+THETA*(CON(ND+I)+THETA1*(CON(2*ND+I)+THETA*
     &           (CON(3*ND+I)+THETA1*CON(4*ND+I))))
      RETURN
      END