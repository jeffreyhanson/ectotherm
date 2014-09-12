      SUBROUTINE ANCORR (GR,RE,ANU) 
C    Forced convection subroutine from empirical data
c    Warren Porter version 31 July 2006  copyright 2006 All rights reserved.


      IMPLICIT None 
      REAL ANU,GR,PR,RE,SH,SC
      real customallom,shp
      INTEGER IMODEL,Nodnum,NumFed,NumHrs,Lometry  
      DIMENSION customallom(8),shp(3)

      COMMON/MODEL/IMODEL
      COMMON/Behav2/NumFed,NumHrs,Lometry,nodnum,customallom,shp 

      PR = .72
      SC = .60

      if((Lometry.eq.3).or.(Lometry.eq.5))then
        ANU = 0.35*RE**0.6
C      FROM P. 216, EDE; AN INTRODUCTION TO HEAT TRANSFER. 1967  
        SH = ANU*(SC/PR)**.333
      endif
      
      if(Lometry.eq.4)then
C      ***********************FROG******************************
C      C.R. TRACY'S LEOPARD FROGS - ECOL. MONOG. 1976 v. 46(3)   

C      CHECKING FOR OUT OF BOUNDS VALUES 
        IF (RE .LT. 80.) THEN  
c        WRITE(0,*)' RE, ',RE,',TOO SMALL FOR FROG ANCORR'  
         ELSE   
          IF (RE .GT. 40000.) THEN  
c          WRITE(0,*)' RE, ',RE,',TOO LARGE FOR FROG ANCORR'   
          ENDIF 
        endif 
      
C      COMPUTING NUSSELT AND SHERWOOD NUMBERS
        IF (RE .LE. 2000.) THEN
          ANU = 0.88*RE**0.5
          SH  = 0.76*RE**0.5
         ELSE 
          ANU = 0.258*RE**0.667 
          SH  = 0.216*RE**0.667 
        ENDIF  
      endif

      RETURN 
      END
