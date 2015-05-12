      SUBROUTINE DEVRESET(dead1,E_H_pres1,E_Hb1,frogbreed1,breeding1,
     &    conth1,contdep1,stage1,frogstage1,reset1,complete1,waiting1
     &,hour1,stages1,completion1)

c    this subroutine checks to see if the user wants resets after metamorphosis


      INTEGER breeding1,frogstage1,frogbreed1,dead1,reset1
     &,complete1,waiting1,hour1,stages1,completion1
      REAL E_H_pres1,E_Hb1,CONTH1,CONTDEP1,stage1

      waiting1=0

      if(reset1.gt.0)then
       if(stage1.ge.reset1)then
c      if(hour.eq.24)then
c       dead=1
c       complete=0
c      else
         complete1=1
         stage1=stages1-1
c      endif
       endif
      endif

      if((complete1.eq.1).and.(hour1.eq.24))then
       completion1=completion1+1
       dead1=1
       complete1=0
       stage1=stages1-1
      endif

      if(frogbreed1.eq.1)then
       if(breeding1.eq.1)then
         if(frogstage1.eq.1)then 
c        resetting at metamorphosis            
         if(stage1.eq.2)then
         dead1=1
         endif
         endif
       endif
      endif

      if(frogbreed1.eq.1)then
       if(stage1.le.1)then
         if(contdep1.le.0.1)then
          dead1=1
         endif
       endif
      endif

c    prevent egg development in soil if not the right season
      if(frogbreed1.eq.2)then
       if(E_H_pres1.le.E_Hb1)then
        if(breeding1.eq.0)then
         dead1=1
        endif
       endif
      endif

c    kill eggs of terrestrial breeders if the pond fills up too soon
C     if(frogbreed1.eq.2)then
C      if(stage1.eq.0)then
C       if(E_H_pres1.le.E_Hb1*0.90)then
C       if(contdep1.gt.50)then
C        dead1=1
C       endif
C      endif
C      endif
C     endif

c    kill tadpoles if pond dries
      if(frogbreed1.eq.2)then
       if(stage1.eq.1)then
         if(contdep1.le.0.1)then
      waiting1=1
      E_H_pres1=E_Hb1
      stage1=0
         endif
       endif
        if(frogstage1.eq.1)then 
         if(stage1.eq.2)then
          dead1=1
      waiting1=0
         endif
        endif
      endif

      if(frogbreed1.eq.2)then
       if(stage1.eq.1)then
         if(contdep1.le.0.1)then
         dead1=1
      waiting1=0
         endif
       endif
      endif

      if(frogbreed1.eq.3)then
       if(frogstage1.eq.1)then
        if(breeding1.eq.1)then
         if(stage1.ge.1)then
         dead1=1
      waiting1=0
         endif
        else
         dead1=1
      waiting1=0
        endif
       endif
      endif

      if(frogbreed1.eq.3)then
       if(frogstage1.eq.0)then
         if(E_H_pres1.le.E_Hb1)then
        if(breeding1.eq.0)then
         dead1=1
      waiting1=0
         endif
        endif
       endif
      endif

c    ********end devreset*************

      RETURN
      end