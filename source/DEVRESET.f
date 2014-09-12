      SUBROUTINE DEVRESET(dead1,E_H_pres1,E_Hb1,frogbreed1,breeding1,
     &    conth1,contdep1,stage1,frogstage1,reset1,complete1,waiting1)

c    this subroutine checks to see if the user wants resets after metamorphosis


      INTEGER breeding1,frogstage1,frogbreed1,dead1,reset1
     &,complete1,waiting1
      REAL E_H_pres1,E_Hb1,CONTH1,CONTDEP1,stage1

      waiting1=0

c    if(frogbreed1.eq.0)then
       if(reset1.gt.0)then
        if(stage1.ge.reset1)then
         dead1=1
         complete1=1
        endif
       endif
c     if(reset1.eq.2)then
c        if(stage1.ge.3)then
c       if(breeding1.eq.1)then
c        dead1=1
c        endif
c       endif
c      endif
c    endif

      if(frogbreed1.eq.1)then
       if(breeding1.eq.1)then
c      if(conth1.gt.0)then
c       if((contdep1.le.0.1).or.(breeding1.eq.0))then
c       dead1=1
c       else
c       dead1=0
c       endif
         if(frogstage1.eq.1)then 
c        resetting at metamorphosis            
         if(stage1.eq.2)then
         dead1=1
         endif
         endif
c      endif
       endif
      endif

      if(frogbreed1.eq.1)then
       if(stage1.le.1)then
c      if(conth1.gt.0)then
         if(contdep1.le.0.1)then
          dead1=1
         endif
c      endif
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
      if(frogbreed1.eq.2)then
       if(stage1.eq.0)then
        if(E_H_pres1.le.E_Hb1*0.90)then
        if(contdep1.gt.50)then
         dead1=1
        endif
       endif
       endif
      endif

c    kill tadpoles if pond dries
      if(frogbreed1.eq.2)then
       if(stage1.eq.1)then
c      if(conth1.gt.0)then
         if(contdep1.le.0.1)then
c        dead1=1
      waiting1=1
      E_H_pres1=E_Hb1
      stage1=0
c       else
c        dead1=0
         endif
c      endif
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
c      if(conth1.gt.0)then
         if(contdep1.le.0.1)then
         dead1=1
      waiting1=0
c       else
c       dead1=0
         endif
c      endif
       endif
      endif

      if(frogbreed1.eq.3)then
       if(frogstage1.eq.1)then
        if(breeding1.eq.1)then
         if(stage1.ge.1)then
         dead1=1
      waiting1=0
c       else
c       dead1=0
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
c       else
c       dead1=0
         endif
        endif
       endif
      endif



      RETURN
      end