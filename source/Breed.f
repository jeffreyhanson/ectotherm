      SUBROUTINE BREED(julday,photostart,photofinish,lengthday
     &,daylengthstart,daylengthfinish,photodirs,photodirf,prevdaylength,
     &lat,firstday,breedact,breedactthres,tbs,hour,
     &breedtempthresh,breedtempcum,daycount)
       
      INTEGER breeding,julday,photostart,photofinish,photodirs,photodirf
     &,firstday,breedact,breedactthres,hour,starttime,endtime,
     &breedtempcum,daycount

      REAL daylengthstart,daylengthfinish,breedtempthresh,
     &lengthday,prevdaylength,lat,tbs,tbmean
      
      dimension tbs(24*7300)
      
      COMMON/BREEDER/breeding

c    photoperiod cues for breeding season
c    1=summer, 2=autumn, 3=spring, 4=winter
c    Jun-21 173 Dec-22 357 Mar-20 80 Sep-22 266            

      breedactthres=0
c    breedtempthresh=20
c    breedtempcum=7*24

      if(photostart.eq.0)then
       breeding=1
      endif

      if((photostart.gt.0).and.(photostart.lt.5))then
       if(lat.lt.0)then
c     southern hemisphere        
        if(photostart.eq.1)then
         if((julday.eq.357).or.(firstday.eq.1))then
         breeding=1
         endif
        endif
        if(photostart.eq.2)then
         if(julday.eq.80)then
         breeding=1
         endif
        endif
        if(photostart.eq.3)then
         if(julday.eq.173)then
         breeding=1
         endif
        endif
        if(photostart.eq.4)then
         if(julday.eq.266)then
         breeding=1
         endif
        endif
       else
c     northern hemisphere
        if(photostart.eq.1)then
         if(julday.eq.173)then
         breeding=1
         endif
        endif
        if(photostart.eq.2)then
         if(julday.eq.266)then
         breeding=1
         endif
        endif
        if(photostart.eq.3)then
         if(julday.eq.357)then
         breeding=1
         endif
        endif
        if(photostart.eq.4)then
         if(julday.eq.80)then
         breeding=1
         endif
        endif
       endif

       if(lat.lt.0)then
c     southern hemisphere        
        if(photofinish.eq.1)then
         if(julday.eq.357)then
         breeding=0
         endif
        endif
        if(photofinish.eq.2)then
         if(julday.eq.80)then
         breeding=0
         endif
        endif
        if(photofinish.eq.3)then
         if(julday.eq.173)then
         breeding=0
         endif
        endif
        if(photofinish.eq.4)then
         if(julday.eq.266)then
         breeding=0
         endif
        endif
       else
c     northern hemisphere
        if(photofinish.eq.1)then
         if(julday.eq.173)then
         breeding=0
         endif
        endif
        if(photofinish.eq.2)then
         if(julday.eq.266)then
         breeding=0
         endif
        endif
        if(photofinish.eq.3)then
         if(julday.eq.80)then
         breeding=0
         endif
        endif
        if(photofinish.eq.4)then
         if(julday.eq.357)then
         breeding=0
         endif
        endif
       endif

c    end check for seasonal breeding
      endif

      if(photostart.eq.5)then
c     using specified daylength thresholds for breeding
       if((lengthday.ge.daylengthstart).and.(prevdaylength.lt.
     &      lengthday))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirs.eq.1)then
           breeding=1
       if((lengthday.ge.daylengthfinish).and.(prevdaylength.lt.
     &      lengthday))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirf.eq.1)then
           breeding=0
          endif
       endif
       if((lengthday.le.daylengthfinish).and.(prevdaylength.gt.
     &      lengthday))then
c      we have reached the critical daylength for breeding cessation and day length is decreasing
          if(photodirf.eq.0)then
           breeding=0
          endif
       endif
           goto 1101
          else
           breeding=0
          endif
       else
        breeding=0
       endif

       if((lengthday.le.daylengthstart).and.(prevdaylength.gt.
     &      lengthday))then
c      we have reached the critical daylength for breeding initiation and day length is decreasing
          if(photodirs.eq.0)then
           breeding=1
       if((lengthday.ge.daylengthfinish).and.(prevdaylength.lt.
     &      lengthday))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirf.eq.1)then
           breeding=0
          endif
       endif
       if((lengthday.le.daylengthfinish).and.(prevdaylength.gt.
     &      lengthday))then
c      we have reached the critical daylength for breeding cessation and day length is decreasing
          if(photodirf.eq.0)then
           breeding=0
          endif
       endif
           goto 1101
          else
           breeding=0
          endif
       else
        breeding=0
       endif

       if((lengthday.ge.daylengthfinish).and.(prevdaylength.lt.
     &      lengthday))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirf.eq.1)then
           breeding=0
          endif
       endif

       if((lengthday.le.daylengthfinish).and.(prevdaylength.gt.
     &      lengthday))then
c      we have reached the critical daylength for breeding cessation and day length is decreasing
          if(photodirf.eq.0)then
           breeding=0
          endif
       endif
      endif

1101  continue

      
      if(breedact.lt.breedactthres)then
         breeding=0
      endif

      if(prevdead.eq.1)then
       if(breeding.eq.0)then
        dead=1
        goto 987
       endif
      endif

      tbmean=0
      starttime=int((daycount-1)*24+hour-breedtempcum)
      endtime=int((daycount-1)*24+hour)
      if(starttime.gt.0)then
       do 10 i=starttime,endtime
        tbmean=tbmean+tbs(i)
10     continue
       tbmean=tbmean/breedtempcum
       if(tbmean.gt.breedtempthresh)then
        breeding=0
       endif
      endif

      
987      RETURN
      end