      Subroutine Parect
c    This subroutine computes the configuration factor between two parallel 
c    identical rectangles, lying one above the other with the same edges of 
c    one directly above the other.  The rectangles are separated by distance, c.
c    Their dimensions are a (length) and b (width).
c    Copyright 2001 Warren P. Porter All rights reserved.

      Implicit none

      Real a,apl,b,bpl,cpl,fparlel
      Real grp1,grp2,grp3,grp4,grp5,grp6,sqrt1,pi,x,y

      Common/plates/apl,bpl,cpl,fparlel

      pi=3.14159265

      x = apl/cpl
      y = bpl/cpl

      grp1 = 2./(pi*x*y)
      a = 1 + x**2
      b = 1 + y**2
      sqrt1 = sqrt((a*b)/(a+y**2))
      grp2 = log(sqrt1)
      grp3 = (y*sqrt(a))*atan(y/sqrt(a))
      grp4 = (x*(sqrt(b)))*atan(x/sqrt(b))
      grp5 = y*atan(y)
      grp6 = x*atan(x)
      fparlel = grp1*(grp2  + grp3 + grp4 - grp5 - grp6)

      Return
      End
