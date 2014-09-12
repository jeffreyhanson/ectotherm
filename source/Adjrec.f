      Subroutine Adjrec
C    Subroutine to compute the configuration factor between 
C    adjacent rectangles with angle, angle, between them.
C    Configuration figure 2, Chapter 7., 'Animal Landscapes' by Porter
C    Copyright 2001 W.P. Porter all rights reserved

      Implicit none

      Real a,atan1,b,c,x,y,z,pi,angle,F
      Real log1,log2,log3
      Real p,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,palpha
      Real pa,pb,pc,pd,pp3,p5a,p5b
      Real StartV,EndV,S,sqrt1

      EXTERNAL func

      Common/intgl/x,angle
      Common/rectngl/a,b,c,F

      pi = 3.14159265

C    Width of body = a 
C    Length of body & wing (parallel to long axis of body) = b
C    Width of wing (from junction w body to wing tip) = c
      x=a/b
      y=c/b
      z=x**2 + y**2 - 2.*x*y*cos(angle)
      
C     Calculate F12 parts of the equation first  
      p = sin(2*angle)/4.    
      p1 = x*y*sin(angle)
      p2 = ((pi/2.)-angle)*(x**2+y**2)
      pp3 = (x-y*cos(angle))/(y*sin(angle))
      p3 = y**2* atan(pp3)
      p4 = x**2*atan((y-x*cos(angle))/(x*sin(angle))) 
      palpha = -p*(p1+p2+p3+p4)

c    A trig equivalence: sin**2(angle) = (0.5*(1.-cos(2*angle)))
c    A trig equivalence: cos**2(angle) = (0.5*(1.+cos(2*angle)))
      p5a = (2./(0.5*(1.-cos(2*angle)))-1.0)
      log1 = ((1.+x**2)*(1.+y**2))/(1+z)
      if(log1.lt.0.000000)then
c      write(0,*)'log1 is negative in Adjrec'
c      pause
      endif
      p5b = log(log1)
      p5 = p5a*p5b
      log2 = (y**2*(1.+z))/((1.+y**2)*z)
      if(log2.lt.0.000000)then
c      write(0,*)'log2 is negative in Adjrec'
c      pause
      endif
      p6 = y**2 *log(log2)
      log3 = (x**2*(1.+x**2)**cos(2*angle))/
     &(z*(1.+z)**cos(2*angle))
      if(log3.lt.0.000000)then
c      write(0,*)'log3 is negative in Adjrec'
c      pause
      endif 
      p7 = x**2 *log(log3)
      p8 = ((0.5*(1.-cos(2*angle)))/4.)* (p5 + p6 + p7)
      p9 = y*atan(1./y)
      p10 = x*atan(1./x)
      p11 = z**0.5*atan(1./z**0.5)

      pa = (sin(angle)*sin(2.*angle))/2.
      sqrt1 = 1.+x**2*(0.5*(1.-cos(2*angle)))
      if(sqrt1.lt.0.000000)then
c      write(0,*)'sqrt1 is negative in Adjrec'
c      pause
      endif
      pb = x*sqrt(sqrt1)

      pc = atan((x*cos(angle))/sqrt(sqrt1))

      atan1 = (y-x*cos(angle))/sqrt(sqrt1)
      pd = atan(atan1)
c    was p12 = pa*pb*pc+pd, Kearney
      p12 = pa*pb*(pc+pd)
  
C    Setting up numerical integrator, Qromb, from Numerical Recipes
C    Starting & ending values for the integration = StartV,EndV
C    Integral = S. It comes from Sub. Trapzd
C    Func = function to integrate
      StartV = 0.0
      EndV = Y
      Call qromb(func,StartV,EndV,S)
      p13 = cos(angle)*S 
                  
      F = (1./(pi*y))*(palpha+p8+p9+p10-p11+p12+p13)
      Return
      End


