	Function Func(xi)
C	Function Xi calculates Xi as part of the adjacent rectangles
C	configuration/numerical integration calculations.

C	Copyright 2001 W.P. Porter all rights reserved

	Implicit none
	
	Real x,angle,ans,fa,xi,fb1,fb2,fb,fc1,fc2,fc,func,degangl,pi 
	Common/intgl/x,angle
	data pi/3.14159/

c	x = lengths ratio, a/b
c	xi = all (sequential) values between integration limits
c	sin**2(angle) = 0.5*(1.-cos(2*angle)
c	debugging info: angle in degrees
	
	degangl = angle*180./pi

	fa = sqrt(1.0 + xi**2*(0.5*(1.-cos(2*angle))))

	fb1 = (x-xi*cos(angle))
	fb2 = sqrt(1.0+xi**2* (0.5*(1.-cos(2*angle))))
	fb = atan(fb1/fb2)
	
	fc1 = xi*cos(angle)
	fc2 = sqrt(1.0+xi**2* (0.5*(1.-cos(2*angle)))) 
	fc = atan(fc1/fc2)
		 
	ans = fa * (fb + fc)
      func = ans
	Return
      End