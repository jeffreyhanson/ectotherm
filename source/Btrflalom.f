      Subroutine Btrflalom
C    This subroutine calculates the allometry and other angles for butterfly with wings.
c    surfaces 1,3 are the wings, surface 2 is the back of the body between the wings,
c    surface 4 is the imaginary surface between the top edges of the two wings. 
c    Surfaces 5 & 6 are the imaginary surfaces at the ends of the wings. 
c    Surfaces 7&8 are imaginary extensions of wings 1&3 respectively below the body 
c    until they meet if the wings are open more than 90 degrees from the back.  
c    All configuration factors are calculated here.  Due to symmetry F12 = F32,
c    F21=F23, F25 = F26, F52 = F62 
c    ***Primary assumption: long axis of body normal to incoming sun's direct beam &
c    center of back normal to incoming sun's direct beam ***

C    Copyright 2001 W.P. Porter all rights reserved

      Implicit none

      Real a,alpha,angle,apl,area1,area2,area3,area4,area5
      Real area6,aref,areaa,areax,areaw,Area4b
      Real b,bodlen,bpl,bref
      Real c,cpl,cref,ctest,capL,capw
      Real degangl
      Real F12,F13,F14,F15,F16
      Real f21,f23,f24,f25,f26,f2x
      Real F,F31,F32,f34,f35,f36
      Real f41,f42,f43,f45,f46
      Real f51,f52,f53,f54,f56
      Real f61,f62,f63,f64,f65
      Real fab
      Real f1x,fx1,fx2,fx3,fx4b,fx5,fxy,fy3,f4x,fx2b
      Real f2a1,f2ax,f2a5a
      Real fwy,fxz,fwz,fy4b,fy5,fparlel
      Real phi,pi,psi,phiref,rarea5
      Real sidea,sideb,sidex
      Real side1,side2,side3,side4,side7,side8
      Real side2b,side4b
      Real sum1all,sum2all,sum3all,sum4all,sum5all,sum6all
      Real term1,term2,test1,theta,trilen
      Real w,x,xprime,y,z,asilp

      integer i

      Dimension phi(20)

      Common/intgl/x,angle
      Common/masters/aref,bref,cref,phiref,phi
      Common/rectngl/a,b,c,F
      Common/plates/apl,bpl,cpl,fparlel
      common/f1x/F12,F13,F14,F15,F16
      common/f2x/f21,f23,f24,f25,f26
      common/f3x/F31,F32,f34,f35,f36
      common/f4x/f41,f42,f43,f45,f46
      common/f5x/f51,f52,f54,f56
      common/f6x/f61,f62
      common/fxx/f1x,fxy,fy3
      Common/As/Area1,Area2,Area3,Area4,Area4b,Area5,Area6,asilp
      common/lengths/sidex
      common/sums/sum1all,sum2all,sum3all,sum4all,sum5all,sum6all

      pi=3.14159265
      i=1

C    Defining characteristic lengths for area and configuration factor calc's
C    Width of body = a 
      side2 = aref
C    Length of body & wing (parallel to long axis of body) = b, a constant
      bodlen = bref
      b = bref
C    Width of wing (from junction w body to wing tip) = c
      side1 = cref
      side3 = side1
c    all other side lengths a function of angle between incoming 
c    solar beam and the wing (or angle from the body to the wing).

C    Body top area in m**2; input lengths, a,b, in cm
      Area2=(side2*bodlen)

C    Calculating wings
c    Testing for crossed wings 
      angle = phiref
      if(phi(i) .lt. 90.)then
c      small angle, check for 'short' wing width, c
c      use a right triangle, where right angle is at middle of back, a
        test1 = (side2/2.)/cos(angle)
        if(test1 .lt. cref)then
c        'short' wing (i.e. crossed wings)
c        Calculating area of one wing (m2)
          Area1 = bodlen*test1 
         else
c        Calculating area of one wing (m2)
          Area1 = bodlen*side1 
        endif
       else
c      Wings at >= 90 degrees from body; No shortened wing.
c      Calculating area of one wing (m2)
        Area1 = bodlen*side1 
      endif

c    Other wing(s) on opposite side of body are equal area
      Area3=Area1

C    Finding the lengths & areas for angle dependent imaginary surfaces
c    to calc. areas & configuration factors
c    Angle = angle from the back to a wing
c    Theta = angle from sun's direct beam to wing 
c    Psi = pi/2 - theta, the angle if the wing tips touch.
      If(phi(i) .gt. 90.)then
c      open on top. Angle > 90.
c      theta = wing angle - 90. at wing-back junction, 
c      psi is at the wing-imaginary surface 4 junction
        theta = angle - pi/2
        psi = pi/2. - theta
c      side4b is the part of 4 that extends beyond the edge of surface 2, the back
        side4b = side1*sin(theta)
        side4 = 2.*side4b + side2
c      sides 5&6 are right triangles, 4b,1,x plus two rectangles side2*sidex
        sidex = cos(theta)*side1
c      imaginary identical sides 5 & 6 are symmetric

c      Calculating areas (m2)
c      Wings at >= 90 degrees from body; No shortened wing.
        Area1 = bodlen*side1 
c      Area2 always the same. Calculated independent of angle
        Area3=Area1
        Area4=side4*bodlen 
c      Area of trapezoid = 0.5*height*(topside+bottomside). Area5=Area6
        Area5=0.5*(cos(theta)*side1)*(side2+side4)
        Area6 = Area5
        Areax = sidex * bodlen   

c      Getting Configuration factors (adjacent rectangles) when angle > 90
c      Getting F21: going from 2 to x to 1, where x is a plane normal to 2 
c      with a common edge at the wing - body junction
 
c      Note: Adjacent rectangle program only works for 0-90 deg angles between 2 planes
c      and it does not work well between 88 and 90 deg. Must use right rectangles 
c      subroutine for 90 deg. Use an imaginary surface, x, with common edge to surface 2 
c      and at a right angle to surface 2. The governing equation is:
c      Q21 = Q2x - Qx_4b - 2Qx5  (which after cancelling common terms epsilon, sigma and T1**4
c      A2*F21 = A2*F2x - Ax*Fx4b - 2*Ax*Fx5
c      Surface 4 is an imaginary surface across the top of the triangle x,4,1. Surface 5
c      is an imaginary surface on either end that is a right triangle.
c      F21 = (A2*F2x - Ax*Fx4b - 2*Ax*Fx5)/A2
c      Getting F2x
        c = side2
        a = sidex
        call rytrec
        f2x = f
c      Getting Fx4b
        c = sidex
        a = side4b
        call rytrec
        fx4b = f
c      Getting Fx1
        c = sidex
        a = side1
        angle = phiref - pi/2.
        call adjrec
        fx1 = f
c      Getting Fx5
        areax = sidex*bodlen
c      Calculation for small triangle on one side between imaginary surface x and surface 1
        Fx5 = (1.0 - fx1 - fx4b)/2.
c      Calculating F21
        f21 = f2x*fx1
        F12 = (area2/area1)*F21
        F23 = F21    

C      Getting F14 (F12 in Config. factor formula for adjacent rectangles)
C      Surface 1 edge = c in Sub. Adjrec
C      Surface 2 edge = a in Sub. Adjrec
C      Angle is the angle between the 2 surfaces in Sub. Adjrec.
       c=side1
        a=side4
        angle = pi/2. - theta
        degangl = angle*180./pi
        Call Adjrec
        F14 = F
        F41 =(Area1/Area4)*F14

c      Getting F13 in steps
c      Getting F1x (in Sub. Adjrec, f12, surf1 = c, surf2 = a)
        angle = phiref - pi/2.
        sidex = side1*cos(angle)
        c = side1
        a = sidex
        Call Adjrec
        F1x = F
c      Getting Fx2; side1 = side3, sidex = sidey, Fx1 = Fy3
        c=sidex
        a=side2
        Call rytrec
        Fx2 = F
c      Getting Fy3
        c = sidex
        a = side1
c      angle = phiref - pi/2.
        Call Adjrec
        Fy3 = F
c      Getting Fx5, 2 right rectangles with common edge sidex = b, c = bodlen, a = side2
        c = bodlen
        b = sidex
        a = side2
        Call Rytrec
        Fx5 = f
c      Getting fxy: parallel identical rectangles coincident with each other
c      a & b are rectangle dimensions, c is the distance between them
        apl = sidex
        bpl = bodlen
        cpl = side2
        Call Parect
        fxy = fparlel
c      Getting Fy4b, 2 right rectangles
        angle = phiref - pi/2.
        side4b = side1*sin(angle)
        c = sidex
        a = side4b
        Call Rytrec
        Fy4b = f
        fy5 = (1.0 - fy4b - fy3)/2.
        area1 = side1 * bodlen
        areax = sidex * bodlen
        term1 = 2.*areax*(fx2 + fx5)
c      areax = areay
        term2 = areax*fy4b + 2.*areax*fy5
c      F13 = (area1*f1x - term1 - term2)/area1
        F13 = f1x*fxy*fy3

c      Getting F24, parallel rectangles, for IR exchange, wing angle > 90
c      In the parallel rectangles subroutine, Parect
c      apl = width
c      bpl = bodlen
c      cpl = distance between plates
c      For trapezoidal cross sections, the short horizontal side,a, = w+x.
c      The long horizontal side,b, = y+z,    where w=y & x=z. 
c      w & y are sides of identical parallel plates 
c      from the common center line to the outer edge.
c      Getting config. factor,Fwy, between identical subplates w & y.
c      Side4 of surface4, the imaginary surface between the butterly wing tips,
c      is LONGER relative to Side2. Using symmetry & considering the body
c      on only 1 side of the body from a center line (plane) perpendicular to the back,
c      and along the center axis of the body, & defining the dimensions 
c      of one of the 2 identical parallel plates & the distance between the 2 plates.
            w = side2/2.
            x = (side4/2.) - w
            y = w
            z = x
            sidea = w + x
            sideb = y + z
c          Top surface width
            apl = w
c          Top surface length
            bpl = bodlen
c          Distance between identical parallel plates
            cpl = sidex
            Call Parect
            Fwy = fparlel

c          Getting config. factor,Fxz, between identical subplates x & z. 
c          This is determined by the larger parallel plate, in this case surface 2.
c          y=w
c           z = side4/2.- y
            apl = x
c          bpl & cpl are unchanged from current Fwy.
            Call Parect
            Fxz = fparlel

c          Aw*Fwz=Ay*Fyx; Ay*Fyx=Ax*Fxy
            Areaw = w*bodlen
            Areax = x*bodlen
            AreaA = Areaw+Areax
            area4=side4*bodlen
            area2 = side2*bodlen
c          Getting Fab between the idential parallel plates wx and yz
c          apl = sidea = x + w, b = bodlen, c = distance between plates; latter 2 unchanged.
            apl = sidea
            Call Parect
            Fab = fparlel
            Fwz = (1./(2*Areaw))*(AreaA*Fab -Areaw*Fwy - Areax*Fxz)

c          By symmetry above the back, there is one imaginary plate2 of the same dimensions
c          directly overhead flanked on either side by an imaginary surface, plate4,
c          that covers the rest of the open space between the wing tips.
c          By conservation of energy, A4*F42 = 2*(AwFwy+AwFwz)
c      By conservation of energy, A4*F42 = 2*(Aw*Fwy+Aw*Fwz)
        F24 = Fwy + Fwz
c      By reciprocity, A4*F42=A2*F24
        F42 = (Area2/Area4)*F24

        if(phi(i).ge.160.)then
c        compute F56 using parallel rectangles approximation for trapezoid end area
c        Area of trapezoid = 0.5*height*(topside+bottomside). Area5=Area6
c        Trapezoid area = rectangle area: 0.5*sidex*(topside+bottomside) = side2*xprime
          xprime = (side2+side4)/2.
c        getting equivalent area for a rectangle from the trapezoid
          rarea5 = sidex*xprime
c        Getting f56: parallel identical rectangles coincident with each other
c        a & b are rectangle dimensions, c is the distance between them
          apl = xprime
          bpl = sidex
          cpl = bodlen
          Call Parect
          F56 = fparlel
c        compute F52 using right rectangles and approximating surface 5 
c        as an equivalent rectangle
c        Getting F52
          c = xprime
c        common edge, b
          b = side2
          a = bodlen
          call rytrec
          f52 = f
c        Getting F54
          c = sidex
c        common edge, b
          b = xprime
          a = bodlen
          call rytrec
          f54 = f
c        Getting F51
          F51 = (1.0 - F56 - F54 - f52)/2.
          F53 = F51
        endif
c      Getting F25, F26 from conservation of energy; F22 = 0. F25=F26, F21=F23. 
c      These are the trapezoids making up the open ends at the head and tail of the body.
        F15 = (1.0 - F12 - F13 - F14)/2.
        F16 = F15


        F25 = 0.5*(1.0 - 2.*F21 - F24)
        F26=F25
        f43 = f41
        f45 = (1.0 - f41 - f42 - f43)/2.
        f46 = f45
        sum4all = f41+f42+f43+f45+f46
      Endif

c    End of wing angle > 90.

      If(phi(i) .eq. 90.)then
c      Getting Configuration factors (adjacent rectangles)
c      Getting F12
C      Body width 
        a=side2
        side4 = side2
        area1 = side1*bodlen
        area2 = side2*bodlen
        area3 = area1
        area4 = side4*bodlen
        area5 = side1*side2
        area6 = area5
c      Body length
        b = bodlen
C      Wing width
        c=cref
c      Calling right rectangles subroutine
        Call Rytrec
c      Wing to body config. factor
        F12 = F
c      Fwing - sky = Fwing - back when wings are parallel
        f14 = f12
c      Body to Wing config. factor
        F21 = (Area1/Area2)*F12
        F23 = F21
c      Getting Configuration factors (parallel idential rectangles with coaxial centers)
c      use parallel plates for wing to wing config. factors, F13; F31=F13, since Area1=Area3
        apl = side1
        bpl = bodlen
        cpl = side2
        Call Parect
        F13 = fparlel
c      Getting back to sky parallel plates config. factor, F24
        apl = side2
        cpl = side1
        Call Parect
        F24 = fparlel
c      Getting body to head imaginary surface and body to tail end imaginary surface; F25=F26.
        F25 = 0.5*(1.0 - 2.*F21 - F24)
        F26=F25
        f15 = (1.0 - f12 - f13 - f14)/2.
        f16 = f15
      Endif
c    End of wing angle = 90 deg.

      If(phi(i) .lt. 90.)then
c      Angle < 90; possibly closed on top
        ctest = (side2/2.)/cos(angle)
        if(ctest .lt. cref)then
c        'short' wing, i.e. completely closed wings
          side1 = ctest
          side3 = side1
          side4 = 0.0
          sidex = (side2/2.)*tan(phiref)
          Area1 = bodlen*side1
          area2 = bodlen*side2 
          Area4 = 0.0000
          Area5 = 0.5*side2*sidex
          area6 = area5 
          F14 = 0.000
          F24 = 0.000
          F34 = 0.000
          F54 = 0.000
          F64 = 0.000
c        getting F12.  Adjacent rectangles do not work well at small or large angles.
c        Must subdivide isoceles triangle into two adjacent right triangles joined 
c        at their bases, x, an imaginary line (surface) at right angles to the back,
c        surface 2, which is bisected into 2a & 2b. F21 = F2a_1 + F2b_x * Fx1
C        Body width
c        Getting F2a_1 : Adj. rect: c is first side, a is second side (Fca)
          c=side2/2.
          b = bodlen
C        Apparent wing width
          a=side1
          angle = phiref
          Call Adjrec
          F2a1 = F
c        Check: getting F2a_x
          c =side2/2.
          a = sidex
          Call rytrec
          F2ax = f
          f2a5a = (1.0 - f2ax - f2a1)/2.
c        From symmetry F25 = F2a_5a + F2b_5b
          F25 = 2.*f2a5a

c        Getting F12
          c = side1
          a = side2
          angle = phiref
          call adjrec
          F12 = f

c        Getting F21: By symmetry 2F21 + F24 + 2F25 = 1.0
          F21 = (1.0 - F24 - 2.*F25)/2.

          f23 = f21

c        Getting F13
c        wings closed; relevant angle now where wings meet
c        Getting angles: wing angle < 90
c        Sidex is distance from base of triangle (back) to where wing tips meet
c        alpha is angle between wing tip junction and plane x (bodlen*sidex)
          sidex = (side2/2.)*tan(phiref)
          alpha = pi/2. - phiref
c        making sure that at angles close to 90, height from body to 
c        plane between wing tips not greater than wing length.
          if(sidex .gt. cref)then
            sidex = cref
          endif

c        Adjacent rectangle for F13 using equivalent height plane with at right angles to surface1
c        computing F1x
          c = side1
          a = sidex
          b=bodlen
          angle = alpha
          Call Adjrec
          f1x = f
c        area of imaginary surface, x
          areax = a*b

c        computing Fx3
          side3 = side1
          c = sidex
          a = side3
          Call Adjrec
          fx3 = f
          f13 = f1x*fx3

          f15 = (1.0 - f12 - f13 -f14)/2.
          f16 = f15
c        F23 = F21; F24 = 0; F26 = F25
          F24 = 0.0
          F26 = F25
          if(phi(i).le. 20)then
c          compute F56 using parallel rectangles approximation for triangle area
c          Triangle area = rectangle area: 0.5*side2*sidex = side2*xprime
            xprime = sidex/2.
c          getting equivalent area for a rectangle from the triangle
            rarea5 = side2*xprime
c          Getting f56: parallel identical rectangles coincident with each other
c          a & b are rectangle dimensions, c is the distance between them
            apl = xprime
            bpl = side2
            cpl = bodlen
            Call Parect
            F56 = fparlel
c          compute F52 using right rectangles and approximating surface 5 
c          as an equivalent rectangle
c          Getting F52
            c = xprime
c          common edge, b
            b = side2
            a = bodlen
            call rytrec
            f52 = f
c          Getting F51
            F51 = (1.0 - F56 - f52)/2.
            F53 = F51
          endif
c        END OF WINGS CLOSED AT TIPS
         else
c        wings slightly open at tips, calc. F14
          degangl = angle*180./pi
          side1 = cref
          Side4 = side2 - 2.*cos(angle)*side1
          Area4=side4*bodlen
          side2b = (side2/2.)-(side4/2.)
          sidex = sin(phiref)*side1
          area5 = ((side2 + side4)/2.)*sidex
          area6 = area5 
          side7 = (side4/2.)/cos(angle)
          side8 = side7
C        Getting F14
c        The governing equation is: Q41 = Q4x - Qx_2b - 2Qx5
c        (which after cancelling common terms epsilon, sigma and T1**4
c        A4*F41 = A4*F4x - Ax*Fx2b - 2*Ax*Fx5
c        Surface 4 is an imaginary surface across the top of the triangle x,4,1. Surface 5
c        is an imaginary surface on either end that is a right triangle.
c        F41 = (A4*F4x - Ax*Fx2b - 2*Ax*Fx5)/A4
c        Getting F4x
          c = side4
          a = sidex
          call rytrec
          f4x = f
c        Getting Fx2b
          c = sidex
          side2b = side2/2. - side4/2.
          a = side2b
          call rytrec
          fx2b = f
c        Getting Fx1
          c = sidex
          a = side1
          angle = pi/2. - phiref 
          call adjrec
          fx1 = f
c        Getting Fx5
          areax = sidex*bodlen
c        Calculation for small triangle on one side between imaginary surface x and surface 1
          Fx5 = (1.0 - fx1 - fx2b)/2.
c        Calculating F41
          f41 = f4x*fx1
          F14 = (area4/area1)*F41
          F43 = F41
              
c        Getting F12
          c=side1
          a=side2
          angle=phiref
          side3=side1
          Call Adjrec
          F12 = F
          F21 = (Area1/Area2)*F12
          F23 = F21
c        Get F13
          if(side4 .gt. 0.01)then
c          significant gap between wing tips, use composite adjacent rectangles, 
c          if angle not close to 90 deg.
            degangl = phiref*(180./pi)
c          FOR ANGLE 85-89.999 USE PARALLEL PLATES, ELSE USE UPSIDE DOWN F13 FOR ANGLE > 90.
            if(phi(i) .ge. 85.)then
c            use parallel plates; the trapezoidal integration for adjacent rectangles doesn't do well
c            using the average distance between top & bottom of wing
              sidex = ((side2/2.)- (side4/2.))*tan(phiref)
              if(sidex .gt. cref)then
                sidex = cref
              endif
              apl = side1
              bpl = bodlen
              cpl = side4 + 0.5*((side2/2.)- (side4/2.))
              Call Parect
              F13 = fparlel
c            Using the same approximation to estimate F12 using right rectangles
              c = side1
              a = side2 - 0.5*((side2/2.)- (side4/2.))
              call rytrec
              f12 = f
              f21 = (area1/area2)*f12
            endif
            if(phi(i).lt.85.)then
c            still open at the top, but wing angle < 85 deg.
c            First compute imaginary surface height from surface 2 (back) 
c            to surface 4 (above & parallel to the back across the wing tips)
c            Surfaces x & y are parallel, x normal to the back (surf. 2) at wing 1 - back junction
c            Surface y normal to the back at wing 3 - back junction
c            Phiref is current back to wing angle
              sidex = side1*sin(phiref)

c            Getting F13: F13 = f1x*fxy*fy3
c            Getting f1x by adjacent rectangles
              c = side1
              a = sidex
              angle = pi/2. - phiref
              call adjrec
              f1x = f
c            Getting fxy by parallel rectangles
c            Dimensions of one of identical rectangles
              apl = sidex
              bpl = bodlen
c            Distance between plates
              cpl = side4
              Call Parect
              fxy = fparlel
c            Getting fy3: x = y; side1 = side3
              c = sidex
              a = side1
c            angle is the same as above
              Call Adjrec
              fy3 = f
              F13 = f1x*fxy*fy3
            endif
c          End wing tips open; back-wing angle < 90 deg.
          else
c          wing tips touching - use adjacent rectangles with wings 'joined'
c          at wing tips as rectangles to form an isoceles triangle that is composed of
c          two right triangles lying on their backs (side2/2) like two bookends. 
c          Imaginary surface x is their common base.
            sidex = (side2/2.)*tan(phiref)
            area5 = 0.5*side2*sidex
c          F13 = f1x*fx3
c          Getting f1x by adjacent rectangles
            c=side1
            side3=side1
            a=sidex
c          Getting angle
            angle = pi/2. - phiref
            call adjrec
            f1x = f
c          Getting fx3
            c = sidex
            a = side3
c          Same angle
            call adjrec
            fx3 = f
c          No effective gap at wing tips
            f13 = f1x*fx3
            f14 = 0.00
          endif        

c        Getting F24 using composite parallel rectangles
c        In the parallel rectangles subroutine, Parect
c        apl = width
c        bpl = bodlen
c        cpl = distance between plates
c        For trapezoidal cross sections, the short side area,a, = w+x.
c        The long side area,b, = y+z,    where w=y & x=z. 
c        w & y are identical parallel plates from the common center line to the outer edge.
c        Getting config. factor,Fwy, between identical subplates w & y.
c        Side4, the imaginary surface between the butterly wings,
c         is now shorter relative to Side2. Using symmetry & taking half the short side.
c        Defining the dimensions of one of the identical parallel plates & distance between.
          if(side4.lt. 0.01)then
c          too small for composite parallel plates - assume no side4 - skip
            f24 = 0.00
            f42=0.00
            area4 = 0.00
           else
c          composite plates on top and bottom; dividing in half to take advantage of symmetry
            w = side4/2.
            x = (side2/2.) - w
            y = w
            z = x
            sidea = w + x
            sideb = y + z
c          Top surface width
            apl = w
c          Top surface length
            bpl = bodlen
c          Distance between identical parallel plates
            cpl = sidex
            Call Parect
            Fwy = fparlel
c          Getting config. factor,Fxz, between identical subplates x & z. 
c          This is determined by the larger parallel plate, in this case surface 2.
c          y=w
c           z = side2/2.- y
            apl = z
c          bpl & cpl are unchanged.
            Call Parect
            Fxz = fparlel
c          Aw*Fwz=Ay*Fyx; Ay*Fyx=Ax*Fxy
            Areaw = w*bodlen
            Areax = x*bodlen
            AreaA = Areaw+Areax
            area4=side4*bodlen
            area2 = side2*bodlen
c          Getting Fab between the idential parallel plates wx and yz
c          apl = sidea = x + w, b = bodlen, c = distance between plates; latter 2 unchanged.
            apl = x + w
            Call Parect
            Fab = fparlel
            Fwz = (1./(2*Areaw))*(AreaA*Fab -Areaw*Fwy - Areax*Fxz)
c          By symmetry above the back, there is one imaginary plate2 of the same dimensions
c          directly overhead flanked on either side by an imaginary surface, plate4,
c          that covers the rest of the open space between the wing tips.
c          By conservation of energy, A4*F42 = 2*(AwFwy+AwFwz)
            F42 = Fwy + Fwz
c          By reciprocity, A4*F42=A2*F24
            F24 = (Area4/Area2)*F42
          endif
c        F21+F22+F23+F24+F25+F26=1.0; F22,F24=0; F26=F25; F21=F23
          f15 = (1.0 - f12 - f13 - f14)/2.
          f16 = f15
          f23 = f21
          F25 = (1.0 - f21 - f23 - f24)/2.
          F26 = F25
        endif
      Endif
      if(side4 .gt. 0.000000)then
        area4 = side4*bodlen
        f41 = (area1/area4)*f14
        f42 = (area2/area4)*f24
        f43 = f41
        f45 = (1.0 - 2.*f41 - f42)/2.
        f46 = f45
        sum4all = f41+f42+f43+f45+f46
       else
        area4 = 0.0
        f41 = 0.0
        f42 = 0.0
        F43 = 0.0
        f45 = 0.0
        f46 = 0.0
        f54 = 0.0
        sum4all = 0.0
      endif
c    End of wing angle < 90 deg.

      if(phi(i).eq. 180.)then
        f12 = 0.0
        f13 = 0.0
        f14 = 1.0
        f15 = 0.0
        f16 = 0.0
        f21 = 0.0
        f23 = 0.0
        f24 = 1.0
        f25 = 0.0
        f31 = 0.0
        f32 = 0.0
        f34 = 1.0
        f35 = 0.0
        f36 = 0.0
        f51 = 0.0
        f52 = 0.0
        f53 = 0.0
        f54 = 0.0
        f56 = 0.0
        f61 = 0.0
        f62 = 0.0
        f63 = 0.0
        f64 = 0.0
        f65 = 0.0
      endif

      F32 = F12
      F31 = F13
      f34 = f14
      f35 = f15
      f36 = f35
      if(phi(i).gt. 20.) then
        if(phi(i).lt.160.)then
          f51 = (area1/area5)*f15
          f52 = (area2/area5)*f25
          f53 = f51
          f54 = (area4/area5)*f45
          f56 = (1.0 - f51 - f52 - f53 - f54)
        endif
      endif
      f61 = f51
      f62 = f52
      f63 = f53
      f64 = f54
      f65 = f56

c    checking for f52 from chart from Sauer
      trilen = side2*sin(angle)
      capL = trilen/side2
      capw = bodlen/side2

      area4b=side4b*bodlen
c     write(*,*)'side4b, side4b = ',side4b,bodlen
      sum1all = f12+f13+f14+f15+f16
      sum2all = f21+f23+f24+f25+f26
      sum3all = f31+f32+f34+f35+f36
      sum5all = f51+f52+f53+f54+f56
      sum6all = f61+f62+f63+f64+f65

      Return
      End

