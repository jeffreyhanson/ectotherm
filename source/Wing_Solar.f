      Subroutine WingSolar
C    This subroutine computes the solar radiation absorbed by the 
c    body of a butterfly using an Fhat solution for all diffuse
c    reflections
C    Copyright 2001 W.P. Porter all rights reserved

      Implicit none

      Real abs1,abs3,alpha2,fref
      Real aref,bref,cref,pctdif,phiref,phi,pi
      Real Qdiff,Qdir,Qsol,Qsolar,rho1,rho2,rho3,rho1_3,trans1
      Real Soldif,Soldir,QsolarW,QsolarT
      Real Area1,Area2,Area3,Area4,Adir,Ardir,Area4b,Area5,Area6
      Real F12,F13,F14,F15,F16
      Real f21,f23,f24,f25,f26
      Real F31,F32,f34,f35,f36
      Real f41,f42,f43,f45,f46,f51,f52,f61,f62
      Real fref21,fref31,asilp

      Dimension phi(20)

      common/f1x/F12,F13,F14,F15,F16
      common/f2x/f21,f23,f24,f25,f26
      common/f3x/F31,F32,f34,f35,f36
      common/f4x/f41,f42,f43,f45,f46
      common/f5x/f51,f52
      common/f6x/f61,f62
      Common/As/Area1,Area2,Area3,Area4,Area4b,Area5,Area6,asilp
      Common/fluxes/QsolarW,QsolarT,Qsol
      Common/refls/rho1_3,rho2,trans1
      Common/masters/aref,bref,cref,phiref,phi

      data pi/3.14159/
c    Percent total solar that is diffuse
      data pctdif/0.15/

c    Absorptivity of the back on the body
      alpha2 = 1.0 - rho2
c    Reflectivity of the wings, surfaces 1 & 3
      rho1 = rho1_3
      rho3 = rho1_3
c    Incident solar radiation (W/m2)
c    Qsol = 1000.
      Soldir = (1.0 - pctdif)*Qsol
      Soldif = pctdif*Qsol

c    Amount reflected from each wing
      abs1 = rho1*area1*Qsol
      abs3 = rho3*area3*Qsol

c    Configuration factor for wing #1 to back surface #2
      Fref=f12/(1.0 - rho3*f13-rho1*rho2*f12*f21-rho2*rho3*f12*f23)
c    Configuration factor for back surface #2 to wing #1 
      Fref21=f21/(1.0-rho3*f23-rho1*rho2*f12*f21-rho1*rho3*f21*f13)
c    Configuration factor for wing #3 to wing #1 
      Fref31=f31/(1.0-rho2*f32-rho1*rho3*f31*f13-rho1*rho3*f21*f13)

c    Total solar absorbed by the butterfly dorsal surface between the wings.
c    Qsolar = Qdir + Qdiff,12 + Qdiff,32
      Adir = area4/area2
      if(adir.gt.1.0)then
        ardir = asilp
       else
        ardir = asilp*adir
      endif
      Qdir = alpha2*ardir*Soldir
      if(area4 .le. 0.000000)then
c      completely closed wings.  Only transmitted diffuse gets to back
        Qdiff = trans1*alpha2*Qsol
       else
        if(phiref.gt.pi/2)then
          Qdiff = 2.*rho1*alpha2*area1*Fref*Qsol
         else
          Qdiff = 2.*trans1*alpha2*area1*Fref*Qsol
        endif
      endif
              
      Qsolar = Qdir + Qdiff
      QsolarT=Qsolar

c    Total solar absorbed by the butterfly wing.
c    Qsolar = Qdir + Qdiff,21 + Qdiff,31
      ardir = Area4b
      Qdir = (1-rho3)*ardir*Soldir
      Qdiff = rho3*area4b*Fref31*(1-rho3)*Qsol+QsolarT*(1-rho3)*
     &    Fref21

      Qsolar = Qdir + Qdiff
      QsolarW=Qsolar
       
c     write(*,*)'Soldir, Soldif, ardir,Qdir,Qdiff = ',
c     &Soldir,Soldif,ardir,Qdir,Qdiff,phiref
      Return
      end