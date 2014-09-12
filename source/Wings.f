      subroutine Wings(rho1_31,rho21,trans11,Qsol1,aref1,bref1,cref1,
     &phi1,F211,f311,f411,f511,f611,f121,f321,f421,f521,sidex1,QsolarW1,
     &QsolarT1,Area11,Area21,Area31,Area41,Area4b1,Area51,Area61,
     &F131,F141,F151,F161,F231,f241,f251,f261,asilp1)
      
C     This program computes the radiant exchange between two rectangles of the
C     same length on the common edge. They may be different widths.
c    One edge is common to both.  They can be at any angle to each other.
c    This is an implimentation of the configuration factor #2, p.340, in Appendix A of 
c    Sparrow,E.M and R.D. Cess. 1966.  Radiation Heat Transfer.  Wadsworth Publ.
c    Co., Inc.  Belmont, CA.366 p
C    Copyright 2001 W.P. Porter all rights reserved
c    Currently this assumes uniform color and temperature of the wings.
      Implicit none

      Real a,aref,b,bref,c,cref,x,phi,pi,angle
      Real phiref
      Real Qsol,rho1_3,rho2,QsolarW,QsolarT
      Real Area1,Area2,Area3,Area4,Area4b,Area5,Area6
      Real F12,F13,F14,F15,F16
      Real f21,f23,f24,f25,f26
      Real F,F31,F32,f34,f35,f36
      Real f41,f42,f43,f45,f46
      Real f51,f52,f54,f56,f61,f62
      Real f1x,fxy,fy3
      Real sidex,surfac,trans1
      Real sum1all,sum2all,sum3all,sum4all,sum5all,sum6all
      real rho1_31,rho21,trans11,Qsol1,aref1,bref1,cref1,phi1,F211,
     &f311,f411,f511,sidex1,QsolarW1,QsolarT1,f121,f321,f421,f521
      real area11,area21,area31,area41,area4b1,area51,area61
      real f131,f141,f151,f161,f611,F231,f241,f251,f261,asilp,asilp1

      Integer i,i2,ndeg
       
      Dimension phi(20)

      Common/intgl/x,angle
      Common/masters/aref,bref,cref,phiref,phi
      Common/rectngl/a,b,c,F
      Common/As/Area1,Area2,Area3,Area4,Area4b,Area5,Area6,asilp
      Common/fluxes/QsolarW,QsolarT,Qsol
      Common/refls/rho1_3,rho2,trans1
      common/f1x/F12,F13,F14,F15,F16
      common/f2x/f21,f23,f24,f25,f26
      common/f3x/F31,F32,f34,f35,f36
      common/f4x/f41,f42,f43,f45,f46
      common/f5x/f51,f52,f54,f56
      common/f6x/f61,f62
      common/fxx/f1x,fxy,fy3
      common/lengths/sidex
      common/sums/sum1all,sum2all,sum3all,sum4all,sum5all,sum6all

      i=0
      asilp=asilp1
      pi=3.14159
C    Number of different angles to process
      ndeg = 1
c    Setting output surface to other surfaces (use a real number)
      surfac = 5.0

      rho1_3=rho1_31
      rho2=1-rho21
      trans1=trans11
      Qsol=Qsol1
c    convert lengths to metres
      aref=aref1/100
      bref=bref1/100
      cref=cref1/100
      phi(1)=phi1

c      I1=5 
      I2=6 
C     USE UNIT I1 FOR INPUT - ANY UNIT BUT 0 WILL DO 
c      OPEN (I1,FILE='DATA.DAT')

C     USE UNIT I2 FOR OUTPUT - ANY UNIT BUT 0 AND 5 ARE OK   
c      OPEN (I2, FILE='OUTPUT')  
C    Read wing reflectances(surfaces 1,3),torso(surface 2), incoming solar (W/cm2)
c    Typical solar radiation of 1000 w/m2 = 0.1 W/cm2
c    Read(i1,*)rho1_3,rho2,trans1,Qsol
c    Write(i2,*)'Wing reflectance = ',rho1_3,'Wing trans. = ',trans1,
c     & ' Body refl. = ',rho2,' Total solar on horiz. surf. = ',Qsol,
c     &'W/cm2'
           
C     Read the three dimensions (cm); for Sub. Allom 
C     and the angle between the two rectangles, phi (degrees)
c    aref = width of surface #2 (back or horizontal or reference surface)
c    bref = common length where the two rectangles join
c    cref = width of surface #1 (wing)     
c      Read(i1,*)aref,bref,cref
c      Write(i2,*)aref,bref,cref
c      Read(i1,*)(phi(i),i=1,ndeg)
c      Write(i2,*)(phi(i),i=1,ndeg)         
      
C     Begin calculations
      Do 1 i=1,ndeg
C       Degrees to radians
        angle = phi(i)*pi/180.
        phiref = angle
C      Body width 
        a=aref
C      Body & wing length (parallel to long axis of body)
        b=bref
C      Wing width
        c=cref
c      Call Allometry subroutine for butterfly to get areas and configuration factors, F
        Call Btrflalom
        Call WingSolar
c      Put labels on output on 1st iteration
        If (i.eq.1)then
c          Write(i2,*)'Angle(deg) F12 f13 f14 f15 sidex'
c     &    ,' Qsolar '
c        Write(0,*)'Angle(deg) F12 f13 f14 f15 sidex'
c     &        ,' Qsolar '
        Endif
c      Write the output
c        Write(i2,100)phi(i),F12,f13,f14,f15,sidex,Qsolar
c      Write(0,100)phi(i),F12,f13,f14,f15,sidex,Qsolar
1     Continue
c    End of wing degrees changing loop

c100   Format(1F4.0,6E11.3)

cc    Write(0,*)'Program done. Results in file Output'
      F211=F21
      f311=F31
      f411=f41
      f511=f51
      f121=f12
      f321=f32
      f421=f42
      f521=f52
      f131=f13
      f141=f14
      f151=f15
      f161=f16
      f611=f61
      f231=f23
      f241=f24
      f251=f25
      f261=f26
      sidex1=sidex
      QsolarW1=QsolarW   
      QsolarT1=QsolarT  
      Area11=area1
      area21=area2
      area31=area3
      area41=area4
      area4b1=area4b
      area51=area5
      area61=area6
c      Stop
      return
      End 
       
                   
