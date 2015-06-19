        SUBROUTINE dget_aELE(N,a,aELE,daELE,RPAR,IPAR)
      integer N,IPAR
      double precision aELE,daELE,RPAR
      double precision a,V,e,r,p_C,dE,dL,E_R,f,k_M,k_E,p_J,p_Am,E_m,g
     &,kap,L,dE_R,dER,e_s        
        DIMENSION aELE(N),daELE(N),RPAR(*),IPAR(*)
      f=RPAR(1)
      k_M=RPAR(2)
      k_E=RPAR(3) 
      p_J=RPAR(4)
      p_Am=RPAR(5)
      E_m=RPAR(6)
      g=RPAR(7)
      kap=RPAR(8) 
      a  = aELE(1)! % d, time since birth
      E  = aELE(2)! % J, reserve
      L  = aELE(3)! % cm, structural length
      E_R= aELE(4)! % J, reproduction buffer
      
      V = L**3D+00                          ! cm^3, structural volume
      e_s = E/ V/ E_m                    ! -, scaled reserve density
      r = (e_s * k_E - g * k_M)/ (e_s + g) ! 1/d, specific growth rate
      p_C = E * (k_E - r)              ! J/d, mobilisation rate
      
      dE = f * p_Am * V - p_C          ! J/d, change in reserve
      dL = r * L/ 3D+00                   ! cm/d, change in length
      dE_R = (1 - kap) * p_C - p_J     ! J/d, change in reprod buffer
      
      dER = dE_R/ V - r * (E_R/ V)       ! J/d.cm^3, change in [E_R]
      daELE(1)=1.0D+00
      daELE(2)=dE
      daELE(3)=dL
      daELE(4)=dE_R
        RETURN
        END       