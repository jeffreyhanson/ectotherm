       SUBROUTINE DEB_Baby 

c    Michael Kearney's implementation of Kooijman's k-rule DEB model, based on excel spreadsheet, for baby of viviparous reptile           

      Implicit None

      real E_baby,V_baby,V_pres,EH_baby    
      real dVdt,dEdt,E_pres,e_baby_init,EH_baby_init
      REAL zfact
      real e_init_baby
      REAL T_A,TAL,TAH,TL,TH,Tcorr,Tb
      REAL T_Ref
      REAL k_R,p_Am,p_Mv,vdot
      REAL f,k_Mdot
      REAL w_E,mu_E,mu_V,w_V,M_V,E_egg
      REAL E_M,E_G,kappa
      REAL d_V
      REAL eggdryfrac,E_Hmoult1,E_Hmet,E_Hecl
      REAL g,E_scaled,V_max,L_T,L_pres,L_max,scaled_l
      REAL dLdt,v_temp,Sc,dUEdt,E_temp,U_H_pres,dUHdt,dE_Hdt,E_H_pres

      REAL Tc,daylengthstart,daylengthfinish,lengthday,lengthdaydir
     &,prevdaylength,lat,k_J,kappa_X,kappa_X_P,mu_X,mu_P

      real lambda,funct
      REAL v_init,E_init,ms_init,cumrepro_init,q_init,hs_init,
     &cumbatch_init,p_Mref,vdotref,h_aref,maxmass,halfsat,x_food,
     &v_baby_init,v_init_baby,k_Jref,s_G,surviv_init,p_Xmref,E_H_start
      REAL andens_deb,delta_deb,clutchsize,msm,E_Hb,E_Hp,E_Hj,E_H_init
      real breedrainthresh,w_N,w_P,w_X,TWING,clutcha,clutchb
      INTEGER countday,breeding,
     &viviparous,pregnant,daycount,batch,photostart,photofinish,
     &photodirs,photodirf,frogbreed,frogstage,metamorph
      integer metab_mode,stages,breedact,breedactthres
      real p_Am1,p_AmIm,disc
      real Vold_init,Vpup_init,Epup_init,E_Hpup_init,gam,Vb

      Common/Treg/Tc,TWING

      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch,MsM
     &,lambda,breedrainthresh,daylengthstart,daylengthfinish,photostart
     &,photofinish,lengthday,photodirs,photodirf,lengthdaydir
     &,prevdaylength,lat,frogbreed,frogstage,metamorph
     &,breedactthres,clutcha,clutchb
      COMMON/DEBPAR3/metab_mode,stages,p_Am1,p_AmIm
     &,disc,gam,E_Hmoult1,E_Hmet,E_Hecl,Vb
      COMMON/DEBINIT/v_init,E_init,ms_init,cumrepro_init,q_init,
     &hs_init,cumbatch_init,p_Mref,vdotref,h_aref,e_baby_init,
     &v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,halfsat,x_food,
     &Vold_init,Vpup_init,Epup_init,E_Hpup_init,p_Xmref
      common/debbaby/v_baby,e_baby,EH_baby
      common/vivip/viviparous,pregnant
      COMMON/BREEDER/breeding
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      COMMON/COUNTDAY/COUNTDAY,daycount

c    Body temperature    
      Tb = Tc
c    Arrhenius temperature correction factor
      Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tb)))/(1+EXP(TAL
     &*(1/(273+Tb)-1/TL))+EXP(TAH*(1/TH-1/(273+Tb))))


      
         V_pres = v_baby
         E_pres = E_baby
         E_H_pres = EH_baby

        if(V_pres.eq.v_init_baby)then
         E_pres=E_egg/V_pres
        endif

      f = 1.

c    temperature corrections and compound parameters
      M_V = ANDENS_deb/w_V
      p_Mv = p_Mref*Tcorr
      k_Mdot = p_Mv/E_G
      k_J = k_Jref*Tcorr
      p_Am = p_Mv*zfact/kappa
      vdot = vdotref*Tcorr
c    fudge for sceloporus
c    vdot = 0.01132/24*Tcorr
      E_M = p_AM/vdot
c    p_Xm = p_Am/kappa_X
      g = E_G/(kappa*E_M)
      E_scaled=E_pres/E_m
      V_max=(kappa*p_Am/p_Mv)**(3.)
c    h_a = h_aref*Tcorr
      L_T = 0.
      L_pres = V_pres**(1./3.)
      L_max = V_max**(1./3.)
      scaled_l = L_pres/L_max

c     embryo equation for length, from Kooijman 2009 eq. 2
       dLdt = (vdot*E_scaled-k_Mdot*g*V_pres**(1./3.))/(3*(E_scaled+g))
       V_temp=(V_pres**(1./3.)+dLdt)**3
       dVdt = V_temp-V_pres

c     embryo equation for scaled reserve, U_E, from Kooijman 2009 eq. 1       
       Sc = L_pres**2*(g*e_scaled)/(g+E_scaled)*
     &   (1+((k_Mdot*L_pres)/vdot))
       dUEdt = -1*Sc 
       E_temp=((E_pres*V_pres/p_Am)+dUEdt)*p_Am/(v_pres+dvdt)
       dEdt=E_temp-E_pres

c     embryo equation for scaled maturity, U_H, from Kooijman 2009 eq. 3
       U_H_pres=E_H_pres/p_Am    
       dUHdt=(1-kappa)*Sc-k_J*U_H_pres
       dE_Hdt=dUHdt*p_Am


c         dEdt = p_Am*V_pres**(-1./3.)*(0-E_pres/E_m)
c
c    if(E_pres.gt.0.)then
c        dVdt = ((kappa*p_Am*f)*V_pres**(2./3.)-p_Mv*V_pres)/
c     &    (kappa*E_pres+E_G)
c    else
c        dVdt = ((kappa*p_Am*0)*V_pres**(2./3.)-p_Mv*V_pres)/
c     &    (kappa*E_pres+E_G)
c    endif

      V_baby = V_pres+dVdt
      E_baby = E_pres+dEdt
      EH_baby = E_H_pres+dE_Hdt
c    make sure ED doesn't go below zero
      if(E_baby.lt.0)then
       E_baby=0
      endif
      
      RETURN   
      END  



