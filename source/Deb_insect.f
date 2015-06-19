       SUBROUTINE DEB_INSECT(hour)  

c    Michael Kearney's implementation of Maino's body condition (reserve density)-based DEB model
c    for insects - 26 Dec 2013           

      Implicit None
c
c      EXTERNAL JAC
c    EXTERNAL GUT
      external dget_aELE,SOLOUT
c      DOUBLE PRECISION Y,YDOT,T,TOUT,RTOL,ATOL,RWORK
      DOUBLE PRECISION dVdt,dqdt,rdot,dhsds,Sc,dUedt,E_temp,dEdt,dE_Hdt
     &    ,dUHdt,dsurvdt,hs,v_pres,vold,e_pres,p_C,e_scaled,p_G,p_R,dLdt
     &,ED,V,vold_pres,L_pres,p_Am_acc,v_acc,p_Xm_acc,v_temp,L_instar,L_b
     &,L_j,ER,dER,ER_old,V_b,vTdot,pT_Am,pT_Xm,E_M,vTdotj,kT_E,kT_El
     &,V_max,scaled_l,g,E_Rj,L_max,E_Hthresh,U_H_pres,p_L,dVpup_dt,dEdt1
     &,dVold_dt,vpup,epup,Vpup_pres,Epup_pres,p_M,V_init,E_init,p_B,
     &Vold_init,Vpup_init,Epup_init,s_j,Tcorr,pT_M,kT_Mdot,kT_J,p_J,Tb
     &,cumrepro_init,cumbatch_init
      REAL SVL,v_baby,e_baby,EH_baby,v_baby1,e_baby1,EH_baby1,funct    
      REAL gutfull,orig_clutchsize
      REAL ms_init,v_baby_init,EH_baby_init
      REAL e_baby_init,e_init_baby,v_init_baby,wetstorage,wetgonad
      REAL wetfood,surviv_init
      REAL E_H_pres,E_H_init,E_H,wetmass,orig_MsM
      REAL zfact,causedeath,deathstage
      REAL T_A,TAL,TAH,TL,TH
      REAL T_Ref,E_Hb,halfsat,x_food,E_H_start
      REAL k_R,p_Am,p_A,p_D
      REAL f,X,p_X,food,annfood
      REAL w_E,mu_E,mu_V,w_V,M_V,E_egg
      REAL E_G,kappa,cumrepro,cumbatch
      REAL d_V,GH2OMET,p_b_past
      REAL q,h_a,ms,ms_pres,dmsdt,MsM,ms_past
      REAL PI,eggdryfrac,hs_pres,q_pres
      REAL QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      REAL AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      REAL Tc,depsel,tcores
      REAL QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,clutchsize
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      REAL tknest,clutchenergy
      REAL acthr,actxbas,thconw
      REAL TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask,temerge
      real ctmin,ctmax,eggsoil
      REAL q_init,hs_init,p_Mref,vdotref,h_aref,maxmass,p_xmref,k_Jref
     &,s_G
      REAL andens_deb,delta_deb,daylengthstart,
     &daylengthfinish,lengthday,lengthdaydir,prevdaylength,lat
      REAL CONTH,CONTW,CONTVOL,CONTDEP,rainfall,lambda,breedrainthresh
      real JMCO2,JMH2O,JMO2,JMNWASTE,JOJx,JOJv,JOJe,JOJp
      real JMCO2_GM,JMH2O_GM,JMO2_GM,JMNWASTE_GM,JOJx_GM,JOJv_GM
     &    ,JOJe_GM,JOJp_GM,etaO,JM_JO,pond_depth,twater,potfreemass
      real L_T,MLO2,debqmet,kappa_G
      real kappa_X,kappa_X_P,mu_X,mu_P,yEX,yXE,yPX,mu_AX,eta_PA
      real MLO2_init,GH2OMET_init,debqmet_init,w_N,w_X,w_P
      real DRYFOOD,FAECES,NWASTE,O2FLUX,CO2FLUX,gutfreemass
      real Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,surviv_pres
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,longev,surviv
      real rhref,E_He,breedtempthresh,repro
      real y_EV_l
      real E_Hpup_init,
     &E_Hpup,pond_env,tbs,oldclutch,stage
      real E_Hpup_pres
      real rainfall2,debfirst,ectoinput,grassgrowth
     &    ,grasstsdm,raindrink,wetlandTemps,wetlandDepths,conthole
      real gutfill,testclutch,cumrepro_temp
      real reprodens,k_Mdot,steps,E_Heggs
      real newclutch,fieldcap,wilting,S_instar

      real stage_rec
      real contwet,shdgrass,clutcha,clutchb
      REAL, dimension(100) :: FEC,SURV

      INTEGER hour,iyear,nyear,countday,i,pregnant,startday,
     &viviparous,daycount,batch,photostart,photofinish,metamorph,reset,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage,deadead
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66,complete
      INTEGER julday,monthly,wingmod,wingcalc,firstday,completion
      integer DEB1,inwater,aquatic,feeding,ctmincum,ctminthresh,ctkill
      integer metab_mode,stages,wetmod,contonly,contype
      integer breedact,breedactthres,waiting,breedtempcum,census
      integer prevdead,f1count,counter,soilmoisture,j

      character*1 transt,dlenth

      DIMENSION V(24),ED(24),wetmass(24),E_H(24),eggsoil(24)
     &,wetstorage(24),wetfood(24),svl(24),Vold(24),Vpup(24),Epup(24)
     &,E_Hpup(24),stage_rec(25),S_instar(4)
      DIMENSION wetgonad(24),repro(24),cumrepro(24),q(24),hs(24),ms(24)
     &,EH_baby1(24),cumbatch(24),food(50),v_baby1(24),e_baby1(24)
     &,surviv(24)
      DIMENSION depsel(25),tcores(25),acthr(25),actxbas(25)
      DIMENSION etaO(4,3),JM_JO(4,4)
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &    FAECES(24),NWASTE(24)
      dimension rainfall2(7300),debfirst(13),ectoinput(127)
      dimension grassgrowth(7300),grasstsdm(7300),wetlandTemps(24*7300)
     &,wetlandDepths(24*7300),pond_env(20,365,25,2),tbs(24*7300)

      DIMENSION QSOL(25),RH(25),TskyC(25),soil1(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TIME(25),shdgrass(25)
      DIMENSION L_instar(5)

      Data PI/3.14159/

      REAL*8 WORK,IWORK,RPAR,RTOL,ATOL,IPAR,YY,XEND
      INTEGER NDGL,NRDENS,LWORK,LIWORK,N,IOUT,ITOL,IDID
        PARAMETER (NDGL=4,NRDENS=4)
        PARAMETER (LWORK=8*NDGL+5*NRDENS+21,LIWORK=NRDENS+21)
        DIMENSION YY(NDGL),WORK(LWORK),IWORK(LIWORK),RPAR(8),RTOL(1),
     &   ATOL(1),IPAR(1)

      COMMON/FUN1/QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      COMMON/FUN2/AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      COMMON/WINGFUN/rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51
     &,sidex,WQSOL,wingmod,phimin,phimax,twing,wingcalc,F12,F32,F42,F52
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,f23,f24,f25,f26
      COMMON/DEBMOD/V,ED,WETMASS,WETSTORAGE,WETGONAD,WETFOOD,
     &O2FLUX,CO2FLUX,CUMREPRO,HS,MS,SVL,p_B_past,CUMBATCH,Q,v_baby1,
     &e_baby1,E_H,stage,dead,EH_baby1,gutfreemass,surviv,Vold,Vpup,Epup
     &,E_Hpup,deadead,startday,raindrink,reset,census,potfreemass
      COMMON/DEBRESP/MLO2,GH2OMET,debqmet,MLO2_init,GH2OMET_init,
     &    debqmet_init,dryfood,faeces,nwaste
      COMMON/DEBMOD2/REPRO,orig_clutchsize,newclutch,orig_MsM
      common/debmass/etaO,JM_JO
      Common/Treg/Tc
      COMMON/REPYEAR/IYEAR,NYEAR
      COMMON/COUNTDAY/COUNTDAY,daycount
      COMMON/DEPTHS/DEPSEL,Tcores
      COMMON/ENVAR1/QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,rhref
     & ,shdgrass
      COMMON/DEBOUTT/fecundity,clutches,monrepro,svlrepro,monmature
     &,minED,annfood,food,longev,completion,complete,fec,surv
      common/gut/gutfull,gutfill
      COMMON/DEBINIT1/v_init,E_init,cumrepro_init,cumbatch_init,
     & Vold_init,Vpup_init,Epup_init
      COMMON/DEBINIT2/ms_init,q_init,hs_init,p_Mref,vdotref,h_aref,
     &e_baby_init,v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,
     &halfsat,x_food,E_Hpup_init,p_Xmref  
      COMMON/z/tknest,Thconw
      Common/Behav3/Acthr,ACTXBAS
      common/julday/julday,monthly
      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_He,E_Heggs,batch
     &,MsM,lambda,breedrainthresh,daylengthstart,daylengthfinish
     &,photostart,photofinish,lengthday,photodirs,photodirf,lengthdaydir
     &,prevdaylength,lat,frogbreed,frogstage,metamorph
     &,breedactthres,clutcha,clutchb   
      COMMON/DEBPAR3/metab_mode,stages,y_EV_l,S_instar
      COMMON/DEBPAR4/s_j,L_b
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      common/vivip/viviparous,pregnant
      common/debbaby/v_baby,e_baby,EH_baby
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,wetmod,contonly,conthole
     &    ,contype,contwet
      Common/Rainfall/Rainfall
      common/ctmaxmin/ctmax,ctmin,ctmincum,ctminthresh,ctkill
      COMMON/EGGSOIL/EGGSOIL
      Common/Usropt/Transt,Dlenth
      common/fileio/I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      COMMON/BREEDER/breeding
      COMMON/ARRHEN/T_A,TAL,TAH,TL,TH,T_ref
      COMMON/REVAP1/Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,DEB1
      COMMON/pond/inwater,aquatic,twater,pond_depth,feeding,pond_env
      common/debinput/debfirst,ectoinput,rainfall2,grassgrowth
     &,grasstsdm,wetlandTemps,wetlandDepths
      common/bodytemp/tbs,breedtempcum,breedtempthresh
      common/death/causedeath,deathstage
      common/stage_r/stage_rec,f1count,counter
      common/soilmoistur/fieldcap,wilting,soilmoisture
      common/accel/p_Am_acc,v_acc,p_Xm_acc,L_j,L_instar,ER

c      y_EV_l = 0.95 !p_AmIm
c      E_He = 2.126e-2 !E_Hecl !E_Hp
c      E_Heggs = E_He !E_Hmet E_Hj
c      S_instar = E_Hmoult1!2.660 !E_Hmoult1        
c      s_j =0.999 !gam
      
      E_Rj=0
      if((daycount.eq.486).and.(hour.eq.21))then
          daycount=486
      endif
      p_R=0.
      p_A=0.
      ER_old=ER
c     initialise variables
       svl(hour)=0.
       wetgonad(hour)=0.
       wetstorage(hour)=0.
       wetfood(hour)=0.
       wetmass(hour)=0.
       MLO2(hour) = 0.
       O2FLUX = 0.
       CO2FLUX = 0.
       v(hour)=0.
       e_H(hour)=0.
       cumrepro(hour)=0.
       cumbatch(hour)=0.
       vpup(hour)=0.
       vold(hour)=0.
       ed(hour)=0.
       hs(hour)=0.
       surviv(hour)=1.
       ms(hour)=0.
       p_B_past=0.    
       reprodens=0.
      prevdead=dead

      if((hour.eq.1).and.(daycount.eq.1))then
       orig_clutchsize=clutchsize
       orig_MsM=MsM
      else
       clutchsize = orig_clutchsize
       MsM=orig_MsM
      endif
       if(photostart.eq.5)then
        lambda=3./12.
       endif

      if((dead.eq.1).and.(deadead.eq.0))then
      do 11 i=1,24
       cumrepro(i)=0
       cumbatch(i)=0
11    continue
       dead=0
      endif
      waiting=0

c    check if first day of simulation
      if((daycount.eq.1).and.(hour.eq.1))then
       firstday=1
       oldclutch=clutchsize
      else
       firstday=0
       clutchsize=oldclutch
      endif

      if((daycount.lt.startday).or.((countday.lt.startday).and.
     &    (v_init.le.3e-9)))then
       dead=1
      goto 987
      endif

c    check if start of a new day            
      if((hour.eq.1).or.((prevdead.eq.1).and.(reset.gt.0)))then
       V_pres = V_init
       E_pres = E_init
       ms_pres = ms_init
       ER = 0.
       if((daycount.eq.1).or.(prevdead.eq.1))then
        minED = E_pres
       endif
       E_H_pres = E_H_init
       q_pres = q_init
       hs_pres = hs_init
       surviv_pres = surviv_init
       Vold_pres = Vold_init
       Vpup_pres = Vpup_init
       Epup_pres = Epup_init
       E_Hpup_pres = E_Hpup_init
      else
       V_pres = V(hour-1)
       E_pres = Ed(hour-1)
       ms_pres = ms(hour-1)
       E_H_pres = E_H(hour-1)
       q_pres = q(hour-1)
       hs_pres = real(hs(hour-1),4)
       surviv_pres = surviv(hour-1)
       Vold_pres = Vold(hour-1)
       Vpup_pres = Vpup(hour-1)
       Epup_pres = Epup(hour-1)
       E_Hpup_pres = E_Hpup(hour-1)
      endif

c    set body temperature
      Tb = Tc
c      Tb = 15.

      if((frogbreed.eq.1).or.(frogbreed.eq.2))then
       contdep=pond_env(iyear,countday,hour,2)
       Tb=pond_env(iyear,countday,hour,1)
      endif

c    if running a frog or aquatic insect, check if terrestrial breeder and set Tb to soil temp
      if(stage.eq.0)then
       if(frogbreed.eq.2)then
        Tb = Eggsoil(hour)
       endif
      endif

c    Arrhenius temperature correction factor 5 parameters

      Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tb)))/(1+EXP(TAL
     &*(1/(273+Tb)-1/TL))+EXP(TAH*(1/TH-1/(273+Tb))))
*********************************************************************     
c      Tcorr=2.0661
c      Feeding=1
*********************************************************************

c    Arrhenius temperature correction factor 1 parameter
c            Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tb)))

c    food availability - to do
      X = 1.
      if(pregnant.eq.1)then
c      f=0.7
        f=1
       lambda=lambda-1./(24.*365.)
       if(lambda.lt.0.1)then
        lambda=0.1
       endif
      else
        f=1
      endif
      


c    check if there is food as indicated by grassgrowth, but also check that a lack of growth isn't due to cold
      if((grassgrowth(daycount).le.0).and.(stage.ne.0).and.(
     &    stage.lt.stages-2))then
        if((taloc(hour).lt.5).and.(grasstsdm(countday).gt.0))then
         funct=1
        else
         funct=0
        endif
       else
        funct=1
      endif

c    f = X/(halfsat+X)

c    temperature corrections and compound parameters
      M_V = d_V/w_V
      pT_M = p_Mref*Tcorr
      k_Mdot = p_Mref/E_G
      kT_Mdot = k_Mdot*Tcorr
      kT_J = k_Jref*Tcorr
c      kT_El = k_El*Tcorr
      p_Am = p_Mref*zfact/kappa
      pT_Am = p_Am*Tcorr
      vTdot = vdotref*Tcorr
      h_a = h_aref*Tcorr
      
c     if not pupated yet, structure is larval structure 'vold'
      if(stage.lt.(stages-2))then
          v_pres=vold_pres
      endif
      
c     acceleration check      
      if((stage.gt.0).and.(stage.lt.stages-2))then
c     larval stage, accelerate and save current values          
C      v_acc = vdotref*(V_pres**(1./3.)/L_b)
C      p_Am_acc = (p_Mref*zfact/kappa)*(V_pres**(1./3.)/L_b)
C      p_Xm_acc = p_Xmref*(V_pres**(1./3.)/L_b)
C      vTdot = v_acc*Tcorr
C      pT_Am = p_Am_acc*Tcorr
C      pT_Xm = p_Xm_acc*Tcorr
Cc     keep track of length at end of acceleration   
C      E_M = p_Am_acc/v_acc
      else
       if(stage.ge.stages-2)then
c     pupa or imago, keep with new accelerated values           
       vTdotj = v_acc*Tcorr
       pT_Am = p_Am_acc*Tcorr
       pT_Xm = p_Xm_acc*Tcorr
       E_M = pT_Am/vTdotj
       else
c     embryo, use non-accelearted values       
       vTdot = vdotref*Tcorr
       pT_Am = p_Am*Tcorr
       pT_Xm = p_Xmref*Tcorr
       E_M = pT_Am/vTdot
       endif
      endif
c    p_Xm = p_Am/kappa_X
      kT_E = vdotref*Tcorr/L_b
      kT_El = vTdotj/L_j

      L_T = 0.
      L_pres = V_pres**(1./3.)
      V_max=(kappa*pT_Am/pT_M)**(3.)
      L_max = V_max**(1./3.)

      E_scaled=E_pres/E_m
      scaled_l = L_pres/L_max
      g = E_G/(kappa*(p_Am/vdotref))

      kappa_G = (d_V*mu_V)/(w_V*E_G)
      yEX=kappa_X*mu_X/mu_E
      yXE=1/yEX
      yPX=kappa_X_P*mu_X/mu_P
      mu_AX=mu_E/yXE
      eta_PA=yPX/mu_AX

      if(stage.eq.0)then
c     egg, use maturity at birth
       E_Hthresh = E_Hb
      endif
      if(stage.eq.stages-2)then
       if(metab_mode.eq.2)then
c     pupa, use maturity at eclosion
c      p_Am = 0.00001/24
        E_Hthresh = E_He
       endif
      endif

      E_Rj=s_J*((1-kappa)*(p_Am/vdotref)*g*(((vdotref/L_b)+k_Mdot)/
     &((vdotref/L_b)-g*k_Mdot)))
      if((stage.gt.0).and.(stage.lt.stages-3))then
c     larva, use critical weight thresholds driven by repro buffer
       E_Hthresh=L_instar(int(stage))
      endif
      if(stage.eq.(stages-3))then
c     last larval instar, use final critical weight threshold [ERj]         
       E_Hthresh=E_Rj
      endif

      if(frogbreed.eq.1)then
       if(contdep.le.0.1)then
        dead=1
        if(stage.gt.deathstage)then
         causedeath=6.
         deathstage=stage
        endif
       endif
      endif

c    call subroutine that assesses photoperiod cues on breeding
      CALL BREED(julday,photostart,photofinish,lengthday
     &,daylengthstart,daylengthfinish,photodirs,photodirf,prevdaylength,
     &lat,firstday,breedact,breedactthres,tbs,hour,
     &breedtempthresh,breedtempcum,daycount)

c    call subroutine that assesses if user wants cumulative resets of development after metamorphosis
      CALL DEVRESET(dead,E_H_pres,E_Hb,frogbreed,breeding,
     &conth,contdep,stage,frogstage,reset,complete,waiting,
     &hour,stages,completion)

c    now checking to see if starting with embryo, and if so setting the appropriate reserve density
      if(hour.eq.1)then
       if(daycount.eq.1)then
        if(stage.eq.0)then
c       E_pres=E_egg/V_pres
         E_pres=E_init
        endif
       endif
c    checking to see if animal died recently and needs to start again as an embryo
       if((daycount.gt.1).and.(dead.eq.1))then
        if(stage.eq.0)then
         if(V_pres.eq.0)then
          E_pres=0
         else
          E_pres=E_egg/V_pres
         endif
        endif
       endif
      endif

      if(stage.eq.0)then
c     use embryo equation for length, from Kooijman 2009 eq. 2
c     solve more frequently to minimise integration errors since dynamics
c     are quite fast
      steps=60.
       do 876 i=1,int(steps)
       if(waiting.eq.1)then
c       growth of structure           
        dLdt = 0
        V_temp=(V_pres**(1./3.)+dLdt)**3
        dVdt = 0
        rdot=0
c       maturation
        U_H_pres=E_H_pres/(pT_Am/steps)    
        dUHdt=0
        dE_Hdt=0
       else
c       growth of structure           
        dLdt=((vTdot/steps)*E_scaled-(kT_Mdot/steps)*g*V_pres**(1./3.)
     &   )/(3*(E_scaled+g))
        V_temp=(V_pres**(1./3.)+dLdt)**3
        dVdt = (V_temp-V_pres)
        rdot=(vTdot/steps)*(e_scaled/L_pres-(1+L_T/L_pres)/L_max)/
     &    (e_scaled+g)
       endif
c        reserve dynamics       
         Sc = L_pres**2*(g*E_scaled)/(g+E_scaled)*
     &       (1+(((kT_Mdot/steps))*L_pres)/(vTdot/steps))
         dUEdt = -1*Sc
         E_temp=((E_pres*V_pres/(pT_Am/steps))+dUEdt)*(pT_Am/steps)/
     &    (V_pres+dvdt)
         dEdt=(E_temp-E_pres)
        if(waiting.eq.0)then
c       maturation
         U_H_pres=E_H_pres/(pT_Am/steps)    
         dUHdt=(1-kappa)*Sc-(kT_J/steps)*U_H_pres
         dE_Hdt=dUHdt*(pT_Am/steps)
         E_H_pres=E_H_pres+real(dE_Hdt,4)
        endif
         E_pres=E_pres+real(dEdt,4)
         E_scaled=E_pres/E_m
         V_pres=V_pres+real(dVdt,4)
         L_pres=V_pres**(1./3.)
876        continue
         vold(hour)=V_pres
         p_B=0.
      endif

      goto 879     
      if((stage.gt.0).and.(stage.lt.(stages-2)))then
c     larva
      dEdt=0.
      steps=600.
       do 877 i=1,int(steps)
      if(ER_old.lt.E_Rj)then     
       v_acc = vdotref*(vold_pres**(1./3.)/L_b)
       p_Am_acc = (p_Mref*zfact/kappa)*(vold_pres**(1./3.)/L_b)
       p_Xm_acc = p_Xmref*(vold_pres**(1./3.)/L_b)
       vTdot = v_acc*Tcorr
       pT_Am = p_Am_acc*Tcorr
       pT_Xm = p_Xm_acc*Tcorr
c     keep track of length at end of acceleration   
       E_M = p_Am_acc/v_acc 
       V_max=(kappa*pT_Am/pT_M)**(3.)
       L_max = V_max**(1./3.)
      L_j = vold_pres**(1./3.)
      endif
      L_pres=Vold_pres**(1./3.)
      E_M=(p_Am/vdotref)
      g=E_G/(kappa*E_M)
       e_scaled=E_pres/E_m
       rdot = (vTdot/steps)*((e_scaled/L_pres)-((kT_Mdot/steps)*g)/
     &  (vTdot/steps))/(e_scaled + g)
       p_C = E_pres*Vold_pres * ((kT_E/steps) - rdot)
c       p_C=(E_pres*V_pres)*((E_G*kT_E+pT_M)/(kappa*E_pres+E_G))
       p_G=kappa*p_C-(pT_M/steps)*Vold_pres
       p_J=(kT_J/steps)*E_Hb
       p_R=(1.-kappa)*p_C-p_J   
c       dVold_dt=p_G/E_G
       dVdt=rdot * Vold_pres
       dER = p_R/vold_pres-rdot*ER_old       
       vold_pres=vold_pres+dVdt
       if(ER_old.lt.E_Rj)then
       ER_old=ER_old+dER
       endif

      if(hour.eq.1)then
       if(ms_init.gt.0.0000001*MsM*Vold_pres)then
c        Equation 2.10 DEB3
       dEdt1 = ((pT_Am/steps)*f-E_pres*(vTdot/steps))/L_pres
       else
       dEdt1 = ((pT_Am/steps)*1-E_pres*(vTdot/steps))/L_pres
       endif
      else
       if(ms(hour-1).gt.0.0000001*MsM*Vold_pres)then
c        Equation 2.10 DEB3
       dEdt1 = ((pT_Am/steps)*f-E_pres*(vTdot/steps))/L_pres
       else
       dEdt1 = ((pT_Am/steps)*1-E_pres*(vTdot/steps))/L_pres
       endif
      endif
      e_pres=e_pres+dEdt1
      dEdt=dEdt+dEdt1
877   continue 
       if(ER_old.gt.E_Rj)then
      i=1
       endif

      vold(hour)=vold_pres
      ER=ER_old
      dE_Hdt=0
      p_B=0.
      p_C=p_C*steps
      p_R=p_R*steps
      p_J=p_J*steps
      p_G=p_G*steps
      rdot=rdot*steps
      endif
879   continue

      if((stage.gt.0).and.(stage.lt.(stages-2)))then
c     larva
       kT_E=vdotref/L_b*Tcorr
       L_pres=Vold_pres**(1./3.)
       E_M=(p_Am/vdotref)
       g=E_G/(kappa*E_M)
       e_scaled=E_pres/E_m
       rdot=(e_scaled*kT_E-g*kT_Mdot)/(e_scaled+g)
       p_C=(E_pres*Vold_pres)*(kT_E-rdot)
       p_G=kappa*p_C-pT_M*Vold_pres
       p_J=E_H_pres*kT_J
       p_R=(1-kappa)*p_C-p_J

C --- DIMENSION OF THE SYSTEM
        N=NDGL
C --- OUTPUT ROUTINE (AND DENSE OUTPUT) IS USED DURING INTEGRATION
        IOUT=0
C --- INITIAL VALUES AND ENDPOINT OF INTEGRATION
       
      RPAR(1)=f
      RPAR(2)=kT_Mdot
      RPAR(3)=vdotref/L_b*Tcorr
      RPAR(4)=p_J
      RPAR(5)=(p_Mref*zfact/kappa)/L_b*Tcorr
      RPAR(6)=E_m
      RPAR(7)=g
      RPAR(8)=kappa
      
      if(hour.eq.1)then
       if(ms_init.gt.0.0000001*MsM*Vold_pres)then
       RPAR(1)=f
       else
       RPAR(1)=0
       endif
      else
       if(ms(hour-1).gt.0.0000001*MsM*Vold_pres)then
c        Equation 2.10 DEB3
       RPAR(1)=f
       else
       RPAR(1)=0
       endif
      endif
      
        X=0.0D0
        YY(1)=0.0D0
        YY(2)=E_pres*vold_pres
        YY(3)=L_pres
        YY(4)=ER_old*vold_pres
        XEND=1.D+00
C --- REQUIRED (RELATIVE AND ABSOLUTE) TOLERANCE
        ITOL=0
        RTOL(1)=1.0D-3
        ATOL(1)=RTOL(1)
C --- DEFAULT VALUES FOR PARAMETERS
        DO 10 I=1,20
        IWORK(I)=0
  10    WORK(I)=0.D0  
C --- DENSE OUTPUT IS USED FOR THE TWO POSITION COORDINATES 1 AND 2
        IWORK(5)=NRDENS
        IWORK(21)=1
        IWORK(22)=2
        IWORK(23)=3
        IWORK(24)=4    
C --- CALL OF THE SUBROUTINE DOPRI5   
        CALL DOPRI5(N,dget_aELE,X,YY,XEND,
     &              RTOL,ATOL,ITOL,
     &              SOLOUT,IOUT,
     &              WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
      vold_pres=YY(3)**3.
      ER_old=YY(4)/YY(3)**3.
      ER=ER_old
      dE_Hdt=0
      p_B=0.
      vold(hour)=vold_pres
       if(ER_old.lt.E_Rj)then     
        v_acc = vdotref*(vold_pres**(1./3.)/L_b)
        p_Am_acc = (p_Mref*zfact/kappa)*(vold_pres**(1./3.)/L_b)
        p_Xm_acc = p_Xmref*(vold_pres**(1./3.)/L_b)
        vTdot = v_acc*Tcorr
        pT_Am = p_Am_acc*Tcorr
        pT_Xm = p_Xm_acc*Tcorr
c       keep track of length at end of acceleration   
        E_M = p_Am_acc/v_acc 
        V_max=(kappa*pT_Am/pT_M)**(3.)
        L_max = V_max**(1./3.)
        L_j = vold_pres**(1./3.)
      endif      
      
C     dEdt=0.
C     steps=6000.
C      do 880 i=1,int(steps)
C      if(ER_old.lt.E_Rj)then     
C       v_acc = vdotref*(vold_pres**(1./3.)/L_b)
C       p_Am_acc = (p_Mref*zfact/kappa)*(vold_pres**(1./3.)/L_b)
C       p_Xm_acc = p_Xmref*(vold_pres**(1./3.)/L_b)
C       vTdot = v_acc*Tcorr
C       pT_Am = p_Am_acc*Tcorr
C       pT_Xm = p_Xm_acc*Tcorr
Cc       keep track of length at end of acceleration   
C       E_M = p_Am_acc/v_acc 
C       V_max=(kappa*pT_Am/pT_M)**(3.)
C       L_max = V_max**(1./3.)
C       L_j = vold_pres**(1./3.)
C     endif
C      kT_E=vdotref/L_b*Tcorr
C      L_pres=Vold_pres**(1./3.)
C      E_M=(p_Am/vdotref)
C      g=E_G/(kappa*E_M)
C      e_scaled=E_pres/E_m
C      rdot=(e_scaled*kT_E/steps-g*kT_Mdot/steps)/(e_scaled+g)
C      p_C=(E_pres*Vold_pres)*(kT_E/steps-rdot)
C      dEdt1=f*p_Am/L_b*Tcorr/steps-E_pres*vTdot/steps/L_pres
C      dLdt=rdot*L_pres/3
C      Vold_pres=(Vold_pres**(1./3.)+dLdt)**3
C      p_G=kappa*p_C-pT_M*Vold_pres
C      p_J=E_H_pres*kT_J/steps
C      p_R=(1-kappa)*p_C-p_J
C      dER=p_R/V_pres-rdot*ER_old
C      if(ER_old.lt.E_Rj)then
C       ER_old=ER_old+dER
C      endif   
C     e_pres=e_pres+dEdt1
C     dEdt=dEdt+dEdt1       
C880   continue
C     vold(hour)=vold_pres
C     ER=ER_old
C     dE_Hdt=0
C     p_B=0.
C     p_C=p_C*steps
C     p_R=p_R*steps
C     p_J=p_J*steps
C     p_G=p_G*steps
C     rdot=rdot*steps
      endif 
 
      if(stage.eq.(stages-2))then
c     pupa     
       cumrepro(hour) = cumrepro_init ! no change to repro bufffer
       p_L=vold_pres*kT_El
       e_scaled=E_pres*(vold_pres+Vpup_pres)/Vpup_pres/E_m
       rdot=vTdotj*(e_scaled/Vpup_pres**(1./3.)-1/L_max)/(e_scaled+g)
       p_C=real((e_scaled*E_m*Vpup_pres)*(vTdotj/Vpup_pres**(1./3.)-rdot
     &  ),4)
C       p_C=(E_pres*(vold_pres+Vpup_pres))*((E_G*vTdotj/Vpup_pres**(1./3.
C     &)+pT_M)/(kappa*(E_pres*(vold_pres+Vpup_pres)/Vpup_pres)+E_G))
       p_J=kT_J*E_H_pres
       p_G=kappa*p_C-pT_M*Vpup_pres
       p_R=(1-kappa)*p_C-p_J
       dVpup_dt=p_G/E_G
c       dVpup_dt=(real(rdot*Vpup_pres**(1./3.)/3.,4)**3.)
       dVold_dt=-1.*p_L
       vold(hour)=vold_pres+dVold_dt
       Vpup(hour)=Vpup_pres+dVpup_dt
       dEdt=(p_L*y_EV_L*mu_E*M_V-p_C)/(vold_pres+Vpup_pres)
       dE_Hdt=p_R
c       if(E_H_pres.gt.E_Heggs)then ! egg production during pupa
c        p_B=kT_E*cumrepro(hour)
c       endif
      endif

      if(stage.gt.(stages-2))then
c     imago  
       p_C=(E_pres*V_pres)*kT_E
       p_B=kT_E*cumrepro_init-(1-k_R)*kT_E*cumrepro_init
       p_J=kT_J*E_He
       p_R=p_C-p_Mref*V_pres-p_J
       dVpup_dt=0
       dVold_dt=0
       vold(hour)=vold_pres+dVold_dt
       Vpup(hour)=dVpup_dt+Vpup_pres
       dE_Hdt=0
       if(feeding.eq.1)then
        if(hour.eq.1)then
         if(ms_init.gt.0.0000001*MsM*V_pres)then
c         Equation 2.10 DEB3
           dEdt = (pT_Am*f-E_pres*vTdot)/L_pres
         else
           dEdt = (pT_Am*0-E_pres*vTdot)/L_pres
         endif
        else
         if(ms(hour-1).gt.0.0000001*MsM*V_pres)then
          dEdt = (pT_Am*f-E_pres*vTdot)/L_pres
         else
          dEdt = (pT_Am*0-E_pres*vTdot)/L_pres
         endif
        endif
       else
        dEdt=(pT_M*V_pres+p_J-p_C)/V_pres ! assimilation is somatic and maturity maintenance rate
       endif
      endif
      p_M = pT_M*(vold_pres+Vpup_pres) ! maintenance power
      
c      if(aest.eq.1)then
c      dVdt=0
c      rdot=0
c      endif

      if(clutchsize.lt.1)then
       clutchsize=1
      endif
      clutchenergy = E_egg*clutchsize

c    diapause before pond fill
      if(frogbreed.eq.1)then
       if(stage.lt.stages-1)then
        if(contdep.le.0.1)then
         dVdt=0
        endif
       endif
      endif

c    other powers

c    J food eaten per hour
       p_X = p_A/kappa_X
c    tallying J food eaten per year
       food(iyear)=food(iyear)+p_X
c    tallying lifetime food eaten
      if(iyear.eq.nyear)then
       if(hour.eq.24)then
        do 1 i=1,nyear
         annfood=annfood+food(i)
1       continue
       endif
      endif
   
      if((stage.gt.0).and.(stage.ne.(stages-2)))then
       p_D = p_M+p_J+(1-k_R)*p_R ! larva or imago, allocate to reproduction
      else
       p_D = p_M+p_J+p_R ! egg or pupa, all (1-kappa) to maturation
      endif

      if(L_pres.eq.0)then
       rdot=0
       dqdt=0
      else
       rdot=vTdot*(E_scaled/L_pres-(1+L_T/L_pres)/L_max)/
     &(E_scaled+g)
       dqdt = (q_pres*(V_pres/V_max)*s_G+h_a)*(E_pres/E_m)
     &*((vTdot/L_pres)-rdot)-rdot*q_pres
      endif      
 
      if(stage.ge.stages-1)then
       if(hour.eq.1)then
        q(hour) = real(q_init + dqdt,4)
       else
        q(hour) = real(q(hour-1)+dqdt,4)
       endif
      else
        q(hour) = 0.
      endif

c    dhsds = h_a*q(hour)/V_pres
      dhsds = q_pres-rdot*hs_pres

      if(stage.ge.stages-1)then
       if(hour.eq.1)then
        hs(hour) = hs_init + dhsds
       else
        hs(hour) = hs(hour-1)+dhsds
       endif
      else
       hs(hour) = 0.
      endif

c    h_w = ((h_a*(E_pres/E_m)*vTdot)/(6.*V_pres**(1./3.)))
c     &    **(1./3.)
c    surviv(hour) = EXP(-1*(h_w*((daycount-1)*24+hour))**3)
      if(stage.ge.stages-1)then
       dsurvdt = -1.*surviv_pres*hs(hour)
      else
       dsurvdt = 0.
      endif
      surviv(hour) = surviv_pres+real(dsurvdt,4)

      if(countday.eq.365)then
       if(hour.eq.24)then
        surv(iyear)=surviv(hour)
       endif
      endif


      if(tb.lt.ctmin)then
      ctmincum=ctmincum+1
      else
      ctmincum=0
      endif

      if(surviv(hour).lt.0.49)then
      continue
      endif

c    average longevity in years
      if((longev.eq.0).or.(reset.gt.0))then
c     if((Tb.lt.CTmin).or.(Tb.gt.CTmax))then
       if(ctkill.eq.1)then
        if((ctmincum.gt.ctminthresh).or.(Tb.gt.CTmax))then
         if(reset.gt.0)then
          dead=1
          if(ctmincum.gt.ctminthresh)then
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=1.
            deathstage=stage
            ctmincum=0
           endif
          else
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=2.
            deathstage=stage
           endif
          endif
         else
          if(ctmincum.gt.ctminthresh)then
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=1.
            deathstage=stage
            ctmincum=0
           endif
          else
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=2.
            deathstage=stage
           endif
          endif
          surviv(hour)=0.49
          census=365
         endif
        endif
       endif
       if(surviv(hour).lt.0.5)then
        longev=(daycount+hour/24.)/365.
        if(reset.gt.0)then
c       need to reset upon death, so don't let deadead=1
         dead=1
         surviv(hour)=1
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=5.
            deathstage=stage
           endif
        else
         dead=1
         deadead=1
           if((stage.gt.deathstage).and.(v_pres.ne.0))then
            causedeath=5.
            deathstage=stage
           endif
        endif
       endif
      endif

c     accumulate energy/matter in reproduction buffer
c     if it is the beginning of the day
      if((stage.gt.0).and.(stage.ne.(stages-2)))then
       if(hour.eq.1)then
c      if the buffer ran out in the previous hour
        if(cumrepro_init.lt.0)then
c       keep it empty
         cumrepro(hour)=0
        else
         cumrepro(hour) = cumrepro_init+p_R-p_B_past
        endif
       else
c     it is not the first first hour and it is not the first day
c      if the buffer ran out in the previous hour
        if(cumrepro(hour-1).lt.0)then
c       keep it empty
         cumrepro(hour)=0
        else
c       otherwise start it filling up according to p_R but subtract anything that goes to the batch
         cumrepro(hour) = cumrepro(hour-1)+p_R-p_B_past
        endif
       endif
      endif
      
c     accumulate energy/matter in egg batch buffer
c    if it is the beginning of the day
      if(hour.eq.1)then
c     then if it is the first day of the simulation
c     if(day+365*(iyear-1).eq.1)then
c      nothing in the buffer yet      
        cumbatch(hour) = cumbatch_init
        else
c     it is not the first first hour of the first day
c       otherwise start it filling up 
         cumbatch(hour) = cumbatch(hour-1)+p_B
      endif
      if(cumbatch(hour).lt.0)then
          cumrepro(hour)=cumrepro(hour)+cumbatch(hour) ! account for deficit
          cumbatch(hour)=0.
      endif

c    if(cumbatch(hour).lt.0.1*clutchenergy)then
c        stage=3
c    endif
      
      if(Vold(hour).lt.0)then
       Vold(hour)=0
      endif
      if(Vpup(hour).lt.0)then
       Vpup(hour)=0
      endif

c     sum structures    
      V(hour)=Vpup(hour)+Vold(hour)

      if(v(hour).le.0)then
        ED(hour)=0
      else
       if(stage.eq.0)then
        ED(hour)=E_pres
       else
       if((stage.gt.0).and.(stage.lt.(stages-2)))then
        ED(hour) = yy(2)/yy(3)**3.
        else
        ED(hour) = E_pres+dEdt
       endif
       endif
      endif

      if(stage.eq.0)then
          E_H(hour)=E_H_pres
      else
       if(hour.eq.1)then
        E_H(hour) = real(E_H_init,4) + real(dE_Hdt,4)
       else
        E_H(hour) = E_H(hour-1)+real(dE_Hdt,4)
       endif
      endif
c     make sure ED doesn't go below zero
      if(ED(hour).lt.0)then
       ED(hour)=0
      endif
c     find min value of ED for the simulation
      if(ED(hour).lt.minED)then
       minED=ED(hour)
      endif

c     svl in mm
      svl(hour) = V(hour)**
     &    (0.3333333333333)/delta_deb*10
     
c     transition between stages for insect
      if(metab_mode.eq.1)then
       if(stage.eq.0)then
        if(E_H(hour).ge.E_Hb)then
c        start the larval stage, define instar transitions
         stage=stage+1
         if(hour.eq.1)then
          V_b=V_init
         else
          V_b=Vold(hour-1)
         endif
         L_b=V_b**(1./3.)
         L_b=0.0611
         L_instar(1)=S_instar(1)**0.5*L_b
         do 88 i=2,(stages-4)
          L_instar(i)=S_instar(i)**0.5*L_instar(i-1)
88       continue  
        endif      
       else
        if(V_pres**(1./3.).gt.E_HThresh)then
         stage=stage+1
        endif 
       endif
      endif
      if(metab_mode.eq.2)then
       if(stage.eq.0)then   
        if(E_H(hour).ge.E_Hb)then
c        start the larval stage, define instar transitions
         stage=stage+1
C        if(hour.eq.1)then
C         V_b=V_init
C        else
C         V_b=Vold(hour-1)
C        endif
C        L_b=V_b**(1./3.)
C        L_b=0.0574
C        L_instar(1)=S_instar(1)**0.5*L_b
C        do 89 i=2,(stages-4)
C         L_instar(i)=S_instar(i)**0.5*L_instar(i-1)
C89       continue   
        endif
       else 
        if(stage.lt.stages-3)then
         if(V_pres**(1./3.).gt.E_Hthresh)then
          stage=stage+1
         endif 
        else
         if(stage.eq.stages-3)then
          if(ER.ge.E_Hthresh)then
           stage=stage+1
           E_H(hour)=0.
           Vpup(hour)=1e-4
          endif
         else
          if(stage.eq.(stages-2))then
           if((E_H(hour)).gt.E_He)then
c          eclosion
            stage=stage+1
           endif
          endif
         endif
        endif
       endif
      endif
             
      if(cumbatch(hour).gt.0)then
       if(monmature.eq.0)then
        monmature=(countday+365*(iyear-1))/30.5
       endif
      endif

      if((cumbatch(hour).gt.clutchenergy).or.(pregnant.eq.1))then
c       for variable clutch size from repro and batch buffers
c    if((cumbatch(hour).gt.clutchenergy).or.(pregnant.eq.1).or
c     &.((viviparous.eq.1).and.(cumbatch(hour)+cumrepro(hour).gt.
c     &clutchenergy)))then
c     batch is ready so if viviparous, start gestation, else dump it
       if(viviparous.eq.1)then
        if((pregnant.eq.0).and.(breeding.eq.1))then
         v_baby=v_init_baby
         e_baby=e_init_baby
         EH_baby=0.
         pregnant=1
         testclutch=floor((cumbatch(hour))/E_egg)
c       for variable clutch size from repro and batch buffers
c       testclutch=floor((cumbatch(hour)+cumrepro(hour))/E_egg)
c       testclutch=real(testclutch)
         if(testclutch.gt.clutchsize)then
          clutchsize=testclutch
          clutchenergy = E_egg*clutchsize
         endif
c       for variable clutch size from repro and batch buffers
         if(cumbatch(hour).lt.clutchenergy)then
c        needs to draw from repro buffer - temporarily store current repro as cumrepro_temp, 
c        then remove what is needed from the repro buffer and add it to the batch buffer
          cumrepro_temp=cumrepro(hour)
          cumrepro(hour)=cumrepro(hour)+cumbatch(hour)-clutchenergy
          cumbatch(hour)=cumbatch(hour)+cumrepro_temp-cumrepro(hour)
         endif
        endif
        if(hour.eq.1)then
         v_baby=v_baby_init
         e_baby=e_baby_init
         EH_baby=EH_baby_init
        endif
        if(pregnant.eq.1)then
        call deb_baby
        endif
        if(EH_baby.gt.E_Hb)then
         if((Tb .lt. tminpr) .or. (Tb .gt. tmaxpr))then
          goto 898
         endif
         cumbatch(hour) = cumbatch(hour)-clutchenergy
         repro(hour)=1
         pregnant=0
         v_baby=v_init_baby
         e_baby=e_init_baby
         EH_baby=0
         newclutch=clutchsize
         fec(iyear)=fec(iyear)+clutchsize
         fecundity=fecundity+clutchsize
         clutches=clutches+1
         if(fecundity.ge.clutchsize)then
          monrepro=(countday+365*(iyear-1))/30.5
          svlrepro=svl(hour)
         endif
         pregnant=0
        endif
       else
c      not viviparous, so lay the eggs at next period of activity
        if(breedrainthresh.gt.0)then
         if(rainfall.lt.breedrainthresh)then
          goto 898
         endif
        endif
        if((frogbreed.eq.1).and.(pond_depth.lt.50))then
          goto 898
         endif
        if((Tb .lt. tminpr) .or. (Tb .gt. tmaxpr))then
         goto 898
        endif
c    change below to active or not active rather than depth-based, in case of fossorial
        if((Tb .lt. tminpr) .or. (Tb .gt. tmaxpr))then
         goto 898
        endif
         testclutch=floor(cumbatch(hour)/E_egg)
         testclutch=real(testclutch)
        if(testclutch.gt.clutchsize)then
         clutchsize=testclutch
        endif
        cumbatch(hour) = cumbatch(hour)-clutchenergy
        repro(hour)=1
        fec(iyear)=fec(iyear)+clutchsize
        fecundity=fecundity+clutchsize
        clutches=clutches+1
        if(fecundity.ge.clutchsize)then
         monrepro=(countday+365*(iyear-1))/30.5
         svlrepro=svl(hour)
        endif
       endif
      endif


898   continue

      if((reset.gt.0).and.(reset.ne.8))then
       fec(iyear)=completion
      endif
      
      if(v_pres.le.0)then
       dMsdt=0
       else
       if((stage.ne.0).and.(stage.ne.stages-2))then
        if((Tb .ge. tminpr) .and. (Tb .le. tmaxpr))then
         if((aquatic.eq.1).and.(feeding.eq.0))then
          dMsdt = pT_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       pT_Xm*v_pres**(2./3.)*funct*(Ms_pres/(MsM*v_pres))
         else
          dMsdt = pT_Xm*v_pres**(2./3.)*funct*(X_food/(halfsat+X_food))-
     &       pT_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
         endif
        else
         dMsdt = pT_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       pT_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
        endif
       else
        dMsdt = pT_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       pT_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
       endif
      endif
      
      if((stage.eq.0).or.(stage.eq.stages-2))then
          dMsdt=0
      endif
      
      if(v_pres.eq.0)then
      dMsdt=0
      endif

      if(hour.eq.1)then
       ms(hour) = ms_init+dmsdt
      else
       ms(hour) = ms(hour-1)+dmsdt
      endif
      if(ms(hour).lt.0)then
       ms(hour)=0
      endif

      if(ms(hour).gt.MsM*v_pres)then
       ms(hour)=MsM*v_pres
      endif

c    ensure gut empty if entering pupal phase
      if(stage.eq.stages-2)then
       ms(hour)=0
       ms_pres=0
      endif
      if(v_pres.le.0)then
       gutfull=0
      else
       gutfull=ms_pres/(MsM*v_pres)*100
      endif
      if(gutfull.gt.1)then
      gutfull=100
      endif   
      ms_past=ms(hour)    
      p_B_past=p_B

c    mass balance

      JOJx=p_A*etaO(1,1)+p_D*etaO(1,2)+p_G*etaO(1,3)
      JOJv=p_A*etaO(2,1)+p_D*etaO(2,2)+p_G*etaO(2,3)
      JOJe=p_A*etaO(3,1)+p_D*etaO(3,2)+p_G*etaO(3,3)
      JOJp=p_A*etaO(4,1)+p_D*etaO(4,2)+p_G*etaO(4,3)

      JOJx_GM=p_D*etaO(1,2)+p_G*etaO(1,3)
      JOJv_GM=p_D*etaO(2,2)+p_G*etaO(2,3)
      JOJe_GM=p_D*etaO(3,2)+p_G*etaO(3,3)
      JOJp_GM=p_D*etaO(4,2)+p_G*etaO(4,3)

      JMCO2=JOJx*JM_JO(1,1)+JOJv*JM_JO(1,2)+JOJe*JM_JO(1,3)+
     &    JOJp*JM_JO(1,4)
      JMH2O=JOJx*JM_JO(2,1)+JOJv*JM_JO(2,2)+JOJe*JM_JO(2,3)+
     &        JOJp*JM_JO(2,4)
      JMO2=JOJx*JM_JO(3,1)+JOJv*JM_JO(3,2)+JOJe*JM_JO(3,3)+
     &        JOJp*JM_JO(3,4)
      JMNWASTE=JOJx*JM_JO(4,1)+JOJv*JM_JO(4,2)+JOJe*JM_JO(4,3)
     &        +JOJp*JM_JO(4,4)

      JMCO2_GM=JOJx_GM*JM_JO(1,1)+JOJv_GM*JM_JO(1,2)+JOJe_GM*
     &    JM_JO(1,3)+JOJp_GM*JM_JO(1,4)
      JMH2O_GM=JOJx_GM*JM_JO(2,1)+JOJv_GM*JM_JO(2,2)+JOJe_GM*
     &    JM_JO(2,3)+JOJp_GM*JM_JO(2,4)
      JMO2_GM=JOJx_GM*JM_JO(3,1)+JOJv_GM*JM_JO(3,2)+JOJe_GM*
     &    JM_JO(3,3)+JOJp_GM*JM_JO(3,4)
      JMNWASTE_GM=JOJx_GM*JM_JO(4,1)+JOJv_GM*JM_JO(4,2)+
     &        JOJe_GM*JM_JO(4,3)+JOJp_GM*JM_JO(4,4)

c    mlO2/h, temperature corrected (including SDA)

      if(DEB1.eq.1)then
       if(ED(hour).ne.ED(hour))then
        ED(hour)=0.
        v(hour)=0
        dead=1
       endif
       if(cumbatch(hour).ne.cumbatch(hour))then
        cumbatch(hour)=0.
       endif
       if(hs(hour).ne.hs(hour))then
        hs(hour)=0.
       endif
c     O2FLUX = -1*JMO2/(T_ref/Tb/24.4)*1000
       O2FLUX = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
      else
c     send the allometric value to the output file
       O2FLUX = 10.**(MR_3*TC)*MR_1*(AMASS*1000)**MR_2
      endif
      CO2FLUX = JMCO2/(T_ref/Tb/24.4)*1000
c     mlO2/h, stp
c     MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000/Tcorr
      MLO2(hour) = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
c     g metabolic water/h
      GH2OMET(hour) = JMH2O*18.01528
c     metabolic heat production (Watts) - growth overhead plus dissipation power (maintenance, maturity maintenance, 
c     maturation/repro overheads) plus assimilation overheads
      DEBQMET(hour) = (kappa_G*p_G+p_D+(p_X-p_A-p_A*mu_P*eta_PA))
     &    /3600
      DRYFOOD(hour)=-1*JOJx*w_X
      FAECES(hour)=JOJp*w_P
      NWASTE(hour)=JMNWASTE*w_N
      wetgonad(hour) = ((cumrepro(hour)/mu_E)*w_E)/eggdryfrac 
     &+((cumbatch(hour)/mu_E)*w_E)/eggdryfrac
      wetstorage(hour) = (((V(hour)*ED(hour))/mu_E)*w_E)/d_V
      wetfood(hour) = ((ms(hour)/mu_E)*w_E)/d_V
      wetmass(hour) = V(hour)*ANDENS_deb+wetgonad(hour)+
     &wetstorage(hour)+wetfood(hour)
      gutfreemass=V(hour)*ANDENS_deb+wetgonad(hour)+
     &wetstorage(hour)
      potfreemass=V(hour)*ANDENS_deb+(((V(hour)*E_m)/mu_E)*w_E)/d_V
      v_baby1(hour)=v_baby
      e_baby1(hour)=e_baby
      EH_baby1(hour)=EH_baby
      if(monthly.eq.2)then
       goto 101
      endif

      if(conth.eq.0)then
       if((viviparous.eq.1).and.(E_H_pres.gt.E_Hb))then
c      make the mass, metabolic heat and O2 flux that of a fully grown individual to get the heat balance of 
c      a thermoregulating mother with full reserves
        amass=maxmass/1000
        p_M = pT_M*V_max
        p_C = (E_m*(vTdot/L_max+kT_Mdot*(1+L_T/L_max))*
     &    (1*g)/(1+g))*v_max
        p_J = kT_J*E_He
        p_R = -1*kappa
        p_D = p_M+p_J+(1-k_R)*p_R
        p_A = V_max**(2./3.)*pT_Am*f
        p_X = p_A/kappa_X
        JOJx=p_A*etaO(1,1)+p_D*etaO(1,2)+p_G*etaO(1,3)
        JOJv=p_A*etaO(2,1)+p_D*etaO(2,2)+p_G*etaO(2,3)
        JOJe=p_A*etaO(3,1)+p_D*etaO(3,2)+p_G*etaO(3,3)
        JOJp=p_A*etaO(4,1)+p_D*etaO(4,2)+p_G*etaO(4,3)
        JMO2=JOJx*JM_JO(3,1)+JOJv*JM_JO(3,2)+JOJe*JM_JO(3,3)+
     &        JOJp*JM_JO(3,4)
c      MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000/Tcorr
        MLO2(hour) = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
        DEBQMET(hour) = (p_D+(p_X-p_A-p_A*mu_P*eta_PA))
     &    /3600
       else
        amass=wetmass(hour)/1000.
       endif
      endif

101   continue

c    starvation for pupa (and late 5th instar for Heteronympha
c      goto 987
      if((stage.eq.stages-2).or.((metab_mode.eq.1).and.
     &    (stage.eq.stages-3)))then
       if(ed(hour).le.E_m*0.05)then
        dead=1
         if(reset.eq.0)then
          surviv(hour)=0.49
          longev=(daycount+hour/24.)/365.
          nyear=iyear
          census=365
         endif
        if((stage.gt.deathstage).and.(v(hour).ne.0))then
         causedeath=4.
         deathstage=stage
        endif
       endif
      else
       if((v(hour).gt.3e-9).and.(ed(hour).le.e_m*0.05))then
        dead=1
         if(reset.eq.0)then
          surviv(hour)=0.49
          longev=(daycount+hour/24.)/365.
          nyear=iyear
          census=365
         endif
        if((stage.gt.deathstage).and.(v(hour).ne.0))then
         causedeath=4.
         deathstage=stage
        endif
       endif
      endif
c    if(stage.eq.stages-2)then
c     if(ed(hour).le.1500)then
c      dead=1
c    endif
c    endif

c ************* yearly output *************************

987   continue
c    if((metab_mode.eq.2).and.(stage.eq.stages-1))then
c    dead=1
c    endif
      if(dead.eq.1)then
       ctmincum=0
       ms_past=0
       p_B_past=0 
       svl(hour)=0
       wetgonad(hour)=0
       wetstorage(hour)=0
       wetfood(hour)=0
       wetmass(hour)=0
       MLO2(hour) = 0
       O2FLUX = 0
       CO2FLUX = 0
       v(hour)=0
       e_H(hour)=0
       cumrepro(hour)=0
       cumbatch(hour)=0
       vpup(hour)=0
       vold(hour)=0
       ed(hour)=0
       hs(hour)=0
       surviv(hour)=1
       ms(hour)=0
       stage=0.
       v_init=debfirst(3)
       E_init=debfirst(4)
       ms_init=debfirst(5)
       cumrepro_init=debfirst(6)
       q_init=debfirst(7)
       hs_init=debfirst(8)
       cumbatch_init=debfirst(9)
       v_baby_init=debfirst(10)
       e_baby_init=debfirst(11)
       E_H_init=debfirst(12)
       EH_baby_init=0.
       MLO2_init=0.
       GH2OMET_init=0.
       debqmet_init=0.
       surviv_init=1.
       Vold_init=debfirst(3)
       Vpup_init=0.
       Epup_init=0.
       E_Hpup_init=0.
       if(hour.gt.1)then
        mlo2(hour-1)=0
       endif
       AMASS=((((V_init*E_init)/mu_E)*w_E)/d_V + V_init)/1000
      endif

      stage_rec(hour)=stage

      RETURN   
      END  
