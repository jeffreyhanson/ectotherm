       SUBROUTINE DEB_INSECT(hour)  

c    Michael Kearney's implementation of Maino's body condition (reserve density)-based DEB model
c    for insects - 26 Dec 2013           

      Implicit None

c      EXTERNAL JAC
c    EXTERNAL GUT

c      DOUBLE PRECISION Y,YDOT,T,TOUT,RTOL,ATOL,RWORK
      DOUBLE PRECISION dVdt,dqdt,rdot,dhsds,dEdt,dE_Hdt
     &    ,dsurvdt,hs

      REAL SVL,v_baby,e_baby,EH_baby,v_baby1,e_baby1,EH_baby1,funct    
      REAL E_pres,ED,V,V_pres,gutfull,orig_clutchsize,Vb,p_Am_scaled
      REAL v_init,E_init,ms_init,v_baby_init,EH_baby_init
      REAL e_baby_init,e_init_baby,v_init_baby,wetstorage,wetgonad
      REAL wetfood,E_scaled,V_max,k_Mdot,surviv_init
      REAL E_H_pres,E_H_init,E_H,E_Hj,wetmass,orig_MsM
      REAL zfact,causedeath,deathstage
      REAL T_A,TAL,TAH,TL,TH,Tcorr,Tb
      REAL T_Ref,E_Hp,E_Hb,halfsat,x_food,E_H_start
      REAL k_R,p_Am,p_A,p_Mv,p_M,p_C,p_R,p_J,p_D,p_G,p_B,vdot
      REAL f,p_Xm,X,p_X,food,annfood
      REAL w_E,mu_E,mu_V,w_V,M_V,E_egg,eggmass
      REAL E_M,E_G,kappa,cumrepro,cumbatch
      REAL d_V,p_B_past,GH2OMET
      REAL q,h_a,ms,ms_pres,dmsdt,MsM,ms_past
      REAL PI,eggdryfrac,hs_pres,q_pres
      REAL G
      REAL QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      REAL AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      REAL Tc,depsel,tcores
      REAL QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,clutchsize
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      REAL fec,tknest,clutchenergy
      REAL acthr,actxbas,thconw
      REAL TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask,temerge
      real svl_met,ctmin,ctmax,eggsoil,surv
      REAL cumrepro_init,q_init,hs_init,
     &cumbatch_init,p_Mref,vdotref,h_aref,maxmass,p_xmref,
     &k_Jref,k_J,s_G
      REAL andens_deb,delta_deb,daylengthstart,
     &daylengthfinish,lengthday,lengthdaydir,prevdaylength,lat
      REAL CONTH,CONTW,CONTVOL,CONTDEP,rainfall,lambda,breedrainthresh
      real JMCO2,JMH2O,JMO2,JMNWASTE,JOJx,JOJv,JOJe,JOJp
      real JMCO2_GM,JMH2O_GM,JMO2_GM,JMNWASTE_GM,JOJx_GM,JOJv_GM
     &    ,JOJe_GM,JOJp_GM,etaO,JM_JO,pond_depth,twater,potfreemass
      real L_T,L_pres,L_max,scaled_l,MLO2,debqmet,kappa_G
      real kappa_X,kappa_X_P,mu_X,mu_P,yEX,yXE,yPX,mu_AX,eta_PA
      real MLO2_init,GH2OMET_init,debqmet_init,w_N,w_X,w_P
      real DRYFOOD,FAECES,NWASTE,O2FLUX,CO2FLUX,gutfreemass
      real Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,surviv_pres
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,longev,surviv
      real rhref,E_Hmoult1,E_Hmet,E_Hecl,breedtempthresh,gam,repro
      real p_Am1,p_AmIm,disc
      real Vold_init,Vpup_init,Epup_init,E_Hpup_init,Vold,Vpup,Epup,
     &E_Hpup,E_Hthresh,pond_env,tbs,oldclutch,stage
      real Vold_pres,Vpup_pres,Epup_pres,E_Hpup_pres,dVold_dt,dVpup_dt
      real rainfall2,debfirst,ectoinput,p_AmImT,p_Am1T,grassgrowth
     &    ,grasstsdm,raindrink,wetlandTemps,wetlandDepths,conthole
      real gutfill
      real tbmean,meanf

      real newclutch,fieldcap,wilting

      real fec1,fec2,fec3,fec4,fec5,fec6,fec7,fec8,fec9,fec10,
     &fec11,fec12,fec13,fec14,fec15,fec16,fec17,fec18,fec19,fec20,act1
     &,act2,act3,act4,act5,act6,act7,act8,act9,act10,act11,act12,act13
     &,act14,act15,act16,act17,act18,act19,act20,stage_rec
      real for1,for2,for3,for4,for5,for6,for7,for8,for9,for10,for11,
     &    for12,for13,for14,for15,for16,for17,for18,for19,for20
      real cri_o,cri_m,cri

      INTEGER day,hour,iyear,nyear,countday,i,pregnant,startday,
     &viviparous,daycount,batch,photostart,photofinish,metamorph,reset,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage,deadead
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66,complete
      INTEGER julday,monthly,wingmod,wingcalc,firstday,completion
      integer DEB1,inwater,aquatic,feeding,ctmincum,ctminthresh,ctkill
      integer metab_mode,stages,wetmod,contonly,contype
      integer breedact,breedactthres,waiting,breedtempcum,census
      integer starttime,endtime,prevdead,f1count,counter,soilmoisture

      character*1 transt,dlenth

      DIMENSION V(24),ED(24),wetmass(24),E_H(24),eggsoil(24)
     &,wetstorage(24),wetfood(24),svl(24),Vold(24),Vpup(24),Epup(24)
     &,E_Hpup(24),stage_rec(25)
      DIMENSION wetgonad(24),repro(24),surv(100)
     &    ,cumrepro(24),q(24),hs(24),ms(24),EH_baby1(24)
     &   ,cumbatch(24),food(50),v_baby1(24),e_baby1(24),surviv(24)
      DIMENSION depsel(25),tcores(25),acthr(25),actxbas(25)
      DIMENSION etaO(4,3),JM_JO(4,4),fec(100)
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),
     &    FAECES(24),NWASTE(24)
      dimension rainfall2(7300),debfirst(13),ectoinput(126)
      dimension grassgrowth(7300),grasstsdm(7300),wetlandTemps(24*7300)
     &    ,wetlandDepths(24*7300),pond_env(20,365,25,2),tbs(24*7300)

      DIMENSION QSOL(25),RH(25),TskyC(25),soil1(25),rhref(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TIME(25)

      Data PI/3.14159/

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
      COMMON/DEBOUT/fecundity,clutches,monrepro,svlrepro,monmature
     &,minED,annfood,food,longev,completion,complete,fec1,fec2,
     &fec3,fec4,fec5,fec6,fec7,fec8,fec9,fec10,fec11,fec12,fec13,fec14,
     &fec15,fec16,fec17,fec18,fec19,fec20,act1,act2,act3,act4,act5,act6,
     &act7,act8,act9,act10,act11,act12,act13,act14,act15,act16,act17,
     &act18,act19,act20,fec,surv,for1,for2,for3,for4,for5,for6,for7,for8
     &,for9,for10,for11,for12,for13,for14,for15,for16,for17,for18,for19,
     &for20
      common/gut/gutfull,gutfill
      COMMON/DEBINIT/v_init,E_init,ms_init,cumrepro_init,q_init,
     &hs_init,cumbatch_init,p_Mref,vdotref,h_aref,e_baby_init,
     &v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,halfsat,x_food,
     &Vold_init,Vpup_init,Epup_init,E_Hpup_init,p_xmref
      COMMON/z/tknest,Thconw
      Common/Behav3/Acthr,ACTXBAS
      common/julday/julday,monthly
      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch,MsM
     &,lambda,breedrainthresh,daylengthstart,daylengthfinish,photostart
     &,photofinish,lengthday,photodirs,photodirf,lengthdaydir
     &,prevdaylength,lat,svl_met,frogbreed,frogstage,metamorph
     &,breedactthres
      COMMON/DEBPAR3/metab_mode,stages,p_Am1,p_AmIm,
     & disc,gam,E_Hmoult1,E_Hmet,E_Hecl,Vb
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      common/vivip/viviparous,pregnant
      common/debbaby/v_baby,e_baby,EH_baby,eggmass
      COMMON/CONT/CONTH,CONTW,CONTVOL,CONTDEP,wetmod,contonly,conthole
     &    ,contype
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
      common/stage_r/stage_rec,f1count,counter,meanf
      common/soilmoist/fieldcap,wilting,soilmoisture

c     James' new parameter names
      cri_o=E_Hb
      cri_m=E_Hj
      cri=cri_o*cri_m**5

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

      if(stage.eq.0)then
       meanf=1.
       counter=0
       f1count=0
      endif
              
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
       minED = E_pres
       E_H_pres = E_init
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
       E_H_pres = Ed(hour-1)
       q_pres = q(hour-1)
       hs_pres = real(hs(hour-1),4)
       surviv_pres = surviv(hour-1)
       Vold_pres = Vold(hour-1)
       Vpup_pres = Vpup(hour-1)
       Epup_pres = Epup(hour-1)
       E_Hpup_pres = E_Hpup(hour-1)
      endif

      v_pres=v_pres*1.00

c    set body temperature
      Tb = Tc
c      Tb = 36.

      if((frogbreed.eq.1).or.(frogbreed.eq.2))then
       contdep=pond_env(iyear,countday,hour,2)
       Tb=pond_env(iyear,countday,hour,1)
      endif

c    Tb=28
c    contdep=100

c    if running a frog or aquatic insect, check if terrestrial breeder and set Tb to soil temp
      if(stage.eq.0)then
       if(frogbreed.eq.2)then
        Tb = Eggsoil(hour)
       endif
      endif

c    Arrhenius temperature correction factor 5 parameters

            Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tb)))/(1+EXP(TAL
     &*(1/(273+Tb)-1/TL))+EXP(TAH*(1/TH-1/(273+Tb))))


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
      M_V = ANDENS_deb/w_V
      p_Mv = p_Mref*Tcorr
      k_Mdot = p_Mv/E_G
      k_J = k_Jref*Tcorr
      p_AmImT=p_AmIm*Tcorr
      p_Am1T=p_Am1*Tcorr
      p_Am = p_Am1T

      if(stage.eq.0)then
c     egg
       E_Hthresh = E_Hb*meanf
      endif
      if(stage.eq.stages-2)then
       if(metab_mode.eq.2)then
c     pupa
c      p_Am = 0.00001/24
       E_Hthresh = E_Hecl*meanf
       else
         stage=stage+1
       endif
      endif
      if(stage.ge.stages-1)then
c      imago
c        if(breeding.eq.1)then
c       p_Am = 0.00001/24
c       if((hour.eq.24).and.(reset.eq.2))then
c        dead=1
c       endif
c      else
c       p_Am = p_AmImT
c       check to see if need reset at the end of breeding season
c       assuming that when there is some energy in the buffer but breeding=0
c       that there has been a breeding season already
c       if((hour.eq.24).and.(reset.eq.3).and.
c     &       (cumbatch(hour-1).gt.0))then
c        dead=1
c       endif
c      endif
       E_Hthresh = E_Hecl*meanf
      endif
      if((stage.gt.0).and.(stage.lt.stages-2))then
c     larva
c        E_Hthresh = E_Hmoult1*exp(disc)**(stage-1)*meanf
c       E_Hthresh = E_Hmoult1*exp(disc)**(stage-1)*meanf
       E_Hthresh = meanf*(E_Hb/1000)*(E_Hmoult1/1000)**(stage)*1000
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

      vdot = vdotref*Tcorr
c    p_Xm = p_Am/kappa_X
      p_Xm=p_Xmref*Tcorr
c    h_a = h_aref*Tcorr
      L_T = 0.
      L_pres = V_pres**(1./3.)
      kappa_G = (d_V*mu_V)/(w_V*E_G)
      yEX=kappa_X*mu_X/mu_E
      yXE=1/yEX
      yPX=kappa_X_P*mu_X/mu_P
      mu_AX=mu_E/yXE
      eta_PA=yPX/mu_AX

c    call subroutine that assesses photoperiod cues on breeding
c    CALL BREED(julday,photostart,photofinish,lengthday
c     &,daylengthstart,daylengthfinish,photodirs,photodirf,prevdaylength,
c     &lat,firstday,breedact,breedactthres,tbs,hour,countday,
c     &breedtempthresh,breedtempcum,daycount)

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

c    call subroutine (for frogs at this stage) that assesses if user wants cumulative resets of development after metamorphosis
c    CALL DEVRESET(dead,E_H_pres,E_Hb,frogbreed,breeding,
c     &    conth,contdep,stage,frogstage,reset,complete,waiting)

      waiting=0

      if(reset.gt.0)then
       if(stage.ge.reset)then
c      if(hour.eq.24)then
c       dead=1
c       complete=0
c      else
         complete=1
         stage=stages-1
c      endif
       endif
      endif

      if((complete.eq.1).and.(hour.eq.24))then
       completion=completion+1
       dead=1
       complete=0
       stage=stages-1
      endif

      if(frogbreed.eq.1)then
       if(breeding.eq.1)then
        if(frogstage.eq.1)then 
c        resetting at metamorphosis            
         if(stage.eq.2)then
          dead=1
         endif
        endif
       endif
      endif

      if(frogbreed.eq.1)then
       if(stage.le.1)then
        if(contdep.le.0.1)then
         dead=1
        endif
       endif
      endif

c    prevent egg development in soil if not the right season
      if(frogbreed.eq.2)then
       if(E_H_pres.le.E_Hb)then
        if(breeding.eq.0)then
         dead=1
        endif
       endif
      endif

c    kill eggs of terrestrial breeders if the pond fills up too soon
c    if(frogbreed.eq.2)then
c     if(stage.eq.0)then
c      if(E_H_pres.le.E_Hb*0.90)then
c      if(contdep.gt.50)then
c       dead=1
c      endif
c     endif
c     endif
c    endif

c    kill tadpoles if pond dries
      if(frogbreed.eq.2)then
       if(stage.eq.1)then
        if(contdep.le.0.1)then
         waiting=1
         E_H_pres=E_Hb
         stage=0.
        endif
       endif
       if(frogstage.eq.1)then 
        if(stage.eq.2)then
         dead=1
         waiting=0
        endif
       endif
      endif

      if(frogbreed.eq.2)then
       if(stage.eq.1)then
        if(contdep.le.0.1)then
         dead=1
         waiting=0
        endif
       endif
      endif

      if(frogbreed.eq.3)then
       if(frogstage.eq.1)then
        if(breeding.eq.1)then
         if(stage.ge.1)then
          dead=1
          waiting=0
         endif
        else
         dead=1
         waiting=0
        endif
       endif
      endif

      if(frogbreed.eq.3)then
       if(frogstage.eq.0)then
        if(E_H_pres.le.E_Hb)then
         if(breeding.eq.0)then
          dead=1
          waiting=0
         endif
        endif
       endif
      endif

c    ********end devreset*************


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
      
c    clutchsize=ANINT(0.023*(V_pres**(0.3333333333333)/delta_deb*10)
c     &    +0.35)
      if(clutchsize.lt.1)then
       clutchsize=1
      endif
      clutchenergy = E_egg*clutchsize

      if(stage.ne.stages-2)then
       p_C=(E_pres*V_pres)*(E_G*vdot/V_pres**(1./3.)+p_Mv)/
     &    ((E_pres*V_pres)/V_pres+E_G)
      else
       p_C=(E_pres*Vpup_pres)*(E_G*vdot/Vpup_pres**(1./3.)+p_Mv)/
     &    ((E_pres*Vpup_pres)/Vpup_pres+E_G)
      endif

      if(stage.eq.0)then
       p_A=0
      else
       if(hour.eq.1)then
        if(ms_init.gt.0.0)then
         p_A=f*p_Am*Vb**(2./3.)*(V_pres/Vb)**gam
         f1count=f1count+1
         meanf=(f1count*1.)/(counter*1.)
         counter=counter+1
        else
         p_A=0
         counter=counter+1
        endif
       else
        if(ms(hour-1).gt.0.0)then
         p_A=f*p_Am*Vb**(2./3.)*(V_pres/Vb)**gam
         f1count=f1count+1
         meanf=(f1count*1.)/(counter*1.)
         counter=counter+1
        else
         p_A=0
         counter=counter+1
        endif
       endif
      endif
      
      if(stage.eq.0)then
       p_Am_scaled=p_Am
      else
       p_Am_scaled=(f*p_Am*Vb**(2./3.)*(V_pres/Vb)**gam)
     &      /V_pres**(2./3.)
      endif

      E_M = p_Am/vdot
c    V_max=(kappa*p_Am_scaled/p_Mv)**(3.)
      V_max=(1*p_Am_scaled/p_Mv)**(3.)
c    increase p_Xmref by proportion that p_Am has been scaled up (reverting to reference temp) then temp correct
      p_Xm = p_Xmref*(p_Am_scaled/Tcorr)/p_Am1*Tcorr
c    g = E_G/(kappa*E_M)
      g = E_G/(1*E_M)
      E_scaled=E_pres/E_m
      h_a = h_aref*Tcorr
      L_T = 0.
      L_pres = V_pres**(1./3.)
      L_max = V_max**(1./3.)
      scaled_l = L_pres/L_max

c    calculate rate of change of Vold (i.e. egg to larval structure and its degredation)
      if(stage.ne.stages-2)then
       dVold_dt=(p_C-V_pres*p_Mv)/E_G
      else
       if(V_pres.le.0)then
        dVold_dt=0
       else
        dVold_dt=-1.*vdot*V_pres**(2./3.)
       endif
      endif

c    calculate rate of change of Vpup (i.e. new structure formed during metamorphosis)
      if(stage.eq.stages-2)then
c    metamorphosis has started, grow the new structure during pupal phase
       if(v_pres.le.0)then
        dVpup_dt=0
       else
c     dVpup_dt=kappa_G*Vpup_pres*((E_pres*V_pres*vdot)/(Vpup_pres**
c     &(4./3.))-(p_Mv/kappa))/((E_pres*V_pres)/Vpup_pres+(E_G/kappa))
        dVpup_dt=(p_C-Vpup_pres*p_Mv)/E_G
       endif
      else
       dVpup_dt=0
      endif

      if(stage.ne.stages-2)then
       if(stage.eq.stages-1)then
        dedt=-1*kappa*Tcorr-V_pres*p_Mv
       else
        dedt=p_A-p_C
       endif
      else
       dedt=-1.*kappa_G*kappa_G*E_G*dVold_dt-p_C
      endif

c    old maturity is now reserve density too
      dE_Hdt=dedt

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

      p_M = p_Mv*V_pres

c    equation 2.20 DEB3

c    this power isn't used in the insect model - kappa is now reproduction power
      p_J = 0.

      if((stage.lt.stages-1).or.(pregnant.eq.1))then
       p_B = 0.
      else
       p_B = kappa
c     end check for whether batch mode is operating
      endif

      p_R = p_B

      if(stage.ge.stages-1)then
       p_D = p_M+(1-k_R)*p_R
      else
       p_D = p_M+p_R
      endif

      p_G = p_C-p_M-p_J-p_R

c    if(frogbreed.eq.1)then
c     if(stage.lt.stages-1)then
c      if(contdep.le.0.1)then
c       dE_Hdt=0.
c      endif
c     endif
c      endif


c    aging
c    dqdt = 1*g*E_m*(dvdt+k_Mdot*V_pres)
        if(L_pres.eq.0)then
         rdot=0
         dqdt=0
        else
        rdot=vdot*(E_scaled/L_pres-(1+L_T/L_pres)/L_max)/
     &    (E_scaled+g)
      dqdt = (q_pres*(V_pres/V_max)*s_G+h_a)*(E_pres/E_m)
     &*((vdot/L_pres)-rdot)-rdot*q_pres
        endif      
            


      if(stage.ge.stages-1)then
       if(hour.eq.1)then
        q(hour) = q_init + dqdt
       else
        q(hour) = q(hour-1)+dqdt
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

c    h_w = ((h_a*(E_pres/E_m)*vdot)/(6.*V_pres**(1./3.)))
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

c        nyear=iyear
c      longev=5
       endif
      endif

c     accumulate energy/matter in reproduction buffer
c    if it is the beginning of the day
      if(stage.ge.stages-1)then
       if(hour.eq.1)then
c      if the buffer ran out in the previous hour
        if(cumrepro_init.lt.0)then
c       keep it empty
         cumrepro(hour)=0
        else
         cumrepro(hour) = cumrepro_init
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

c    if(cumbatch(hour).lt.0.1*clutchenergy)then
c        stage=3
c    endif
      
c    calculate Vold for this hour, ensuring it doesn't go negative
      Vold(hour)=Vold_pres+dVold_dt
      if(Vold(hour).lt.0)then
       Vold(hour)=0
      endif

c    calculate Vpup for this hour, ensuring it doesn't go negative
      Vpup(hour)=Vpup_pres+dVpup_dt
      if(Vpup(hour).lt.0)then
       Vpup(hour)=0
      endif

c    sum structures    
      V(hour)=Vpup(hour)+Vold(hour)

      if(stage.eq.stages-2)then
       if(v(hour).le.0)then
        ED(hour)=0
       else
        ED(hour)=(E_pres*V(hour)+real(dEdt,4))/V(hour)
       endif
      else
       ED(hour) = (E_pres*V_pres+real(dEdt,4))/V(hour)
      endif
      E_H(hour) = Ed(hour)
c    make sure ED doesn't go below zero
      if(ED(hour).lt.0)then
       ED(hour)=0
      endif
c    find min value of ED for the simulation
      if(ED(hour).lt.minED)then
       minED=ED(hour)
      endif

c    svl in mm
      svl(hour) = V(hour)**
     &    (0.3333333333333)/delta_deb*10

c    transition between stages for insect
      if(stage.lt.stages-2)then
c     before pupal stage
       if(stage.eq.0)then
        if(E_H(hour).le.E_Hb)then
c       start the larval stage
        stage=stage+1
         Vb=Vold(hour)
        endif
       else
        if(E_H(hour).gt.E_Hthresh)then
         stage=stage+1
        endif
       endif
      else
       if(stage.eq.stages-2)then
        if(metab_mode.eq.2)then
        if((E_H(hour)*V(hour)/Vpup(hour)).lt.E_Hecl)then
c        eclosion
         stage=stage+1
        endif
        else
c       hemimetabolous - so skip this stage        
        stage=stage+1
        endif
       endif
      endif
      
      if(cumbatch(hour).gt.0)then
       if(monmature.eq.0)then
        monmature=(day+365*(iyear-1))/30.5
       endif
      endif

      if(cumbatch(hour).gt.clutchenergy)then
c     batch is ready so if viviparous, start gestation, else dump it
       if(viviparous.eq.1)then
        if(pregnant.eq.0)then
         v_baby=v_init_baby
         e_baby=e_init_baby
         EH_baby=0.
         pregnant=1
        endif
        if(hour.eq.1)then
         v_baby=v_baby_init
         e_baby=e_baby_init
         EH_baby=EH_baby_init
        endif
        call deb_baby
        if(EH_baby.gt.E_Hb)then
         if((Tb .lt. tminpr) .or. (Tb .gt. tmaxpr))then
          goto 898
         endif
         cumbatch(hour) = cumbatch(hour)-clutchenergy
         repro(hour)=1
        if(iyear.eq.1)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.2)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.3)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.4)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.5)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.6)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.7)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.8)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.9)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.10)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.11)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.12)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.13)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.14)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.15)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.16)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.17)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.18)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.19)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.20)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
         fecundity=fecundity+clutchsize
         clutches=clutches+1
         if(fecundity.ge.clutchsize)then
          monrepro=(day+365*(iyear-1))/30.5
          svlrepro=svl(hour)
         endif
         pregnant=0
        endif
       else
c      not viviparous, so lay the eggs at next period of activity
        if(breeding.eq.0)then
        goto 898
        endif
        if(breedrainthresh.gt.0)then
         if(rainfall.lt.breedrainthresh)then
          goto 898
         endif
        endif
        if((Tb .lt. tminpr) .or. (Tb .gt. tmaxpr))then
         goto 898
        endif
c    change below to active or not active rather than depth-based, in case of fossorial
        if(Acthr(hour).eq. 0)then
         goto 898
        endif
        if(complete.eq.1)then
         clutchsize=0.
        else
         clutchsize=ANINT((cumbatch(hour)+cumrepro(hour))/E_egg)
         newclutch=clutchsize
        endif
c      cumbatch(hour) = cumbatch(hour)-clutchenergy
c      specific for heteronympha
        if(metab_mode.eq.2)then
        stage=stage+1
        endif
        repro(hour)=1
        if(iyear.eq.1)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.2)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.3)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.4)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.5)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.6)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.7)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.8)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.9)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.10)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.11)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.12)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.13)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.14)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.15)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.16)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.17)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.18)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.19)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        if(iyear.eq.20)then
         fec(iyear)=fec(iyear)+clutchsize
        endif
        fecundity=fecundity+clutchsize
        clutches=clutches+1
        clutchsize = orig_clutchsize
        if(fecundity.ge.clutchsize)then
         monrepro=(day+365*(iyear-1))/30.5
         svlrepro=svl(hour)
        endif
       endif
      endif


898   continue

      if((reset.gt.0).and.(reset.ne.8))then
        if(iyear.eq.1)then
         fec(iyear)=completion
        endif
        if(iyear.eq.2)then
         fec(iyear)=completion
        endif
        if(iyear.eq.3)then
         fec(iyear)=completion
        endif
        if(iyear.eq.4)then
         fec(iyear)=completion
        endif
        if(iyear.eq.5)then
         fec(iyear)=completion
        endif
        if(iyear.eq.6)then
         fec(iyear)=completion
        endif
        if(iyear.eq.7)then
         fec(iyear)=completion
        endif
        if(iyear.eq.8)then
         fec(iyear)=completion
        endif
        if(iyear.eq.9)then
         fec(iyear)=completion
        endif
        if(iyear.eq.10)then
         fec(iyear)=completion
        endif
        if(iyear.eq.11)then
         fec(iyear)=completion
        endif
        if(iyear.eq.12)then
         fec(iyear)=completion
        endif
        if(iyear.eq.13)then
         fec(iyear)=completion
        endif
        if(iyear.eq.14)then
         fec(iyear)=completion
        endif
        if(iyear.eq.15)then
         fec(iyear)=completion
        endif
        if(iyear.eq.16)then
         fec(iyear)=completion
        endif
        if(iyear.eq.17)then
         fec(iyear)=completion
        endif
        if(iyear.eq.18)then
         fec(iyear)=completion
        endif
        if(iyear.eq.19)then
         fec(iyear)=completion
        endif
        if(iyear.eq.20)then
         fec(iyear)=completion
        endif
      endif
      
c    if((stage.ne.0).and.(stage.ne.stages-2))then
c       if(Depsel(hour).ge. 0)then
c      if((Tb .ge. tminpr) .and. (Tb .le. tmaxpr))then
c       if((aquatic.eq.1).and.(feeding.eq.0))then
c        dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
c    &       p_Xm*v_pres**(2./3.)*funct*(Ms_pres/(MsM*v_pres))
c       else
c        dMsdt = p_Xm*v_pres**(2./3.)*funct*(X_food/(halfsat+X_food))-
c     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
c         endif
c      else
c       dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
c     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
c      endif
c     else
c       dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
c     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
c     endif
      if(v_pres.le.0)then
      dMsdt=0
      else
      if((stage.ne.0).and.(stage.ne.stages-2))then
c       if(Depsel(hour).ge. 0)then
        if((Tb .ge. tminpr) .and. (Tb .le. tmaxpr))then
         if((aquatic.eq.1).and.(feeding.eq.0))then
          dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       p_Xm*v_pres**(2./3.)*funct*(Ms_pres/(MsM*v_pres))
         else
          dMsdt = p_Xm*v_pres**(2./3.)*funct*(X_food/(halfsat+X_food))-
     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
         endif
        else
         dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
        endif
       else
         dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
       endif
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
       if(cumrepro(hour).ne.cumrepro(hour))then
        cumrepro(hour)=0.
       endif
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
c    mlO2/h, stp
c    MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000/Tcorr
      MLO2(hour) = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
c    g metabolic water/h
      GH2OMET(hour) = JMH2O*18.01528
c    metabolic heat production (Watts) - growth overhead plus dissipation power (maintenance, maturity maintenance, 
c    maturation/repro overheads) plus assimilation overheads
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
        p_M = p_Mv*V_max
        p_C = (E_m*(vdot/L_max+k_Mdot*(1+L_T/L_max))*
     &    (1*g)/(1+g))*v_max
        p_J = k_J*E_Hecl
        p_R = -1*kappa
        p_D = p_M+p_J+(1-k_R)*p_R
        p_A = V_max**(2./3.)*p_Am*f
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

      if(amass.lt.0.001)then
c    amass=0.001
      endif

101   continue

c    e_m_thresh=e_m*0.05

c    starvation for pupa (and late 5th instar for Heteronympha
      goto 987
      if((stage.eq.stages-2).or.((metab_mode.eq.1).and.
     &    (stage.eq.stages-3)))then
       if(ed(hour).le.((p_Am1T*gam**(stage-1))/vdot)*0.05)then
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



