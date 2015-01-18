       SUBROUTINE DEB(hour)  

c    Michael Kearney's implementation of Kooijman's k-rule DEB model, based on excel spreadsheet           

      Implicit None

c      EXTERNAL JAC
c    EXTERNAL GUT

      DOUBLE PRECISION dVdt,dqdt,rdot,dhsds,Sc,dUedt,E_temp,dEdt,dE_Hdt
     &    ,dUHdt,dsurvdt,hs

      REAL SVL,v_baby,e_baby,EH_baby,v_baby1,e_baby1,EH_baby1,funct    
      REAL E_pres,ED,V,V_pres,gutfull
      REAL v_init,E_init,ms_init,dldt,V_temp,v_baby_init,EH_baby_init
      REAL e_baby_init,e_init_baby,v_init_baby,wetstorage,wetgonad
      REAL wetfood,E_scaled,U_H_pres,V_max,k_Mdot,surviv_init
      REAL E_H_pres,E_H_init,E_H,E_Hj,wetmass
      REAL zfact,causedeath,deathstage,testclutch
      REAL T_A,TAL,TAH,TL,TH,Tcorr,Tb
      REAL T_Ref,E_Hp,E_Hb,halfsat,x_food
      REAL k_R,p_Am,p_A,p_Mv,p_M,p_C,p_R,p_J,p_D,p_G,p_B,vdot
      REAL f,p_Xm,p_X,food,annfood,stage_rec
      REAL w_E,mu_E,mu_V,w_V,M_V,E_egg,eggmass
      REAL E_M,E_G,kappa,cumrepro,cumbatch
      REAL d_V,p_B_past,GH2OMET
      REAL q,h_a,ms,ms_pres,dmsdt,MsM,ms_past
      REAL PI,eggdryfrac,hs_pres,q_pres
      REAL G,orig_MsM
      REAL QSOLAR,QIRIN,QMETAB,QRESP,QSEVAP,QIROUT,QCONV,QCOND
      REAL AMASS,RELHUM,ATOT,FATOSK,FATOSB,EMISAN,SIG,Flshcond
      REAL Tc,depsel,tcores,orig_clutchsize
      REAL QSOL,RH,TskyC,SOIL1,SOIL3,TIME,Taloc,TREF,clutchsize
      REAL fecundity,clutches,monrepro,svlrepro,monmature,minED
      REAL fec,tknest,clutchenergy
      REAL acthr,actxbas,thconw,cumrepro_temp
      REAL TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask,temerge
      real svl_met,ctmin,ctmax,eggsoil,surv
      REAL cumrepro_init,q_init,hs_init,
     &cumbatch_init,p_Mref,vdotref,h_aref,maxmass,p_Xmref,
     &k_Jref,k_J,batchprep,s_G,potfreemass
      REAL andens_deb,delta_deb,daylengthstart,
     &daylengthfinish,lengthday,lengthdaydir,prevdaylength,lat
      REAL CONTH,CONTW,CONTVOL,CONTDEP,rainfall,lambda,breedrainthresh
      real JMCO2,JMH2O,JMO2,JMNWASTE,JOJx,JOJv,JOJe,JOJp
      real JMCO2_GM,JMH2O_GM,JMO2_GM,JMNWASTE_GM,JOJx_GM,JOJv_GM
     &    ,JOJe_GM,JOJp_GM,etaO,JM_JO,pond_depth,twater
      real L_T,L_pres,L_max,scaled_l,MLO2,debqmet,kappa_G
      real kappa_X,kappa_X_P,mu_X,mu_P,yEX,yXE,yPX,mu_AX,eta_PA
      real MLO2_init,GH2OMET_init,debqmet_init,w_N,w_X,w_P
      real DRYFOOD,FAECES,NWASTE,O2FLUX,CO2FLUX,gutfreemass
      real Tlung,DELTAR,EXTREF,RQ,MR_1,MR_2,MR_3,surviv_pres
      real rho1_3,trans1,aref,bref,cref,phi,F21,f31,f41,f51,sidex,WQSOL
     &    ,phimin,phimax,TWING,F12,F32,F42,F52,f23,f24,f25,f26
     &,f61,TQSOL,A1,A2,A3,A4,A4b,A5,A6,f13,f14,f15,f16,longev,h_w
      real rhref,E_Hmoult1,E_Hmet,E_Hecl,newclutch,depress,surviv
      real p_Am1,p_AmIm,disc
      real Vold_init,Vpup_init,Epup_init,E_Hpup_init,Vold,Vpup,Epup,
     &    E_Hpup,E_H_start,breedtempthresh,gam,repro,tbmean
      real rainfall2,debfirst,ectoinput,grassgrowth,grasstsdm,raindrink
      real wetlandTemps,wetlandDepths,conthole,pond_env,tbs,stage
      real fec1,fec2,fec3,fec4,fec5,fec6,fec7,fec8,fec9,fec10,
     &fec11,fec12,fec13,fec14,fec15,fec16,fec17,fec18,fec19,fec20,act1
     &,act2,act3,act4,act5,act6,act7,act8,act9,act10,act11,act12,act13
     &,act14,act15,act16,act17,act18,act19,act20,fieldcap,wilting
      real for1,for2,for3,for4,for5,for6,for7,for8,for9,for10,for11,
     &for12,for13,for14,for15,for16,for17,for18,for19,for20,Vb
      real gutfill,contwet,shdgrass,grass

      INTEGER day,hour,iyear,nyear,countday,i,pregnant,soilmoisture,
     &viviparous,daycount,batch,photostart,photofinish,metamorph,
     &photodirs,photodirf,breeding,dead,frogbreed,frogstage,census
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I15,I21,I22,I66
      INTEGER julday,monthly,wingmod,wingcalc,aest,dehydrated
      integer DEB1,inwater,aquatic,feeding,ctmincum,ctminthresh,ctkill
      integer metab_mode,stages,deadead,startday,reset,wetmod,contonly
      integer contype,firstday,completion,complete,waiting,breedact
      integer breedactthres,breedtempcum,starttime,endtime,aestivate

      character*1 transt,dlenth

      DIMENSION V(24),ED(24),wetmass(24),E_H(24),eggsoil(24)
     &,wetstorage(24),wetfood(24),svl(24),Vold(24),Vpup(24),Epup(24),
     &E_Hpup(24),stage_rec(25)
      DIMENSION wetgonad(24),repro(24),surv(100)
     &    ,cumrepro(24),q(24),hs(24),ms(24),EH_baby1(24)
     &   ,cumbatch(24),food(50),v_baby1(24),e_baby1(24),surviv(24)
      DIMENSION depsel(25),tcores(25),acthr(25),actxbas(25)
      DIMENSION etaO(4,3),JM_JO(4,4),fec(100)
      DIMENSION MLO2(24),GH2OMET(24),debqmet(24),DRYFOOD(24),FAECES
     &    (24),NWASTE(24),rhref(25),pond_env(20,365,25,2)
      dimension rainfall2(7300),debfirst(13),ectoinput(127),
     &    wetlandTemps(7300*24),wetlandDepths(7300*24)
      dimension grassgrowth(7300),grasstsdm(7300),tbs(24*7300),SOIL1(25)
      DIMENSION SOIL3(25),Taloc(25),TREF(25),TIME(25),shdgrass(25)

      Data PI/3.14159/

c      COMMON/WDSUB1/ANDENS,ASILP,EMISSB,EMISSK,FLUID,G,IHOUR
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
      COMMON/DEBOUT/fecundity,clutches,monrepro,svlrepro,monmature
     &,minED,annfood,food,longev,completion,complete,fec1,fec2,
     &fec3,fec4,fec5,fec6,fec7,fec8,fec9,fec10,fec11,fec12,fec13,fec14,
     &fec15,fec16,fec17,fec18,fec19,fec20,act1,act2,act3,act4,act5,act6
     &,act7,act8,act9,act10,act11,act12,act13,act14,act15,act16,act17,
     &act18,act19,act20,fec,surv,for1,for2,for3,for4,for5,for6,for7
     &,for8,for9,for10,for11,for12,for13,for14,for15,for16,for17,for18,
     &for19,for20   
      common/gut/gutfull,gutfill
      COMMON/DEBINIT/v_init,E_init,ms_init,cumrepro_init,q_init,
     &hs_init,cumbatch_init,p_Mref,vdotref,h_aref,e_baby_init,
     &v_baby_init,EH_baby_init,k_Jref,s_G,surviv_init,halfsat,x_food,
     &Vold_init,Vpup_init,Epup_init,E_Hpup_init,p_Xmref
      COMMON/z/tknest,Thconw
      Common/Behav3/Acthr,ACTXBAS
      common/julday/julday,monthly
      COMMON/DEBPAR1/clutchsize,andens_deb,d_V,eggdryfrac,w_E,mu_E,
     &mu_V,w_V,e_egg,kappa_X,kappa_X_P,mu_X,mu_P,w_N,w_P,w_X,funct
      COMMON/DEBPAR2/zfact,kappa,E_G,k_R,delta_deb,E_H_start,breedact
     &,maxmass,e_init_baby,v_init_baby,E_H_init,E_Hb,E_Hp,E_Hj,batch
     &,MsM,lambda,breedrainthresh,daylengthstart,daylengthfinish
     &,photostart,photofinish,lengthday,photodirs,photodirf
     &,lengthdaydir,prevdaylength,lat,svl_met,frogbreed,frogstage
     &,metamorph,breedactthres
      COMMON/DEBPAR3/metab_mode,stages,p_Am1,p_AmIm
     &,disc,gam,E_Hmoult1,E_Hmet,E_Hecl,Vb
      COMMON/TPREFR/TMAXPR,TMINPR,TDIGPR,ACTLVL,AMTFUD,XBAS,TPREF,tbask
     &,temerge
      common/vivip/viviparous,pregnant
      common/debbaby/v_baby,e_baby,EH_baby,eggmass
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
      common/stage_r/stage_rec
      common/metdep/depress,aestivate,aest,dehydrated
      common/soilmoist/fieldcap,wilting,soilmoisture
      
      dead=0
      waiting=0
      if(daycount.eq.1494)then
      continue
      endif

      if(hour.eq.1)then
      complete=0
      endif

c    check if first day of simulation
      if((daycount.eq.1).and.(hour.eq.1))then
       firstday=1
      else
       firstday=0
      endif



      if((hour.eq.1).and.(daycount.eq.1))then
       orig_clutchsize=clutchsize
       orig_MsM=MsM
      else
       if(pregnant.eq.0)then
        clutchsize = orig_clutchsize
        MsM=orig_MsM
       endif
      endif
       clutchsize=FLOOR(0.1696*(SVL(hour)/10)-16.855)
c      clutch size below for sleepy lizards
      if(SVL(hour).lt.300)then
       clutchsize=1
      endif
      
      if((daycount.lt.startday).or.((countday.lt.startday).and.
     &    (v_init.le.3e-9)).or.(deadead.eq.1))then
       dead=1
      goto 987
      endif

c    if((countday.eq.census-1).and.(hour.eq.24))then
c     dead=1
c    goto 987
c    endif

      if((countday.eq.14).and.(hour.eq.23))then
      continue
      endif

c    check if start of a new day            
      if(hour.eq.1)then
       V_pres = v_init
       E_pres = E_init
       ms_pres = ms_init
       minED = E_pres
       E_H_pres = E_H_init
       q_pres = q_init
       hs_pres = hs_init
       surviv_pres = surviv_init
      else
       V_pres = V(hour-1)
       E_pres = Ed(hour-1)
       ms_pres = ms(hour-1)
       E_H_pres = E_H(hour-1)
       q_pres = q(hour-1)
       hs_pres = real(hs(hour-1),4)
       surviv_pres = surviv(hour-1)
      endif

      E_H_start = E_H_init

c    set body temperature
      Tb = Tc
c    Tb=15

      if((frogbreed.eq.1).or.(frogbreed.eq.2))then
       contdep=pond_depth
       if((stage.eq.1).or.((stage.eq.0).and.(frogbreed.eq.1)))then
        Tb=twater
       endif
      endif

c    if running a frog, check if terrestrial breeder and set Tb to soil temp
      if(E_H_pres.le.E_Hb)then
       if((frogbreed.eq.2).or.(frogbreed.eq.4))then
        Tb = Eggsoil(hour)
       endif
      endif

c    Arrhenius temperature correction factor
       Tcorr = EXP(T_A*(1/(273+T_ref)-1/(273+Tb)))/(1+EXP(TAL
     & *(1/(273+Tb)-1/TL))+EXP(TAH*(1/TH-1/(273+Tb))))

c    food availability - to do
c      grass=soil1(hour)
c      if(grass.lt.(wilting/2.+1))then
c          grass=0.
c      else
c          grass=grass/fieldcap*10.
c      endif
      
      X_food = grassgrowth(daycount)

c      if((grassgrowth(daycount).eq.0).and.(taloc(hour).ge.5))then
c       X_food=0.05
c      endif
c      if(pond_depth.eq.wilting/2)then
c      X_food=0
c      else
c      X_food=pond_depth/fieldcap*10.
c      X_food=10.
c      X_food=3265.
c      endif
      
      if(clutchsize.lt.1)then
       clutchsize=1
      endif
C      if(fecundity.eq.0)then
C      clutchenergy = E_egg*1
C      clutchsize=1
C     else
       clutchenergy = E_egg*clutchsize
C     endif
      if((cumbatch(hour)/clutchenergy).gt.1)then
      MsM=0
      else
      MsM=orig_MsM
      endif
      if(countday.eq.150)then
      continue
      endif

      if(pregnant.eq.1)then
        f=funct
       lambda=lambda-1./(24.*365.)
       if(lambda.lt.0.1)then
        lambda=0.1
       endif
      else
       f=funct
      endif
c    f = X/(halfsat+X)

c    if(viviparous.eq.1)then
c      if((cumbatch(hour)/clutchenergy).gt.1)then
c       MsM=MsM*0.5
c      else
c       MsM=MsM*(1-(cumbatch(hour)/clutchenergy)*.5)
c      endif
c      if(MsM.lt.0)then
c       MsM=0
c      endif
c    endif
      aest=0
c    if((aestivate.eq.1).and.(pond_depth.lt.0.1))then
C     if((aestivate.eq.1).and.(countday.gt.105).
C    &    and.(countday.lt.270))then
      if((aestivate.eq.1).and.(pond_depth.eq.0))then
      aest=1
c    depress=0.35
      continue
      endif

c    temperature corrections and compound parameters
      M_V = ANDENS_deb/w_V
      p_Mv = p_Mref*Tcorr
      k_Mdot = p_Mv/E_G
      k_J = k_Jref*Tcorr
      if(aest.eq.1)then
       p_Mv = p_Mv*depress
       k_Mdot = p_Mv/E_G
       k_J=k_Mdot
      endif
      p_Am = p_Mv*zfact/kappa
      vdot = vdotref*Tcorr
      E_M = p_AM/vdot
      p_Xm = p_Xmref*Tcorr
c    p_Xm = p_Am/kappa_X*1
      g = E_G/(kappa*E_M)
      E_scaled=E_pres/E_m
      V_max=(kappa*p_Am/p_Mv)**(3.)
      h_a = h_aref*Tcorr
      L_T = 0.
      L_pres = V_pres**(1./3.)
      L_max = V_max**(1./3.)
      scaled_l = L_pres/L_max
      kappa_G = (d_V*mu_V)/(w_V*E_G)
      yEX=kappa_X*mu_X/mu_E
      yXE=1/yEX
      yPX=kappa_X_P*mu_X/mu_P
      mu_AX=mu_E/yXE
      eta_PA=yPX/mu_AX

      if(countday.eq.98)then
      continue
      endif



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
     &      daylengthstart))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirs.eq.1)then
           breeding=1
          endif
       endif

       if((lengthday.le.daylengthstart).and.(prevdaylength.gt.
     &      daylengthstart))then
c      we have reached the critical daylength for breeding initiation and day length is decreasing
          if(photodirs.eq.0)then
           breeding=1
          endif
       endif
       if((lengthday.ge.daylengthfinish).and.(prevdaylength.lt.
     &      daylengthfinish))then
c      we have reached the critical daylength for breeding initiation and day length is increasing
          if(photodirf.eq.1)then
           breeding=0
          endif
       endif

       if((lengthday.le.daylengthfinish).and.(prevdaylength.gt.
     &      daylengthfinish))then
c      we have reached the critical daylength for breeding initiation and day length is decreasing
          if(photodirf.eq.0)then
           breeding=0
          endif
       endif
      endif

      if(breedact.lt.breedactthres)then
         breeding=0
      endif
C     if(breeding.eq.1)then
C         if(cumrepro

c    if(countday.lt.213)then
c    breeding=1
c    else
c    breeding=0
c    endif
c    lambda=0.3

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
         dead=1
         complete=1
        endif
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
      if(frogbreed.eq.2)then
       if(stage.eq.0)then
        if(E_H_pres.le.E_Hb*0.90)then
        if(contdep.gt.50)then
         dead=1
        endif
       endif
       endif
      endif

c    kill tadpoles if pond dries
      if(frogbreed.eq.2)then
       if(stage.eq.1)then
         if(contdep.le.0.1)then
      waiting=1
      E_H_pres=E_Hb
      stage=0
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
        if(E_H_pres.le.E_Hb)then
c       E_pres=E_egg/V_pres
         E_pres=E_init
        endif
       endif
c    checking to see if animal died recently and needs to start again as an embryo
       if((daycount.gt.1).and.(dead.eq.1))then
        if(E_H_pres.le.E_Hb)then
         E_pres=E_egg/debfirst(3)
        endif
       endif
      endif
c    clutch size below for Egernia species    
c    clutchsize=ANINT(0.023*(V_pres**(0.3333333333333)/delta_deb*10)
c     &    +0.35)
c    clutch size below for Pogona        
c    clutchsize=ANINT(0.316*(V_pres**(0.3333333333333)/delta_deb*10)
c     &    +3.712)

c        if(clutchsize.lt.10)then
c         clutchsize=10
c        endif
c        if(clutchsize.gt.30)then
c         clutchsize=30
c        endif
      if(E_H_pres.le.E_Hb)then
c     use embryo equation for length, from Kooijman 2009 eq. 2
       if(waiting.eq.1)then
        dLdt = 0
        V_temp=(V_pres**(1./3.)+dLdt)**3
        dVdt = 0
        rdot=0
       else
        dLdt=(vdot*E_scaled-k_Mdot*g*V_pres**(1./3.))/(3*(E_scaled+g))
        V_temp=(V_pres**(1./3.)+dLdt)**3
        dVdt = V_temp-V_pres
        rdot=vdot*(e_scaled/L_pres-(1+L_T/L_pres)/L_max)/
     &    (e_scaled+g)
       endif
      else
c    equation 2.21 from DEB3
       rdot=vdot*(e_scaled/L_pres-(1+L_T/L_pres)/L_max)/
     &    (e_scaled+g)
       dVdt = v_pres*rdot
       if(dVdt.lt.0)then
        dVdt=0
       endif
      endif

c      if(aest.eq.1)then
c      dVdt=0
c      rdot=0
c      endif

      if(hour.eq.1)then
       if(E_H_pres.le.E_Hb)then
c       use embryo equation for scaled reserve, U_E, from Kooijman 2009 eq. 1
         Sc = L_pres**2*(g*e_scaled)/(g+E_scaled)*
     &       (1+((k_Mdot*L_pres)/vdot))
         dUEdt = -1*Sc 
         E_temp=((E_pres*V_pres/p_Am)+dUEdt)*p_Am/(v_pres+dvdt)
         dEdt=E_temp-E_pres
       else
        if(ms_init.gt.0.0000001*MsM*V_pres)then
c        Equation 2.10 DEB3
          dEdt = (p_Am*f-E_pres*vdot)/L_pres
        else
          dEdt = (p_Am*0-E_pres*vdot)/L_pres
        endif
       endif
      else
       if(E_H_pres.le.E_Hb)then
c      use embryo equation for scaled reserve, U_E, from Kooijman 2009 eq. 1
        Sc = L_pres**2*(g*e_scaled)/(g+E_scaled)*
     &       (1+((k_Mdot*L_pres)/vdot))
        dUEdt = -1*Sc 
        E_temp=((E_pres*V_pres/p_Am)+dUEdt)*p_Am/(v_pres+dvdt)
        dEdt=E_temp-E_pres
       else
        if(ms(hour-1).gt.0.0000001*MsM*V_pres)then
         dEdt = (p_Am*f-E_pres*vdot)/L_pres
        else
         dEdt = (p_Am*0-E_pres*vdot)/L_pres
        endif
       endif
      endif

      p_M = p_Mv*V_pres
      p_J = k_J*E_H_pres

c      if(aest.eq.1)then
c       dEdt = (p_Mv+p_J/v_pres)*(-1.)
c      endif

c    diapause before pond fill
      if(frogbreed.eq.1)then
       if((E_H_pres.gt.E_Hb).and.(stage.lt.1))then
        if(contdep.le.0.1)then
         dVdt=0
        endif
       endif
      endif

c    powers
      if(hour.eq.1)then
       if(ms_init.gt.0.0000001*MsM*V_pres)then
        p_A = V_pres**(2./3.)*p_Am*f
       else
        p_A = 0
       endif
      else
       if(ms(hour-1).gt.0.0000001*MsM*V_pres)then
        p_A = V_pres**(2./3.)*p_Am*f
       else
        p_A = 0
       endif
      endif
c    J food eaten per hour
       p_X = p_A/kappa_X
c    tallying J food eaten per year
       food(iyear)=food(iyear)+p_X
c    tallying lifetime food eaten
      if(iyear.eq.nyear)then
       if(hour.eq.24)then
          do 1 i=1,nyear
           annfood=annfood+food(i)
1         continue
       endif
      endif



c    equation 2.20 DEB3
      p_C = (E_m*(vdot/L_pres+k_Mdot*(1+L_T/L_pres))*
     &    (e_scaled*g)/(e_scaled+g))*v_pres


      p_R = (1.-kappa)*p_C-p_J

c      if(aest.eq.1)then
c      p_R=0
c      endif

      if((E_H_pres.le.E_Hp).or.(pregnant.eq.1))then
       p_B = 0.
       else
        if(batch.eq.1)then
         batchprep=(k_R/lambda)*((1-kappa)*(E_m*(vdot*V_pres**(2./3.)+
     &      k_Mdot*V_pres)/(1+(1/g)))-p_J)
         if(breeding.eq.0)then
          p_B =0.
         else 
          if(hour.eq.1)then
c        if the repro buffer is lower than what p_B would be(see below), p_B is p_R
           if(cumrepro_init.lt.batchprep)then        
            p_B = p_R
           else
c          otherwise it is a faster rate, as specified in Pecquerie et. al JSR 2009 Anchovy paper, 
c         with lambda (the fraction of the year the animals breed if food/temperature not limiting) = 0.583 or 7 months of the year
            p_B = batchprep
           endif
         else
c       if the repro bufffer is lower than what p_B would be(see below), p_B is p_R
          if(cumrepro(hour-1).lt.batchprep)then        
           p_B = p_R
          else
c         otherwise it is a faster rate, as specified in Pecquerie et. al JSR 2009 Anchovy paper, 
c        with lambda (the fraction of the year the animals breed if food/temperature not limiting) = 0.583 or 7 months of the year
           p_B = batchprep
          endif
         endif
        endif
       else
        p_B=p_R
c     end check for whether batch mode is operating
       endif
c    end check for immature or mature
      endif

c      if(aest.eq.1)then
c      p_B=0
c      endif

c    maturity
      if(E_H_pres.lt.E_Hp)then
       if(E_H_pres.le.E_Hb)then
c       use embryo equation for scaled maturity, U_H, from Kooijman 2009 eq. 3
       if(waiting.eq.1)then
        U_H_pres=E_H_pres/p_Am    
        dUHdt=0
        dE_Hdt=dUHdt*p_Am
       else
        U_H_pres=E_H_pres/p_Am    
        dUHdt=(1-kappa)*Sc-k_J*U_H_pres
        dE_Hdt=dUHdt*p_Am
       endif
       else
        dE_Hdt = (1-kappa)*p_C-p_J
       endif
      else
       dE_Hdt = 0
      endif

c      if(aest.eq.1)then
c      dE_Hdt=0
c      endif

      if(E_H_pres.ge.E_Hp)then
       p_D = p_M+p_J+(1-k_R)*p_R
      else
       p_D = p_M+p_J+p_R
      endif

      p_G = p_C-p_M-p_J-p_R

c    if(frogbreed.eq.2)then
c     if((E_H_pres.ge.E_Hb).and.(stage.lt.1))then
c      if(contdep.le.0.1)then
c       dE_Hdt=0
c      endif
c     endif
c      endif

      if(hour.eq.1)then
        E_H(hour) = real(E_H_init,4) + real(dE_Hdt,4)
       else
        E_H(hour) = E_H(hour-1)+real(dE_Hdt,4)
      endif

c    aging
c    dqdt = 1*g*E_m*(dvdt+k_Mdot*V_pres)
      
      dqdt = (q_pres*(V_pres/V_max)*s_G+h_a)*(E_pres/E_m)*
     &((vdot/L_pres)-rdot)-rdot*q_pres

      if(E_H_pres.gt.E_Hb)then
       if(hour.eq.1)then
        q(hour) = real(q_init,4) + real(dqdt,4)
       else
        q(hour) = q(hour-1)+real(dqdt,4)
       endif
      else
        q(hour) = 0
      endif

c    dhsds = h_a*q(hour)/V_pres
      dhsds = q_pres-rdot*hs_pres

      if(E_H_pres.gt.E_Hb)then
       if(hour.eq.1)then
        hs(hour) = hs_init + dhsds
       else
        hs(hour) = hs(hour-1)+dhsds
       endif
      else
       hs(hour) = 0
      endif

      h_w = ((h_a*(E_pres/E_m)*vdot)/(6*V_pres**(1./3.)))**(1./3.)
c    surviv(hour) = EXP(-1*(h_w*((daycount-1)*24+hour))**3)
      dsurvdt = -1*surviv_pres*hs(hour)
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

c    average longevity in years
      if(longev.eq.0)then
c     if((Tb.lt.CTmin).or.(Tb.gt.CTmax))then
       if(ctkill.eq.1)then
        if((ctmincum.gt.ctminthresh).or.(Tb.gt.CTmax))then
         if(reset.gt.0)then
          dead=1
          if(ctmincum.gt.ctminthresh)then
           if(stage.gt.deathstage)then
            causedeath=1.
            deathstage=stage
            ctmincum=0
           endif
          else
           if(stage.gt.deathstage)then
            causedeath=2.
            deathstage=stage
           endif
          endif
         else
          census=countday
          deadead=1
          surv(iyear)=surviv(hour)
          surviv(hour)=0.49
          if(ctmincum.gt.ctminthresh)then
           if(stage.gt.deathstage)then
            causedeath=1.
            deathstage=stage
            ctmincum=0
           endif
          else
           if(stage.gt.deathstage)then
            causedeath=2.
            deathstage=stage
           endif
          endif
         endif
        endif
       endif
       if(surviv(hour).lt.0.5)then
        longev=(daycount+hour/24.)/365.
        nyear=iyear
        dead=1
        if(reset.eq.0)then
         surv(iyear)=surviv(hour)
         deadead=1
         surviv(hour)=0.49
        endif
           if(stage.gt.deathstage)then
            causedeath=5.
            deathstage=stage
           endif
c      longev=5
       endif
      endif

      if((e_pres.lt.E_m*0.1).and.(dead.eq.0))then
      dead=1
           if(stage.gt.deathstage)then
            causedeath=4.
            deathstage=stage
           endif
       if(reset.eq.0)then
        surviv(hour)=0.49
        longev=(daycount+hour/24.)/365.
        nyear=iyear
        census=countday
       endif
      endif

c     accumulate energy/matter in reproduction buffer
c    if it is the beginning of the day
      if(E_H_pres.gt.E_Hp)then
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
         cumrepro(hour) = cumrepro(hour-1)+p_R*k_R-p_B_past
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

      if(stage.eq.2)then
       if(cumbatch(hour).lt.0.1*clutchenergy)then
          stage=3
       endif
      endif

c    if(dvdt.lt.0)then
c     V(hour)=V_pres
c    else
      V(hour)=V_pres+real(dVdt,4)
c    endif
      
      if(V(hour).lt.0)then
       V(hour)=0
      endif

      ED(hour) = E_pres+real(dEdt,4)
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

      if(E_H(hour).le.E_Hb)then
       stage=0
      else
       if(E_H(hour).lt.E_Hj)then
        stage=1
       else
        if(E_H(hour).lt.E_Hp)then
         stage=2
        else
         stage=3
        endif
       endif
      endif
      
      if(cumbatch(hour).gt.0)then
       if(E_H(hour).gt.E_Hp)then
        stage=4
       else
        stage=stage
       endif
       if(monmature.eq.0)then
        monmature=(day+365*(iyear-1))/30.5
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

      
c    if(E_H_pres.ge.E_Hb)then
c       if(Depsel(hour).ge. 0)then
c      if((Tb .ge. tminpr) .and. (Tb .le. tmaxpr))then
c       if((aquatic.eq.1).and.(feeding.eq.0))then
c        dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
c     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
c       else
c        dMsdt = p_Xm*v_pres**(2./3.)*(X_food/(halfsat+X_food))-
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
c    else
c       dMsdt = p_Xm*v_pres**(2./3.)*0*(X_food/(halfsat+X_food))-
c     &       p_Xm*v_pres**(2./3.)*f*(Ms_pres/(MsM*v_pres))
c    endif
      gutfull=ms_pres/(MsM*v_pres)
c      write(0,*) gutfull
      if(gutfull.gt.1)then
      gutfull=1
      endif

      if(E_H_pres.gt.E_Hb)then
       if(acthr(hour).gt. 1)then
c       if((aquatic.eq.1).and.(feeding.eq.0))then
c        dMsdt = -1.*(p_Am/kappa_X)*v_pres**(2./3.)
c       else
c        dMsdt = p_Xm*30*v_pres**(2./3.)*f*(X_food/(halfsat+X_food))*
c     &   (1-(gutfull))-p_Xm*v_pres**(2./3.)
c     &    *f*gutfull
c        dMsdt = MsM*v_pres*f*(X_food/(halfsat+X_food))*
c     &   (1-(gutfull))-p_Xm*v_pres**(2./3.)
c     &    *f*gutfull
          dMsdt = p_Xm*((X_food/halfsat)/(1+X_food/halfsat))*v_pres**
     &        (2./3.)*funct-1.*(p_Am/kappa_X)*v_pres**(2./3.)
c         endif
        else
         dMsdt = -1.*(p_Am/kappa_X)*v_pres**(2./3.)
        endif
      else
         dMsdt = -1.*(p_Am/kappa_X)*v_pres**(2./3.)
      endif

      if(aest.eq.1)then
      dMsdt = -1.*(p_Am/kappa_X)*v_pres**(2./3.)
      endif

      if(v_pres.eq.0)then
      dMsdt=0
      endif

c    remove this - for testing only
c    if(E_H_pres.ge.E_Hb)then
c       dMsdt = p_Xm*v_pres**(2./3.)*(1-(Ms_pres/(MsM*v_pres)))
c    endif

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
      
      gutfull=ms(hour)/(MsM*v_pres)
      if(gutfull.gt.1)then
      gutfull=1
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

      JMCO2=JOJx*JM_JO(1,1)+JOJv*JM_JO(1,2)+JOJe*JM_JO(1
     &    ,3)+JOJp*JM_JO(1,4)
      JMH2O=JOJx*JM_JO(2,1)+JOJv*JM_JO(2,2)+JOJe*JM_JO(
     &    2,3)+JOJp*JM_JO(2,4)
      JMO2=JOJx*JM_JO(3,1)+JOJv*JM_JO(3,2)+JOJe*JM_JO
     &    (3,3)+JOJp*JM_JO(3,4)
      JMNWASTE=JOJx*JM_JO(4,1)+JOJv*JM_JO(4,2)+JOJe*
     &    JM_JO(4,3)+JOJp*JM_JO(4,4)

      JMCO2_GM=JOJx_GM*JM_JO(1,1)+JOJv_GM*JM_JO(1,2)+JO
     &    Je_GM*JM_JO(1,3)+JOJp_GM*JM_JO(1,4)
      JMH2O_GM=JOJx_GM*JM_JO(2,1)+JOJv_GM*JM_JO(2,2)+JO
     &    Je_GM*JM_JO(2,3)+JOJp_GM*JM_JO(2,4)
      JMO2_GM=JOJx_GM*JM_JO(3,1)+JOJv_GM*JM_JO(3,2)+JOJ
     &    e_GM*JM_JO(3,3)+JOJp_GM*JM_JO(3,4)
      JMNWASTE_GM=JOJx_GM*JM_JO(4,1)+JOJv_GM*JM_JO(4,2)+
     &        JOJe_GM*JM_JO(4,3)+JOJp_GM*JM_JO(4,4)

c    mlO2/h, temperature corrected (including SDA)

      if(DEB1.eq.1)then
       O2FLUX = -1*JMO2/(T_ref/Tb/24.4)*1000
      else
c     send the allometric value to the output file
       O2FLUX = 10.**(MR_3*TC)*MR_1*(AMASS*1000)**MR_2
      endif
      if (cumrepro(hour).ne.cumrepro(hour))then
      cumrepro(hour)=0.
      endif
      if (ED(hour).ne.ED(hour))then
      ED(hour)=0.
      v(hour)=0
      dead=1
      endif
      if (cumbatch(hour).ne.cumbatch(hour))then
      cumbatch(hour)=0.
      endif
      if (hs(hour).ne.hs(hour))then
      hs(hour)=0.
      endif
      CO2FLUX = JMCO2/(T_ref/Tb/24.4)*1000
c    mlO2/h, stp
c    MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000
      MLO2(hour) = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
c    g metabolic water/h
      GH2OMET(hour) = JMH2O*18.01528
c    metabolic heat production (Watts) - growth overhead plus dissipation power (maintenance, maturity maintenance, 
c    maturation/repro overheads) plus assimilation overheads - correct to 20 degrees so it can be temperature corrected
c    in MET.f for the new guessed Tb
      DEBQMET(hour) = ((1-kappa_G)*p_G+p_D+(p_X-p_A-p_A*mu_P*eta_PA))
     &    /3600/Tcorr

      DRYFOOD(hour)=-1*JOJx*w_X
      FAECES(hour)=JOJp*w_P
      NWASTE(hour)=JMNWASTE*w_N
      if(pregnant.eq.1)then
      wetgonad(hour) = ((cumrepro(hour)/mu_E)*w_E)/eggdryfrac 
     &+((((v_baby*e_baby)/mu_E)*w_E)/d_V + v_baby)*clutchsize
      else
      wetgonad(hour) = ((cumrepro(hour)/mu_E)*w_E)/eggdryfrac 
     &+((cumbatch(hour)/mu_E)*w_E)/eggdryfrac
      endif
      wetstorage(hour) = (((V(hour)*ED(hour))/mu_E)*w_E)/d_V
c    wetfood(hour) = ((ms(hour)/mu_E)*w_E)/d_V
c     conversion below is for clover based on Shine 1971
      wetfood(hour) = ms(hour)/21525.37/0.18
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
       if((viviparous.eq.1).and.(E_H(hour).le.E_Hb))then
c      make the mass, metabolic heat and O2 flux that of a fully grown individual to get the heat balance of 
c      a thermoregulating mother with full reserves
        amass=maxmass/1000
        
        p_M = p_Mv*V_max
        p_C = (E_m*(vdot/L_max+k_Mdot*(1+L_T/L_max))*
     &    (1*g)/(1+g))*v_max
        p_J = k_J*E_Hp
        p_R = (1.-kappa)*p_C-p_J
        p_D = p_M+p_J+(1-k_R)*p_R
        p_A = V_max**(2./3.)*p_Am*f
        p_X = p_A/kappa_X
        JOJx=p_A*etaO(1,1)+p_D*etaO(1,2)+p_G*etaO(1,3)
        JOJv=p_A*etaO(2,1)+p_D*etaO(2,2)+p_G*etaO(2,3)
        JOJe=p_A*etaO(3,1)+p_D*etaO(3,2)+p_G*etaO(3,3)
        JOJp=p_A*etaO(4,1)+p_D*etaO(4,2)+p_G*etaO(4,3)
        JMO2=JOJx*JM_JO(3,1)+JOJv*JM_JO(3,2)+JOJe*JM_JO(3,3)+
     &        JOJp*JM_JO(3,4)
c      MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000
      MLO2(hour) = (-1*JMO2*(0.082058*(Tb+273.15))/
     &    (0.082058*293.15))*24.06*1000
        DEBQMET(hour) = (p_D+(p_X-p_A-p_A*mu_P*eta_PA))
     &    /3600/Tcorr
       else
        amass=wetmass(hour)/1000.
       endif
c       if((reset.gt.0).and.(E_H_start.eq.0))then
c      make the mass, metabolic heat and O2 flux that of a fully grown individual to get the heat balance of 
c      a thermoregulating mother with full reserves
c      amass=maxmass/1000
c        p_M = p_Mv*V_max
c      p_C = (E_m*(vdot/L_max+k_Mdot*(1+L_T/L_max))*
c     &    (1*g)/(1+g))*v_max
c        p_J = k_J*E_Hp
c      p_R = (1.-kappa)*p_C-p_J
c      p_D = p_M+p_J+(1-k_R)*p_R
c      p_A = V_max**(2./3.)*p_Am*f
c      p_X = p_A/kappa_X
c      JOJx=p_A*etaO(1,1)+p_D*etaO(1,2)+p_G*etaO(1,3)
c      JOJv=p_A*etaO(2,1)+p_D*etaO(2,2)+p_G*etaO(2,3)
c      JOJe=p_A*etaO(3,1)+p_D*etaO(3,2)+p_G*etaO(3,3)
c      JOJp=p_A*etaO(4,1)+p_D*etaO(4,2)+p_G*etaO(4,3)
c      JMO2=JOJx*JM_JO(3,1)+JOJv*JM_JO(3,2)+JOJe*JM_JO(3,3)+
c     &        JOJp*JM_JO(3,4)
c      MLO2(hour) = -1*JMO2/(T_ref/Tb/24.4)*1000
c      DEBQMET(hour) = (p_D+(p_X-p_A-p_A*mu_P*eta_PA))
c     &    /3600
c      if(acthr(hour).ge.1)then
c       breedact=breedact+1
c      endif
c     else
c      AMASS=((((V_init*E_init)/mu_E)*w_E)/d_V + V_init)/1000
c      breedact=0
c     endif
      endif

101   continue
987   continue
      if(dead.eq.1)then
       ms_past=0
       p_B_past=0 
       svl(hour)=0
       wetgonad(hour)=0
       wetstorage(hour)=0
       wetfood(hour)=0
       wetmass(hour)=0
       e_h(hour)=0
       Ed(hour)=0
       V(hour)=0
       AMASS=((((V_init*E_init)/mu_E)*w_E)/d_V + V_init)/1000
      endif

      if(hour.eq.24)then
       if(complete.eq.1)then
        completion=completion+1
        complete=0
       endif
      endif

      if((dead.eq.0).and.(dvdt.gt.0))then
      dead=0
      endif
      stage_rec(hour)=stage
      RETURN   
      END
