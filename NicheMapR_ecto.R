ectotherm <- function(ecto) {
  # If the library hasn't been loaded yet, load it  
  if (!is.loaded('ectotherm')) {
    dyn.load('ectotherm.dll')
  }
  a <- .Fortran("ectotherm", 
    as.double(ecto$ectoinput), 
    as.double(ecto$metout), 
    as.double(ecto$shadmet), 
    as.double(ecto$soil), 
    as.double(ecto$shadsoil),
    as.double(ecto$soilmoist), 
    as.double(ecto$shadmoist), 
    as.double(ecto$soilpot), 
    as.double(ecto$shadpot), 
    as.double(ecto$humid), 
    as.double(ecto$shadhumid),    
    as.double(ecto$DEP), 
    as.double(ecto$RAINFALL),              
    as.double(ecto$debmod), 
    as.double(ecto$deblast), 
    as.double(ecto$grassgrowths),
    as.double(ecto$grasstsdms),
    as.double(ecto$wetlandTemps),
    as.double(ecto$wetlandDepths), 
    as.double(ecto$arrhenius),
    as.double(ecto$thermal_stages),
    as.double(ecto$behav_stages), 
    as.double(ecto$water_stages),
    as.double(ecto$MAXSHADES),
    environ=matrix(data = 0., nrow = 24*7300, ncol = 20), 
    enbal=matrix(data = 0., nrow = 24*7300, ncol = 14), 
    masbal=matrix(data = 0., nrow = 24*7300, ncol = 21),              
    debout=matrix(data = 0., nrow = 24*7300, ncol = 20), 
    yearout=matrix(data = 0., nrow = 1, ncol = 80),
    yearsout=matrix(data = 0., nrow = 20, ncol = 45))
  dyn.unload("ectotherm.dll")
  
  environ <- matrix(data = 0., nrow = 24*7300, ncol = 20)
  enbal <- matrix(data = 0., nrow = 24*7300, ncol = 14)
  masbal <- matrix(data = 0., nrow = 24*7300, ncol = 21)
  debout <- matrix(data = 0., nrow = 24*7300, ncol = 20)
  yearout <- matrix(data = 0., nrow = 1, ncol = 80)
  yearsout <- matrix(data = 0., nrow = 20, ncol = 45)
  
  storage.mode(environ)<-"double"
  storage.mode(enbal)<-"double"
  storage.mode(masbal)<-"double"
  storage.mode(debout)<-"double"
  storage.mode(yearout)<-"double"
  storage.mode(yearsout)<-"double"
  environ<-a$environ
  enbal<-a$enbal
  masbal<-a$masbal
  debout<-a$debout
  yearout<-a$yearout
  yearsout<-a$yearsout
  environ.names<-c("JULDAY","YEAR","DAY","TIME","TC","SHADE","ORIENT","DEP","ACT","TA","VEL","RELHUM","ZEN","CONDEP","WATERTEMP","DAYLENGTH","WINGANGLE","WINGTEMP","FLYING","FLYTIME")
  enbal.names<-c("JULDAY","YEAR","DAY","TIME","TC","QSOL","QIRIN","QMET","QEVAP","QIROUT","QCONV","QCOND","ENB","NTRY")
  masbal.names<-c("JULDAY","YEAR","DAY","TIME","TC","O2_ml","CO2_ml","NWASTE_g","H2OFree_g","H2OMet_g","DryFood_g","WetFood_g","DryFaeces_g","WetFaeces_G","Urine_g","H2OResp_g","H2OCut_g","H2OEvap_g","H2OBal_g","H2OCumBal_g","GutFreeMass_g")
  debout.names<-c("JULDAY","YEAR","DAY","TIME","WETMASS","RESERVE_DENS","CUMREPRO","HS","MASS_GUT","SVL","V","E_H","CUMBATCH","V_baby","E_baby","Pregnant","Stage","WETMASS_STD","Body_cond","Surviv_Prob")
  yearout.names<-c("DEVTIME","BIRTHDAY","BIRTHMASS","MONMATURE","MONREPRO","SVLREPRO","FECUNDITY","CLUTCHES","ANNUALACT","MINRESERVE","LASTFOOD","TOTFOOD","FEC1","FEC2","FEC3","FEC4","FEC5","FEC6","FEC7","FEC8","FEC9","FEC10","FEC11","FEC12","FEC13","FEC14","FEC15","FEC16","FEC17","FEC18","FEC19","FEC20","ACT1","ACT2","ACT3","ACT4","ACT5","ACT6","ACT7","ACT8","ACT9","ACT10","ACT11","ACT12","ACT13","ACT14","ACT15","ACT16","ACT17","ACT18","ACT19","ACT20","SUR1","SUR2","SUR3","SUR4","SUR5","SUR6","SUR7","SUR8","SUR9","SUR10","SUR11","SUR12","SUR13","SUR14","SUR15","SUR16","SUR17","SUR18","SUR19","SUR20","MINTB","MAXTB","Pct_Dess","LifeSpan","GenTime","R0","rmax","SVL")
  yearsout.names<-c("YEAR","MaxStg","MaxWgt","MaxLen","Tmax","Tmin","MinRes","MaxDess","MinShade","MaxShade","MinDep","MaxDep","Bsk","Forage","Dist","Food","Drink","NWaste","Feces","O2","Clutch","Fec","CauseDeath","tLay","tEgg","tStg1","tStg2","tStg3","tStg4","tStg5","tStg6","tStg7","tStg8","mStg1","mStg2","mStg3","mStg4","mStg5","mStg6","mStg7","mStg8","surviv","ovip_surviv","fitness","deathstage")
  
  colnames(environ)<-environ.names
  colnames(enbal)<-enbal.names
  colnames(masbal)<-masbal.names
  colnames(debout)<-debout.names
  colnames(yearout)<-yearout.names
  colnames(yearsout)<-yearsout.names
  
  return (list(environ=environ, enbal=enbal, masbal=masbal, debout=debout, yearout=yearout, yearsout=yearsout))
}