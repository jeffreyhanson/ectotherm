NicheMapR <- function(niche) {
  #setwd('c:/')
  cap<-1
  dates<-Sys.time()-60*60*24
  curyear<-as.numeric(format(dates,"%Y"))
  unlist(niche)
  write_input<-0
  input_dir_micro<-"/NicheMapR/microclimate"
  input_dir_ecto<-"/NicheMapR/ectotherm"     
  #snowdens<-1.#5.  # factor by which initial snow density is multiplied
  #snowmelt<-2.5 #  # cm of snow that melts per hour that
  rungads<-1
  vlsci<-1
  if(vlsci==1){
    ndays<-365*nyears
  }
  vlsci_setup<-0
  if(timeinterval==365){
  monthly<-0
  }else{
    monthly<-2
  }
  REFL<-rep(REFL,timeinterval) # soil reflectances
  SoilMoist<-rep(SoilMoist,timeinterval)
  Density<-Density/1000 # density of minerals - convert to Mg/m3
  BulkDensity<-BulkDensity/1000 # density of minerals - convert to Mg/m3
  data_out<-0
  grasshade<-0
  grassfact<-1.
  grasstest<-1
  rainwet<-1.5 # mm rain that causes soil to become 90% wet
  pctwet_mult<-0#0.01 # factor by which uppper soil wetness is multiplied to get surface %wet for evaporative cooling
  if(soildata==0){
    soilprop<-cbind(0,0)
  }

  adiab_cor<-1
  if(dbase=='AWAP' | dbase=='bom'){
    microyear<-1
  }else{
    microyear<-0
  }
  
  
  ################## loading packages ###################################
  if(require("zoo")){
    print("zoo is loaded correctly")
  } else {
    print("trying to install zoo")
    install.packages("zoo")
    if(require(zoo)){
      print("zoo installed and loaded")
    } else {
      stop("could not install zoo")
    }
  }
  if(require("raster")){
    print("raster is loaded correctly")
  } else {
    print("trying to raster zoo")
    install.packages("raster")
    if(require(raster)){
      print("raster installed and loaded")
    } else {
      stop("could not install raster")
    }
  }
  if(require("ncdf")){
    print("ncdf is loaded correctly")
  } else {
    print("trying to install ncdf")
    install.packages("ncdf")
    if(require(ncdf)){
      print("ncdf installed and loaded")
    } else {
      stop("could not install ncdf")
    }
  }
  # if(require("rgdal")){
  #   print("rgdal is loaded correctly")
  # } else {
  #   print("trying to install rgdal")
  #   install.packages("rgdal")
  #   if(require(rgdal)){
  #     print("rgdal installed and loaded")
  #   } else {
  #     stop("could not install rgdal")
  #   }
  # }
  if(require("XML")){
    print("XML is loaded correctly")
  } else {
    print("trying to install XML")
    install.packages("XML")
    if(require(XML)){
      print("XML installed and loaded")
    } else {
      stop("could not install XML")
    }
  }
  
  if(require("dismo")){
    print("dismo is loaded correctly")
  } else {
    print("trying to install dismo")
    install.packages("dismo")
    if(require(dismo)){
      print("dismo installed and loaded")
    } else {
      stop("could not install dismo")
    }
  }
#   if(require("RMySQL")){
#     print("RMySQL is loaded correctly")
#   } else {
#     print("trying to install RMySQL")
#     install.packages("RMySQL")
#     if(require(RMySQL)){
#       print("RMySQL installed and loaded")
#     } else {
#       stop("could not install RMySQL")
#     }
#   }
  if(vlsci==0){  
  if(require("RODBC")){
    print("RODBC is loaded correctly")
  } else {
    print("trying to install RODBC")
    install.packages("RODBC")
    if(require(RODBC)){
      print("RODBC installed and loaded")
    } else {
      stop("could not install RODBC")
    }
  }
  }
  if(require("chron")){
    print("chron is loaded correctly")
  } else {
    print("trying to install chron")
    install.packages("chron")
    if(require(chron)){
      print("chron installed and loaded")
    } else {
      stop("could not install chron")
    }
  }
  ################## end loading packages ###################################
  
  #metchamber<-0 # only applies for ectotherm model
  tester<-0 # for just doing one hour (tester<-1)
  
  juldays12<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.)
  juldaysn<-juldays12
  if(nyears>1){ # create sequence of days for splining across multiple years
    for(i in 1:(nyears-1)){
      juldaysn<-c(juldaysn,(juldays12+365*i))
    }
  }
  if(timeinterval!=365){
    census<-juldaysn[length(juldaysn)]
    startday<-juldaysn[1]
  }
  ######################### begin ectotherm model setup ###############################
  if(ectomodel==1){
    # these shape definitions are to provide a slightly different solution to the energy
    # balance if tolerance isn't met for the lizard or the frog
    if(lometry==2){
      shape_a<-1.
      shape_b<-1.
      shape_c<-2.
    }
    if(lometry==3){
      shape_a<-1.
      shape_b<-1.
      shape_c<-4.
    }
    if(lometry==4){
      shape_a<-1.
      shape_b<-1.
      shape_c<-0.5
    }
    
    #turn on container model if aquatic egg/larval phase
    if(frogbreed==1 | frogbreed==2){
      container<-1
    }
    if(frogbreed==3){
      container<-0
    }
    
    # container/pond initial conditons
    contlast<-0.
    templast<-7.
    
    #if(container==0){
    #conth<-0. # cylindrical container/pond height
    #contw<-0. # cylindrical container/pond diameter
    #rainmult<-1 # rainfall multiplier to reflect catchment
    #}
    
    # check if user wants to simulate constant environment (i.e. metabolic chamber)
    if(metchamber==1){
      maxshade<-100
      minshade<-99.999
      burrow<-0 # shelter in burrow allowed (1) or not (0)?
      CkGrShad<-0 # shade seeking allowed (1) or not (0)?
      dayact<-1 # diurnal activity allowed (1) or not (0)?
      nocturn<-0 # nocturnal activity allowed (1) or not (0)?
      crepus<-0 # crepuscular activity allowed (1) or not (0)?
      Usrhyt <- 200 # local height (cm) at which animal/container calculations will be made ]
      DELTAR <- 0.
      TPREF<-chambertemp # preferred body temperature (animal will attempt to regulate as close to this value as possible)
    }
  } #end check for ectotherm model running
  ######################### end ectotherm model setup ##################################
  
  
  ######################### begin database and general I/O setup #########################
  
  iyear<-0 #initializing year counter
  countday<-1 #initializing day counter
  numsites<-1 #initializing the number of sites
  times<-c(0,60,120,180,240,300,360,420,480,540,600,660,720,780,840,900,960,1020,1080,1140,1200,1260,1320,1380,1440)
  lat5s<-seq(-90,90,5) #lat range for GADS
  lon5s<-seq(-180,175,5) #long range for GADS
  
  if(dbase=='global'){ #make sure soil is turned off if global is selected
    soildata<-0
  }
  
  if(timeinterval<365){
    microdaily<-0 # run microclimate model as normal, where each day is iterated 3 times starting with the initial condition of uniform soil temp at mean monthly temperature
  }else{
    microdaily<-1 # run microclimate model where one iteration of each day occurs and last day gives initial conditions for present day
  }
  
  password <- 'password' # MySQL database password
  
  # check if using the bom database and, if so, get the data
  if(dbase=='bom' & sitemethod<3){
    weather_gen=0
    channel <- odbcConnect("BOM",uid = "student", pwd = "student")
    # go through each sitemethod
    if(sitemethod<=1){ # look up nearest station to stated locality
      if(sitemethod==1){
        longlat <- as.numeric(geocode(loc)[1, 3:4]) # assumes first geocode match is correct
      }else{
        longlat <- longlat
      } 
      cat(sprintf('Address resolves to %s E, %s S.\n', longlat[1],abs(longlat[2])))
      query<-paste("SELECT a.* FROM [BOM].[dbo].[All_Stations] as a" 
                   ,sep="")
      stations<- as.data.frame(sqlQuery(channel,query))
      distance <- sqrt((longlat[1]-stations[, 'Longitude'])^2+(longlat[2]-stations[, 'Latitude'])^2)
      stations<-cbind(stations,distance)
      cat('Finding nearest weather station with data', '\n')
      stations<-stations[order(distance),]
      stationID<-stations[1,'Station']
      query<-paste("SELECT a.latitude, a.longitude, a.Station Name, a.State, b.Year, b.Month, b.Day, b.rainfall, b.Tmax, b.Tmin, b.Relhum00hrs, b.Relhum03hrs, b.Relhum06hrs, b.Relhum09hrs, b.Relhum12hrs, b.Relhum15hrs, b.Relhum18hrs, b.Relhum21hrs, b.Windspeed00hrs, b.Windspeed03hrs, b.Windspeed06hrs, b.Windspeed09hrs, b.Windspeed12hrs, b.Windspeed15hrs, b.Windspeed18hrs, b.Windspeed21hrs,b.Cloud00hrs, b.Cloud03hrs, b.Cloud06hrs, b.Cloud09hrs, b.Cloud12hrs, b.Cloud15hrs, b.Cloud18hrs, b.Cloud21hrs
    FROM [BOM].[dbo].[All_Stations] as a
                   , [BOM].[dbo].[All] as b where (a.Station = b.Station) and (a.Station=",stationID,") and (b.Month * b.Day != 58)" 
    ,sep="")
      dayclim<- sqlQuery(channel,query)
      dayclim$State<-trim(as.character(dayclim$State))
      
      # get years to run
      dayclim<-subset(dayclim,dayclim$Year>=ystart&dayclim$Year<=ystart+nyears-1)
    ndays<-nrow(dayclim)
      # date-formatted column
      date<-as.Date(ISOdate(dayclim$Year,dayclim$Month,dayclim$Day))
    }  
    
    if(sitemethod==2){  # look specified station
      query<-paste("SELECT a.latitude, a.longitude, a.Station Name, a.State, b.Year, b.Month, b.Day, b.rainfall, b.Tmax, b.Tmin, b.Relhum00hrs, b.Relhum03hrs, b.Relhum06hrs, b.Relhum09hrs, b.Relhum12hrs, b.Relhum15hrs, b.Relhum18hrs, b.Relhum21hrs, b.Windspeed00hrs, b.Windspeed03hrs, b.Windspeed06hrs, b.Windspeed09hrs, b.Windspeed12hrs, b.Windspeed15hrs, b.Windspeed18hrs, b.Windspeed21hrs,b.Cloud00hrs, b.Cloud03hrs, b.Cloud06hrs, b.Cloud09hrs, b.Cloud12hrs, b.Cloud15hrs, b.Cloud18hrs, b.Cloud21hrs
  FROM [BOM].[dbo].[All_Stations] as a
                   , [BOM].[dbo].[All] as b where (a.Station = b.Station) and (a.Station=",bomStation,")" 
  ,sep="")
      dayclim<- sqlQuery(channel,query)
      dayclim$State<-trim(as.character(dayclim$State))
      longlat<-c(dayclim$longitude[1],dayclim$latitude[1])
      
      # get years to run
      dayclim<-subset(dayclim,dayclim$Year>=ystart&dayclim$Year<=ystart+nyears-1)
      ndays<-nrow(dayclim)
      # date-formatted column
      date<-as.Date(ISOdate(dayclim$Year,dayclim$Month,dayclim$Day))
    }  
    # add julian days to data  
    jday<-matrix(nrow=length(date),data=0)
    for(i in 1: length(date)){ 
      jday[i,]<-julian(dayclim$Month[i],dayclim$Day[i],dayclim$Year[i], origin=c(month=1,day=0,year=dayclim$Year[i]))  
    }
    dayclim<-as.data.frame(cbind(date,jday,dayclim))
  }
  
    if(dbase=='bom'){
      dayclim<-dayclim[order(dayclim$Year,dayclim$Month,dayclim$Day),]
    }
    
    if(sitemethod==1 & dbase!='bom'){
      longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
    }
  if(vlsci==1){
    #load(paste(barcoo,'longlat.bin',sep=''))
    #longlat <- data[jobnum,1:2]
  }  
    x <- rbind(longlat)
    
    # creating the shade array if not using gridded data on shade but rather the user-input max shade
    if(dbase=="bom"){
      maxshades <- rep(maxshade,length(dayclim$date))
      maxshades <-rep(maxshades,length(x[,1]))
    }else{
      if(timeinterval != 365){
        maxshades <- rep(maxshade,timeinterval)
        maxshades <-rep(maxshades,length(x[,1]))
      }else{
        if(soildata==0){
          maxshades <- rep(maxshade,365*nyears)
          maxshades <- rep(maxshades,length(x[,1]))
        }  
      } 
    }
    
    if(aussiegrass==0){
      grassgrowths<-rep(X,timeinterval*nyears)
      grasstsdms<-rep(X,timeinterval*nyears)
    }
    
  # now extract terrain and soil data from grids
  if(dbase=='ausclim'|| dbase=='bom' || dbase=='AWAP'){
    if(vlsci==0){
    f1 <- "/NicheMapR_Working/Spatial_Data/ausclim_rowids.nc";
    f2 <- "/NicheMapR_Working/Spatial_Data/ausdem_full.nc";
    f3 <- "/NicheMapR_Working/Spatial_Data/agg_9secdem.nc";
    f4 <- "/NicheMapR_Working/Spatial_Data/Aust9secDEM.tif";
    }else{
    f1 <- "/vlsci/VR0212/shared/Spatial_Data/ausclim_rowids.nc";
    f2 <- "/vlsci/VR0212/shared/Spatial_Data/ausdem_full.nc";
    f3 <- "/vlsci/VR0212/shared/Spatial_Data/agg_9secdem.nc";
    f4 <- "/vlsci/VR0212/shared/Spatial_Data/Aust9secDEM.grd";
    }
    if(soildata==1){
      cat("extracting soil data", '\n')  
      if(vlsci==0){
       static_soil<-'/NicheMapR_Working/Spatial_Data/static_soil.nc'
       emissivities<-'/NicheMapR_Working/Spatial_Data/aus_emissivities.nc'
      }else{
       static_soil<-'/vlsci/VR0212/shared/Spatial_Data/static_soil.nc'
       emissivities<-'/vlsci/VR0212/shared/Spatial_Data/aus_emissivities.nc'
      }
      # read data in from netcdf file
      static_soil_data<-brick(static_soil) 
      static_soil_vars <- extract(static_soil_data,x)
      labels<-c('albedo','FAPAR1','FAPAR2','FAPAR3','FAPAR4','FAPAR5','FAPAR6','FAPAR7','FAPAR8','FAPAR9','FAPAR10','FAPAR11','FAPAR12','volwater_Upper','volwater_lower','thick_upper','thick_lower','code')
      colnames(static_soil_vars)<-labels  
      emissivities_data<-brick(emissivities) 
      SLES2 <- extract(emissivities_data,x)
            
      # read in other soil related files for working out lumped soil type and properties
      # such as clay % for getting water potential
      if(vlsci==0){
      filename<-'/NicheMapR_Working/Spatial_Data/ppfInterpAll.txt'
      ppf<-as.data.frame(read.table(file = filename, sep = ",", header=TRUE))
      filename<-'/NicheMapR_Working/Spatial_Data/Lumped soil types.txt'
      lumped.soil<-as.data.frame(read.table(file = filename, sep = ","))
      filename<-'/NicheMapR_Working/Spatial_Data/SoilTypeLUT_725_AWAP.csv'
      soiltype<-as.data.frame(read.table(file = filename, sep = ","))
      }else{
      filename<-'/vlsci/VR0212/shared/Spatial_Data/ppfInterpAll.txt'
      ppf<-as.data.frame(read.table(file = filename, sep = ",", header=TRUE))
      filename<-'/vlsci/VR0212/shared/Spatial_Data/Lumped soil types.txt'
      lumped.soil<-as.data.frame(read.table(file = filename, sep = ","))
      filename<-'/vlsci/VR0212/shared/Spatial_Data/SoilTypeLUT_725_AWAP.csv'
      soiltype<-as.data.frame(read.table(file = filename, sep = ","))
      }
        soilcode<-subset(soiltype, soiltype[1]==static_soil_vars[18])
        lumped<-subset(lumped.soil, V4==as.character(soilcode[1,2]))
        soiltype<-lumped[1,6]
        soilprop<-subset(ppf, ppf==soilcode[1,2]) 
    }else{
      REFL<-rep(0.15,length(x[,1]))
      SLES2 <- rep(SLE,timeinterval*nyears)
      if(manualshade==0){
      cat("extracting shade data", '\n')  
      if(vlsci==0){
        static_soil<-'/NicheMapR_Working/Spatial_Data/static_soil.nc'
        emissivities<-'/NicheMapR_Working/Spatial_Data/aus_emissivities.nc'
      }else{
        static_soil<-'/vlsci/VR0212/shared/Spatial_Data/static_soil.nc'
        emissivities<-'/vlsci/VR0212/shared/Spatial_Data/aus_emissivities.nc'
      }
      # read data in from netcdf file
      static_soil_data<-brick(static_soil) 
      static_soil_vars <- extract(static_soil_data,x)
      labels<-c('albedo','FAPAR1','FAPAR2','FAPAR3','FAPAR4','FAPAR5','FAPAR6','FAPAR7','FAPAR8','FAPAR9','FAPAR10','FAPAR11','FAPAR12','volwater_Upper','volwater_lower','thick_upper','thick_lower','code')
      colnames(static_soil_vars)<-labels 
      }
    }
    if(terrain==1){
          if(vlsci==0){
          #horizons<-'c:/NicheMapR_Working/Spatial_Data/horizons.nc'
          #elevslpasp<-'c:/NicheMapR_Working/Spatial_Data/elevslpasp.nc'
        }else{
          horizons<-'/vlsci/VR0212/shared/Spatial_Data/horizons.nc'
          elevslpasp<-'/vlsci/VR0212/shared/Spatial_Data/elevslpasp.nc'
        }
        cat("extracting terrain data")
        if(vlsci==0){
          path<-'c:/NicheMapR_Working/Spatial_Data/'
          for(i in 1:24){
            horifile<-paste(path,'horizon',i,'.tif',sep="")
            horiz<-raster(horifile)
            if(i==1){
              horizons_data<-horiz
            }else{
              horizons_data<-stack(horizons_data,raster(horifile))
            }
          }
          HORIZONS <- t(extract(horizons_data,x))
          elevslpasp<-stack(paste(path,'elev.tif',sep=""),paste(path,'slope.tif',sep=""),paste(path,'aspect.tif',sep=""))
          ELEVSLPASP <- extract(elevslpasp,x)
          ELEVSLPASP<-as.matrix((ifelse(is.na(ELEVSLPASP),0,ELEVSLPASP)))
          ALTITUDES <- ELEVSLPASP[,1]
          SLOPES <- ELEVSLPASP[,2]
          AZMUTHS <- ELEVSLPASP[,3]
          HORIZONS <- (ifelse(is.na(HORIZONS),0,HORIZONS))/10 # get rid of na and get back to floating point
          HORIZONS <- data.frame(HORIZONS)
          VIEWF_all <- 1-rowSums(sin(t(HORIZONS)*pi/180))/length(t(HORIZONS)) # convert horizon angles to radians and calc view factor(s)
          VIEWF_all
        }else{
  path<-'/vlsci/VR0212/shared/Spatial_Data/'
          for(i in 1:24){
            horifile<-paste(path,'horizon',i,'.tif',sep="")
            horiz<-raster(horifile)
            if(i==1){
              horizons_data<-horiz
            }else{
              horizons_data<-stack(horizons_data,raster(horifile))
            }
          }
          HORIZONS <- t(extract(horizons_data,x))
          elevslpasp<-stack(paste(path,'elev.tif',sep=""),paste(path,'slope.tif',sep=""),paste(path,'aspect.tif',sep=""))
          ELEVSLPASP <- extract(elevslpasp,x)
          ELEVSLPASP<-as.matrix((ifelse(is.na(ELEVSLPASP),0,ELEVSLPASP)))
          ALTITUDES <- ELEVSLPASP[,1]
          SLOPES <- ELEVSLPASP[,2]
          AZMUTHS <- ELEVSLPASP[,3]
          HORIZONS <- (ifelse(is.na(HORIZONS),0,HORIZONS))/10 # get rid of na and get back to floating point
          HORIZONS <- data.frame(HORIZONS)
          VIEWF_all <- 1-rowSums(sin(t(HORIZONS)*pi/180))/length(t(HORIZONS)) # convert horizon angles to radians and calc view factor(s)
        #write.table(cbind(x,ELEVSLPASP,VIEWF_all),paste('/vlsci/VR0212/shared/NicheMapR_Working/projects/snow/elevspasp_',quadrangle,'.csv',sep=''), append=TRUE, col.names=FALSE,sep=",")
        #write.table(cbind(x,ELEVSLPASP,VIEWF_all),paste('/scratch/VR0212/snow/bioreg6_hi/elevspasp_',quadrangle,'.csv',sep=''), append=TRUE, col.names=FALSE,sep=",")

        }


        # the horizons have been arranged so that they go from 0 degrees azimuth (north) clockwise - r.horizon starts
        # in the east and goes counter clockwise!
        

      r1 <- raster(f1)
      r2 <- raster(f2)
      r3 <- raster(f3)
      dbrow <- extract(r1, x)
      AUSDEM <- extract(r2, x)
      AGG <- extract(r3, x)
    }else{
      r1 <- raster(f1)
      r2 <- raster(f2)
      r3 <- raster(f3)
      r4 <- raster(f4)
      dbrow <- extract(r1, x)
      AUSDEM <- extract(r2, x)
      AGG <- extract(r3, x)
      ALTITUDES <- extract(r4, x)
      #ALTITUDES <- AUSDEM
      #cat("using 0.05 res DEM!")
      HORIZONS <- rep(0,24)
      HORIZONS <- rep(HORIZONS,length(x[,1]))
      HORIZONS <- data.frame(HORIZONS)
      VIEWF_all <- rep(1,length(x[,1]))
      SLOPES<-rep(slope,length(x[,1]))
      AZMUTHS<-rep(aspect,length(x[,1]))
    } 
    
    # setting up for temperature correction using lapse rate given difference between 9sec DEM value and 0.05 deg value
      if(AUSDEM==-9999 | is.na(AUSDEM)=='TRUE'){
        delta_elev = AGG - ALTITUDES
      }else{
        delta_elev = AUSDEM - ALTITUDES
      }
      adiab_corr = delta_elev * 0.0058 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_max = delta_elev * 0.0077 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
    adiab_corr_min = delta_elev * 0.0039 # Adiabatic temperature correction for elevation (C), mean for Australian Alps
  } #end of check if ausclim or bom for terrain data extraction
  
  if(dbase=='global'){
    #  adiab_corr<-rep(0.,numsites) # not using fine scale DEM as yet with global database
    #  filename1 <- "/NicheMapR_Working/id_rows.txt" 
    #  filename2 <- "/NicheMapR_Working/alt.txt"
    #  r1 <- raster(filename1)
    #  r2 <- raster(filename2)
    #  dbrow <- extract(r1, x)
    #  ALTITUDES <- extract(r2, x)
    #  SLOPES<- rep(slope,length(x[,1]))
    #  AZMUTHS<- rep(aspect,length(x[,1]))
    #  HORIZONS <- rep(0,24)
    #  HORIZONS <- rep(HORIZONS,length(x[,1]))
    #  HORIZONS <- data.frame(HORIZONS)
    #  password <- 'password'
    if(vlsci==0){
     f1 <- "/NicheMapR_Working/Spatial_Data/elev.nc"
     f2 <- "/NicheMapR_Working/Spatial_Data/global_climate.nc"
     f3<- '/NicheMapR_Working/Spatial_Data/soilw.mon.ltm.v2.nc'
    }else{
     f1 <- "/vlsci/VR0212/shared/NicheMapR_Working/NicheMapR_global/elev.nc"
     f2 <- "/vlsci/VR0212/shared/NicheMapR_Working/NicheMapR_global/global_climate.nc"
     f3<- '/vlsci/VR0212/shared/NicheMapR_Working/NicheMapR_global/soilw.mon.ltm.v2.nc'
    }
    elev<-raster(f1)
    ALTITUDES<-extract(elev,x)*1000
    #delta_elev<-ALTITUDES-elevation # SCAN test comment this out
    #adiab_corr = delta_elev * 0.0055 # SCAN test comment this out
    #ALTITUDES<-elevation # SCAN test comment this out
    global_climate<-brick(f2)
    soilmoisture<-brick(f3)
    cat("extracting climate data", '\n')
    CLIMATE <- extract(global_climate,x)
    longlat1<-longlat
    if(longlat1[1]<0){
      longlat1[1]<-360+longlat1[1]
    }
    xx<-t(cbind(longlat1))
    soilm<-extract(soilmoisture,xx)/1000
    rainfall <- CLIMATE[,1:12]
    maxwinds <- CLIMATE[,13:24]
    mintemps <- CLIMATE[,25:36]
    maxtemps <- CLIMATE[,37:48]
    clouds <- 100-CLIMATE[,49:60]
    rainydays <- CLIMATE[,61:72]
    minhumidities <- CLIMATE[,73:84]
    maxhumidities <- CLIMATE[,85:96]
    RAINFALL2<-rainfall
    
    
    adiab_corr<-rep(0.,numsites)
    dbrow <- as.matrix(rep(1,length(x)))
    SLOPES<- rep(slope,length(x[,1]))
    AZMUTHS<- rep(aspect,length(x[,1]))
    HORIZONS <- rep(0,24)
    HORIZONS <- rep(HORIZONS,length(x[,1]))
    HORIZONS <- data.frame(HORIZONS)
    VIEWF_all <- rep(1,length(x[,1]))
    VIEWF_all <- data.frame(VIEWF_all)
    
  }
  
  if(dbase=='bom'){
    mon<-c(1.)
    daymon<-c(1.)
    juldays<-dayclim$jday 
    microdaily<-1
  }else{
    mon<-c(1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.)
    daymon<-c(31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31.)
    daystart<-as.integer(ceiling(365/timeinterval/2))
    if(timeinterval!=12){
    juldays<-seq(daystart,365,as.integer(ceiling(365/timeinterval)))
    }else{
    juldays<-juldaysn
    }
  }
  
  if(vlsci==0){
  # connect to database
  #drv = dbDriver("MySQL")
  if(dbase=='bom' || dbase=='AWAP'){
#    con = dbConnect(drv,host="localhost",dbname='ausclim',user="root",pass=password)
    channel2 <- odbcConnect("ausclim_predecol",uid = "student", pwd = "student")
  }else{
#    con = dbConnect(drv,host="localhost",dbname=dbase,user="root",pass=password)
  }
  
  if(dbase=='AWAP'){
    channel <- odbcConnect("AWAPDaily",uid = "student", pwd = "student")
    
  # preliminary test for incomplete year, if simulation includes the present year  
    yearlist<-seq(ystart,(ystart+(nyears-1)),1)
    for(j in 1:nyears){ # start loop through years
      yeartodo<-yearlist[j]
      lat1<-x[2]-0.024
      lat2<-x[2]+0.025
      lon1<-x[1]-0.024
      lon2<-x[1]+0.025 
      query<-paste("SELECT a.latitude, a.longitude, b.*
                   FROM [AWAPDaily].[dbo].[latlon] as a
                   , [AWAPDaily].[dbo].[",yeartodo,"] as b
                   where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                   order by b.day",sep="")
     output<- sqlQuery(channel,query)
              output$sol<-as.numeric(as.character(output$sol))
              #if(yeartodo==2012){
              #  output<-output[-170,]
              #}
              if(nrow(output)>365){
                # fix leap years   
                #output<-output[1:365,]
                output<-output[-60,]
              }
              if(j==1){
                results<-output
              }else{
                results<-rbind(results,output)
              } 
    }
    nyears2<-nrow(results)/365
    ndays<-nrow(results)
    juldaysn2<-juldaysn[juldaysn<=ndays]
    juldaysn2<-juldaysn[1:round(nyears2*12)]
    
  }
  }
  ######################### end database and general I/O setup ############################
    if(soildata==1){
        VIEWF<-VIEWF_all
        SLES<-SLES2
    }else{
      VIEWF<-VIEWF_all
    }
    
    if((soildata==1 & nrow(soilprop)>0)|soildata==0){

      if(dbase=='global'){
          TMAXX<-maxtemps
          TMINN<-mintemps
          CCMAXX<-clouds
          CCMINN<-CCMAXX
          RHMINN<-minhumidities
          RHMAXX<-maxhumidities
          WNMAXX<-maxwinds
          WNMINN<-WNMAXX
          RAINFALL<-rainfall
          RAINYDAYS<-rainydays
      } #end check for whether dbase=global  
      
      if(soildata==1){
          # get static soil data into arrays
          REFL <- static_soil_vars[,1]  # albedo/reflectances
          maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
          upperetasat <- static_soil_vars[,14]
          loweretasat <- static_soil_vars[,15]
          upperdep <- static_soil_vars[,16] # thickness of A horizon
          lowerdep <- static_soil_vars[,17] # thickness of B horizon
    
        if(vlsci==0){
        #### start query of soil moisture ###
        channel <- odbcConnect("AWAPSoil",uid = "student", pwd = "student")  
        lat1<-x[2]-0.025
        lat2<-x[2]+0.0249
        lon1<-x[1]-0.025
        lon2<-x[1]+0.0249
        
        yearlist<-seq(ystart,(ystart+(nyears-1)),1)
        for(j in 1:nyears){ # start loop through years
          yeartodo<-yearlist[j]
          # syntax for query
          query<-paste("SELECT a.latitude, a.longitude, b.*
   FROM [AWAPSoil].[dbo].[latlon] as a
                       , [AWAPSoil].[dbo].[",yeartodo,"] as b
                       where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                       order by b.month",sep="")
   # exectue query
          if(j==1){
            output<- sqlQuery(channel,query)
            output<-cbind(output$latitude,output$longitude,output$id,output$month,output$WRel1,output$WRel2)
            year<-as.data.frame(rep(yearlist[j],nrow(output)))
            colnames(year)<-'rep(yearlist[j], nrow(output1))'
          }else{
            output1<-sqlQuery(channel,query)
            output1<-cbind(output1$latitude,output1$longitude,output1$id,output1$month,output1$WRel1,output1$WRel2)
            output<-rbind(output,output1)
            year<-rbind(year,as.data.frame(rep(yearlist[j],nrow(output1))))
          }
        } # end loop through years
        
        output<-cbind(year,output)
        dates<-paste("15/",output[,5],"/",output[,1],sep="")
        dates<-strptime(dates, "%d/%m/%Y") # convert to date format
        dates<-format(dates, "%d/%m/%Y")
        output<-cbind(as.Date(dates, "%d/%m/%Y"),output)
        colnames(output)<-c("date","year","latitude","longitude","id","month","WRel1","WRel2")
      
        uppermoist <- output$WRel1 
        lowermoist <- output$WRel2
        # add an extra value for soil moisture if doing current year, which will be incomplete
        if(yfinish==curyear){
          uppermoist<-c(uppermoist,uppermoist[length(uppermoist)])
          lowermoist<-c(lowermoist,lowermoist[length(lowermoist)])
          # do it again for now - Nov 2013, Oct not in yet!
          uppermoist<-c(uppermoist,uppermoist[length(uppermoist)])
          lowermoist<-c(lowermoist,lowermoist[length(lowermoist)])
        }
        moistupper<-uppermoist
        moistlower<-lowermoist

        }        #### end query of soil moisture ###
        
        shademax<-maxshades
        
      }else{
        if(manualshade==0){
           maxshades <- static_soil_vars[,2:13] # assuming FAPAR represents shade
        }
        shademax<-maxshades
      }
      
      if(aussiegrass==1){
       if(vlsci==0){
        channel3 <- odbcConnect("AUSSIEGRASS",uid = "aussiegrass", pwd = "Grass11")  
        lat1<-x[2]-0.025
        lat2<-x[2]+0.0249
        lon1<-x[1]-0.025
        lon2<-x[1]+0.0249
        # syntax for query
        query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AUSSIEGRASS].[dbo].[latlon] as a
                     , [AUSSIEGRASS].[dbo].[grass] as b
                     where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.year between ",ystart," and ",yfinish,")
                     order by b.year, b.month",sep="")

        # exectue query
        output<- sqlQuery(channel3,query)
        if(nrow(output)>0){
        # insert dates
        dates<-paste("15/",output[,5],"/",output[,4],sep="")
        dates<-strptime(dates, "%d/%m/%Y") # convert to date format
        output<-cbind(dates,output)
        colnames(output)[1]<-"date"
        
        grassgrowth<-output$growth
        grasstsdm<-output$tsdm
        }else{
          grasstest<-NA
        }
       }
      }
      
      if(is.na(dbrow)!=TRUE & is.na(ALTITUDES)!=TRUE & is.na(grasstest)!=TRUE){

        hori<-HORIZONS

        if(rungads==1){
        ####### get solar attenuation due to aerosols with program GADS #####################
        
        lat5<-lat5s[which.min(abs(lat5s-x[2]))]
        lon5<-lon5s[which.min(abs(lon5s-x[1]))]
        if(dbase!='global'){
        lat5<--35 # fixing these to be within one gads box for Aust calcs
        lon5<-140 # fixing these to be within one gads box for Aust calcs 
        }
        
        #if(oldlat5!=lat5 & oldlon5!=lon5){ #only redo calcs if in next 5deg region
          relhum<-1.
          season<-0.
          gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
          # If the library hasn't been loaded yet, load it
          if(vlsci==1){
            setwd("/vlsci/VR0212/shared/NicheMapR_Working/gads/gads22/")
          }else{
           setwd("/NicheMapR/gads/gads22/")
          }
          if (!is.loaded('gads')) {
           if(vlsci==1){
            dyn.load('gads.so')
           }else{
            dyn.load('gads.dll')
           }
          } 
          a <- .Fortran("gads", 
                        as.double(gadin$lat5), 
                        as.double(gadin$lon5), 
                        as.double(gadin$relhum), 
                        as.double(gadin$season), 
                        optdep=matrix(data = 0., nrow = 25, ncol = 2))
           if(vlsci==1){
            dyn.unload('gads.so')
           }else{
            dyn.unload('gads.dll')
           } 
          
          optdep <- matrix(data = 0., nrow = 25, ncol = 2)
          storage.mode(optdep)<-"double"
          optdep<-a$optdep
          optdep.names<-c("LAMBDA","OPTDEPTH")
          colnames(optdep)<-optdep.names
          optdep[,1]<-optdep[,1]*1000
          optdep.summer<-as.data.frame(optdep)
          
          season<-1.
          gadin<-list(lat5=lat5,lon5=lon5,relhum=relhum,season=season)
          # If the library hasn't been loaded yet, load it
          if(vlsci==1){
            setwd("/vlsci/VR0212/shared/NicheMapR_Working/gads/gads22/")
          }else{
           setwd("/NicheMapR/gads/gads22/")
          }
          if (!is.loaded('gads')) {
           if(vlsci==1){
            dyn.load('gads.so')
           }else{
            dyn.load('gads.dll')
           }

          } 
          a <- .Fortran("gads", 
                        as.double(gadin$lat5), 
                        as.double(gadin$lon5), 
                        as.double(gadin$relhum), 
                        as.double(gadin$season), 
                        optdep=matrix(data = 0., nrow = 25, ncol = 2))
           if(vlsci==1){
            dyn.unload('gads.so')
           }else{
            dyn.unload('gads.dll')
           } 
          
          optdep <- matrix(data = 0., nrow = 25, ncol = 2)
          storage.mode(optdep)<-"double"
          optdep<-a$optdep
          optdep.names<-c("LAMBDA","OPTDEPTH")
          colnames(optdep)<-optdep.names
          optdep[,1]<-optdep[,1]*1000
          optdep.winter<-as.data.frame(optdep)
          #plot(optdep.summer$OPTDEPTH~optdep.summer$LAMBDA, type='l')
          #points(optdep.winter$OPTDEPTH~optdep.winter$LAMBDA)
          optdep<-cbind(optdep.winter[,1],rowMeans(cbind(optdep.summer[,2],optdep.winter[,2])))
          optdep<-as.data.frame(optdep)
          colnames(optdep)<-c("LAMBDA","OPTDEPTH")
          
          a<-lm(OPTDEPTH~poly(LAMBDA, 6, raw=TRUE),data=optdep)
          LAMBDA<-c(290,295,300,305,310,315,320,330,340,350,360,370,380,390,400,420,440,460,480,500,520,540,560,580,600,620,640,660,680,700,720,740,760,780,800,820,840,860,880,900,920,940,960,980,1000,1020,1080,1100,1120,1140,1160,1180,1200,1220,1240,1260,1280,1300,1320,1380,1400,1420,1440,1460,1480,1500,1540,1580,1600,1620,1640,1660,1700,1720,1780,1800,1860,1900,1950,2000,2020,2050,2100,2120,2150,2200,2260,2300,2320,2350,2380,2400,2420,2450,2490,2500,2600,2700,2800,2900,3000,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000)
          TAI<-predict(a,data.frame(LAMBDA))
          TAI2<-as.data.frame(cbind(LAMBDA,TAI))
          #points(TAI2$TAI~TAI2$LAMBDA)
        #}
        oldlat5<-lat5
        oldlon5<-lon5
        ################ end GADS ################################################## 
        }else{
       TAI<-c(0.0670358341290886,0.0662612704779235,0.065497075238002,0.0647431301168489,0.0639993178022531,0.0632655219571553,0.0625416272145492,0.0611230843885423,0.0597427855962549,0.0583998423063099,0.0570933810229656,0.0558225431259535,0.0545864847111214,0.0533843764318805,0.0522154033414562,0.0499736739981675,0.047855059159556,0.0458535417401334,0.0439633201842001,0.0421788036108921,0.0404946070106968,0.0389055464934382,0.0374066345877315,0.0359930755919066,0.0346602609764008,0.0334037648376212,0.0322193394032758,0.0311029105891739,0.0300505736074963,0.0290585886265337,0.0281233764818952,0.0272415144391857,0.0264097320081524,0.0256249068083005,0.0248840604859789,0.0241843546829336,0.0235230870563317,0.0228976873502544,0.0223057135186581,0.0217448478998064,0.0212128934421699,0.0207077699817964,0.0202275105711489,0.0197702578594144,0.0193342605242809,0.0189178697551836,0.0177713140039894,0.0174187914242432,0.0170790495503944,0.0167509836728154,0.0164335684174899,0.0161258546410128,0.0158269663770596,0.0155360978343254,0.0152525104459325,0.0149755299703076,0.0147045436435285,0.0144389973831391,0.0141783930434343,0.0134220329447663,0.0131772403830191,0.0129356456025128,0.0126970313213065,0.0124612184223418,0.0122280636204822,0.01199745718102,0.0115436048739351,0.0110993711778668,0.0108808815754663,0.0106648652077878,0.0104513876347606,0.0102405315676965,0.00982708969547694,0.00962473896278535,0.00903679230300494,0.00884767454432418,0.0083031278398166,0.00796072474935954,0.00755817587626185,0.00718610751850881,0.00704629977586921,0.00684663903049612,0.00654155580333479,0.00642947339729728,0.00627223096874308,0.00603955966866779,0.00580920937536261,0.00568506186880564,0.00563167068287251,0.00556222005081865,0.00550522989971023,0.00547395763028062,0.0054478983436216,0.00541823364504573,0.00539532163908382,0.00539239864119488,0.00541690124712384,0.00551525885358836,0.00564825853509463,0.00577220185074264,0.00584222986640171,0.00581645238345584,0.00566088137411449,0.00535516862329704,0.00489914757707667,0.00432017939770409,0.0036813032251836,0.00309019064543606,0.00270890436501562,0.00276446109239711,0.00356019862584603)
        } #end check if running gads
        
        
        if(dbase=='ausclim' || dbase=='AWAP'){
          if(dbase=='AWAP'){
            if(vlsci==0){
              yearlist<-seq(ystart,(ystart+(nyears-1)),1)
              for(j in 1:nyears){ # start loop through years
                yeartodo<-yearlist[j]
                lat1<-x[2]-0.024
                lat2<-x[2]+0.025
                lon1<-x[1]-0.024
                lon2<-x[1]+0.025 
                query<-paste("SELECT a.latitude, a.longitude, b.*
                             FROM [AWAPDaily].[dbo].[latlon] as a
                             , [AWAPDaily].[dbo].[",yeartodo,"] as b
                             where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                             order by b.day",sep="")
                output<- sqlQuery(channel,query)
                output$sol<-as.numeric(as.character(output$sol))
                
                if(nrow(output)>365){
                  # fix leap years   
                  output<-output[-60,]
                }
                if(j==1){
                  results<-output
                }else{
                  results<-rbind(results,output)
                } 
              } 
              if(dailywind==1){
                channel <- odbcConnect("dailywind",uid = "student", pwd = "student")
              for(j in 1:nyears){ # start loop through years
                yeartodo<-yearlist[j]
                lat1<-x[2]-0.024
                lat2<-x[2]+0.025
                lon1<-x[1]-0.024
                lon2<-x[1]+0.025 
                query<-paste("SELECT a.latitude, a.longitude, b.*
                             FROM [dailywind].[dbo].[latlon] as a
                             , [dailywind].[dbo].[",yeartodo,"] as b
                             where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                             order by b.day",sep="")
                output<- sqlQuery(channel,query)
                
                if(nrow(output)>365){
                  # fix leap years   
                  output<-output[-60,]
                }
                if(j==1){
                  dwind<-output
                }else{
                  dwind<-rbind(dwind,output)
                } 
              } 
              dwind<-dwind$wind/15.875
              }
            }else{ #vlsci==1
             load(paste(barcoo,'TMAXX.bin',sep=''))
             TMAXX <- as.matrix(data[quadrangle,2:7301])
             load(paste(barcoo,'TMINN.bin',sep=''))
             TMINN <- as.matrix(data[quadrangle,2:7301])
             load(paste(barcoo,'RHMAXX.bin',sep=''))
             RHMAXX <- as.numeric(as.matrix(data[quadrangle,2:7301]))
             load(paste(barcoo,'RHMINN.bin',sep=''))
             RHMINN <- as.numeric(as.matrix(data[quadrangle,2:7301]))
             load(paste(barcoo,'CCMAXX.bin',sep=''))
             CCMAXX <- as.numeric(as.matrix(data[quadrangle,2:7301]))
             load(paste(barcoo,'CCMINN.bin',sep=''))
             CCMINN <- as.numeric(as.matrix(data[quadrangle,2:7301]))
             
             load(paste(barcoo,'RAINFALL.bin',sep=''))
             RAINFALL <- as.matrix(data[quadrangle,2:7301])

RHMAXX<-t(RHMAXX)
RHMINN<-t(RHMINN)
CCMAXX<-t(CCMAXX)
CCMINN<-t(CCMINN)
	     TMAXX<-t(as.data.frame(TMAXX[((ystart-1989)*365-364):((yfinish-1989)*365)]))
	     TMINN<-t(as.data.frame(TMINN[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             RHMAXX<-t(as.data.frame(RHMAXX[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             RHMINN<-t(as.data.frame(RHMINN[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             CCMAXX<-t(as.data.frame(CCMAXX[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             CCMINN<-t(as.data.frame(CCMINN[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             RAINFALL<-t(as.data.frame(RAINFALL[((ystart-1989)*365-364):((yfinish-1989)*365)]))

              if(dailywind==1){
                
                load(paste(barcoo,'dwind.bin',sep=''))
                dwind<- as.matrix(data[quadrangle,2:7301])
	        dwind<-t(as.data.frame(dwind[((ystart-1989)*365-364):((yfinish-1989)*365)]))
		#WNMINN<-WNMAXX
              }
              if(adiab_cor==1){
                TMAXX<-TMAXX+adiab_corr_max
                TMINN<-TMINN+adiab_corr_min
              }
            } #end vlsci check
            if(vlsci==0){
              if(adiab_cor==1){
                TMAXX<-as.matrix(results$tmax+adiab_corr_max)
                TMINN<-as.matrix(results$tmin+adiab_corr_min)
              }else{
                TMAXX<-as.matrix(results$tmax)
                TMINN<-as.matrix(results$tmin)
              }
              RAINFALL<-results$rr
              output_AWAPDaily<-results
            }
            
            #            if(vlsci==1){
            #              filenm<-paste(jobdir,"/output_ausclim.csv",sep="")
            #              b.output_ausclim<-as.data.frame(read.table(file = filenm, sep = ",",head=TRUE))
            #              filenm<-paste(jobdir,"/CCMAXX.csv",sep="")
            #              b.CCMAXX <-as.data.frame(read.table(file = filenm, sep = ",",head=TRUE)) 
            #            }
            
            # cloud cover
            if(vlsci==0){          
            if(ystart>1989 & sum(results[,9],na.rm=TRUE)>0){ # solar radiation data available
                query<-paste("SELECT a.*
                             FROM [ausclim].[dbo].[clearskysol] as a
                             where (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") 
                             ",sep="")
                output_ausclim<- sqlQuery(channel,query)
              
              if(nrow(output_ausclim)==0){ #no satellite coverage, get data from ausclim
                clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
                #CCMAXX <- dbGetQuery(con,statement=clouds)*100
                if(vlsci==0){
                  CCMAXX<- sqlQuery(channel2,clouds)*100
                }
                CCMINN <- CCMAXX
                CCMAXX1 <-spline(juldays12,CCMAXX,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
                CCMAXX <- CCMAXX1$y
                CCMINN <- CCMAXX
              }else{
                weekly_sol<-cbind(1:52,t(output_ausclim[3:54]))
                daily_sol <-spline(seq(3,361,7),weekly_sol[,2],n=365,xmin=1,xmax=365,method="periodic")
                daily_sol<-as.numeric(daily_sol$y)
                daily_sol<-rep(daily_sol,nyears)
                if(is.na(output_AWAPDaily[1,9])==TRUE){
                  output_AWAPDaily[1,9]=mean(output_AWAPDaily[,9],na.rm=TRUE)
                }
                if(is.na(output_AWAPDaily[7300,9])==TRUE){
                  output_AWAPDaily[nrow(output_AWAPDaily),9]=mean(output_AWAPDaily[,9],na.rm=TRUE)
                }
                solar<-na.approx(output_AWAPDaily[,9])
                cloud<-(1-as.data.frame(solar)/as.data.frame(daily_sol))*100
                cloud[cloud<0]<-0
                cloud[cloud>100]<-100
                cloud<-as.matrix(cbind(output_AWAPDaily[,4],cloud))
                CCMAXX<-cloud[,2]
                CCMINN<-CCMAXX
              }
            }else{
              clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
              #CCMAXX <- dbGetQuery(con,statement=clouds)*100
              if(vlsci==0){
                CCMAXX<- sqlQuery(channel2,clouds)*100
              }else{
                siteind<-seq(min(b.CCMAXX$l),max(b.CCMAXX$l))
                check<-siteind[l]
                CCMAXX<-subset(b.CCMAXX,b.CCMAXX$l==check)
                CCMAXX<-CCMAXX[,-1]
                CCMAXX<-CCMAXX*100
              }
              CCMINN <- CCMAXX
              CCMAXX1 <-spline(juldays12,CCMAXX,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
              CCMAXX <- CCMAXX1$y
              CCMINN <- CCMAXX
            }# end check for year 1990 or later
            if(ystart>1970){ #vapour pressure data available
              if(is.na(output_AWAPDaily[1,8])==TRUE){
                output_AWAPDaily[1,8]=mean(output_AWAPDaily[,8],na.rm=TRUE)
              }
              VAPRES<-na.approx(output_AWAPDaily[,8])
              VAPRES<-VAPRES*100 # convert from hectopascals to pascals
              TMAXK<-TMAXX+273.15
              loge<-TMAXK
              loge[loge>273.16]<- -7.90298*(373.16/TMAXK-1.)+5.02808*log10(373.16/TMAXK)-1.3816E-07*(10.^(11.344*(1.-TMAXK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMAXK-1.))-1.)+log10(1013.246)
              loge[loge<=273.16]<- -9.09718*(273.16/TMAXK-1.)-3.56654*log10(273.16/TMAXK)+.876793*(1.-TMAXK/273.16)+log10(6.1071)
              estar<-(10.^loge)*100. 
              RHMINN<-(VAPRES/estar)*100
              RHMINN[RHMINN>100]<-100
              RHMINN[RHMINN<0]<-0.01
              #RHMINN
              TMINK<-TMINN+273.15
              loge<-TMINK
              loge[loge>273.16]<- -7.90298*(373.16/TMINK-1.)+5.02808*log10(373.16/TMINK)-1.3816E-07*(10.^(11.344*(1.-TMINK/373.16))-1.)+8.1328E-03*(10.^(-3.49149*(373.16/TMINK-1.))-1.)+log10(1013.246)
              loge[loge<=273.16]<- -9.09718*(273.16/TMINK-1.)-3.56654*log10(273.16/TMINK)+.876793*(1.-TMINK/273.16)+log10(6.1071)
              estar<-(10.^loge)*100. 
              RHMAXX<-(VAPRES/estar)*100
              RHMAXX[RHMAXX>100]<-100
              RHMAXX[RHMAXX<0]<-0.01
            } #end check for year is 1971 or later
            } #end vlsci check
          }else{
            maxtemps<-paste("select maxtemp1,maxtemp2,maxtemp3,maxtemp4,maxtemp5,maxtemp6,maxtemp7,maxtemp8,maxtemp9,maxtemp10,maxtemp11,maxtemp12 FROM maxtemp WHERE i = ",dbrow,sep="")
            mintemps<-paste("select mintemp1,mintemp2,mintemp3,mintemp4,mintemp5,mintemp6,mintemp7,mintemp8,mintemp9,mintemp10,mintemp11,mintemp12 FROM mintemp WHERE i = ",dbrow,sep="")
            rainfall<-paste("select rainfall1,rainfall2,rainfall3,rainfall4,rainfall5,rainfall6,rainfall7,rainfall8,rainfall9,rainfall10,rainfall11,rainfall12 FROM rainfall WHERE i = ",dbrow,sep="")
          }
          clouds<-paste("select cloud1,cloud2,cloud3,cloud4,cloud5,cloud6,cloud7,cloud8,cloud9,cloud10,cloud11,cloud12 FROM cloudcover WHERE i = ",dbrow,sep="")
          maxwinds<-paste("select maxwind1,maxwind2,maxwind3,maxwind4,maxwind5,maxwind6,maxwind7,maxwind8,maxwind9,maxwind10,maxwind11,maxwind12 FROM maxwind WHERE i = ",dbrow,sep="")
          minwinds<-paste("select minwind1,minwind2,minwind3,minwind4,minwind5,minwind6,minwind7,minwind8,minwind9,minwind10,minwind11,minwind12 FROM minwind WHERE i = ",dbrow,sep="")
          maxhumidities<-paste("select maxhum1,maxhum2,maxhum3,maxhum4,maxhum5,maxhum6,maxhum7,maxhum8,maxhum9,maxhum10,maxhum11,maxhum12 FROM maxhum WHERE i = ",dbrow,sep="")
          minhumidities<-paste("select minhum1,minhum2,minhum3,minhum4,minhum5,minhum6,minhum7,minhum8,minhum9,minhum10,minhum11,minhum12 FROM minhum WHERE i = ",dbrow,sep="")
          rainfall<-paste("select rainfall1,rainfall2,rainfall3,rainfall4,rainfall5,rainfall6,rainfall7,rainfall8,rainfall9,rainfall10,rainfall11,rainfall12 FROM rainfall WHERE i = ",dbrow,sep="")
          rainydays<-paste("select rainy1,rainy2,rainy3,rainy4,rainy5,rainy6,rainy7,rainy8,rainy9,rainy10,rainy11,rainy12 FROM rainydays WHERE i = ",dbrow,sep="")
        }
        
        if(dbase=='bom'){
          TMAXX <- na.approx(as.double(dayclim$Tmax[1:length(dayclim$date)],na.rm=TRUE))
          #if(length(TMAXX)<length(dayclim$date){ # check that last day wasn't an na value
          #TMAXX[length(dayclim$date)]<-TMAXX[length(TMAXX)]
          #}
          TMINN <- na.approx(as.double(dayclim$Tmin[1:length(dayclim$date)],na.rm=TRUE))
          RHMAXX <- na.approx(as.double(pmax(dayclim$Relhum00hrs[1:length(dayclim$date)],dayclim$Relhum03hrs[1:length(dayclim$date)],dayclim$Relhum06hrs[1:length(dayclim$date)],dayclim$Relhum09hrs[1:length(dayclim$date)],
                                             dayclim$Relhum12hrs[1:length(dayclim$date)],dayclim$Relhum15hrs[1:length(dayclim$date)],dayclim$Relhum18hrs[1:length(dayclim$date)],dayclim$Relhum21hrs[1:length(dayclim$date)],na.rm=TRUE)))
          RHMINN <- na.approx(as.double(pmin(dayclim$Relhum00hrs[1:length(dayclim$date)],dayclim$Relhum03hrs[1:length(dayclim$date)],dayclim$Relhum06hrs[1:length(dayclim$date)],dayclim$Relhum09hrs[1:length(dayclim$date)],
                                             dayclim$Relhum12hrs[1:length(dayclim$date)],dayclim$Relhum15hrs[1:length(dayclim$date)],dayclim$Relhum18hrs[1:length(dayclim$date)],dayclim$Relhum21hrs[1:length(dayclim$date)],na.rm=TRUE)))
          CCMAXX <- na.approx(as.double(pmax(dayclim$Cloud00hrs[1:length(dayclim$date)],dayclim$Cloud03hrs[1:length(dayclim$date)],dayclim$Cloud06hrs[1:length(dayclim$date)],dayclim$Cloud09hrs[1:length(dayclim$date)],
                                             dayclim$Cloud12hrs[1:length(dayclim$date)],dayclim$Cloud15hrs[1:length(dayclim$date)],dayclim$Cloud18hrs[1:length(dayclim$date)],dayclim$Cloud21hrs[1:length(dayclim$date)],na.rm=TRUE)))/8*100
          CCMINN <- na.approx(as.double(pmin(dayclim$Cloud00hrs[1:length(dayclim$date)],dayclim$Cloud03hrs[1:length(dayclim$date)],dayclim$Cloud06hrs[1:length(dayclim$date)],dayclim$Cloud09hrs[1:length(dayclim$date)],
                                             dayclim$Cloud12hrs[1:length(dayclim$date)],dayclim$Cloud15hrs[1:length(dayclim$date)],dayclim$Cloud18hrs[1:length(dayclim$date)],dayclim$Cloud21hrs[1:length(dayclim$date)],na.rm=TRUE)))/8*100
          
          WNMAXX <- na.approx(as.double(pmax(dayclim$Windspeed00hrs[1:length(dayclim$date)],dayclim$Windspeed03hrs[1:length(dayclim$date)],dayclim$Windspeed06hrs[1:length(dayclim$date)],dayclim$Windspeed09hrs[1:length(dayclim$date)],
                                             dayclim$Windspeed12hrs[1:length(dayclim$date)],dayclim$Windspeed15hrs[1:length(dayclim$date)],dayclim$Windspeed18hrs[1:length(dayclim$date)],
                                             dayclim$Windspeed21hrs[1:length(dayclim$date)],na.rm=TRUE)))*1000/3600
          
          WNMINN <- na.approx(as.double(pmin(dayclim$Windspeed00hrs[1:length(dayclim$date)],dayclim$Windspeed03hrs[1:length(dayclim$date)],dayclim$Windspeed06hrs[1:length(dayclim$date)],dayclim$Windspeed09hrs[1:length(dayclim$date)],
                                             dayclim$Windspeed12hrs[1:length(dayclim$date)],dayclim$Windspeed15hrs[1:length(dayclim$date)],dayclim$Windspeed18hrs[1:length(dayclim$date)],
                                             dayclim$Windspeed21hrs[1:length(dayclim$date)],na.rm=TRUE)))*1000/3600
          WNMAXX[WNMAXX==0]<-0.1
          WNMINN[WNMINN==0]<-0.1 
          # correct for fact that wind is measured at 10 m height
          # wind shear equation v / vo = (h / ho)^a
          #where
          #v = the velocity at height h (m/s)
          #vo = the velocity at height ho (m/s)
          #a = the wind shear exponent
          #Terrain   Wind Shear Exponent
          #- a -
          #  Open water   0.1
          #Smooth, level, grass-covered 	0.15 (or more commonly 1/7)
          #Row crops 	0.2
          #Low bushes with a few trees 	0.2
          #Heavy trees 	0.25
          #Several buildings 	0.25
          #Hilly, mountainous terrain 	0.25
          # source http://www.engineeringtoolbox.com/wind-shear-d_1215.html
          WNMINN<-WNMINN*(1.2/10)^0.15
          WNMAXX<-WNMAXX*(1.2/10)^0.15
          RAINFALL <- na.approx(as.double(dayclim$rainfall[1:length(dayclim$date)],na.rm=TRUE))
          RAINFALL <- RAINFALL
          SNOW <- (1:length(dayclim$date))*0
          REFLS <- (1:length(dayclim$date))*0+REFL
          PCTWET <- (1:length(dayclim$date))*0+PCTWET
          if(soildata==1){
          SLES <- (1:length(dayclim$date))*0+mean(SLES)
          }else{
          SLES <- (1:length(dayclim$date))*0+SLE
          }
          ALLMINTEMPS<-TMINN
          ALLMAXTEMPS<-TMAXX
        }else{ #using ausclim databases
          if(dbase!='AWAP' & dbase!='global'){
            TMAXX<- sqlQuery(channel2,maxtemps)+adiab_corr
            TMINN<- sqlQuery(channel2,mintemps)+adiab_corr
          }
          ALLMINTEMPS<-TMINN
          ALLMAXTEMPS<-TMAXX
          if((dbase!='AWAP' | (dbase=='AWAP' & ystart<1971)) & dbase!='global'){
            RHMAXX <- sqlQuery(channel2,maxhumidities)
            RHMINN <- sqlQuery(channel2,minhumidities)
          }
          if((dbase!='AWAP' | (dbase=='AWAP' & ystart<1990)) & dbase!='global'){
            CCMAXX <- sqlQuery(channel2,clouds)
            CCMINN <- CCMAXX
          }
          if(dbase!='global'){
           if(vlsci==0){
            WNMAXX <- sqlQuery(channel2,maxwinds)
            WNMINN <- sqlQuery(channel2,minwinds)  
           }else{
             if(dailywind!=1){

             load(paste(barcoo,'WNMAXX.bin',sep=''))
              WNMAXX <- as.numeric(as.matrix(data[quadrangle,2:7301]))
             load(paste(barcoo,'WNMINN.bin',sep=''))
             WNMINN<-as.numeric(as.matrix(data[quadrangle,2:7301]))
	   
	     WNMAXX<-t(WNMAXX)
	     WNMINN<-t(WNMINN)
	     WNMAXX<-t(as.data.frame(WNMAXX[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             WNMINN<-t(as.data.frame(WNMINN[((ystart-1989)*365-364):((yfinish-1989)*365)]))
             }
           }
          }
          
          if(timeinterval!=12){
          
            if(dbase!='AWAP'){
              TMAXX1 <-spline(juldays12,TMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
              TMAXX<-rep(TMAXX1$y,nyears)
              TMINN1 <-spline(juldays12,TMINN,n=timeinterval,xmin=1,xmax=365,method="periodic")
              TMINN <- rep(TMINN1$y,nyears)
            }
            if(dbase!='AWAP'){
              soilmoist1 <-spline(juldays12,soilm,n=timeinterval,xmin=1,xmax=365,method="periodic")
              soilmoist<-rep(soilmoist1$y,nyears)
            }
            if(dbase!='AWAP' | (dbase=='AWAP' & ystart<1971)){
              RHMAXX1 <-spline(juldays12,RHMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
              RHMAXX <- rep(RHMAXX1$y,nyears)
              RHMINN1 <-spline(juldays12,RHMINN,n=timeinterval,xmin=1,xmax=365,method="periodic")
              RHMINN <- rep(RHMINN1$y,nyears)
            }
            if(dbase!='AWAP' | (dbase=='AWAP' & ystart<1990)){
              CCMAXX1 <-spline(juldays12,CCMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
              CCMAXX <- rep(CCMAXX1$y,nyears)
              CCMINN <- CCMAXX
            }
            if(dailywind!=1 & vlsci!=1){
            WNMAXX1 <-spline(juldays12,WNMAXX,n=timeinterval,xmin=1,xmax=365,method="periodic")
            WNMAXX<-rep(WNMAXX1$y,nyears) 
            WNMINN1 <-spline(juldays12,WNMINN,n=timeinterval,xmin=1,xmax=365,method="periodic")
            WNMINN<-rep(WNMINN1$y,nyears) 
            }
          }
          
          if(metchamber==1){
            TMAXX <- rep(chambertemp,timeinterval*nyears)
            TMINN <- rep(chambertemp,timeinterval*nyears)
            RHMAXX <- rep(chamberrh,timeinterval*nyears)
            RHMINN <- rep(chamberrh,timeinterval*nyears)
            CCMAXX <- rep(100,timeinterval*nyears)
            CCMINN <- rep(100,timeinterval*nyears)
            WNMAXX <- rep(chamberwind,timeinterval*nyears)
            WNMINN <- rep(chamberwind,timeinterval*nyears)
          }
          
          if(dbase=='ausclim' || dbase=='AWAP'){
            if(timeinterval!=12){
              
              if(aussiegrass==1){
                if(vlsci==0){                
                grassgrowths1<-spline(juldaysn2,grassgrowth,n=ndays,xmin=1,xmax=ndays,method="periodic")
                grassgrowths<-grassgrowths1$y
                grassgrowths[grassgrowths<0]<-0
                grasstsdms1<-spline(juldaysn2,grasstsdm,n=ndays,xmin=1,xmax=ndays,method="periodic")
                grasstsdms<-grasstsdms1$y
                grasstsdms[grasstsdms<0]<-0
              }else{

                  load(paste(barcoo,'grassgrowths.bin',sep=''))
                  grassgrowths <- as.numeric(as.matrix(data[quadrangle,2:7301]))
                  load(paste(barcoo,'grasstsdms.bin',sep=''))
                  grasstsdms <- as.numeric(as.matrix(data[quadrangle,2:7301]))
                
grassgrowths<-t(grassgrowths)
grasstsdms<-t(grasstsdms)
		grassgrowths<-t(as.data.frame(grassgrowths[((ystart-1989)*365-364):((yfinish-1989)*365)]))
		grasstsdms<-t(as.data.frame(grasstsdms[((ystart-1989)*365-364):((yfinish-1989)*365)]))
                grassgrowths[grassgrowths<0]<-0

                grasstsdms[grasstsdms<0]<-0
              }
              }
              if(soildata==1){
                if(vlsci==0){
                  uppermoist1<-spline(juldaysn2,moistupper,n=ndays,xmin=1,xmax=ndays,method="periodic")
                  lowermoist1<-spline(juldaysn2,moistlower,n=ndays,xmin=1,xmax=ndays,method="periodic")
                  uppermoists<-uppermoist1$y
                  lowermoists<-lowermoist1$y
                }else{

                  load(paste(barcoo,'moists1.bin',sep=''))
                  uppermoists <- as.numeric(as.matrix(data[quadrangle,2:7301]))
                  load(paste(barcoo,'moists2.bin',sep=''))
                  lowermoists <- as.numeric(as.matrix(data[quadrangle,2:7301]))
                  uppermoists <-t(uppermoists)
		  lowermoists <-t(lowermoists )
		  uppermoists<-t(as.data.frame(uppermoists[((ystart-1989)*365-364):((yfinish-1989)*365)]))
 		  lowermoists<-t(as.data.frame(lowermoists[((ystart-1989)*365-364):((yfinish-1989)*365)]))
                }
                SLES1<-spline(juldays12,SLES,n=timeinterval,xmin=1,xmax=365,method="periodic")
                SLES<-rep(SLES1$y,nyears)
                SLES<-SLES[1:ndays]
                maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
                MAXSHADES<-rep(maxshades1$y*100,nyears)
                MAXSHADES<-MAXSHADES[1:ndays]
                if(manualshade==1){
                  maxshades <- rep(maxshade,365)
                  maxshades <- rep(maxshades,nyears)
                  MAXSHADES<-maxshades
                  minshades <- rep(minshade,365)
                  minshades <- rep(minshades,nyears)
                  MINSHADES<-minshades
                }
              }else{
                if(manualshade==0){
                maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
                MAXSHADES<-rep(maxshades1$y*100,nyears)
                minshades <- rep(minshade,365)
                minshades <- rep(minshades,nyears)
                MINSHADES<-minshades
                }else{
                MAXSHADES<-maxshades
                }
              }
            }else{
              if(soildata==1){
                uppermoists<-moistupper
                lowermoists<-moistlower
              }
              MAXSHADES<-maxshades
            }
          }else{
            MAXSHADES<-maxshades
          }
          if(dbase!='AWAP' & dbase!='bom' & dbase!='global'){
            #RAINFALL2 <- dbGetQuery(con,statement=rainfall)
            RAINFALL2 <- sqlQuery(channel2,rainfall)
          }
          if((dbase=='ausclim' || dbase=='AWAP') & dbase!='global'){
            #RAINYDAYS <- dbGetQuery(con,statement=rainydays)
            if(vlsci==0){
            RAINYDAYS <- sqlQuery(channel2,rainydays)
            RAINYDAYS <- round(RAINYDAYS)
            }else{
              RAINYDAYS<-rep(0,7300)
            }
            RAINYDAYS <- round(RAINYDAYS)
          }else{
            #RAINYDAYS <- rep(15,12)
            RAINYDAYS<-rainydays
          }
          
          if(dbase!='AWAP' & dbase != 'bom'){
            RAINFALL1<-1:365
            if(weather_gen==0){
              if(dbase=='ausclim' | (dbase=='global' | timeinterval==365)){
                m<-1
                for (i in 1:12){ #begin loop throught 12 months of year
                  ndays=daymon[i]  
                  for (k in 1:ndays){
                    if(k<RAINYDAYS[i]){
                      RAINFALL1[m]=RAINFALL2[i]/RAINYDAYS[i] # make rain fall evenly over the number of rainy days for the month, starting at the beginning of the month
                    }else{
                      RAINFALL1[m]=0.
                    }  
                    m<-m+1
                  }
                }
                RAINFALL<-rep(as.double(RAINFALL1),nyears)
              } #end check if dbase is ausclim
            } #end check if weather generator in use for rainfall
          }
          ALLTEMPS <- cbind(ALLMAXTEMPS,ALLMINTEMPS)
          
          SNOW <- (1:timeinterval)*0
          
          if(dbase=='ausclim' || dbase=='AWAP'){ 
            REFLS <- (1:timeinterval)*0+REFL
            if((soildata==1)&(length(RAINFALL)>0)){
              soilwet<-RAINFALL
              soilwet[soilwet<=rainwet] = 0 
              soilwet[soilwet>0] = 90
              #PCTWET <- uppermoists*pctwet_mult
              PCTWET <- uppermoists*soilprop$A_01bar*pctwet_mult*100
              PCTWET<-pmax(soilwet,PCTWET)
            }else{
              REFLS <- (1:(timeinterval*nyears))*0+REFL
              PCTWET <- (1:(timeinterval*nyears))*0+PCTWET
              soilwet<-RAINFALL
              soilwet[soilwet<=rainwet] = 0 
              soilwet[soilwet>0] = 90
              PCTWET<-pmax(soilwet,PCTWET)
            }
          }else{
            REFLS <- (1:(timeinterval*nyears))*0+REFL
            PCTWET <- (1:(timeinterval*nyears))*0+PCTWET
            #soilwet<-RAINFALL
            #soilwet[soilwet<=5.] = 0 
            #soilwet[soilwet>0] = 90
            #PCTWET<-pmax(soilwet,PCTWET)
          }
        } #end check for whether using bom or ausclim/global
        
        if(dbase=='bom'){ 
          
          if(aussiegrass==1){
            grassgrowths1<-spline(juldaysn,grassgrowth,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
            grassgrowths<-grassgrowths1$y
            grassgrowths[grassgrowths<0]<-0
            grasstsdms1<-spline(juldaysn,grasstsdm,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
            grasstsdms<-grasstsdms1$y
            grasstsdms[grasstsdms<0]<-0
          } 
          
          if(soildata==1){
            
            uppermoist1<-spline(juldaysn,moistupper,n=365*nyears,xmin=1,xmax=365*nyears,method="periodic")
            lowermoist1<-spline(juldaysn,moistlower,n=365*nyears,xmin=1,xmax=365*nyears,method="periodic")
            uppermoists<-uppermoist1$y
            lowermoists<-lowermoist1$y
            maxshades1 <-spline(juldays12,shademax,n=timeinterval,xmin=1,xmax=365,method="periodic")
            MAXSHADES<-rep(maxshades1$y*100,nyears)
            if(grasshade==1){
              MAXSHADES<-grasstsdms/(18119/grassfact)*100 # make shade relate to maximum grass tsdm in the entire aussiegrass dbase (18119 kg/ha)
              MAXSHADES[MAXSHADES>100]<-100
              MINSHADES<-MAXSHADES 
            }
            if(manualshade==1){
              maxshades <- rep(maxshade,365)
              maxshades <- rep(maxshades,nyears)
              MAXSHADES<-maxshades
              minshades <- rep(minshade,365)
              minshades <- rep(minshades,nyears)
              MINSHADES<-minshades
            }
          }else{
            MAXSHADES<-maxshades
          }
          
          if((soildata==1)&(length(RAINFALL)>0)){
            soilwet<-RAINFALL
            soilwet[soilwet<=rainwet] = 0 
            soilwet[soilwet>0] = 90
            #PCTWET <- uppermoists*pctwet_mult
            PCTWET <- uppermoists*soilprop$A_01bar*pctwet_mult*100
            PCTWET<-pmax(soilwet,PCTWET)
          }else{
            PCTWET <- (1:length(dayclim$date))*0+PCTWET  
          }
        }
        
        if(dbase=='bom'){
          julnum <- length(dayclim$date) # days to run
          julday <- as.double(dayclim$jday)
        }else{
          julnum <- timeinterval*nyears
          julday <- subset(juldays, juldays!=0)
          if(dbase=='AWAP'){
            julnum<-ndays
            #julday<-juldays[1:julnum]
          }
        }
        
        if(Usrhyt<0.5){ #check that size isn't too small
          Usrhyt<-0.5
        }

        if(soildata==1){
          # extra code for soil moisture start 
          Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
          Numint <- julnum  # number of time intervals
          Numtyps <- 4
          depinterval<-findInterval(upperdep*100, DEP)
          deepnode1<-depinterval
          depinterval<-findInterval(lowerdep*100, DEP)
          deepnode2<-depinterval
          deepnode3<-10
          toprow<-rep(deepnode1,julnum)
          middlerow<-rep(deepnode2,julnum)
          bottomrow<-rep(deepnode3,julnum)
          Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
          Nodes[1,1:julnum]<-3
          Nodes[2,1:julnum]<-toprow
          Nodes[3,1:julnum]<-middlerow
          Nodes[4,1:julnum]<-bottomrow
        }else{
          Intrvls<-rep(0,7300)  
          Intrvls[1] <- 1 # user-supplied last Julian day in each time interval sequence
          Numtyps <- 1 # number of substrate types
          Numint <- 1  # number of time intervals
          Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
          Nodes[1,1] <- 10. # deepest nodes for each substrate type
        }
        if(dbase=='global'){ # setting up a generic soil profile for the global sims
          Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
          Numint <- julnum  # number of time intervals
          Numtyps <- 4
          deepnode1<-5
          deepnode2<-8
          deepnode3<-10
          toprow<-rep(deepnode1,julnum)
          middlerow<-rep(deepnode2,julnum)
          bottomrow<-rep(deepnode3,julnum)
          if(vlsci_setup==0){
            Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
          }else{
            Nodes <- matrix(data = 0, nrow = 10, ncol = timeinterval*nyears) # deepest nodes for each substrate type
          }
          Nodes[1,1:julnum]<-3
          Nodes[2,1:julnum]<-toprow
          Nodes[3,1:julnum]<-middlerow
          Nodes[4,1:julnum]<-bottomrow
        }  
        idayst <- 1 # start month
        if(dbase=='bom'){
          ida <- length(dayclim$date) # end month 
        }else{
          ida<-timeinterval
        }
        
        # location and terrain
        if(dbase=="bom"){
          if(dayclim$State[1]=="VIC") {
            ALREF<-15*10
          }
          if(dayclim$State[1]=="NSW") { 
            ALREF<-15*10
          }
          if(dayclim$State[1]=="SA") {
            ALREF<-15*9.5   
          }
          if(dayclim$State[1]=="NT") {
            ALREF<-15*9.5
          }
          if(dayclim$State[1]=="QLD") {
            ALREF<-15*10
          }
          if(dayclim$State[1]=="TAS") {
            ALREF<-15*10
          }
          if(dayclim$State[1]=="WA") {
            ALREF<-15*8
          }
        }else{
          if(timezone==1){
            if(!require(geonames)){
              stop('package "geonames" is required.')
            }
            ALREF<-(GNtimezone(longlat[2],longlat[1])[4])*-15
          }else{  
            ALREF <- abs(trunc(x[1]))
          }
        }
        
        
        HEMIS <- ifelse(x[2]<0,2.,1.) 
        ALAT <- abs(trunc(x[2]))
        AMINUT <- (abs(x[2])-ALAT)*60
        ALONG <- abs(trunc(x[1]))
        ALMINT <- (abs(x[1])-ALONG)*60
        ALTT<-ALTITUDES
        SLOPE<-SLOPES
        AZMUTH<-AZMUTHS 
        
        if(countday==1){
          avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
          soilinit<-rep(avetemp,length(DEP))
        }  
        
        if(dbase=='bom'){
          tannul<-c(dayclim[,'Tmin'],dayclim[,'Tmax'])
          tannul<-mean(tannul,na.rm=T)
          if(nyears==1){
            avetemp<-c(dayclim[,'Tmin'],dayclim[,'Tmax'])
            avetemp<-mean(avetemp,na.rm=T)
            tannulrun<-rep(avetemp,365)
          }else{
            avetemp<-rowMeans(cbind(dayclim[,'Tmin'], dayclim[,'Tmax']), na.rm=TRUE)
            library("TTR")
            tannulrun<-SMA(na.approx(avetemp),n=365)
            yearone<-rep((sum(dayclim[,'Tmax'])+sum(dayclim[,'Tmin']))/(length(dayclim[,'Tmax'])*2),365)
            tannulrun[1:365]<-yearone
            # SST
            #SST<-read.csv("C:/NicheMapR_Working/projects/SeaTurtles/SST_Test.csv")
            #days<-seq(1,7300)
            #SST2 <-spline(SST$Day,SST$SST,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
            #SST2<-SST2$y
            #tannulrun<-rowMeans(cbind(tannulrun,SST2),na.rm=TRUE)
            #tannulrun<-SST2
          }          
          
          
        }else{
          tannul<-mean(unlist(ALLTEMPS))
        }
        if(dbase=='AWAP'){
         if(nyears==1){
           avetemp<-(sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2)
           tannulrun<-rep(avetemp,365)
         }else{
           if(nrow(TMAXX)==1){
           avetemp<-colMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
           }else{
           avetemp<-rowMeans(cbind(TMAXX, TMINN), na.rm=TRUE)
           }
           #library("TTR")
           #tannulrun<-SMA(avetemp,n=365)
           if(length(TMAXX)<365){
             tannulrun<-rep((sum(TMAXX)+sum(TMINN))/(length(TMAXX)*2),length(TMAXX))
           }else{
           tannulrun<-movingFun(avetemp,n=365,fun=mean,type='to')
           yearone<-rep((sum(TMAXX[1:365])+sum(TMINN[1:365]))/(365*2),365)
           tannulrun[1:365]<-yearone
           # SST
           }
#            SST<-read.csv("C:/NicheMapR_Working/projects/SeaTurtles/SST.csv")
#            SST<-subset(SST,year>=ystart & year<=yfinish)
#            days<-seq(1,365*nyears)
#            SST2 <-spline(SST$Day,SST$SST_D,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
#            #SST2 <-spline(SST$Day,SST$SST_G,n=timeinterval*nyears,xmin=1,xmax=365*nyears,method="periodic")
#            SST2<-SST2$y
#            window<-1
#            SSTrun<-movingFun(SST2,n=window,fun=mean,type='to')
#            yearone<-rep(sum(SST2[1:window]/window),window)
#            SST2<-SSTrun
#            SST2[1:window]<-yearone
           #tannulrun<-tannulrun+(SST2-tannulrun)
           #tannulrun<-rowMeans(cbind(tannulrun,SST2),na.rm=TRUE)
           #tannulrun<-SST2
         }
        }else{
          if(dbase!= 'BOM'){
          tannulrun<-rep(tannul,julnum)
          }
        }
        
        if(dbase=='ausclim' || dbase=='AWAP'){
          if(grasshade==0){
            MAXSHADES <- MAXSHADES # daily max shade (%)
            if(manualshade==1){
             MINSHADES <- (1:timeinterval)*0+minshade # daily min shade (%)
            }else{
             #MINSHADES<-MAXSHADES
             MINSHADES <- (1:timeinterval)*0+minshade # daily min shade (%)
             
            }
          }
        }
        if(dbase=='global'){
          MAXSHADES <- (1:timeinterval)*0+maxshade # daily max shade (%)
          MINSHADES <- (1:timeinterval)*0+minshade # daily min shade (%)
        }
        if(dbase=='bom'){
          if(grasshade==0){
            MAXSHADES <- (1:length(dayclim$date)*0+maxshade) # daily max shade (%)
            MINSHADES <- (1:length(dayclim$date)*0+minshade) # daily min shade (%)
          }
        }
        if(weeknum>1){
          week<-julday[1:(7*(weeknum-1))]
          julday<-julday[-(1:(7*(weeknum-1)))]
          julday<-c(julday,week)
          week<-TMAXX[(1:(7*(weeknum-1)))]
          TMAXX<-TMAXX[-(1:(7*(weeknum-1)))]
          TMAXX<-c(TMAXX,week)
          week<-TMINN[(1:(7*(weeknum-1)))]
          TMINN<-TMINN[-(1:(7*(weeknum-1)))]
          TMINN<-c(TMINN,week)
          week<-RHMAXX[(1:(7*(weeknum-1)))]
          RHMAXX<-RHMAXX[-(1:(7*(weeknum-1)))]
          RHMAXX<-c(RHMAXX,week)
          week<-RHMINN[(1:(7*(weeknum-1)))]
          RHMINN<-RHMINN[-(1:(7*(weeknum-1)))]
          RHMINN<-c(RHMINN,week)
          week<-WNMAXX[(1:(7*(weeknum-1)))]
          WNMAXX<-WNMAXX[-(1:(7*(weeknum-1)))]
          WNMAXX<-c(WNMAXX,week)
          week<-WNMINN[(1:(7*(weeknum-1)))]
          WNMINN<-WNMINN[-(1:(7*(weeknum-1)))]
          WNMINN<-c(WNMINN,week)
          week<-CCMAXX[(1:(7*(weeknum-1)))]
          CCMAXX<-CCMAXX[-(1:(7*(weeknum-1)))]
          CCMAXX<-c(CCMAXX,week)
          week<-CCMINN[(1:(7*(weeknum-1)))]
          CCMINN<-CCMINN[-(1:(7*(weeknum-1)))]
          CCMINN<-c(CCMINN,week)
        }
        hori<-as.matrix(hori)
        if(dbase=='AWAP'){
          julday<-rep(julday,nyears)
          ida<-ndays
          if(grasshade==0){
            MINSHADES<-rep(MINSHADES[1],ndays)
          }
          SNOW<-rep(SNOW[1],ndays)
          REFLS<-rep(REFLS[1],ndays)
          #PCTWET<-rep(PCTWET,nyears)
        }
        if(dbase=='global'){
          julday<-rep(julday,nyears)
          ida<-ida*nyears
          MAXSHADES<-rep(MAXSHADES,nyears)
          MINSHADES<-rep(MINSHADES,nyears)
          SNOW<-rep(SNOW,nyears)
        }

        if(dbase=='AWAP'){
          # correct for fact that wind is measured at 10 m height
          # wind shear equation v / vo = (h / ho)^a
          #where
          #v = the velocity at height h (m/s)
          #vo = the velocity at height ho (m/s)
          #a = the wind shear exponent
          #Terrain   Wind Shear Exponent
          #- a -
          #  Open water 	0.1
          #Smooth, level, grass-covered 	0.15
          #Row crops 	0.2
          #Low bushes with a few trees 	0.2
          #Heavy trees 	0.25
          #Several buildings 	0.25
          #Hilly, mountainous terrain 	0.25
            if(dailywind!=1){
          WNMINN<-WNMINN*(1.2/10)^0.15*.1 # reduce min wind further because have only 9am/3pm values to get max/min
          WNMAXX<-WNMAXX*(1.2/10)^0.15
          WNMINN<-WNMINN#*3.25 # for snow
          WNMAXX<-WNMAXX#*3.25 # for snow
          cat('min wind * 0.1 ')
          #cat('max wind * 2.0 for snow ')
          }else{

              if(snowmodel==0){
              WNMAXX<-dwind*(1.2/2)^0.15
              WNMINN<-WNMAXX
              WNMAXX<-WNMAXX*2#*3.5#*5
              WNMINN<-WNMINN*0.5#1.5#*3.5#*2
              cat('min wind * 0.5')
              cat('max wind * 2')
              }else{
                WNMAXX<-dwind*(1.2/2)^0.15
                WNMINN<-WNMAXX
                WNMAXX<-WNMAXX*2#*2.5#*3.5#*5
                WNMINN<-WNMINN*0.5#*3#1.5#*3.5#*2
                cat('min wind * 0.5 * 1.5 for snow ')
                cat('max wind * 2 * 2.5 for snow')
              }
          }
          CCMINN<-CCMINN*0.5
          CCMAXX<-CCMAXX*2
          CCMINN[CCMINN>100]<-100
          CCMAXX[CCMAXX>100]<-100
          cat('min cloud * 0.5 ')
          cat('max cloud * 2')
        }

        TMAXX<-TMAXX+warm
        TMINN<-TMINN+warm


        hori<-as.matrix(hori)
        if(soildata!=1){
          if(vlsci_setup==1){
          SLES<-matrix(nrow=timeinterval*nyears,data=0) 
          }else{
            SLES<-matrix(nrow=7300,data=0) 
          }
          SLES<-SLES+SLE
        }
        
#         #quick fix to make it so that MINSHADES is from the FAPAR database and MAXSHADES is fixed at the user specified value
#         if(soildata==1 & manualshade==0){
#           MAXSHADES[1:length(MAXSHADES)]<-maxshade
#         }
        
        #quick fix to make it so that MINSHADES is at the user-specified value and MAXSHADES is from the FAPAR database 
        if(soildata==1 & manualshade==0){
          MINSHADES<-MAXSHADES
          MINSHADES[1:length(MINSHADES)]<-minshade
        }
        
        
    if(snowmodel==1){
        microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,SLOPE,AZMUTH,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch,rainmelt)
    }else{
            microinput<-c(julnum,RUF,ERR,Usrhyt,Numtyps,Numint,Z01,Z02,ZH1,ZH2,idayst,ida,HEMIS,ALAT,AMINUT,ALONG,ALMINT,ALREF,SLOPE,AZMUTH,ALTT,CMH2O,microdaily,tannul,EC,VIEWF,snowtemp,snowdens,snowmelt,undercatch)
     }
       if(soildata==1){
        moists2<-matrix(nrow=10, ncol = ndays, data=0)
        soilwet[soilwet>0] = 1
        moists2[1,]<-uppermoists# soilwet #moists2[1,]*0+0#uppermoists
        moists2[2,]<-uppermoists
        moists2[3,]<-lowermoists
        moists2[4,]<-lowermoists
        moists<-moists2
       }else{
         if(dbase!='global'){
        moists2<-matrix(nrow=10, ncol = ndays, data=0)
        moists2[1,ndays]<-SoilMoist[1]
        moists<-moists2
         }
       }
        
       if(dbase=='global'){
         if(timeinterval==365){
         moists2<-matrix(nrow=10, ncol = 365*nyears, data=0)
         #moists2[1,]<-rep(SoilMoist,nyears)# soilwet #moists2[1,]*0+0#uppermoists
         moists2[1,]<-soilmoist# soilwet #moists2[1,]*0+0#uppermoists
         moists2[2,]<-moists2[1,]
         moists2[3,]<-moists2[1,]
         moists2[4,]<-moists2[1,]
         moists<-moists2
         }else{
           moists2<-matrix(nrow=10, ncol = timeinterval, data=0)
           moists2[1,]<-SoilMoist
           moists2[2,]<-moists2[1,]
           moists2[3,]<-moists2[1,]
           moists2[4,]<-moists2[1,]
           moists<-moists2
         }
       } 
        
        soilprops<-matrix(data = 0, nrow = 10, ncol = 6)
        
       if(soildata==1){
        if(is.na(soilprop$BBDensity50)==FALSE){
        # soil properties (bulk density, saturated water content, proportion clay) for each level
          soilprops[1,1]<-soilprop$ABDensity50#*.65 # bulk density
          soilprops[2,1]<-soilprop$ABDensity50 
          soilprops[3,1]<-soilprop$BBDensity50#*.7#soilprop$BBDensity50
          soilprops[4,1]<-soilprop$BBDensity50#*.7#soilprop$BBDensity50
          soilprops[1,2]<-soilprop$A_01bar     # saturated water content
          soilprops[2,2]<-soilprop$A_01bar
          soilprops[3,2]<-soilprop$B_01bar
          soilprops[4,2]<-soilprop$B_01bar
          soilprops[1,3]<-soilprop$Aclay50     # percent clay
          soilprops[2,3]<-soilprop$Aclay50
          soilprops[3,3]<-soilprop$Bclay50
          soilprops[4,3]<-soilprop$Bclay50
        }else{
          # soil properties (bulk density, saturated water content, proportion clay) for each level
          soilprops[1,1]<-soilprop$ABDensity50#*.65 # bulk density
          soilprops[2,1]<-soilprop$ABDensity50 
          soilprops[3,1]<-soilprop$ABDensity50
          soilprops[4,1]<-soilprop$ABDensity50
          soilprops[1,2]<-soilprop$A_01bar     # saturated water content
          soilprops[2,2]<-soilprop$A_01bar
          soilprops[3,2]<-soilprop$A_01bar
          soilprops[4,2]<-soilprop$A_01bar
          soilprops[1,3]<-soilprop$Aclay50     # percent clay
          soilprops[2,3]<-soilprop$Aclay50
          soilprops[3,3]<-soilprop$Aclay50
          soilprops[4,3]<-soilprop$Aclay50
        }
        if(cap==1){
        soilprops[1,4]<-0.2#1.#Thcond#0.25 #0.07 #Thcond #0.07#Thcond #0.03 #2.5                # mineral thermal conductivity
        }else{
          soilprops[1,4]<-Thcond
        }
        soilprops[2,4]<-Thcond
        soilprops[3,4]<-Thcond
        soilprops[4,4]<-Thcond
        if(cap==1){
        soilprops[1,5]<-1920#SpecHeat#1920#1320           # mineral heat capacity
        }else{
          soilprops[1,5]<-SpecHeat 
        }
        soilprops[2,5]<-SpecHeat
        soilprops[3,5]<-SpecHeat
        soilprops[4,5]<-SpecHeat
        soilprops[1,6]<-Density#1.3 #2.65
        soilprops[2,6]<-Density
        soilprops[3,6]<-Density
        soilprops[4,6]<-Density
       }else{
         soilprops[1,1]<-BulkDensity 
         soilprops[1,2]<-SatWater    
         soilprops[1,3]<-Clay       
         soilprops[1,4]<-Thcond 
         soilprops[1,5]<-SpecHeat        
         soilprops[1,6]<-Density 
       }
      
      if(dbase=='global'){
        soilprops[1,1]<-BulkDensity
        soilprops[2,1]<-BulkDensity 
        soilprops[3,1]<-BulkDensity
        soilprops[4,1]<-BulkDensity
        soilprops[1,2]<-0.     # saturated water content
        soilprops[2,2]<-SatWater
        soilprops[3,2]<-SatWater
        soilprops[4,2]<-SatWater
        soilprops[1,3]<-Clay     # percent clay
        soilprops[2,3]<-Clay
        soilprops[3,3]<-Clay
        soilprops[4,3]<-Clay
        soilprops[1,4]<-0.2 #0.07 #Thcond #0.07#Thcond #0.03 #2.5                # mineral thermal conductivity
        soilprops[2,4]<-Thcond
        soilprops[3,4]<-Thcond
        soilprops[4,4]<-Thcond
        soilprops[1,5]<-1920#1320           # mineral heat capacity
        soilprops[2,5]<-SpecHeat
        soilprops[3,5]<-SpecHeat
        soilprops[4,5]<-SpecHeat
        soilprops[1,6]<-Density#1.3 #2.65
        soilprops[2,6]<-Density
        soilprops[3,6]<-Density
        soilprops[4,6]<-Density
      }  
        soilprops<-(ifelse(is.na(soilprops),0,soilprops))
        #REFLS<-REFLS*0+0.1
        #PCTWET[PCTWET<50]<-0
        
                ############## manual soil properties ######################
#                         Intrvls <-(1:julnum) # user-supplied last Julian day in each time interval sequence
#                         Numint <- julnum  # number of time intervals
#                         Numtyps <- 4
#                  
#                         Nodes <- matrix(data = 0, nrow = 10, ncol = 7300) # deepest nodes for each substrate type
#                         Nodes[1,]<-Nodes[1,]*0+4
#                         Nodes[2,]<-Nodes[2,]*0+5
#                         Nodes[3,]<-Nodes[3,]*0+6
#                         Nodes[4,]<-Nodes[4,]*0+9#[4,]*0+9
#                 
#                                 soilprops[1,1]<- 0.65#1.4# soil bulk density Mg/Mg
#                                 soilprops[2,1]<-1.4
#                                 soilprops[3,1]<-1.4
#                                 soilprops[4,1]<-1.4
#                                 soilprops[1,2]<- 0.4# saturated water content m3/m3
#                                 soilprops[2,2]<-0.4
#                                 soilprops[3,2]<-0.4
#                                 soilprops[4,2]<-0.4
#                                 soilprops[1,3]<-0 # percent clay
#                                 soilprops[2,3]<-0
#                                 soilprops[3,3]<-0
#                                 soilprops[4,3]<-0
#                                 soilprops[1,4]<- 0.2# mineral thermal conductivity (W/mC)
#                                 soilprops[2,4]<-2.5
#                                 soilprops[3,4]<-2.5
#                                 soilprops[4,4]<-2.5
#                                 soilprops[1,5]<- 1920#870# mineral head capacity (J/kgK)
#                                 soilprops[2,5]<-870
#                                 soilprops[3,5]<-870
#                                 soilprops[4,5]<-870
#                                 soilprops[1,6]<- 1.3#2.65# mineral density (Mg/Mg)
#                                 soilprops[2,6]<- 2.65
#                                 soilprops[3,6]<- 2.65
#                                 soilprops[4,6]<-2.65
#         
#                 moists[1,]<-moists[1,]*0#0.2#+0
#                 moists[2,]<-moists[2,]*0#*0.2#+0
#                 moists[3,]<-moists[3,]*0#*0.2#+0
#                 moists[4,]<-moists[4,]*0#*0.2#+0#*0.2#+0
        
         #PCTWET <- (1:(timeinterval*nyears))*0+30
         #hori<-rep(45,24)
         #RAINFALL[249]<-100
        
        #REFLS<-REFLS*0+0.2 # soil reflectances (decimal percent) # Cape Range
        #                 #REFLS<-REFLS*0+0.45 # soil reflectances (decimal percent) # Dirk Hartog
        #                 
        #                 #PCTWET<-PCTWET#*0+0.3 # percentage of unit surface area of ground that is wet (decimal percent)
        #                SLES<-SLES*0+0.96 # Substrate longwave IR emissivity (decimal %)
        #                 
        #                # DEP<-as.matrix(c(0., 2.5,  5.,  10., 15.,  30.,  40.,  50.,  60.,  250.)) # Soil nodes (cm)
        #                 
        #                 
        #                 #microinput[18]<-0 # slope (degrees)
        #                 #microinput[19]<-0 # aspect (degrees, 0 = North)
        #                 #microinput[22]<-1 # cm H2O in air column
        #                 #microinput[4] <- 1 # local height (cm) at which animal/container calculations will be made
        #                 #microinput[2]<-0.002 # Roughness height (m) (make sure it is smaller than user height)
        #                 
        #                 #WNMAXX<-WNMAXX*.45 # Change wind speed
        #                 #WNMINN<-WNMINN*.1 # Change wind speed
        #         ############################# end manual soil properties #######################
        #    
        
############## manual soil properties sea turtles ######################
#         soilprops[1,1]<- 1.29# soil bulk density Mg/Mg
#         soilprops[2,1]<-1.29
#         soilprops[3,1]<-1.29
#         soilprops[4,1]<-1.29
#         soilprops[1,2]<- 0.4# saturated water content m3/m3
#         soilprops[2,2]<-0.4
#         soilprops[3,2]<-0.4
#         soilprops[4,2]<-0.4
#         soilprops[1,3]<-0 # percent clay
#         soilprops[2,3]<-0
#         soilprops[3,3]<-0
#         soilprops[4,3]<-0
#         soilprops[1,4]<- 8.8# mineral thermal conductivity (W/mC)
#         soilprops[2,4]<-8.8
#         soilprops[3,4]<-8.8
#         soilprops[4,4]<-8.8
#         soilprops[1,5]<- 800# mineral head capacity (J/kgK)
#         soilprops[2,5]<-800
#         soilprops[3,5]<-800
#         soilprops[4,5]<-800
#         soilprops[1,6]<- 2.66# mineral density (Mg/Mg)
#         soilprops[2,6]<- 2.66
#         soilprops[3,6]<- 2.66
#         soilprops[4,6]<-2.66
#         
#         moists[1,]<-moists[1,]*0+.9
#         moists[2,]<-moists[2,]*0+.9
#         moists[3,]<-moists[3,]*0+.9
#         moists[4,]<-moists[4,]*0+.9
#         
#         Nodes[1,]<-Nodes[1,]*0+2
#         Nodes[2,]<-Nodes[2,]*0+4
#         Nodes[3,]<-Nodes[3,]*0+6
#         Nodes[4,]<-Nodes[4,]*0+9
#         
#         #REFLS<-REFLS*0+0.60 # soil reflectances (decimal percent) # Cape Range
#         REFLS<-REFLS*0+0.30 # soil reflectances (decimal percent) # Dirk Hartog (actually - changed for snow, fix this)
#         #REFLS<-REFLS*0+0.62 # soil reflectances (decimal percent) # Gnaraloo
#         
#         PCTWET<-PCTWET#*0+0.3 # percentage of unit surface area of ground that is wet (decimal percent)
#         SLES<-SLES#*0+ # Substrate longwave IR emissivity (decimal %)
#         
#        # DEP<-as.matrix(c(0., 2.5,  5.,  10., 15.,  30.,  40.,  50.,  60.,  250.)) # Soil nodes (cm)
#         
#         
#         microinput[18]<-0 # slope (degrees)
#         microinput[19]<-0 # aspect (degrees, 0 = North)
#         microinput[22]<-1 # cm H2O in air column
#         microinput[4] <- 1 # local height (cm) at which animal/container calculations will be made
#         microinput[2]<-0.002 # Roughness height (m) (make sure it is smaller than user height)
#         WNMAXX.orig<-WNMAXX
#         WNMAXX<-WNMAXX.orig*1.5#*(1.8*1.3)#*1.8#*.45 # Change wind speed
#         WNMINN<-WNMAXX.orig*0.5#*(.5*1.3)#*.45 # Change wind speed
#         
#         TMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/TMAXX_obs.csv", sep = ",",header=TRUE)
#         names(TMAXX)<-NULL
#         TMAXX<-as.matrix(TMAXX[-1])
#         TMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/TMINN_obs.csv", sep = ",",header=TRUE)
#         names(TMINN)<-NULL
#         TMINN<-as.matrix(TMINN[-1])
#         RHMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/RHMAXX_obs.csv", sep = ",",header=TRUE)
#         names(RHMAXX)<-NULL
#         RHMAXX<-as.matrix(RHMAXX[-1])
#         RHMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/RHMINN_obs.csv", sep = ",",header=TRUE)
#         names(RHMINN)<-NULL
#         RHMINN<-as.matrix(RHMINN[-1])
#         CCMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/CCMAXX_obs.csv", sep = ",",header=TRUE)
#         names(CCMAXX)<-NULL
#         CCMAXX<-as.matrix(CCMAXX[-1])
#         CCMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/CCMINN_obs.csv", sep = ",",header=TRUE)
#         names(CCMINN)<-NULL
#         CCMINN<-as.matrix(CCMINN[-1])
#         WNMAXX<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/WNMAXX_obs.csv", sep = ",",header=TRUE)
#         names(WNMAXX)<-NULL
#         WNMAXX<-as.matrix(WNMAXX[-1])
#         WNMINN<-read.table("/NicheMapR_Working/projects/SeaTurtles/Dirk Hartog Test/WNMINN_obs.csv", sep = ",",header=TRUE)
#         names(WNMINN)<-NULL
#         WNMINN<-as.matrix(WNMINN[-1])
# #         
#         WNMAXX.orig<-WNMAXX
#         WNMINN.orig<-WNMINN
#         WNMAXX<-WNMAXX.orig#*1.3#*.45 # Change wind speed
#         WNMINN<-WNMINN.orig#*1.3#*.5#*.45 # Change wind speed
        ############################# end manual soil properties #######################
        
        
        if(dbase=="global"){
         #RAINFALL<-rep(0,timeinterval*nyears)
         #REFLS<-rep(0.20,timeinterval*nyears)
         #TMAXX<-TMAXX+adiab_corr # SCAN test, comment this out
         #TMINN<-TMINN+adiab_corr # SCAN test, comment this out
        }        
        
        
        
if(loop>0){
  TMAXX<-c(TMAXX[((loop)*365+1):(nyears*365)],TMAXX[1:((loop)*365)])
  TMINN<-c(TMINN[((loop)*365+1):(nyears*365)],TMINN[1:((loop)*365)])
  RHMAXX<-c(RHMAXX[((loop)*365+1):(nyears*365)],RHMAXX[1:((loop)*365)])
  RHMINN<-c(RHMINN[((loop)*365+1):(nyears*365)],RHMINN[1:((loop)*365)])
  CCMAXX<-c(CCMAXX[((loop)*365+1):(nyears*365)],CCMAXX[1:((loop)*365)])
  CCMINN<-c(CCMINN[((loop)*365+1):(nyears*365)],CCMINN[1:((loop)*365)])
  WNMAXX<-c(WNMAXX[((loop)*365+1):(nyears*365)],WNMAXX[1:((loop)*365)])
  WNMINN<-c(WNMINN[((loop)*365+1):(nyears*365)],WNMINN[1:((loop)*365)])
  PCTWET<-c(PCTWET[((loop)*365+1):(nyears*365)],PCTWET[1:((loop)*365)])
  moists<-cbind(moists[,((loop)*365+1):(nyears*365)],moists[,1:((loop)*365)])
  RAINFALL<-c(RAINFALL[((loop)*365+1):(nyears*365)],RAINFALL[1:((loop)*365)])
  grassgrowths<-c(grassgrowths[((loop)*365+1):(nyears*365)],grassgrowths[1:((loop)*365)])
  
}

        micro<-list(microinput=microinput,julday=julday,SLES=SLES,DEP=DEP,Intrvls=Intrvls,Nodes=Nodes,MAXSHADES=MAXSHADES,MINSHADES=MINSHADES,TIMAXS=TIMAXS,TIMINS=TIMINS,TMAXX=TMAXX,TMINN=TMINN,RHMAXX=RHMAXX,RHMINN=RHMINN,CCMAXX=CCMAXX,CCMINN=CCMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,SNOW=SNOW,REFLS=REFLS,PCTWET=PCTWET,soilinit=soilinit,hori=hori,TAI=TAI,soilprops=soilprops,moists=moists,RAINFALL=RAINFALL,tannulrun=tannulrun)
        #micro<-list(julnum=julnum,julday=julday,RUF=RUF,SLES=SLES,ERR=ERR,Usrhyt=Usrhyt,DEP=DEP,Numtyps=Numtyps,Numint=Numint,Thconds=Thconds,Densitys=Densitys,Spheats=Spheats,Intrvls=Intrvls,Nodes=Nodes,Z01=Z01,Z02=Z02,ZH1=ZH1,ZH2=ZH2,idayst=idayst,ida=ida,MAXSHADES=MAXSHADES,MINSHADES=MINSHADES,HEMIS=HEMIS,ALAT=ALAT,AMINUT=AMINUT,ALONG=ALONG,ALMINT=ALMINT,ALREF=ALREF,SLOPE=SLOPE,AZMUTH=AZMUTH,ALTT=ALTT,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,TMAXX=TMAXX,TMINN=TMINN,RHMAXX=RHMAXX,RHMINN=RHMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,SNOW=SNOW,REFLS=REFLS,PCTWET=PCTWET,soilinit=soilinit,microdaily=microdaily,tannul=tannul,hori=hori,TAI=TAI,EC=EC,VIEWF=VIEWF)

        
        if(write_input==1){
#           #cat(input_dir_micro)
#           if (file.exists(input_dir_micro)){
#             setwd(input_dir_micro)
#           } else {
#             dir.create(input_dir_micro,recursive=TRUE)
#             setwd(input_dir_micro)
#           }
            if(vlsci==0){
              setwd("/NicheMapR/microclimate")
            }else{
              setwd("/vlsci/VR0212/shared/NicheMapR_Working/microclimate")
            }
        write.table(as.matrix(microinput), file = "microinput.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(julday, file = "julday.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(SLES, file = "SLES.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(DEP, file = "DEP.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(Intrvls, file = "Intrvls.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(Nodes, file = "Nodes.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(MAXSHADES, file = "Maxshades.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(MINSHADES, file = "Minshades.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(TIMAXS, file = "TIMAXS.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(TIMINS, file = "TIMINS.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(TMAXX, file = "TMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(TMINN, file = "TMINN.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(RHMAXX, file = "RHMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(RHMINN, file = "RHMINN.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(CCMAXX, file = "CCMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(CCMINN, file = "CCMINN.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(WNMAXX, file = "WNMAXX.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(WNMINN, file = "WNMINN.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(SNOW, file = "SNOW.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(REFLS, file = "REFLS.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(PCTWET, file = "PCTWET.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(soilinit, file = "soilinit.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(hori, file = "hori.csv", sep = ",", col.names = NA, qmethod = "double")
         write.table(TAI, file = "TAI.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(soilprops, file="soilprop.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(moists,file="moists.csv", sep = ",", col.names = NA, qmethod = "double")
        write.table(RAINFALL,file="rain.csv", sep = ",", col.names = NA, qmethod = "double")
          write.table(tannulrun,file="tannulrun.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(grassgrowths, file = "grassgrowths.csv", sep = ",", col.names = NA, qmethod = "double")

        }

        #climates<-cbind(TMAXX,TMINN,WNMAXX,WNMINN,RHMAXX,RHMINN,CCMAXX,CCMINN)
          if(dbase=='AWAP'){
            #write.table(climates, file = "climates_awap.csv", sep = ",", col.names = NA, qmethod = "double") 
          }
          if(dbase=='bom'){  
            #write.table(climates, file = "climates_bom.csv", sep = ",", col.names = NA, qmethod = "double")
            #write.table(dayclim, file = "dayclim.csv", sep = ",", col.names = NA, qmethod = "double")
          }
        if(is.na(TMAXX[1])!=TRUE & TMAXX[1]!=-9999 & CCMAXX[1]!=-9999 & RHMAXX[1]!=-9999 & WNMAXX[1]!=-9999){
          if(vlsci_setup==0){
          wetlandTemps=matrix(data = 0., nrow = 24*7300, ncol = 1)
          wetlandDepths=matrix(data = 0., nrow = 24*7300, ncol = 1)
          }else{
            wetlandTemps=matrix(data = 0., nrow = 24*timeinterval*nyears, ncol = 1)
            wetlandDepths=matrix(data = 0., nrow = 24*timeinterval*nyears, ncol = 1)
          }
          if(vlsci_setup==0){
            if(vlsci==0){
              setwd("/NicheMapR/microclimate")
            }else{
              setwd("/vlsci/VR0212/shared/NicheMapR_Working/microclimate")
            }
            if(snowmodel==0){
            source('NicheMapperMicro.R')
             }else{
            source('NicheMapperMicroSnow.R') 
            }
          microut<-microclimate(micro)
          
          data_out<-1
          metout<-microut$metout
          shadmet<-microut$shadmet
          soil<-microut$soil
          shadsoil<-microut$shadsoil
 
          if(wetmod==1){ 
            days<-seq(1,365*nyears)
            RAINFALL2<-as.matrix(cbind(days,RAINFALL/24))
            for(k in 1:24){
              rainfall2<-cbind(rep(k,365),RAINFALL2)
              if(k==1){
                rainfall3<-rainfall2
              }else{
                rainfall3<-rbind(rainfall2,rainfall3)
              }
            }
            rainfall3<-as.data.frame(rainfall3)
            precip<-rainfall3[order(rainfall3$days,rainfall3$V1),]
            climfile<-metout[1:(24*julnum),1:14]
            nums<-seq(1,(24*julnum))
            climfile<-cbind(nums,climfile)
            precip<-cbind(climfile[,1:3],precip[,3])
            setwd("/NicheMapR_Working/projects/WST")
            #write.table(climfile, file = "climfile.csv", sep = ",", col.names = NA, qmethod = "double")
            #write.table(precip, file = "precip.csv", sep = ",", col.names = NA, qmethod = "double")
            
            setwd("/NicheMapR_Working/Wet0D")
            source('wetland2.R')
            res<-wetland2(climfile,precip)
            wetlandTemps[1:(julnum*24)] <- res$results[,1]
            wetlandTemps[wetlandTemps<(-30)]<-0
            wetlandTemps[wetlandTemps>100]<-0
            wetlandTemps <- (ifelse(is.na(wetlandTemps),0,wetlandTemps)) # get rid of na
            wetlandDepths[1:(julnum*24)] <- res$results[,5]*1000-3550
            wetlandDepths[wetlandDepths<50]<-0
          } #end wetland model
          
        
          # summarise hydroperiod in months per year
          if(wetmod==1 & container==1){
            count <- function(x) {
              length(na.omit(x))
            }
            tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
            dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours") 
            
            if(dbase=='AWAP' | dbase=='global'){
              dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
            }  
            dates<-subset(dates, !duplicated(as.matrix(dates[2110:2120])))
            dates<-unique(dates)
            wet.times<-cbind(dates,as.data.frame(wetlandDepths[1:(nyears*365*24),]))
            colnames(wet.times)<-c('date','depth')
            wet.times<-subset(wet.times,depth>0)
            hydroperiods<-as.data.frame(aggregate(wet.times$depth,by=list((substr(wet.times$date,1,4))),count))
            hydroperiods$x<-hydroperiods$x/(30.5*24)
            hydrolabels<-seq(ystart,yfinish)
            dummy.periods<-cbind(seq(ystart,yfinish),rep(0,nyears))
            for(c in 1:nrow(hydroperiods)){
            for(d in 1:nyears){
              if(dummy.periods[d,1]==hydroperiods[c,1]){
                 dummy.periods[d,2]<-hydroperiods[c,2]
            }}}
            hydroperiods<-t(dummy.periods[,2])
            colnames(hydroperiods)<-hydrolabels
          }
         } #end vlsci check for running microclimate model
          
          if(ectomodel==1){
            # Ectotherm model input data
            
            # habitat
            ALT<-ALTT # altitude (m)
            OBJDIS<-1.0 # distance from object (e.g. bush)
            OBJL<-0.0001
            PCTDIF<-0.1 # percent of sunlight that is diffuse (decimal %)
            EMISSK<-1.0 # emissivity of the sky (decimal %)
            EMISSB<-1.0 # emissivity of the substrate (decimal %)
            ABSSB<-1-mean(REFLS) # solar absorbtivity of the substrate (decimal %)
            shade<-minshade # shade (%)
            if(monthly==2){
              RAINFALL<-RAINFALL2
            }
            # animal properties
            AMASS<-amass/1000 # animal mass (kg)
            absan<-ABSMAX # animal solar absorbtivity
            RQ<-0.8 # respiratory quotient
            
            FATOBJ<-0.
            #  if(container==1){
            #    live<-0}else{live<-1
            #  }
            #live<-1
            TIMBAS<-1.
            #  if(container==1){
            #    SKINW<-100.}else{
            SKINW<-skinwet
            #    }
            skint<-0.
            O2gas<-20.95
            CO2gas<-0.03
            N2gas<-79.02
            gas<-c(O2gas,CO2gas,N2gas)
            #  if(container==1){
            #    transt<-1
            #  }else{
            transt<-0
            #  }
            tranin<-1
            if(vlsci==0 & vlsci_setup==0){
             tcinit<-metout[1,"TALOC"]
            }else{
             tcinit<-(TMAXX[1]+TMINN[1])/2
            }
            ACTLVL<-1
            nodnum<-length(DEP)
            spec<-0. # spectacle covering eye surface? (adds to water loss for lizard/frog/turtle geometry)
            xbas<-1.
            nofood<-0 
            tdigpr<-TPREF 
            o2max<-extref
            #  if(container==1){
            #	maxshd<-1.
            #  minshd<-0.
            #  }else{
            maxshd<-maxshade
            minshd<-minshade
            #  }
            behav=c(dayact,nocturn,crepus,rainact,burrow,CkGrShad,climb,fosorial,nofood)
            julday<-1
            
            # DEB model initial conditions
            V_init_baby<-3e-9
            E_init_baby<-E_Egg/V_init_baby
            E_baby_init<-E_init_baby
            V_baby_init<-V_init_baby
            ms_init<-0.
            cumrepro_init<-0.
            q_init<-0.
            hs_init<-0.
            cumbatch_init<-0.
            pregnant<-0
            E_m<-(p_Mref*z/kappa)/v_dotref
            
            # conversions from percent to proportion
            PTUREA1<-PTUREA/100
            PFEWAT1<-PFEWAT/100
            FoodWater1<-FoodWater/100
            water_stages[,3]<-water_stages[,3]/100
            water_stages[,4]<-water_stages[,4]/100
            water_stages[,5]<-water_stages[,5]/100
            eggmass<-0 # initial dry mass of an egg (g) - no longer used so delete
            
            #DEB mass balance calculations
            nO<-cbind(nX,nV,nE,nP) # matrix of composition of organics, i.e. food, structure, reserve and faeces
            CHON<-c(12,1,16,14)
            wO<-CHON%*%nO
            w_V=wO[3]
            M_V<-d_V/w_V
            yEX<-kappa_X*mu_X/mu_E # yield of reserve on food
            yXE<-1/yEX # yield of food on reserve
            yVE<-mu_E*M_V/E_G  # yield of structure on reserve
            yPX<-kappa_X_P*mu_X/mu_P # yield of faeces on food
            yXP<-1/yPX # yield of food on faeces
            yPE<-yPX/yEX # yield of faeces on reserve  0.143382353
            nM<-matrix(c(1,0,2,0,0,2,1,0,0,0,2,0,N_waste),nrow=4)
            N_waste_inv<-c(-1*N_waste[1]/N_waste[4],(-1*N_waste[2])/(2*N_waste[4]),(4*N_waste[1]+N_waste[2]-2*N_waste[3])/(4*N_waste[4]),1/N_waste[4])
            nM_inv<-matrix(c(1,0,-1,0,0,1/2,-1/4,0,0,0,1/2,0,N_waste_inv),nrow=4)
            JM_JO<--1*nM_inv%*%nO
            etaO<-matrix(c(yXE/mu_E*-1,0,1/mu_E,yPE/mu_E,0,0,-1/mu_E,0,0,yVE/mu_E,-1/mu_E,0),nrow=4)
            w_N<-CHON%*%N_waste
            
            lat<-x[,2]
            if(dbase=='bomscraper'){
              julstart<-dayclim.julianday[1]
            }else{
              julstart<-1
            }  
            if(monthly==2){
              julstart<-15
            }
            
            if(vlsci_setup==0){
            if(monthly==2){
              metout2=matrix(data = 0., nrow = 24*7300, ncol = 18) 
              soil2=matrix(data = 0., nrow = 24*7300, ncol = 12)
              shadmet2=matrix(data = 0., nrow = 24*7300, ncol = 18)
              shadsoil2=matrix(data = 0., nrow = 24*7300, ncol = 12)
              wetlandTemps=matrix(data = 0., nrow = 24*7300, ncol = 1)
              wetlandDepths=matrix(data = 0., nrow = 24*7300, ncol = 1)
              metout2[1:(24*12),]<-metout[1:(24*12),]
              shadmet2[1:(24*12),]<-shadmet[1:(24*12),]
              soil2[1:(24*12),]<-soil[1:(24*12),]
              shadsoil2[1:(24*12),]<-shadsoil[1:(24*12),]
              metout<-metout2
              shadmet<-shadmet2
              soil<-soil2
              shadsoil<-shadsoil2
              metout.names<-c("JULDAY","TIME","TALOC","TAREF","RHLOC","RH","VLOC","VREF","TS","T2","TDEEP","ZEN","SOLR","TSKYC","DEW","FROST","SNOWFALL","SNOWDEP")
              colnames(metout)<-metout.names
              colnames(shadmet)<-metout.names
              soil.names<-c("JULDAY","TIME",paste("D",DEP,"cm", sep = ""))
              colnames(soil)<-soil.names
              colnames(shadsoil)<-soil.names
            }
            }
            #metout<-metout[,-14]
            #shadmet<-shadmet[,-14]
            if(metchamber==1){
              metout[,3]<-chambertemp
              metout[,9]<-chambertemp
              metout[,10]<-chambertemp
              metout[,11]<-chambertemp
              metout[,13]<-chambertemp
              metout[,5]<-chamberrh
              metout[,6]<-chamberrh
              metout[,7]<-chamberwind
              metout[,13]<-0.
              
              shadmet<-metout
              soil[,3:12]<-chambertemp
              shadsoil<-soil
            }
            # bucket model for soil moisture
            
            fieldcap<-soilprop$A_01bar*100# %vol, water content at 0.1ba = 10kPa
            wilting<-soilprop$A_15bar*100 # %vol, water content at 15ba = 1500kPa (wiki for thresholds)
            soilmoisture<-1
            if(soilmoisture==1){
              conth<-fieldcap/10
            }
            ectoinput<-c(ALT,FLTYPE,OBJDIS,OBJL,PCTDIF,EMISSK,EMISSB,ABSSB,shade,enberr,AMASS,EMISAN,absan,RQ,rinsul,lometry,live,TIMBAS,Flshcond,Spheat,Andens,ABSMAX,ABSMIN,FATOSK,FATOSB,FATOBJ,TMAXPR,TMINPR,DELTAR,SKINW,spec,xbas,extref,TPREF,ptcond,skint,gas,transt,soilnode,o2max,ACTLVL,tannul,nodnum,tdigpr,maxshd,minshd,ctmax,ctmin,behav,julday,actrainthresh,viviparous,pregnant,conth,contw,contlast,tranin,tcinit,nyears,lat,rainmult,julstart,monthly,customallom,MR_1,MR_2,MR_3,DEB,tester,rho1_3,trans1,aref,bref,cref,phi,wings,phimax,phimin,shape_a,shape_b,shape_c,minwater,microyear,container,flyer,flyspeed,timeinterval,maxdepth,ctminthresh,ctkill,gutfill,mindepth,TBASK,TEMERGE,p_Xm,SUBTK,flymetab,continit,wetmod,contonly,conthole,contype,shdburrow,breedtempthresh,breedtempcum,contwet,fieldcap,wilting)
            debmod<-c(clutchsize,andens_deb,d_V,eggdryfrac,mu_X,mu_E,mu_V,mu_P,T_REF,z,kappa,kappa_X,p_Mref,v_dotref,E_G,k_R,MsM,delta,h_aref,V_init_baby,E_init_baby,k_J,E_Hb,E_Hj,E_Hp,eggmass,batch,breedrainthresh,photostart,photofinish,daylengthstart,daylengthfinish,photodirs,photodirf,svl_met,frogbreed,frogstage,etaO,JM_JO,E_Egg,kappa_X_P,PTUREA1,PFEWAT1,wO,w_N,FoodWater1,f,s_G,K,X,metab_mode,stages,p_Am1,p_AmIm,disc,gam,startday,raindrink,reset,ma,mi,mh,aestivate,depress)
            deblast<-c(iyear,countday,v_init,E_init,ms_init,cumrepro_init,q_init,hs_init,cumbatch_init,V_baby_init,E_baby_init,E_H_init,stage)
            
                        if(write_input==1){
#                           if (file.exists(input_dir_ecto)){
#                             setwd(input_dir_ecto)
#                           } else {
#                             dir.create(input_dir_ecto,recursive=TRUE)
#                             setwd(input_dir_ecto)
#                           }
                        write.table(ectoinput, file = "ectoinput.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(debmod, file = "debmod.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(deblast, file = "deblast.csv", sep = ",", col.names = NA, qmethod = "double")
                       if(vlsci==0 & vlsci_setup==0){
                                                    write.csv(metout, file= "metout.csv", row.names=FALSE)
                                                    write.csv(soil, file= "soil.csv", row.names=FALSE)
                                                    write.csv(shadmet, file= "shadmet.csv", row.names=FALSE)
                                                    write.csv(shadsoil, file= "shadsoil.csv", row.names=FALSE)
                                                  }
                        write.table(RAINFALL, file = "rainfall.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(DEP, file = "dep.csv", sep = ",", col.names = NA, qmethod = "double")
                        #misc<-c(iyear,countday,debmod,deblast)
                        #write.table(misc, file = "misc.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(grassgrowths, file = "grassgrowths.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(grasstsdms, file = "grasstsdms.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(wetlandTemps, file = "wetlandTemps.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(wetlandDepths, file = "wetlandDepths.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(arrhenius, file = "arrhenius.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(thermal_stages, file = "thermal_stages.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(behav_stages, file = "behav_stages.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(water_stages, file = "water_stages.csv", sep = ",", col.names = NA, qmethod = "double")
                        write.table(MAXSHADES, file = "Maxshades.csv", sep = ",", col.names = NA, qmethod = "double")
                        }
#             grassgrowths[288:460]<-0 # for test against Kerr and Bull 2004
#             for(i in 1:length(grassgrowths)){
#               if(grassgrowths[i]==0){
#                 if(RAINFALL[i]>=5){
#                   grassgrowths[i]<-15
#                 }
#               }
#             }
                        
            if(vlsci==0){
              setwd("/NicheMapR/ectotherm")
            }else{
              setwd("/hsm/VR0212/shared/NicheMapR_Working/ectotherm")
            } 
            ecto<-list(ectoinput=ectoinput,metout=metout,shadmet=shadmet,soil=soil,shadsoil=shadsoil,DEP=DEP,RAINFALL=RAINFALL,iyear=iyear,countday=countday,debmod=debmod,deblast=deblast,grassgrowths=grassgrowths,grasstsdms=grasstsdms,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,arrhenius=arrhenius,thermal_stages=thermal_stages,behav_stages=behav_stages,water_stages=water_stages,MAXSHADES=MAXSHADES)
            source('NicheMapperItadayYears.r') 
           
            
            ectout<-ectotherm(ecto)
            
            if(microyear==1 | dbase=='global'){
              environ<-ectout$environ[1:(julnum*24),]
              enbal<-ectout$enbal[1:(julnum*24),]
              masbal<-ectout$masbal[1:(julnum*24),]
              debout<-ectout$debout[1:(julnum*24),]
              yearout<-ectout$yearout
              yearsout<-ectout$yearsout[1:nyears,]
            }else{
              environ<-ectout$environ[1:(julnum*24*nyears),]
              enbal<-ectout$enbal[1:(julnum*24*nyears),]
              masbal<-ectout$masbal[1:(julnum*24*nyears),]
              debout<-ectout$debout[1:(julnum*24*nyears),]
              yearout<-ectout$yearout
              yearsout<-ectout$yearsout[1:nyears,]
            }
            
            # summarise hydroperiod in months per year
            if(wetmod==0 & container==1){
              count <- function(x) {
                length(na.omit(x))
              }
              tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
              dates<-seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours") 
              
              if(dbase=='AWAP' | dbase=='global'){
                dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
              }  
              dates<-subset(dates, !duplicated(as.matrix(dates[2110:2120])))
              dates<-unique(dates)
              wet.times<-cbind(dates,as.data.frame(environ[1:(nyears*365*24),]))
              wet.times<-subset(wet.times,CONDEP>0)
              hydroperiods<-as.data.frame(aggregate(wet.times$CONDEP,by=list((substr(wet.times$dates,1,4))),count))
              hydroperiods$x<-hydroperiods$x/(30.5*24)
              hydrolabels<-seq(ystart,yfinish)
              dummy.periods<-cbind(seq(ystart,yfinish),rep(0,nyears))
              for(c in 1:nrow(hydroperiods)){
                for(d in 1:nyears){
                  if(dummy.periods[d,1]==hydroperiods[c,1]){
                    dummy.periods[d,2]<-hydroperiods[c,2]
                  }}}
              hydroperiods<-t(dummy.periods[,2])
              colnames(hydroperiods)<-hydrolabels
            }
            
                    
            
            if(container==1){
            yearout<-cbind(yearout,hydroperiods)
            }
            if(vlsci_setup==0){
            if(monthly==2){
              metout<-metout[1:(julnum*24),]
              shadmet<-shadmet[1:(julnum*24),]
              soil<-soil[1:(julnum*24),]
              shadsoil<-shadsoil[1:(julnum*24),]
            }
            }
          } # end of check for ectotherm model

        } # end of check for TMAXX na
      } # end of check for na sites
    } # end of check if soil data is being used but no soil data returned

  if(vlsci_setup==0){
  if(data_out==1){
    if(ectomodel==1){
      if(wetmod==1){
        if(DEB==0){
          return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,enbal=enbal,environ=environ,masbal=masbal,yearout=yearout,julnum=julnum,x=x,juldays=juldays,times=times,ALTITUDES=ALTITUDES,WNMAXX=WNMAXX,WNMINN=WNMINN,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,grassgrowths=grassgrowths,grasstsdms=grasstsdms,moists=moists,microinput=microinput))   
        }else{
          return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,enbal=enbal,masbal=masbal,environ=environ,debout=debout,yearout=yearout,yearsout=yearsout,julnum=julnum,x=x,juldays=juldays,times=times,ALTITUDES=ALTITUDES,WNMAXX=WNMAXX,WNMINN=WNMINN,wetlandTemps=wetlandTemps,wetlandDepths=wetlandDepths,grassgrowths=grassgrowths,grasstsdms=grasstsdms,moists=moists,microinput=microinput))
        }
      }else{
        if(DEB==0){
          return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,enbal=enbal,environ=environ,masbal=masbal,yearout=yearout,yearsout=yearsout,julnum=julnum,x=x,juldays=juldays,times=times,ALTITUDES=ALTITUDES,WNMAXX=WNMAXX,WNMINN=WNMINN,grassgrowths=grassgrowths,grasstsdms=grasstsdms,moists=moists,microinput=microinput))   
        }else{
          return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,enbal=enbal,masbal=masbal,environ=environ,debout=debout,yearout=yearout,yearsout=yearsout,julnum=julnum,x=x,juldays=juldays,times=times,ALTITUDES=ALTITUDES,WNMAXX=WNMAXX,WNMINN=WNMINN,grassgrowths=grassgrowths,grasstsdms=grasstsdms,moists=moists,microinput=microinput))
        }
      }  
    }else{
      return(list(soil=soil,shadsoil=shadsoil,metout=metout,shadmet=shadmet,RAINFALL=RAINFALL,julnum=julnum,x=x,juldays=juldays,times=times,ALTITUDES=ALTITUDES,CCMAXX=CCMAXX,CCMINN=CCMINN,WNMAXX=WNMAXX,WNMINN=WNMINN,moists=moists,microinput=microinput))
    }
  }else{
    julnum<-NA
    return(julnum)
  }
  
  rm(metout)
  } #end of vlsci check
} # end of NicheMapR_micro function