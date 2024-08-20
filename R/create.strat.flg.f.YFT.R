#' xxx
#' 
#' \code{create.strat.flg.f.YFT} yyy
#' 
#' @export

create.strat.flg.f.YFT = function(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS) {

  # number of records in input vectors
  nrecs<-length(lat.5deg)
  
  # initialize and fill gear vector
  gear<-rep(NA,nrecs)
  gear[setype==1 & vessel.class<=5]<-4
  gear[setype==1 & vessel.class==6]<-7
  gear[setype==4 & vessel.class<=5]<-3
  gear[setype==4 & vessel.class==6]<-6
  gear[setype==5 & vessel.class<=5]<-2
  gear[setype==5 & vessel.class==6]<-5
  
  # if lat.5deg and lon.5deg are lower right corner, convert these to 5 degree square center
  if(is.lwrght){
    lat.5deg<-lat.5deg+2.5
    lon.5deg<-lon.5deg-2.5
  }
  
  # YFT assessment for SAC 2020

  # Catch areas for YFT in DEL
  if(PS=="DEL") {
    print("Using catch stratification: YFT SAC 12 DEL")
    
    YFT_DEL <- read.csv("D:/OneDrive - IATTC/IATTC/2024/Irregular clustering/YFT DEL/cluster_YFT.csv")
    
    Locations <- data.frame("lat" = lat.5deg, "lon" = lon.5deg)
    Locations <- dplyr::left_join(Locations, YFT_DEL)
    
    # define areas for all grids
    for (i in 1:nrecs) {
      if(is.na(Locations$area[i]) == TRUE) {
        if((Locations$lat[i] > 10 & Locations$lon[i] > (-105))) Locations$area[i] <- 1
        if(Locations$lat[i] < 0) Locations$area[i] <- 3
        if(Locations$lon[i] < (-130)) Locations$area[i] <- 3
        if(Locations$lat[i] > 15 & Locations$lon[i] < (-125)) Locations$area[i] <- 3
        if(Locations$lat[i] > 30) Locations$area[i] <- 1
        if(Locations$lat[i] > 5 & Locations$lat[i] < 30 & Locations$lon[i] < (-115) & Locations$lon[i] > (-125)) Locations$area[i] <- 2
      }
    }
    
    if(sum(is.na(Locations$area)) > 0) {
      print(Locations[which(is.na(Locations$area)==1),1:2])
      stop("Error YFT DEL area definitions!")
    }
    
    area <- Locations$area
      
    # Fishery area-gears for YFT in DEL (UNA and OBJ are junk)
    print("Using fishery stratification: YFT SAC 12 DEL")
    fishery.areagear<-rep(NA,nrecs)
    fishery.areagear[(gear==2 | gear==5) & area<=5]<-"FO.A1"
    fishery.areagear[(gear==2 | gear==5) & area>5]<-"FO.A2"
    
    fishery.areagear[(gear==3 | gear==6) & area<=5]<-"UN.A1"
    fishery.areagear[(gear==3 | gear==6) & area>5]<-"UN.A2"
    
    fishery.areagear[(gear==4 | gear==7) & area==1]<-"DP.A1"
    fishery.areagear[(gear==4 | gear==7) & area==2]<-"DP.A2"
    fishery.areagear[(gear==4 | gear==7) & area==3]<-"DP.A3"
  }
  

  # Catch areas for YFT in UNA
  if(PS=="NOA") {
    print("Using catch stratification: YFT SAC 12 UNA")

    YFT_DEL <- read.csv("D:/OneDrive - IATTC/IATTC/2024/Irregular clustering/YFT DEL/cluster_YFT.csv")
    
    Locations <- data.frame("lat" = lat.5deg, "lon" = lon.5deg)
    Locations <- dplyr::left_join(Locations, YFT_DEL)
    
    # define areas for all grids
    for (i in 1:nrecs) {
      if(is.na(Locations$area[i]) == TRUE) {
        if((Locations$lat[i] > 10 & Locations$lon[i] > (-105))) Locations$area[i] <- 1
        if(Locations$lat[i] < 0) Locations$area[i] <- 3
        if(Locations$lon[i] < (-130)) Locations$area[i] <- 3
        if(Locations$lat[i] > 15 & Locations$lon[i] < (-125)) Locations$area[i] <- 3
        if(Locations$lat[i] > 30) Locations$area[i] <- 1
        if(Locations$lat[i] > 5 & Locations$lat[i] < 30 & Locations$lon[i] < (-115) & Locations$lon[i] > (-125)) Locations$area[i] <- 2
      }
    }
    
    if(sum(is.na(Locations$area)) > 0) {
      print(Locations[which(is.na(Locations$area)==1),1:2])
      stop("Error YFT DEL area definitions!")
    }
    
    area <- Locations$area
    
    # Fishery area-gears for YFT in UNA (DEL and OBJ are junk)
    print("Using fishery stratification: YFT SAC 12 UNA")
    fishery.areagear<-rep(NA,nrecs)
    fishery.areagear[(gear==2 | gear==5) & (area==1 | area==3)]<-"FO.A1"
    fishery.areagear[(gear==2 | gear==5) & (area==2 | area==4)]<-"FO.A2"
    
    fishery.areagear[(gear==3 | gear==6) & area==1]<-"UN.A1"
    fishery.areagear[(gear==3 | gear==6) & area==2]<-"UN.A2"
    fishery.areagear[(gear==3 | gear==6) & area==3]<-"UN.A3"
    
    fishery.areagear[(gear==4 | gear==7) & (area==1 | area==3)]<-"DP.A1"
    fishery.areagear[(gear==4 | gear==7) & (area==2 | area==4)]<-"DP.A2"
  }
  
  # Catch areas for YFT in OBJ
  if(PS=="OBJ") {
    print("Using catch stratification: YFT SAC 12 OBJ")
    
    YFT_DEL <- read.csv("D:/OneDrive - IATTC/IATTC/2024/Irregular clustering/YFT DEL/cluster_YFT.csv")
    
    Locations <- data.frame("lat" = lat.5deg, "lon" = lon.5deg)
    Locations <- dplyr::left_join(Locations, YFT_DEL)
    
    # define areas for all grids
    for (i in 1:nrecs) {
      if(is.na(Locations$area[i]) == TRUE) {
        if((Locations$lat[i] > 10 & Locations$lon[i] > (-105))) Locations$area[i] <- 1
        if(Locations$lat[i] < 0) Locations$area[i] <- 3
        if(Locations$lon[i] < (-130)) Locations$area[i] <- 3
        if(Locations$lat[i] > 15 & Locations$lon[i] < (-125)) Locations$area[i] <- 3
        if(Locations$lat[i] > 30) Locations$area[i] <- 1
        if(Locations$lat[i] > 5 & Locations$lat[i] < 30 & Locations$lon[i] < (-115) & Locations$lon[i] > (-125)) Locations$area[i] <- 2
      }
    }
    
    if(sum(is.na(Locations$area)) > 0) {
      print(Locations[which(is.na(Locations$area)==1),1:2])
      stop("Error YFT DEL area definitions!")
    }
    
    area <- Locations$area
    
    # Fishery area-gears for YFT in OBJ (DEL and UNA are junk)
    print("Using fishery stratification: YFT SAC 12 OBJ")
    fishery.areagear<-rep(NA,nrecs)
    fishery.areagear[(gear==2 | gear==5) & area==1]<-"FO.A1"
    fishery.areagear[(gear==2 | gear==5) & area==2]<-"FO.A2"
    fishery.areagear[(gear==2 | gear==5) & area==3]<-"FO.A3"

    fishery.areagear[(gear==3 | gear==6) & area<5]<-"UN.A1"
    fishery.areagear[(gear==3 | gear==6) & area==5]<-"UN.A2"
    
    fishery.areagear[(gear==4 | gear==7) & area<5]<-"DP.A1"
    fishery.areagear[(gear==4 | gear==7) & area==5]<-"DP.A2"
  }

  # return stratum id data frame
  stratum.id<-data.frame(area,month,gear,fishery.areagear)
  stratum.id$fishery.areagear<-as.character(stratum.id$fishery.areagear)
  
  return(stratum.id)
}