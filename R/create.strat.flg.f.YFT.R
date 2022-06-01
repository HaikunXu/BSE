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
  area<-rep(1,nrecs)
  area[lon.5deg>(-132.5) & lon.5deg<=(-122.5) & lat.5deg>(2.5)]<-2
  area[lat.5deg>(17.5) & lon.5deg>(-122.5)]<-3
  area[lat.5deg>(7.5) & lat.5deg<=(17.5) & lon.5deg>(-122.5)]<-4
  area[lat.5deg>(-2.5) & lat.5deg<=(7.5) & lon.5deg>(-122.5)]<-5
  area[lat.5deg<=(2.5) & lon.5deg<=(-97.5)]<-6
  area[lat.5deg<=(2.5) & lon.5deg>(-97.5)]<-7
  
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
  fishery.areagear[(gear==4 | gear==7) & area==4]<-"DP.A4"
  fishery.areagear[(gear==4 | gear==7) & area==5]<-"DP.A5"
  fishery.areagear[(gear==4 | gear==7) & area==6]<-"DP.A6"
  fishery.areagear[(gear==4 | gear==7) & area==7]<-"DP.A7"
  
  }

  # Catch areas for YFT in UNA
  if(PS=="NOA") {
  print("Using catch stratification: YFT SAC 12 UNA")
  area<-rep(2,nrecs)
  area[lat.5deg>(12.5)]<-1
  area[lat.5deg>(2.5) & lat.5deg<=(12.5) & lon.5deg>(-117.5)]<-3
  area[lat.5deg<=(2.5) & lon.5deg>(-87.5)]<-4
  
  # Fishery area-gears for YFT in UNA (DEL and OBJ are junk)
  print("Using fishery stratification: YFT SAC 12 UNA")
  fishery.areagear<-rep(NA,nrecs)
  fishery.areagear[(gear==2 | gear==5) & (area==1 | area==3)]<-"FO.A1"
  fishery.areagear[(gear==2 | gear==5) & (area==2 | area==4)]<-"FO.A2"
  
  fishery.areagear[(gear==3 | gear==6) & area==1]<-"UN.A1"
  fishery.areagear[(gear==3 | gear==6) & area==2]<-"UN.A2"
  fishery.areagear[(gear==3 | gear==6) & area==3]<-"UN.A3"
  fishery.areagear[(gear==3 | gear==6) & area==4]<-"UN.A4"
  
  fishery.areagear[(gear==4 | gear==7) & (area==1 | area==3)]<-"DP.A1"
  fishery.areagear[(gear==4 | gear==7) & (area==2 | area==4)]<-"DP.A2"
  
  }
  
  # Catch areas for YFT in OBJ
  if(PS=="OBJ") {
  print("Using catch stratification: YFT SAC 12 OBJ")
  area<-rep(2,nrecs)
  area[lon.5deg<=(-97.5) & lat.5deg>(-2.5)]<-1
  area[lat.5deg>(-12.5) & lat.5deg<=(-2.5) & lon.5deg<=(-97.5)]<-3
  area[lat.5deg>(-12.5) & lat.5deg<=(-2.5) & lon.5deg>(-97.5)]<-4
  area[lat.5deg<=(-12.5)]<-5
  
  # Fishery area-gears for YFT in OBJ (DEL and UNA are junk)
  print("Using fishery stratification: YFT SAC 12 OBJ")
  fishery.areagear<-rep(NA,nrecs)
  fishery.areagear[(gear==2 | gear==5) & area==1]<-"FO.A1"
  fishery.areagear[(gear==2 | gear==5) & area==2]<-"FO.A2"
  fishery.areagear[(gear==2 | gear==5) & area==3]<-"FO.A3"
  fishery.areagear[(gear==2 | gear==5) & area==4]<-"FO.A4"
  fishery.areagear[(gear==2 | gear==5) & area==5]<-"FO.A5"
  
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