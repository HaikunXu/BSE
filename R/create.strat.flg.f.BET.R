#' xxx
#' 
#' \code{create.strat.flg.f.BET} yyy
#' 
#' @export

create.strat.flg.f.BET = function(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS) {

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
  
  # BET assessment for SAC 2020
  
  area<-rep(6,nrecs)
  area[lon.5deg<=(-127.5) & lat.5deg>=(-7.5)]<-1
  area[lat.5deg>=(-7.5) & lon.5deg>(-127.5) & lon.5deg<=(-112.5)]<-2
  area[lat.5deg>=(2.5) & lon.5deg>(-112.5)]<-3
  area[lat.5deg<=(-2.5) & lat.5deg>=(-7.5) & lon.5deg>(-112.5)]<-4
  area[lat.5deg<=(-12.5) & lon.5deg<=(-112.5)]<-5
  area[lat.5deg<=(-17.5) & lon.5deg>(-92.5)]<-7
  
  # Catch areas for BET in OBJ
  print("Using fishery stratification: BET assessment 2020 for OBJ")
  
  fishery.areagear<-rep(NA,nrecs)
  fishery.areagear[(gear==2 | gear==5) & area>=1 & area<=2]<-"FO.A1"
  fishery.areagear[(gear==2 | gear==5) & area>=3 & area <=4]<-"FO.A2"
  fishery.areagear[(gear==2 | gear==5) & area==5]<-"FO.A3"
  fishery.areagear[(gear==2 | gear==5) & area==6]<-"FO.A4"
  fishery.areagear[(gear==2 | gear==5) & area==7]<-"FO.A5"
  
  fishery.areagear[(gear==3 | gear==6) & area>=1 & area<=2]<-"UN.A1"
  fishery.areagear[(gear==3 | gear==6) & area>=3 & area<=4]<-"UN.A2"
  fishery.areagear[(gear==3 | gear==6) & area==5]<-"UN.A3"
  fishery.areagear[(gear==3 | gear==6) & area==6]<-"UN.A4"
  fishery.areagear[(gear==3 | gear==6) & area==7]<-"UN.A5"
  
  fishery.areagear[(gear==4 | gear==7) & area>=1 & area<=2]<-"DP.A1"
  fishery.areagear[(gear==4 | gear==7) & area>=3 & area<=4]<-"DP.A2"
  fishery.areagear[(gear==4 | gear==7) & area==5]<-"DP.A3"
  fishery.areagear[(gear==4 | gear==7) & area==6]<-"DP.A4"
  fishery.areagear[(gear==4 | gear==7) & area==7]<-"DP.A5"
  
    # return stratum id data frame
  stratum.id<-data.frame(area,month,gear,fishery.areagear)
  stratum.id$fishery.areagear<-as.character(stratum.id$fishery.areagear)
  
  return(stratum.id)
}