#' xxx
#' 
#' \code{create.fishery.flg.f.BET} yyy
#' 
#' @export

create.fishery.flg.f.BET = function(strat.defns,PS)
{

  # number of records in input vectors
  nrecs<-nrow(strat.defns)
  
  # compute quarter id vector
  quarter<-ceiling(strat.defns$month/3)
  
  if(PS=="OBJ") {
    print("Using fishery stratification: SKJ SAC 2022 for OBJ (DP and UN largely junk)")
    areagear<-rep(NA,nrecs)
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==2]<-"FO.A2"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==3]<-"FO.A3"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==4]<-"FO.A4"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==5]<-"FO.A5"

    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>1]<-"UN.A2"

    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>1]<-"DP.A2"
  }

  if(PS=="NOA") {
    print("Using fishery stratification: SKJ SAC 2022 for UNA (DP and FO largely junk)")
    areagear<-rep(NA,nrecs)
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>1]<-"FO.A2"

    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==2]<-"UN.A2"

    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>1]<-"DP.A2"
  }

  if(PS=="DEL") {
    print("Using fishery stratification: SKJ SAC 2022 for DEL (FO and UN largely junk)")
    areagear<-rep(NA,nrecs)
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
    areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>1]<-"FO.A2"
    #
    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
    areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>1]<-"UN.A2"
    #
    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
    areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==2]<-"DP.A2"
  }
  
  # return fishery ids data frame
  return(list(fishery.quarter=quarter,fishery.areagear=areagear))
}
