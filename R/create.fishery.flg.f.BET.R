#' xxx
#' 
#' \code{create.fishery.flg.f.BET} yyy
#' 
#' @export

create.fishery.flg.f.BET = function(strat.defns,PS)
{

  # number of records in input vectors
  nrecs<-nrow(strat.defns)
  print(nrecs)
  
  # compute quarter id vector
  quarter<-ceiling(strat.defns$month/3)
  
  # SKJ Mark SAC 2022 assessment
  
  print("Using fishery stratification: Juan/Haikun BET spatial movement model 10S 10N")
  
  areagear<-rep(NA,nrecs)
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>=1 & strat.defns$area<=2]<-"FO.A1"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>=3 & strat.defns$area<=4]<-"FO.A2"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==5]<-"FO.A3"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==6]<-"FO.A4"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==7]<-"FO.A5"

  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>=1 & strat.defns$area<=2]<-"UN.A1"
  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>=3 & strat.defns$area<=4]<-"UN.A2"
  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==5]<-"UN.A3"
  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==6]<-"UN.A4"
  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==7]<-"UN.A5"

  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>=1 & strat.defns$area<=2]<-"DP.A1"
  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>=3 & strat.defns$area<=4]<-"DP.A2"
  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==5]<-"DP.A3"
  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==6]<-"DP.A4"
  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==7]<-"DP.A5"
  
  # return fishery ids data frame
  return(list(fishery.quarter=quarter,fishery.areagear=areagear))
}
