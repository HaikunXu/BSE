#' xxx
#' 
#' \code{create.fishery.flg.f} yyy
#' 
#' @export

create.fishery.flg.f = function(strat.defns)
{
  # from input data frame with catch stratum definitions (area, month, gear) creates id for
  #    fisheries: quarter x areas x gears 
  # strat.defns is output data frame of stratum definitions from either stratum.estimates.f or substitute.f
  #
  # this function is called by fishery.estimates.f
  #
  # NOTE: area-gear combinations in this function must match those in function create.strat.flg.f
  #
  # assumes input vectors are only for one year (ie, function does not stratify by year)
  #

  #
  print("*** create.fishery.flg.f: Please edit first to make sure stratum definitions are correct ***")
  #
  # number of records in input vectors
  nrecs<-nrow(strat.defns)
  #
  # compute quarter id vector
  quarter<-ceiling(strat.defns$month/3)
  #
  #########
  # SKJ Mark SAC 2022 assessment
  #
  # print("Using fishery stratification: SKJ SAC 2022 for DEL (FO and UN largely junk)")
  # areagear<-rep(NA,nrecs)
  # areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
  # areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>1]<-"FO.A2"
  # #
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>1]<-"UN.A2"
  # #
  # areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
  # areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==2]<-"DP.A2"
  ##
  # print("Using fishery stratification: SKJ SAC 2022 for UNA (DP and FO largely junk)")
  # areagear<-rep(NA,nrecs)
  # areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
  # areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area>1]<-"FO.A2"
  #
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==2]<-"UN.A2"
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==3]<-"UN.A3"
  # areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==4]<-"UN.A4"
  #
  # areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
  # areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>1]<-"DP.A2"
  ##
  print("Using fishery stratification: SKJ SAC 2022 for OBJ (DP and UN largely junk)")
  areagear<-rep(NA,nrecs)
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==1]<-"FO.A1"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==2]<-"FO.A2"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==3]<-"FO.A3"
  areagear[(strat.defns$gear==2 | strat.defns$gear==5) & strat.defns$area==4]<-"FO.A4"

  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area==1]<-"UN.A1"
  areagear[(strat.defns$gear==3 | strat.defns$gear==6) & strat.defns$area>1]<-"UN.A2"

  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area==1]<-"DP.A1"
  areagear[(strat.defns$gear==4 | strat.defns$gear==7) & strat.defns$area>1]<-"DP.A2"
  #
  ######
  #
  # return fishery ids data frame
  return(list(fishery.quarter=quarter,fishery.areagear=areagear))
}
