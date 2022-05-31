#' xxx
#' 
#' \code{get.catch.estimates.7599.f} yyy
#' 
#' @export

get.catch.estimates.7599.f = function(cae.in,caestrtflg.in,totunlds.in,lfgrpd.in,lfmm.in,my.year,minsamps.in,well.estimates,PS,Species) {
  # single spp P-S
  # january 7, 2015: edited version of get.catch.estimates.f; does not run well.estimates.f
  # runs functions that compute catch estimates for one year: my.year
  # for input data field definitions, see documentation in functions called below
  #
  # Get total unloads for each catch stratum
  totunlds.bystrat<-get.strat.unloads.7599.f(cae.in[cae.in$year==my.year,],caestrtflg.in[cae.in$year==my.year,],totunlds.in[totunlds.in$year==my.year,])
  # assign(paste("totunlds.bystrat.",my.year,sep=""),totunlds.bystrat,pos=1)
  #
  # Get well-level estimates 
  # well.estimates<-well.estimates.f(lfgrpd.in[lfgrpd.in$year.firstset==my.year,],lfmm.in)
  # assign(paste("well.estimates.",my.year,sep=""),well.estimates,pos=1)
  #
  # Create a catch stratum id variable for the well-level estimate output
  if(Species=="BET") tmp<-create.strat.flg.f(well.estimates$ancillary.info[[1]]$lat.5deg,well.estimates$ancillary.info[[1]]$lon.5deg,is.lwrght=T,well.estimates$ancillary.info[[1]]$month,well.estimates$ancillary.info[[1]]$setype,well.estimates$ancillary.info[[1]]$class,PS,"BET")
  if(Species=="YFT") tmp<-create.strat.flg.f(well.estimates$ancillary.info[[2]]$lat.5deg,well.estimates$ancillary.info[[2]]$lon.5deg,is.lwrght=T,well.estimates$ancillary.info[[2]]$month,well.estimates$ancillary.info[[2]]$setype,well.estimates$ancillary.info[[2]]$class,PS,"YFT")
  if(Species=="SKJ") tmp<-create.strat.flg.f(well.estimates$ancillary.info[[3]]$lat.5deg,well.estimates$ancillary.info[[3]]$lon.5deg,is.lwrght=T,well.estimates$ancillary.info[[3]]$month,well.estimates$ancillary.info[[3]]$setype,well.estimates$ancillary.info[[3]]$class,PS,"SKJ")
  
  well.stratflg<-list(bet=tmp,yft=tmp,skj=tmp)
  rm(list=c("tmp"))
  # assign(paste("well.stratflg.",my.year,sep=""),well.stratflg,pos=1)
  #
  # Get stratum-level estimates for catch strata that have sufficient well sample data;
  #   sufficient sample data is defined in terms of the minimum number of samples required per stratum.
  #      (in the example below, the minimum number of samples is minsamps.in)
  stratum.estimates.withsamps<-call.stratum.estimates.f(totunlds.bystrat,well.estimates,well.stratflg,minsamps.in,Species)
  # assign(paste("stratum.estimates.",my.year,".withsamps",sep=""),stratum.estimates.withsamps,pos=1)
  
  return(list("totunlds.bystrat"=totunlds.bystrat,
              "well.stratflg"=well.stratflg,
              "stratum.estimates.withsamps"=stratum.estimates.withsamps))
}
