#' xxx
#' 
#' \code{get.catch.estimates.f} yyy
#' 
#' @export

get.catch.estimates.f = function(cae.in,caestrtflg.in,totunlds.in,lfgrpd.in,lfgrpd.stratflg.in,lfmm.in,my.year,minsamps.in,well.estimates,myarea.submat,growshrink.incrs.mat.touse,PS,Species) {
  # spp comp P-S
  #
  # runs functions that compute catch estimates for one year: my.year
  # for input data field definitions, see documentation in functions called below
  # this version requires that well.estimates.f has already been run and that you specify the area substitution matrix
  # myarea.submat is the area substitution matrix that corresponds to whatever strata you are using
  #
  
  # June 13 2022: add a checking for the area.substitution.mat
  if(length(unique(apply(myarea.submat,1,sum))) > 1) stop("Wrong area.substitution.mat!")
  
  # Get total unloads for each catch stratum
  totunlds.bystrat<-get.strat.unloads.f(cae.in[cae.in$year==my.year,],caestrtflg.in[cae.in$year==my.year,],totunlds.in[as.numeric(dimnames(totunlds.in)[[1]])==my.year])
  # assign(paste("totunlds.bystrat.",my.year,sep=""),totunlds.bystrat,pos=1)
  #
  # Get well-level estimates 
  # well.estimates<-well.estimates.f(lfgrpd.in[lfgrpd.in$year.firstset==my.year,],lfmm.in)
  # assign(paste("well.estimates.",my.year,sep=""),well.estimates,pos=1)
  #
  # Create a catch stratum id variable for the well-level estimate output
  well.stratflg<-create.strat.flg.f(well.estimates$ancillary.info$lat.5deg,well.estimates$ancillary.info$lon.5deg,is.lwrght=T,well.estimates$ancillary.info$month,well.estimates$ancillary.info$setype,well.estimates$ancillary.info$class,PS,Species)
  # assign(paste("well.stratflg.",my.year,sep=""),well.stratflg,pos=1)
  #
  # Get stratum-level estimates for catch strata that have sufficient sample data
  #   sufficient sample data is defined in terms of the minimum number of samples required per stratum
  #   in the example below, the minimum number of samples is minsamps.in
  stratum.estimates.withsamps<-stratum.estimates.f(totunlds.bystrat,well.estimates,well.stratflg,minsamps.in)
  # assign(paste("stratum.estimates.",my.year,".withsamps",sep=""),stratum.estimates.withsamps,pos=1)
  #
  # Get stratum-level estimates for catch strata that DO NOT have sufficient sample data 
  #  NOTE: minimum number of samples per stratum should match that used by stratum.estimates.f above
  #
  # before running substitute.f also need to get the fishery.area for the unloads strata with no/inadequate sample data
  #     this is really awkward and should be improved.... ;-(  
  tmpcl<-create.fishery.flg.f(totunlds.bystrat$str.defns,PS,Species)
  tmp.unlds.stratdefns.miss<-data.frame(totunlds.bystrat$str.defns,tmpcl)[,c(1:3,5)][is.na(stratum.estimates.withsamps$unloads.vs.wells),]
  tmp.unlds.stratdefns.miss$fishery.areagear<-as.character(tmp.unlds.stratdefns.miss$fishery.areagear)
  rm(tmpcl)
  # assign(paste("tmp.unlds.stratdefns.miss",my.year,sep=""),tmp.unlds.stratdefns.miss,pos=1)
  #
  stratum.estimates.NOsamps<-substitute.f(tmp.unlds.stratdefns.miss,totunlds.bystrat$totunlds.bystr[is.na(stratum.estimates.withsamps$unloads.vs.wells)],lfgrpd.in[lfgrpd.in$year.firstset==my.year,],lfgrpd.stratflg.in[lfgrpd.in$year.firstset==my.year,],lfmm.in,gear.substitution.mat,myarea.submat,month.substitution.mat,minsamps.in,growshrink.incrs.mat.touse)
  # assign(paste("stratum.estimates.",my.year,".NOsamps",sep=""),stratum.estimates.NOsamps,pos=1)
  return(list("totunlds.bystrat"=totunlds.bystrat,
              "well.stratflg"=well.stratflg,
              "stratum.estimates.withsamps"=stratum.estimates.withsamps,
              "tmp.unlds.stratdefns.miss"=tmp.unlds.stratdefns.miss,
              "stratum.estimates.NOsamps"=stratum.estimates.NOsamps))
}