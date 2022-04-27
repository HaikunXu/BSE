#' xxx
#' 
#' \code{compile.sizecomps.output.f} yyy
#' 
#' @export

compile.sizecomps.output.f = function(yr.start,yr.end,PS,Species) {
  # for PS spp comp estimates
  # this funciton loads all the SIZE COMPS into one data frame
  # yr.start and yr.end are start and end years
  # afishery is the set type (DP, UN or FO)
  # spp.col is the column number of the species of interest in phatik.byfshry.samps of fishery.estimates.yyyy
  # edited March 2020 to only do loop over years at the end if there is more than one year
  #
  if(Species=="BET") spp.col <- 1
  if(Species=="YFT") spp.col <- 2
  if(Species=="SKJ") spp.col <- 3
  
  if(PS=="OBJ") afishery <- "FO"
  if(PS=="NOA") afishery <- "UN"
  if(PS=="DEL") afishery <- "DP"
  
  # get first year ancillary info and size comps for species of interest
  fish.ests<-get(paste("fishery.estimates.",yr.start,sep=""))
  ancil<-fish.ests$fishery.defn.samps
  comps<-fish.ests$phatik.byfshry.samps[,,spp.col]
  # subset for fishery (set type) of interest
  tmpflg<-rep(F,nrow(ancil))
  tmpflg[substr(as.character(ancil$geararea.quarter),1,2)==afishery]<-T
  ancil<-ancil[tmpflg,]
  comps<-comps[tmpflg,]
  # get size comp quarters and fishery areas
  tmpsplt<-matrix(unlist(strsplit(as.character(ancil$geararea.quarter),split=":",fixed=T)),ncol=2,byrow=T)
  quarter<-as.numeric(as.character(tmpsplt[,2]))
  fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
  # make temporary data frame
  tmpfrm<-data.frame(rep(yr.start,nrow(ancil)),quarter,fisharea,ancil$nwells.sampled,comps)
  names(tmpfrm)[c(1,3,4)]<-c("year","area","nwells")
  # save formatted output for first year
  frmtd.comps<-tmpfrm
  # now get and format data for rest of years
  if((yr.end-yr.start)>0){
    for(iy in (yr.start+1):yr.end){
      fish.ests<-get(paste("fishery.estimates.",iy,sep=""))
      ancil<-fish.ests$fishery.defn.samps
      comps<-fish.ests$phatik.byfshry.samps[,,spp.col]
      # subset for fishery (set type) of interest
      tmpflg<-rep(F,nrow(ancil))
      tmpflg[substr(as.character(ancil$geararea.quarter),1,2)==afishery]<-T
      ancil<-ancil[tmpflg,]
      comps<-comps[tmpflg,]
      # get size comp quarters and fishery areas
      tmpsplt<-matrix(unlist(strsplit(as.character(ancil$geararea.quarter),split=":",fixed=T)),ncol=2,byrow=T)
      quarter<-as.numeric(as.character(tmpsplt[,2]))
      fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
      # make temporary data frame
      tmpfrm<-data.frame(rep(iy,nrow(ancil)),quarter,fisharea,ancil$nwells.sampled,comps)
      names(tmpfrm)[c(1,3,4)]<-c("year","area","nwells")
      # save formatted output for first year
      frmtd.comps<-rbind(frmtd.comps,tmpfrm)
    }
  }
  # remove any rows that are all 0s for comps of this species (would be more efficient to do this earlier on...)
  tmpsum<-apply(frmtd.comps[,5:205],1,sum)
  frmtd.comps<-frmtd.comps[tmpsum>0,]
  #
  return(frmtd.comps)
}