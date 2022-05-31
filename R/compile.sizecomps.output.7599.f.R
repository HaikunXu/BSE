#' xxx
#' 
#' \code{compile.sizecomps.output.7599.f} yyy
#' 
#' @export

compile.sizecomps.output.7599.f = function(yr.start,yr.end,PS,Species)
{
  # for PS single spp estimates
  # this funciton loads all the SIZE COMPS into one data frame
  # yr.start and yr.end are start and end years
  # afishery is the set type (DP, UN or FO)
  # aspp is the species(bet, yft or skj)
  #
  if(Species=="BET") aspp <- "bet"
  if(Species=="YFT") aspp <- "yft"
  if(Species=="SKJ") aspp <- "skj"
  
  if(PS=="OBJ") afishery <- "FO"
  if(PS=="NOA") afishery <- "UN"
  if(PS=="DEL") afishery <- "DP"
  
  # get first year ancillary info and size comps
  fish.ests<-get(paste("fishery.estimates.",aspp,".",yr.start,sep=""))
  ancil<-fish.ests$fishery.defn.samps
  comps<-fish.ests$phatk.byfshry.samps
  year<-rep(yr.start,nrow(fish.ests$fishery.defn.samps))
  # now get rest of years
  for(iy in (yr.start+1):yr.end){
    fish.ests<-get(paste("fishery.estimates.",aspp,".",iy,sep=""))
    ancil<-rbind(ancil,fish.ests$fishery.defn.samps)
    comps<-rbind(comps,fish.ests$phatk.byfshry.samps)
    year<-c(year,rep(iy,nrow(fish.ests$fishery.defn.samps)))
  }
  # subset for fishery (set type)
  tmpflg<-rep(F,nrow(ancil))
  tmpflg[substr(as.character(ancil$geararea.quarter),1,2)==afishery]<-T
  ancil<-ancil[tmpflg,]
  comps<-comps[tmpflg,]
  year<-year[tmpflg]
  # continue if there are data
  frmtd.comps<-NULL
  if(dim(ancil)[1]>=1){
    # get quarters and fishery areas
    tmpsplt<-matrix(unlist(strsplit(as.character(ancil$geararea.quarter),split=":",fixed=T)),ncol=2,byrow=T)
    quarter<-as.numeric(as.character(tmpsplt[,2]))
    fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
    # format output
    if(dim(ancil)[1]>1){
      frmtd.comps<-data.frame(year,quarter,fisharea,ancil$nwells.sampled,comps)
      names(frmtd.comps)[3]<-"area"
      names(frmtd.comps)[4]<-"nwells"
    }
    #
    if(dim(ancil)[1]==1){
      frmtd.comps<-data.frame(as.matrix(c(year,quarter,fisharea,ancil$nwells.sampled),nrow=1,ncol=4,byrow=T),as.matrix(comps.out,nrow=1,ncol=201,byrow=T))
      names(frmtd.comps)[1:4]<-c("year","quarter","area","nwells")
      frmtd.comps$year<-as.numeric(frmtd.comps$year)
      frmtd.comps$quarter<-as.numeric(frmtd.comps$quarter)
      frmtd.comps$nwells<-as.numeric(frmtd.comps$nwells)
    }
  }
  #
  return(frmtd.comps)
}