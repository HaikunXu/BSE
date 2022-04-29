#' xxx
#' 
#' \code{ss.catch.output.f} yyy
#' 
#' @export

ss.catch.output.f = function(yr.start,yr.end,PS,Species,fisheryareas) {
  # for PS spp comp estimates
  # this funciton loads all the CATCH into one data frame
  # yr.start and yr.end are start and end years
  # afishery is the set type (DP, UN or FO)
  # spp.col is the column number of the species of interest in total.catch.byspp of fishery.estimates.yyyy
  # fisheryareas is a vector with the possible fishery area names for the species of interest
  # edited October 19 2018 to switch order of trimming 0 rows from output and converting output to data frame
  # edited March 2020 to only do loop over years at end if there is more than one year
  #
  if(Species=="BET") spp.col <- 3
  if(Species=="YFT") spp.col <- 4
  if(Species=="SKJ") spp.col <- 5
  
  if(PS=="OBJ") afishery <- "FO"
  if(PS=="NOA") afishery <- "UN"
  if(PS=="DEL") afishery <- "DP"
  
  # get first year catch estimates (and ancillary info) for species of interest
  catch<-get(paste("fishery.estimates.",yr.start,sep=""))$total.catch.byspp[,c(1:2,spp.col)]
  # subset for fishery (set type) of interest
  catch<-catch[substr(as.character(catch$geararea.quarter),1,2)==afishery,]
  if(dim(catch)[1]>=1){
    # get catch quarters and fishery areas
    tmpsplt<-matrix(unlist(strsplit(as.character(catch$geararea.quarter),split=":",fixed=T)),ncol=2,byrow=T)
    quarter<-as.numeric(as.character(tmpsplt[,2]))
    fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
    # get number of possible fishery areas and the column numbers to match fisheryareas
    nareas<-length(fisheryareas)
    areacols<-seq(1,nareas)
    # format data for first year; tmpmat has year in column 1, quarter in column 2 and catch by area in the remaining columns
    tmpmat<-matrix(0,ncol=(nareas+2),nrow=4)
    tmpmat[,1]<-rep(yr.start,4)
    tmpmat[,2]<-seq(1,4)
    # get number of catch values to fill into tmpmat
    nrecs<-dim(catch)[1]
    # fill tmpmat with catch
    for(irec in 1:nrecs){
      tmpmat[quarter[irec],(areacols[fisharea[irec]==fisheryareas]+2)]<-catch[irec,3]
    }
    # save formatted output for first year
    frmtd.catch<-tmpmat
  } else {
    frmtd.catch<-matrix(0,ncol=(nareas+2),nrow=4)
  }
  # now get and format data for rest of years
  if((yr.end-yr.start)>0){
    for(iy in (yr.start+1):yr.end){
      # get catch data frame
      catch<-get(paste("fishery.estimates.",iy,sep=""))$total.catch.byspp[,c(1:2,spp.col)]
      # subset for fishery (set type) of interest
      catch<-catch[substr(as.character(catch$geararea.quarter),1,2)==afishery,]
      if(dim(catch)[1]>=1){
        # get catch quarters and fishery areas
        tmpsplt<-matrix(unlist(strsplit(as.character(catch$geararea.quarter),split=":",fixed=T)),ncol=2,byrow=T)
        quarter<-as.numeric(as.character(tmpsplt[,2]))
        fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
        # set up temporary catch output
        tmpmat<-matrix(0,ncol=(nareas+2),nrow=4)
        tmpmat[,1]<-rep(iy,4)
        tmpmat[,2]<-seq(1,4)
        # get number of catch values to fill into tmpmat
        nrecs<-dim(catch)[1]
        # fill tmpmat with catch
        for(irec in 1:nrecs){
          tmpmat[quarter[irec],(areacols[fisharea[irec]==fisheryareas]+2)]<-catch[irec,3]
        }
        # save output for this year
        frmtd.catch<-rbind(frmtd.catch,tmpmat)
      }
    }
  }
  #
  frmtd.catch<-data.frame(frmtd.catch)
  names(frmtd.catch)<-c("year","quarter",fisheryareas)
  # remove any rows (quarters) that are totally without catch
  # tmpsum<-apply(frmtd.catch[,3:(nareas+2)],1,sum)
  # frmtd.catch<-frmtd.catch[tmpsum>0,]
  #

  return(frmtd.catch)
}