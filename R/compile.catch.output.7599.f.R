#' xxx
#' 
#' \code{compile.catch.output.7599.f} yyy
#' 
#' @export

compile.catch.output.7599.f = function(yr.start,yr.end,PS,Species,fisheryareas) {
  # for PS single spp estimates
  # this funciton loads all the CATCH into one data frame
  # yr.start and yr.end are start and end years
  # afishery is the set type (DP, UN or FO)
  # aspp is the species (bet, yft, or skj)
  # fisheryareas is a vector with the possible fishery area names for the species of interest
  # edited October 19 2018 to switch the order of removing rows with 0s and making data frame for frmtd.catch
  #
  
  if(Species=="BET") aspp <- "bet"
  if(Species=="YFT") aspp <- "yft"
  if(Species=="SKJ") aspp <- "skj"
  
  if(PS=="OBJ") afishery <- "FO"
  if(PS=="NOA") afishery <- "UN"
  if(PS=="DEL") afishery <- "DP"
  
  # get first year catch estimates (and ancillary info) for species of interest
  fish.ests<-get(paste("fishery.estimates.",aspp,".",yr.start,sep=""))
  catch<-as.vector(fish.ests$catchtotals)
  # subset for fishery (set type) of interest
  catch<-catch[substr(as.character(fish.ests$catchdefns),1,2)==afishery]
  if(length(catch)>=1){
    catch.names<-fish.ests$catchdefns[substr(as.character(fish.ests$catchdefns),1,2)==afishery]
    # get catch quarters and fishery areas
    tmpsplt<-matrix(unlist(strsplit(as.character(catch.names),split=":",fixed=T)),ncol=2,byrow=T)
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
    nrecs<-length(catch)
    # fill tmpmat with catch
    for(irec in 1:nrecs){
      tmpmat[quarter[irec],(areacols[fisharea[irec]==fisheryareas]+2)]<-catch[irec]
    }
    # save formatted output for first year
    frmtd.catch<-tmpmat
  } else {
    frmtd.catch<-matrix(0,ncol=(nareas+2),nrow=4)
  }
  # now get and format data for rest of years
  for(iy in (yr.start+1):yr.end){
    # get catch data frame
    fish.ests<-get(paste("fishery.estimates.",aspp,".",iy,sep=""))
    catch<-as.vector(fish.ests$catchtotals)
    # subset for fishery (set type) of interest
    catch<-catch[substr(as.character(fish.ests$catchdefns),1,2)==afishery]
    if(length(catch)>=1){
      catch.names<-fish.ests$catchdefns[substr(as.character(fish.ests$catchdefns),1,2)==afishery]
      # get catch quarters and fishery areas
      tmpsplt<-matrix(unlist(strsplit(as.character(catch.names),split=":",fixed=T)),ncol=2,byrow=T)
      quarter<-as.numeric(as.character(tmpsplt[,2]))
      fisharea<-matrix(unlist(strsplit(tmpsplt[,1],split=".",fixed=T)),ncol=2,byrow=T)[,2]
      # set up temporary catch output
      tmpmat<-matrix(0,ncol=(nareas+2),nrow=4)
      tmpmat[,1]<-rep(iy,4)
      tmpmat[,2]<-seq(1,4)
      # get number of catch values to fill into tmpmat
      nrecs<-length(catch)
      # fill tmpmat with catch
      for(irec in 1:nrecs){
        tmpmat[quarter[irec],(areacols[fisharea[irec]==fisheryareas]+2)]<-catch[irec]
      }
      # save output for this year
      frmtd.catch<-rbind(frmtd.catch,tmpmat)
    }
  }
  #
  frmtd.catch<-data.frame(frmtd.catch)
  names(frmtd.catch)<-c("year","quarter",fisheryareas)
  #
  # remove any rows (quarters) that are totally without catch
  tmpsum<-apply(frmtd.catch[,3:(nareas+2)],1,sum)
  frmtd.catch<-frmtd.catch[tmpsum>0,]
  #
  return(frmtd.catch)
}