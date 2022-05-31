#' xxx
#' 
#' \code{get.strat.unloads.7599.f} yyy
#' 
#' @export

get.strat.unloads.7599.f = function(cae.frm,cae.stratum,total.unloads) {
  # single spp P-S
  # this function prorates annual total P-S unloads to catch estimation strata, for a specific year
  #
  # cae.frm is the output from read.cae.f (this is a data frame with stratum info and catch of each of the 3 spp)
  # cae.stratum is the output from create.strat.flg.f (this has the three variables that define the strata: area, month, gear)
  # total.unloads is the output from get.corrected.unloads.f (this is the total annual corrected unloads of each of the 3 spp)
  #
  # ** NOTE: function assumes that all three data frames are already subsetted by year.
  #
  # species names vector
  spp.names<-c("bet","yft","skj")
  #
  # loop over three species (in order of spp.names)
  for(isp in 1:3){
    #
    # first set to null the spp output
    strat.defns<-NULL
    unloads.bystrat.vec<-NULL
    #
    # get cae catch for species
    cae.spp<-cae.frm[,spp.names[isp]]
    # 
    # get unloads for species
    unlds.spp<-total.unloads[,spp.names[isp]]
    #
    # check if there are both unloads and CAE data for this species; otherwise skip
    if(sum(cae.spp,na.rm=T)>0 & sum(unlds.spp,na.rm=T)>0){
      #
      # sum CAE catches for species by catch strata
      sum.cae.bystrat<-tapply(cae.spp,list(cae.stratum$area,cae.stratum$month,cae.stratum$gear),sum)
      sum.cae.bystrat[is.na(sum.cae.bystrat)]<-0
      #
      # prorate total species unloads to strata
      total.unloads.bystrat<-(unlds.spp/sum(cae.spp))*sum.cae.bystrat
      #
      # reformat unloads by strata
      unloads.bystrat.vec<-as.vector(total.unloads.bystrat)
      assign(paste(spp.names[isp],"unlds.bystrata",sep="."),unloads.bystrat.vec)
      #
      # get stratum definitions to go with reformatted (vectorized) unloads by strata
      n.areas<-length(dimnames(total.unloads.bystrat)[[1]])
      n.months<-length(dimnames(total.unloads.bystrat)[[2]])
      n.gears<-length(dimnames(total.unloads.bystrat)[[3]])
      #
      # fill arrays with values of area, month and gear
      area.array<-array(rep(as.numeric(dimnames(total.unloads.bystrat)[[1]]),(n.months*n.gears)),dim=c(n.areas,n.months,n.gears))
      #
      month.array<-array(rep(as.vector(matrix(rep(as.numeric(dimnames(total.unloads.bystrat)[[2]]),n.areas),ncol=n.months,nrow=n.areas,byrow=T)),n.gears),dim=c(n.areas,n.months,n.gears))
      #
      gear.array<-array(sort(rep(as.numeric(dimnames(total.unloads.bystrat)[[3]]),(n.areas*n.months))),dim=c(n.areas,n.months,n.gears))
      #
      # reformat stratum definitions to match that of unloads
      strat.defns<-data.frame(as.vector(area.array),as.vector(month.array),as.vector(gear.array))
      names(strat.defns)<-c("area","month","gear")
      assign(paste("strat.defns",spp.names[isp],sep="."),strat.defns)
      #  
      # assign null because insufficient data
    } else {
      assign(paste(spp.names[isp],"unlds.bystrata",sep="."),unloads.bystrat.vec)
      assign(paste("strat.defns",spp.names[isp],sep="."),strat.defns)
      print(paste("WARNING: no species data in Unloads or CAE or both for: ",spp.names[isp],sep=""))
    } 
    # end of loop on species
  }
  #
  # return only those strata with unloads>0, by species (this code is lame and should be improved...)
  if(!is.null(bet.unlds.bystrata)){
    strat.defns.bet<-strat.defns.bet[bet.unlds.bystrata>0,]
    bet.unlds.bystrata<-bet.unlds.bystrata[bet.unlds.bystrata>0]
  }
  #
  if(!is.null(yft.unlds.bystrata)){
    strat.defns.yft<-strat.defns.yft[yft.unlds.bystrata>0,]
    yft.unlds.bystrata<-yft.unlds.bystrata[yft.unlds.bystrata>0]
  }
  #
  if(!is.null(skj.unlds.bystrata)){
    strat.defns.skj<-strat.defns.skj[skj.unlds.bystrata>0,]
    skj.unlds.bystrata<-skj.unlds.bystrata[skj.unlds.bystrata>0]
  }
  #
  return(list(bet.totunlds.bystr=bet.unlds.bystrata,bet.str.defns=strat.defns.bet,yft.totunlds.bystr=yft.unlds.bystrata,yft.str.defns=strat.defns.yft,skj.totunlds.bystr=skj.unlds.bystrata,skj.str.defns=strat.defns.skj))
}