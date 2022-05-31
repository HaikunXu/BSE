#' xxx
#' 
#' \code{fishery.estimates.7599.f} yyy
#' 
#' @export

fishery.estimates.7599.f = function(strat.ests.withsamps,spp.unlds.bystrat,iyear,PS,Species)
{
  # single spp P-S
  # creates stock assessment estimates of total catch (mt) and size comps (wtd. avg. proportion of fish)
  #   within stock assessments fisheries (quarter x areas x gears) FOR A SINGLE SPECIES
  #
  # total catch estimates are the sum of total stratum catches for all catch strata in a fishery
  # size comp estimates are the weighted average of size comps from strata with samples (weights are number of wells sampled per stratum)
  #
  # this function is called by: call.fishery.estimates.f
  #
  # strat.ests.withsamps is output of function stratum.estimates.f - SUBSETTED FOR A SPECIES
  # spp.unlds.bystrat is output from get.strat.unloads.f (applied to output of get.corrected.unloads.f) - SUBSETTED FOR A SPECIES
  # iyear the year for which estimates are being made; used only to add column with year to output data frame
  #
  # this function uses: create.fishery.flg.f
  #
  # edited 12.18.2014: added code to return number of wells sampled per strata, for strata with adequate well sample data
  #
  # size comp estimates from strata WITH sample data
  #
  # get fishery id information (id vector; unique id vector; number unique ids)
  fishery.samps.defns<-create.fishery.flg.f(strat.ests.withsamps$strats,PS,Species)
  fishery.samps.id<-paste(fishery.samps.defns$fishery.areagear,fishery.samps.defns$fishery.quarter,sep=":")
  fishery.samps.id.unq<-unique(fishery.samps.id)
  n.fshry.samps<-length(fishery.samps.id.unq)
  
  #
  # initialize size comp output storage and stratum definitions output 
  #
  phatk.byfshry.samps<-matrix(NA,nrow=n.fshry.samps,ncol=201)
  #
  fishery.defns.samps<-data.frame(matrix(NA,ncol=3,nrow=n.fshry.samps))
  names(fishery.defns.samps)<-c("year","geararea.quarter","nwells.sampled")
  #
  # loop over number of fisheries
  for(ifsh in 1:n.fshry.samps){
    # save fishery definitions (including year)
    fishery.defns.samps$year[ifsh]<-iyear
    fishery.defns.samps$geararea.quarter[ifsh]<-fishery.samps.id.unq[ifsh]
    fishery.defns.samps$nwells.sampled[ifsh]<-sum(strat.ests.withsamps$numwells.perstrat[fishery.samps.id==fishery.samps.id.unq[ifsh]])
    #
    # get size comp estimates
    #
    # number of individual wells per catch stratum for this fishery stratum (used to weight catch stratum size comps)
    tmp.nwells<-as.vector(strat.ests.withsamps$numwells.perstrat[fishery.samps.id==fishery.samps.id.unq[ifsh]])
    #
    # size comps
    #  sum number of fish by catch strata
    tmpsums<-as.vector(strat.ests.withsamps$Nhat.k[fishery.samps.id==fishery.samps.id.unq[ifsh],]%*%rep(1,201))
    if(length(tmpsums)==1){
      # there is only one catch stratum in this fishery stratum
      phatk.byfshry.samps[ifsh,]<-strat.ests.withsamps$Nhat.k[fishery.samps.id==fishery.samps.id.unq[ifsh],]/sum(strat.ests.withsamps$Nhat.k[fishery.samps.id==fishery.samps.id.unq[ifsh],])
    } else {
      # there is more than one catch stratum in this fishery stratum
      tmpspp<-(strat.ests.withsamps$Nhat.k[fishery.samps.id==fishery.samps.id.unq[ifsh],]*tmp.nwells)/tmpsums
      phatk.byfshry.samps[ifsh,]<-(1/sum(tmp.nwells))*apply(tmpspp,2,sum,na.rm=T)           
    }
    # end of loop on fishery strata
  }
  #
  # Estimates of total catch by fishery areagear-quarter
  #
  #  make fishery areagear x quarter id
  tmpspp.fshryflg<-create.fishery.flg.f(spp.unlds.bystrat$str.defns,PS,Species)
  tmpspp.qrtrfshry.id<-paste(tmpspp.fshryflg$fishery.areagear,tmpspp.fshryflg$fishery.quarter,sep=":")
  #
  # sum catch by areagear x quarter
  totcatch.byfshry<-tapply(spp.unlds.bystrat$totunlds.bystr,tmpspp.qrtrfshry.id,sum)
  #
  return(list(fishery.defn.samps=fishery.defns.samps,phatk.byfshry.samps=phatk.byfshry.samps,catchtotals=totcatch.byfshry,catchdefns=dimnames(totcatch.byfshry)[[1]]))
}