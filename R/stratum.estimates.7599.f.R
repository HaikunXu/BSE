#' xxx
#' 
#' \code{stratum.estimates.7599.f} yyy
#' 
#' @export

stratum.estimates.7599.f = function(unloads.bystrat,well.estimates,well.strats,min.sampsize) {
  # single spp P-S
  #
  # this function is called from: call.stratum.estimates.f
  #
  # computes stratum-level estimates Nhat.k from well-level estimates and stratum unloads
  #   (see Tomlinson et al. 1992)
  #
  # Note: notation here stinks (was borrowed from spp comp functions); Mhat.i here is Nhat.j and Mhat.ik is Nhat.jk :-((
  #
  # unloads.bystrat is subsetted output from get.strat.unloads.f
  # well.estimates is subsetted output from well.estimates.f
  # well.strats is subsetted output from create.strat.flg.f, applied to the ancillary information output from well.estimates.f
  # min.sampsize is the minimum number of samples per stratum required to make an estimate
  #
  # edited January 8, 2015: to check if any strata has >= min.sampsize well samples, and to compute Nhat.k if there is only one well sample
  #
  # get stratum ids for wells, number of well samples per stratum and unique well stratum ids
  well.stratid<-paste(well.strats$area,well.strats$month,well.strats$gear,sep=":")
  nwells.perstrat<-table(well.stratid)
  uniq.wellstrat<-dimnames(nwells.perstrat)[[1]]
  #
  # check is any stratum has at least min.sampsize well samples
  if(max(nwells.perstrat)<min.sampsize){
    print("*** WARNING: no stratum has at least min.sampsize well samples; NO SIZE COMPS COMPUTED for this species")
    return(NULL)
  }
  #
  # get well strata that meet minimum sample size requirement and sample sizes for those strata
  uniq.wellstrat.touse<-uniq.wellstrat[nwells.perstrat>=min.sampsize]
  tmp.nwells<-nwells.perstrat[nwells.perstrat>=min.sampsize]
  #
  # create unique id for unloads strata
  uniq.unldstrat<-paste(unloads.bystrat$str.defns$area,unloads.bystrat$str.defns$month,unloads.bystrat$str.defns$gear,sep=":")
  #
  # create vector of positions in unload id vector that correspond to well ids which meet minimum sample size
  well.matches<-match(uniq.wellstrat.touse,uniq.unldstrat)
  #
  # for output, also get unload strata without well samples (this will be used by function for missing data)
  unload.matches<-match(uniq.unldstrat,uniq.wellstrat.touse)
  #
  # create output data frame of stratum definitions for which estimates will be made
  strat.defns<-data.frame(matrix(as.numeric(unlist(strsplit(uniq.wellstrat.touse[!is.na(well.matches)],"\\:"))), ncol = 3, byrow = T))
  names(strat.defns)<-c("area","month","gear")
  #
  # trim well strata to those matching unloads strata and get number of strata for which to make estimates
  #   and save numbers of wells per catch stratum for strata with at least minimum sample size
  # NOTE: this code drops (ignores) strata with well samples but no CAE records
  if(length(tmp.nwells)>length(tmp.nwells[!is.na(well.matches)])){
    print("WARNING: there are one or more strata with well samples but no CAE data")
  }
  #
  uniq.wellstrat.touse<-uniq.wellstrat.touse[!is.na(well.matches)]
  nwells.perstrat.trimmed<-tmp.nwells[!is.na(well.matches)]
  well.matches<-well.matches[!is.na(well.matches)]
  nstrats<-length(well.matches)
  #
  # initialize output storage for number of fish by 1cm bin (Nhat.k)
  Nhat.k<-matrix(0,nrow=nstrats,ncol=201)
  #
  # loop over strata, computing Nhat.k for each stratum
  for(nstr in 1:nstrats){
    #
    #(note that code is not totally efficient; could be simplified somewhat, but not done at this point to keep readability)
    #
    # get well stratum and find if there is an unloads stratum that matches
    W<-unloads.bystrat$totunlds.bystr[well.matches[nstr]]   
    #
    # compute Nhat.k
    denominator.wbar<-sum(well.estimates$Nhat.j[well.stratid==uniq.wellstrat.touse[nstr]])
    numerator.wbar<-sum(well.estimates$Nhat.j[well.stratid==uniq.wellstrat.touse[nstr]]*well.estimates$wbar.j[well.stratid==uniq.wellstrat.touse[nstr]])
    #
    if(!is.vector(well.estimates$Nhat.jk[well.stratid==uniq.wellstrat.touse[nstr],])){
      # there is more than well sample for this stratum
      Nhat.k[nstr,]<-((W*1000)/(numerator.wbar/denominator.wbar))*(apply(well.estimates$Nhat.jk[well.stratid==uniq.wellstrat.touse[nstr],],2,sum)/denominator.wbar)
    } else {
      # there is only one well sample for this stratum
      Nhat.k[nstr,]<-((W*1000)/(numerator.wbar/denominator.wbar))*(well.estimates$Nhat.jk[well.stratid==uniq.wellstrat.touse[nstr],]/denominator.wbar)
    }
    #
    # end of loop over strata with sufficient samples
  }
  #
  return(list(strats=strat.defns,Nhat.k=Nhat.k,unloads.vs.wells=unload.matches,numwells.perstrat=nwells.perstrat.trimmed))
}