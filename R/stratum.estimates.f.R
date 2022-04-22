#' xxx
#' 
#' \code{stratum.estimates.f} yyy
#' 
#' @export

stratum.estimates.f = function(unloads.bystrat,well.estimates,well.strats,min.sampsize) {
  # computes stratum-level estimates (What.i and Nhat.ik) from well-level estimates and stratum unloads
  #   (see Tomlinson document in SAR4 2004)
  #
  # unloads.bystrat is output from get.strat.unloads.f
  # well.estimates is output from well.estimates.f
  # well.strats is output from create.strat.flg.f, applied to the ancillary information output from well.estimates.f
  # min.sampsize is the minimum number of samples per stratum required to make an estimate
  #
  # get stratum ids for wells, number of well samples per stratum and unique well stratum ids
  well.stratid<-paste(well.strats$area,well.strats$month,well.strats$gear,sep=":")
  nwells.perstrat<-table(well.stratid)
  uniq.wellstrat<-dimnames(nwells.perstrat)[[1]]
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
  #    and, save numbers of wells per catch strata for strata with at least minimum sample size
  # NOTE: this code drops (ignores) strata that have well samples by no CAE data
  if(length(tmp.nwells)>length(tmp.nwells[!is.na(well.matches)])){
    print("WARNING: there are one or more strata with well samples but no CAE data")
  }
  #
  uniq.wellstrat.touse<-uniq.wellstrat.touse[!is.na(well.matches)]
  nwells.perstrat.trimmed<-tmp.nwells[!is.na(well.matches)]
  well.matches<-well.matches[!is.na(well.matches)]
  nstrats<-length(well.matches)
  #
  # initialize output storage for stratum weight by species (What.i) and number of fish by 1cm bin by species (Nhat.ik)
  What.i<-matrix(0,nrow=nstrats,ncol=3)
  Nhat.ik<-array(0,dim=c(nstrats,201,3))
  #
  # loop over strata, computing What.i, Nhat.ik
  for(nstr in 1:nstrats){
    #
    #(note that code is not totally efficient; could be simplified somewhat, but not done at this point to keep readability)
    #
    # get well stratum and find if there is an unloads stratum that matches
    W<-unloads.bystrat$totunlds.bystr[well.matches[nstr]]   
    #
    # compute What.i
    sum.Wj<-sum(well.estimates$ancillary.info$wellmt.3spp[well.stratid==uniq.wellstrat.touse[nstr]])
    What.i[nstr,]<-W*(apply(well.estimates$What.ij[well.stratid==uniq.wellstrat.touse[nstr],],2,sum)/sum.Wj)
    #
    # compute Nhat.ik
    for(ispp in 1:3){
      sum.Nj<-sum(well.estimates$Nhat.j[well.stratid==uniq.wellstrat.touse[nstr]])
      sum.Nij<-sum(well.estimates$Nhat.ij[well.stratid==uniq.wellstrat.touse[nstr],ispp])
      if(sum.Nij>0){
        if(!is.vector(well.estimates$Nhat.ijk[well.stratid==uniq.wellstrat.touse[nstr],,ispp])){
          # there is more than one well sample for this species
          Nhat.ik[nstr,,ispp]<-(W/(sum.Wj/sum.Nj))*(sum.Nij/sum.Nj)*(apply(well.estimates$Nhat.ijk[well.stratid==uniq.wellstrat.touse[nstr],,ispp],2,sum)/sum.Nij)
        } else {
          # there is only one well sample for this species
          Nhat.ik[nstr,,ispp]<-(W/(sum.Wj/sum.Nj))*(sum.Nij/sum.Nj)*(well.estimates$Nhat.ijk[well.stratid==uniq.wellstrat.touse[nstr],,ispp]/sum.Nij)
        }
      }
    }
    # end of loop over strata with sufficient samples
  }
  #
  return(list(strats=strat.defns,What.i=What.i,Nhat.ik=Nhat.ik,unloads.vs.wells=unload.matches,numwells.perstrat=nwells.perstrat.trimmed))
}