#' xxx
#' 
#' \code{fishery.estimates.f} yyy
#' 
#' @export

fishery.estimates.f = function(strat.ests.withsamps,strat.ests.nosamps,iyear,PS,Species)
{
  # creates stock assessment estimates of total species catch (mt) and species size comps (wt-avg proportion fish)
  #   within stock assessments fisheries (quarter x areas x gears)
  #
  # species total catch estiamtes are the sum of total stratum catches by species for all catch strata in a fishery
  # species size comp estimates are the weighted average of size comps from strata with samples (weights are number of wells sampled per stratum)
  #
  # strat.ests.withsamps is output of function stratum.estimates.f
  # strat.ests.nosamps is output of function substitute.f (used only for species catch)
  # iyear the year for which estimates are being made; used only to add column with year to output data frame
  #
  # this function uses: create.fishery.flg.f
  #
  # edited 12.15.14: added number of wells sampled to output for stratum with sample data
  #
  # Estimates from strata WITH sample data
  #
  # get fishery id information (id vector; unique id vector; number unique ids)
  fishery.samps.defns<-create.fishery.flg.f(strat.ests.withsamps$strats,PS,Species)
  fishery.samps.id<-paste(fishery.samps.defns$fishery.areagear,fishery.samps.defns$fishery.quarter,sep=":")
  fishery.samps.id.unq<-unique(fishery.samps.id)
  n.fshry.samps<-length(fishery.samps.id.unq)
  #
  # initialize species catch and size comp output storage and stratum definitions output 
  Whati.byfshry.samps<-data.frame(matrix(NA,ncol=3,nrow=n.fshry.samps))
  names(Whati.byfshry.samps)<-c("bet","yft","skj")
  #
  phatik.byfshry.samps<-array(NA,dim=c(n.fshry.samps,201,3))
  #
  fishery.defns.samps<-data.frame(matrix(NA,ncol=3,nrow=n.fshry.samps))
  names(fishery.defns.samps)<-c("year","geararea.quarter","nwells.sampled")
  #
  # loop over number of fisheries
  for(ifsh in 1:n.fshry.samps){
    # save fishery defitions (including year)
    fishery.defns.samps$year[ifsh]<-iyear
    fishery.defns.samps$geararea.quarter[ifsh]<-fishery.samps.id.unq[ifsh]
    fishery.defns.samps$nwells.sampled[ifsh]<-sum(strat.ests.withsamps$numwells.perstrat[fishery.samps.id==fishery.samps.id.unq[ifsh]])
    #
    # NOT USED number of wells in this fishery geararea x quarter (for size comps)
    #    nwells.tot<-sum(strat.ests.withsamps$numwells.perstrat[fishery.samps.id==fishery.samps.id.unq[ifsh]])
    #
    # get species estimates
    #
    # number of individual wells per catch stratum for this fishery stratum (used for size comps)
    tmp.nwells<-as.vector(strat.ests.withsamps$numwells.perstrat[fishery.samps.id==fishery.samps.id.unq[ifsh]])
    #
    # loop over three species (bet, yft, skj) 
    for(ispp in 1:3){
      # catch
      Whati.byfshry.samps[ifsh,ispp]<-sum(strat.ests.withsamps$What.i[fishery.samps.id==fishery.samps.id.unq[ifsh],ispp])
      # size comps
      # check how many catch strata have this species, and compute proportions accordingly
      #  sum number of fish by catch strata
      tmpsums<-as.vector(strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp]%*%rep(1,201))
      #  flag 0 and non-0 strata
      tmpsums.zeroflg<-rep(0,length(tmpsums))
      tmpsums.zeroflg[tmpsums>0]<-1
      # no fish at all
      if(sum(tmpsums.zeroflg)==0){phatik.byfshry.samps[ifsh,,ispp]<-rep(0,201)}
      # fish in one catch stratum
      if(sum(tmpsums.zeroflg)==1){
        if(length(tmpsums)==1){
          # and, there is only one catch stratum 
          phatik.byfshry.samps[ifsh,,ispp]<-strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp]/sum(strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp])
        } else {
          # and, there is more than one catch stratum, but only 1 with this spp
          phatik.byfshry.samps[ifsh,,ispp]<-strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp][tmpsums.zeroflg==1,]/sum(strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp][tmpsums.zeroflg==1,])
        }
      }
      # fish in more than one catch stratum
      if(sum(tmpsums.zeroflg)>1){
        tmpspp<-(strat.ests.withsamps$Nhat.ik[fishery.samps.id==fishery.samps.id.unq[ifsh],,ispp][tmpsums.zeroflg==1,]*tmp.nwells[tmpsums.zeroflg==1])/tmpsums[tmpsums.zeroflg==1]
        phatik.byfshry.samps[ifsh,,ispp]<-(1/sum(tmp.nwells*tmpsums.zeroflg))*apply(tmpspp,2,sum,na.rm=T)           
      }
    }
  }
  #
  # Estimates from strata WITHOUT sample data (species catch only)
  #
  # get fishery id information (id vector; unique id vector; number unique ids)
  fishery.NOsamps.defns<-create.fishery.flg.f(strat.ests.nosamps$strats,PS,Species)
  fishery.NOsamps.id<-paste(fishery.NOsamps.defns$fishery.areagear,fishery.NOsamps.defns$fishery.quarter,sep=":")
  fishery.NOsamps.id.unq<-unique(fishery.NOsamps.id)
  n.fshry.NOsamps<-length(fishery.NOsamps.id.unq)
  #
  # initialize species catch and stratum definitions output 
  Whati.byfshry.NOsamps<-data.frame(matrix(NA,ncol=3,nrow=n.fshry.NOsamps))
  names(Whati.byfshry.NOsamps)<-c("bet","yft","skj")
  strat.defns.NOsamps<-data.frame(matrix(NA,ncol=2,nrow=n.fshry.NOsamps))
  names(strat.defns.NOsamps)<-c("year","geararea.quarter")
  #
  # loop over number of fisheries
  for(ifsh in 1:n.fshry.NOsamps){
    # fishery definitions (including year)
    strat.defns.NOsamps$year[ifsh]<-iyear
    strat.defns.NOsamps$geararea.quarter[ifsh]<-fishery.NOsamps.id.unq[ifsh]
    #
    # get species catch
    # loop over three species (bet, yft, skj) 
    for(ispp in 1:3){
      Whati.byfshry.NOsamps[ifsh,ispp]<-sum(strat.ests.nosamps$What.i[fishery.NOsamps.id==fishery.NOsamps.id.unq[ifsh],ispp])
    }
  }
  #
  # finally, get total species catch estimates for fishery-quarter 
  #   need this step because fisheries can have catch coming from both catch strata with samples and catch strata without samples
  # get area-gear-quarter id
  tmp.fshqrtr.id<-c(fishery.defns.samps$geararea.quarter,strat.defns.NOsamps$geararea.quarter)
  total.fshqrtr.id.unq<-sort(unique(tmp.fshqrtr.id))
  nfinal<-length(total.fshqrtr.id.unq)
  # get catch
  tmp.catch.fshqrtr<-rbind(Whati.byfshry.samps,Whati.byfshry.NOsamps)
  total.catch.fshqrtr<-matrix(0,nrow=nfinal,ncol=3) 
  # sum over strata with and without samples
  for(ifnl in 1:nfinal){
    for(ispp in 1:3){
      total.catch.fshqrtr[ifnl,ispp]<-sum(tmp.catch.fshqrtr[tmp.fshqrtr.id==total.fshqrtr.id.unq[ifnl],ispp],na.rm=T)
    }
  }
  # format total catch output
  total.catch.byspp<-data.frame(rep(iyear,nfinal),total.fshqrtr.id.unq,total.catch.fshqrtr)
  names(total.catch.byspp)<-c("year","geararea.quarter","bet","yft","skj")
  #
  return(list(fishery.defn.samps=fishery.defns.samps,catch.samps=Whati.byfshry.samps,phatik.byfshry.samps=phatik.byfshry.samps,fishery.defn.NOsamps=strat.defns.NOsamps,catch.NOsamps=Whati.byfshry.NOsamps,total.catch.byspp=total.catch.byspp))
}