#' xxx
#' 
#' \code{get.sub.f} yyy
#' 
#' @export

get.sub.f = function(area.str,mon.str,gear.str,fishery.areagear,lfgrp.stratflg,gear.submat,area.submat,month.submat,min.subsize,lfgrp.sampleid)
{
  # this function finds a substitute stratum for an input stratum; this function is called by substitute.f 
  #
  # area.str, mon.str, gear.str and fishery.areagear are the input stratum values (stratum with missing/insufficient sample data)
  #
  # lfgrp.stratflg is output of create.stratflg.f applied to grouped length-frequency data (e.g. lfgrpd.all.frm) 
  #
  # lfgrp.sampleid is the sample identifier (tripno:wellsampno) that corresponds to lfgrp.stratflg (and lfgrpd.all.frm which is not passed)(added Dec 2021)
  #
  # the matrices area.submat, month.submat and gear.submat determine which substitutions can be made
  #  they each have the number of rows equal to the number of unique areas, months, gears, respectively.
  #  their respective number of columns should be the maximum number of allowed substitutions.
  #  the first column of each matrix gives the unique area, month, or gear values, sorted from lowest to highest.
  #  it is assumed that area, month, gear codes are numeric.
  #
  # min.subsize is the minimum number of samples for a stratum to be a substitute; 
  #       probably should be same as min.sampsize used in call to stratum.estimates.f
  #
  # the order preference for substitutions is: 
  #       (1) change month, keeping same area and gear
  #       (2) change gear (and then month), keeping same area
  #       (3) change area within fishery region,  then month (same gear), gear (same month), then month and gear
  #       (4) last resort is to use fishery region (pooled over month and gears allowed by gear.submat for gear.str; see well.miss.f) 
  #                NOTE: may want to improve this, particularly option (4)...
  #
  # edited 12.15.14: added print if reach step (4) because with current area and fishery defns, i think this step is not reached/needed; needs to be revised
  # edited Dec 2021: to fix bug when computing substitute stratum (nsamps was not computed correctly because rows in lfgrp.stratflg are not necessarily unique samples)
  #
  # make look up matrices for gear, area (it is assumed month is 1 to 12, and so no look-up matrix is needed)
  unq.gear<-sort(unique(lfgrp.stratflg$gear))
  n.gears<-length(unq.gear)
  gear.lookup<-matrix(c(unq.gear,seq(1,n.gears)),ncol=2,nrow=n.gears,byrow=F)
  #
  unq.area<-sort(unique(lfgrp.stratflg$area))
  n.areas<-length(unq.area)
  area.lookup<-matrix(c(unq.area,seq(1,n.areas)),ncol=2,nrow=n.areas,byrow=F)
  #
  # get maximum column number for possible substitutions for each of area, month, gear
  max.areasub<-ncol(area.submat)
  max.monthsub<-ncol(month.submat)
  max.gearsub<-ncol(gear.submat)
  #
  # start search for a substitute stratum
  #
  # first try substituting data from same area and same gear, but month(s) before/after
  for(imon in 2:max.monthsub){
    if(!is.na(month.submat[month.submat[,1]==mon.str,imon])){
      # candidate is available, get samples for the new month
      new.month<-month.submat[month.submat[,1]==mon.str,imon]
      nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==area.str & lfgrp.stratflg$gear==gear.str & lfgrp.stratflg$month==new.month]))
      # check sample size
      if(nsamps>=min.subsize){
        # substitute found; stop
        return(list(area=area.str,month=new.month,gear=gear.str,grwshrk.flg=T,fishery.areagear=NA))
      }
      # end of test for month
    }
    # end of loop on imon
  } 
  #
  # if we get this far, we didn't find a neighboring month with data in area.str and gear.str
  # try substituting a new gear, and then a new month within new gear
  for(igr in 2:max.gearsub){
    if(!is.na(gear.submat[gear.submat[,1]==gear.str,igr])){
      # candidate is available, get samples for the new gear
      new.gear<-gear.submat[gear.submat[,1]==gear.str,igr]
      nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==area.str & lfgrp.stratflg$gear==new.gear & lfgrp.stratflg$month==mon.str]))
      # check sample size
      if(nsamps>=min.subsize){
        # substitute found; stop
        return(list(area=area.str,month=mon.str,gear=new.gear,grwshrk.flg=F,fishery.areagear=NA))       
      }
      #
      # no samples for this new gear at month=mon.str; check month(s) ahead/behind for this new gear
      for(imon in 2:max.monthsub){
        if(!is.na(month.submat[month.submat[,1]==mon.str,imon])){
          # candidate is available, get samples for the new month-gear
          new.month<-month.submat[month.submat[,1]==mon.str,imon]
          nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==area.str & lfgrp.stratflg$gear==new.gear & lfgrp.stratflg$month==new.month]))
          # check sample size
          if(nsamps>=min.subsize){
            # substitute found; stop
            return(list(area=area.str,month=new.month,gear=new.gear,grwshrk.flg=T,fishery.areagear=NA))
          }
          # end of test of month inside igr loop
        }
        # end of loop on imon inside igr loop
      }
      # end of test for gear
    }
    # end of loop on igr
  } 
  #
  # if we get this far, we didn't find a neighboring gear-month combination with data for this area.str
  # try a new area, then new month and/or new gear within new area, and then new area-gear-month
  for(iar in 2:max.areasub){
    if(!is.na(area.submat[area.submat[,1]==area.str,iar])){
      # candidate is available; get samples for the area
      new.area<-area.submat[area.submat[,1]==area.str,iar]
      nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==new.area & lfgrp.stratflg$gear==gear.str & lfgrp.stratflg$month==mon.str]))
      # check sample size
      if(nsamps>=min.subsize){
        # substitute found; stop
        return(list(area=new.area,month=mon.str,gear=gear.str,grwshrk.flg=F,fishery.areagear=NA))
      }
      # look for samples for new area-month, but same gear
      for(i3mon in 2:max.monthsub){
        if(!is.na(month.submat[month.submat[,1]==mon.str,i3mon])){
          # candidate is available; get samples for the month-area
          new.month<-month.submat[month.submat[,1]==mon.str,i3mon]
          nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==new.area & lfgrp.stratflg$gear==gear.str & lfgrp.stratflg$month==new.month]))
          # check sample size
          if(nsamps>=min.subsize){
            # substitute found; stop
            return(list(area=new.area,month=new.month,gear=gear.str,grwshrk.flg=T,fishery.areagear=NA))
          }
          # look for sample for new area-gear, but same month
          for(i2gr in 2:max.gearsub){
            if(!is.na(gear.submat[gear.submat[,1]==gear.str,i2gr])){
              # candidate is available; get samples for this area-gear
              new.gear<-gear.submat[gear.submat[,1]==gear.str,i2gr]
              nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==new.area & lfgrp.stratflg$gear==new.gear & lfgrp.stratflg$month==mon.str]))
              # check sample size
              if(nsamps>=min.subsize){
                # substitute found;stop
                return(list(area=new.area,month=mon.str,gear=new.gear,grwshrk.flg=F,fishery.areagear=NA))
              }
              # look for month(s) ahead/behind for this new area-gear (ie, look for new area-gear-month)
              for(i4mon in 2:max.monthsub){
                if(!is.na(month.submat[month.submat[,1]==mon.str,i4mon])){
                  # candidate available; set number of samples for this new area-gear-month
                  new.month<-month.submat[month.submat[,1]==mon.str,i4mon]
                  nsamps<-length(unique(lfgrp.sampleid[lfgrp.stratflg$area==new.area & lfgrp.stratflg$gear==new.gear & lfgrp.stratflg$month==new.month]))
                  # check sample size
                  if(nsamps>=min.subsize){
                    # substitute found; stop
                    return(list(area=new.area,month=new.month,gear=new.gear,grwshrk.flg=T,fishery.areagear=NA))
                  }
                  # end of test of month inside area-gear
                }
                # end of loop on i4mon   
              }
              # end of test for gear inside area
            }
            # end of loop on i2gr within area
          } 
          # end of test for month inside area
        }
        # end of loop on i3mon within area
      } 
      # end of test for area
    }
    # end of loop on iar
  }
  #
  # and, if we get this far, indicate that option (4) must be taken (fishery)
  print("WARNING: Reached step (4) in get.sub.f looking for substitute; please tell CL")
  return(list(area=area.str,month=mon.str,gear=gear.str,grwshrk.flg=F,fishery.areagear=fishery.areagear))
  #
}
