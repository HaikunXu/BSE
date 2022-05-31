#' xxx
#' 
#' \code{get.corrected.unloads.f} yyy
#' 
#' @export

get.corrected.unloads.f = function(raw_data_dir,afile,start.year,end.year,corrections.frm) {
  # single spp P-S
  # edited version of read.unloads.f to work on data prior to 2000
  #
  # reads in unloadings data that were output by Miscellaneous VB program
  #   named: Get prorated Unload (June 11, 2014), which is under: Cleridy data files
  # AND, corrects the species composition according to Pat's method (for details, see notes
  #   or Joanne's Excel spreadsheets: CatchEstAdjustedDone2005.xls, How bse applied early years.xls
  #
  # afile is file name; for path to file, see read.table call below
  # start.year and end.year are the begining and ending years to use
  # corrections.frm is the avg tab in Joannes spreadsheet 
  #
  # corrected purse-seine total species unloads are formated for use with other stock assessment R programs
  #
  # flag look-up list
  flag.lookup<-list(flag.no=c(3,5,7,8,10,12,16,28),a.flag=c("COL","ECU","MEX","PAN","USA","ESP","VEN","VUT"))
  #
  # read in data
  tmpfrm<-read.table(paste0(raw_data_dir,afile),header=F)
  names(tmpfrm)<-c("year","gear","flag","yft","skj","bet","pbf","alb","bkj","bzx")
  #
  # trim by gear and years
  tmpfrm.ps<-tmpfrm[tmpfrm$gear==2 & tmpfrm$year>=start.year & tmpfrm$year<=end.year,]
  #
  # trim species (retain only yft, bet, skj) and re-order to match order in corrections.frm
  tmpfrm.ps<-tmpfrm.ps[,c("year","gear","flag","bet","yft","skj")]
  #
  # get sum bet, yft, skj unloads by year flag
  tmpfrm.uncor.sum<-apply(tmpfrm.ps[,c("bet","yft","skj")],1,sum)
  #
  # apply species-flag specific correction factors to the unloads (note: correction factors are the same for all years)
  # first get unique flags in unloads data
  unq.flag.unlds<-sort(unique(tmpfrm.ps$flag))
  nflags<-length(unq.flag.unlds)
  # loop over individual flags that are not part of Others, apply adjustment to each of the three species
  for(iflg in 1:nflags){
    if(nrow(corrections.frm[corrections.frm$flag==flag.lookup$a.flag[flag.lookup$flag.no==unq.flag.unlds[iflg]],])==1){
      tmpfrm.ps$bet[tmpfrm.ps$flag==unq.flag.unlds[iflg]]<-tmpfrm.ps$bet[tmpfrm.ps$flag==unq.flag.unlds[iflg]]*corrections.frm$AvgOfBETr[corrections.frm$flag==flag.lookup$a.flag[flag.lookup$flag.no==unq.flag.unlds[iflg]]]
      tmpfrm.ps$yft[tmpfrm.ps$flag==unq.flag.unlds[iflg]]<-tmpfrm.ps$yft[tmpfrm.ps$flag==unq.flag.unlds[iflg]]*corrections.frm$AvgOfYFTr[corrections.frm$flag==flag.lookup$a.flag[flag.lookup$flag.no==unq.flag.unlds[iflg]]]
      tmpfrm.ps$skj[tmpfrm.ps$flag==unq.flag.unlds[iflg]]<-tmpfrm.ps$skj[tmpfrm.ps$flag==unq.flag.unlds[iflg]]*corrections.frm$AvgOfSKJr[corrections.frm$flag==flag.lookup$a.flag[flag.lookup$flag.no==unq.flag.unlds[iflg]]]
    }
  }
  # now adjust the Other flags (those in the unloads that are not in flag look-up table)
  found.flg<-rep(F,length(tmpfrm.ps$flag))
  found.flg[tmpfrm.ps$flag==3 | tmpfrm.ps$flag==5 | tmpfrm.ps$flag==7 | tmpfrm.ps$flag==8 | tmpfrm.ps$flag==10 | tmpfrm.ps$flag==12 | tmpfrm.ps$flag==16 | tmpfrm.ps$flag==28]<-T
  #
  tmpfrm.ps$bet[!found.flg]<-tmpfrm.ps$bet[!found.flg]*corrections.frm$AvgOfBETr[corrections.frm$flag=="Others"]
  tmpfrm.ps$yft[!found.flg]<-tmpfrm.ps$yft[!found.flg]*corrections.frm$AvgOfYFTr[corrections.frm$flag=="Others"]
  tmpfrm.ps$skj[!found.flg]<-tmpfrm.ps$skj[!found.flg]*corrections.frm$AvgOfSKJr[corrections.frm$flag=="Others"]
  #
  # sum these adjusted catches across species for each flag x year (these are total unloads by flag x year)
  tmpfrm.sum<-apply(tmpfrm.ps[,c("bet","yft","skj")],1,sum)
  #
  # compute species proportions by flag x year from the adjusted species catches 
  tmpfrm.sppprop<-tmpfrm.ps[,c("bet","yft","skj")]/tmpfrm.sum
  #  
  # compute corrected species unloads by flag x year = adjusted proportion x total unloads
  corrected.byflgyr.bet<-tmpfrm.sppprop[,1]*tmpfrm.uncor.sum
  corrected.byflgyr.yft<-tmpfrm.sppprop[,2]*tmpfrm.uncor.sum
  corrected.byflgyr.skj<-tmpfrm.sppprop[,3]*tmpfrm.uncor.sum 
  #
  # sum these corrected species catches across flags for each year 
  corrected.bet.byyear<-tapply(corrected.byflgyr.bet,tmpfrm.ps$year,sum)
  corrected.yft.byyear<-tapply(corrected.byflgyr.yft,tmpfrm.ps$year,sum)
  corrected.skj.byyear<-tapply(corrected.byflgyr.skj,tmpfrm.ps$year,sum)
  #
  # check that there is catch for each species in each year
  nyears<-end.year-start.year+1
  if(length(corrected.bet.byyear)!=nyears | length(corrected.yft.byyear)!=nyears | length(corrected.skj.byyear)!=nyears){
    print("*** ERROR: unequal number of years of catch of each species; edit function")
  }
  #
  # make data frame of yearly corrected species catches
  corrected.unloads<-data.frame(seq(start.year,end.year),corrected.bet.byyear,corrected.yft.byyear,corrected.skj.byyear)
  names(corrected.unloads)<-c("year","bet","yft","skj")
  #
  # return corrected annual species unloads
  return(corrected.unloads)
}