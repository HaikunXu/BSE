#' xxx
#' 
#' \code{read.unloads.f} yyy
#' 
#' @export

read.unloads.f = function(dir,file,start.year,end.year) {
  # reads in unloadings data that were output by Miscellaneous VB program
  #   named: Get prorated Unload (June 11, 2014), which is under: Cleridy data files
  #
  # afile is file name; for path to file, see read.table call below
  # start.year and end.year are the begining and ending years to use
  #
  # yft+skj+bet purse-seine unloads are formated for use with other stock assessment R programs
  #
  # read in data
  tmpfrm<-read.table(paste0(dir,file),header=F)
  names(tmpfrm)<-c("year","gear","flag","yft","skj","bet","pbf","alb","bkj","bzx")
  #
  # trim by gear and years
  tmpfrm.ps<-tmpfrm[tmpfrm$gear==2 & tmpfrm$year>=start.year & tmpfrm$year<=end.year,]
  #
  # compute annual sum of main three species 
  #  note: assumes there are no NAs in species columns
  trop.tunas<-tmpfrm.ps$yft+tmpfrm.ps$skj+tmpfrm.ps$bet
  sum.tunas.byyr<-tapply(trop.tunas,tmpfrm.ps$year,sum)
  
  # return annual sums of tropical tuna unloads
  return(sum.tunas.byyr)
}