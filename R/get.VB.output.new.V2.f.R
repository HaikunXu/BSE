#' xxx
#' 
#' \code{get.VB.output.new.V2.f} yyy
#' 
#' @export

get.VB.output.new.V2.f = function(dir,name.unloads,name.cae,name.lfmm,name.lfgrpd,yr.start,yr.end)
{
  # Get the total unloads for the PS fleet
  # read in data
  tmpfrm<-read.table(paste0(dir,afile),header=F)
  names(tmpfrm)<-c("year","gear","flag","yft","skj","bet","pbf","alb","bkj","bzx")
  #
  # trim by gear and years
  tmpfrm.ps<-tmpfrm[tmpfrm$gear==2 & tmpfrm$year>=start.year & tmpfrm$year<=end.year,]
  #
  # compute annual sum of main three species 
  #  note: assumes there are no NAs in species columns
  trop.tunas<-tmpfrm.ps$yft+tmpfrm.ps$skj+tmpfrm.ps$bet
  total.unlds<-tapply(trop.tunas,tmpfrm.ps$year,sum)
  
  assign(paste("total.unlds.",yr.start,yr.end,sep=""),total.unlds,pos=1)
  #
  # Get the CAE+IDM data
  cae<-read.cae.new.V2.f(aname.cae,yr.start,yr.end)
  assign(paste("cae.",yr.start,yr.end,sep=""),cae,pos=1)
  #
  # Get the length-frequency data (length in millimeters
  lfmm<-read.lfmmdata.f(aname.lfmm)
  assign(paste("lfmm.",yr.start,yr.end,sep=""),lfmm,pos=1)
  #
  # Get the grouped length-frequency output
  lfgrpd<-read.lengthfreq.f(aname.lfgrpd)
  assign(paste("lfgrpd.",yr.start,yr.end,sep=""),lfgrpd,pos=1)
}