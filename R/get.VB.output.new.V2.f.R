#' xxx
#' 
#' \code{get.VB.output.new.V2.f} yyy
#' 
#' @export

get.VB.output.new.V2.f = function(aname.unloads,aname.cae,aname.lfmm,aname.lfgrpd,yr.start,yr.end)
{
  # spp comp P-S
  #
  # get and minimally processes the data output by the Miscellaneous.exe VB programs
  #
  # the filenames of the VB output are specified with: aname.*
  # yr.start and yr.end are the start and end years of the data
  #
  # January 24, 2017: edited to use new cae read function: read.cae.new.f
  #
  # Get the total unloads for the PS fleet
  total.unlds<-read.unloads.f(aname.unloads,yr.start,yr.end)
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