#' xxx
#' 
#' \code{get.summedLF.5deg.7599.YFT.f} yyy
#' 
#' @export

get.summedLF.5deg.7599.YFT.f = function(well.samps,dph.vesno.flg,nyear) {
  # May 21 2019: edited to run on format of well.estimates lists for 1975-1999
  # June 3 2019: edited to output number of well samples per cell
  # NOTE: ONLY runs for YFT
  #
  # get 5 degree square, QUARTER and set type information in data
  quarter<-rep(1,length(well.samps$ancillary.info$yft$month))
  quarter[well.samps$ancillary.info$yft$month>=4 & well.samps$ancillary.info$yft$month<=6]<-2
  quarter[well.samps$ancillary.info$yft$month>=7 & well.samps$ancillary.info$yft$month<=9]<-3
  quarter[well.samps$ancillary.info$yft$month>=10]<-4
  #
  dg5.setype.qrtr.id<-paste(well.samps$ancillary.info$yft$lat.5deg[dph.vesno.flg],well.samps$ancillary.info$yft$lon.5deg[dph.vesno.flg],well.samps$ancillary.info$yft$setype[dph.vesno.flg],quarter[dph.vesno.flg],sep=":")
  unq.setype.dg5.qrtr.id<-unique(dg5.setype.qrtr.id)
  num.setype.dg5.qrtr.id<-length(unq.setype.dg5.qrtr.id)
  #
  num.wellsamps<-rep(NA,num.setype.dg5.qrtr.id)
  #
  tmplf<-well.samps$Nhat.jk$yft[dph.vesno.flg,]
  yft.summedlf<-matrix(NA,ncol=201,nrow=num.setype.dg5.qrtr.id)
  # sum yft LF by set type, 5 degree square and quarter
  for(i in 1:num.setype.dg5.qrtr.id){
    if(num.setype.dg5.qrtr.id==1) tmplf <- matrix(tmplf,nrow=1,ncol=length(tmplf))
    tmplf.sub<-tmplf[dg5.setype.qrtr.id==unq.setype.dg5.qrtr.id[i],]
    if(is.matrix(tmplf.sub)){
      yft.summedlf[i,]<-as.vector(apply(tmplf.sub,2,sum))
      num.wellsamps[i]<-nrow(tmplf.sub)
    } else {
      yft.summedlf[i,]<-as.vector(tmplf.sub)
      num.wellsamps[i]<-1
    }
  }
  #
  xvars <- data.frame(matrix(as.numeric(unlist(strsplit(unq.setype.dg5.qrtr.id, "\\:"))), ncol = 4, byrow = T))
  ancil.frm <- data.frame(xvars,num.wellsamps)
  names(ancil.frm)<-c("lat.5deg","lon.5deg","setype","quarter","num.wellsamps")
  ancil.frm$year<-rep(nyear,length(ancil.frm$setype))
  #
  return(list(ancil.info=ancil.frm,lf=yft.summedlf))
}