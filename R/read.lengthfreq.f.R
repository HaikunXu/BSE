#' xxx
#' 
#' \code{read.lengthfreq.f} yyy
#' 
#' @export

read.lengthfreq.f = function(dir,file) {
  # reads grouped length frequency data file (LengthFreqyyyy-yyyy.txt) output by VB program: Length frequency II (May 16, 2008), 
  #   which is under: Cleridy data files
  #
  tmpfrm<-read.table(paste0(dir,file),header=F)
  tmp.names<-paste(rep("lfcm.",202),as.character(seq(0,201,1)),sep="")
  names(tmpfrm)<-c("tripno","wellsampno","sampno","vesno","flag","class","gear","setype","year.firstset","moda.firstset","numdays.fillwell","numsets","lat.center","lon.center","maxset.distance","numlengths.msred","sorted.flg","sample.area","cannery.code","sample.method","sampler.lf","sampler.cnt","stratum.code","lat.5deg","lon.5deg","sqr1","totalmt.catch","species","sample.size","fish.count","fishcount.est","total.pbfcount","avgwt","estcatchwt","estnumfish","pcnt.totalcatch","totalmt.yftskjbet","size.lower","size.upper","lfwellsect","countwellsect",tmp.names)
  #
  # remove samples from: baitboat/sport/etc, outside EPO and samples that do not have bet, yft or skj
  tmpfrm<-tmpfrm[tmpfrm$gear==2 & tmpfrm$lon.5deg<0 & tmpfrm$lon.5deg>(-150) & !is.na(tmpfrm$lon.5deg) & tmpfrm$lat.5deg>(-50) & tmpfrm$lat.5deg<(50) & !is.na(tmpfrm$lat.5deg) & (tmpfrm$species==106 | tmpfrm$species==110 | tmpfrm$species==111),]
  #
  return(tmpfrm)
}
