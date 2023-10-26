#' xxx
#' 
#' \code{read.cae.old.f} yyy
#' 
#' @export

read.cae.old.f = function(dir,file,start.year,end.year) {
  # single spp P-S
  # reads in CAE(+IDM, but I don't think IDM existed prior to 2000...) data from Miscellaneous VB program: 
  #     CAE data, which is under: Data dump files
  # afile is input file name; path is specified below in read.table call
  # keeps only PS data (BB is gear==1; PS is gear==2), and data from start.year to end.year
  #
  # January 26, 2017: edited read.table section to accomodate new fields added to CAE extraction
  #
  # read in data
  tmpcae.frm<-read.table(paste0(dir,file),header=F)
  names(tmpcae.frm)<-c("tripno","vesno","flag","gear","source","year","month","day","latc1","lonc1","latc5","lonc5","class","vescap.mt","vescap.mcube.current","vescap.mcube.historical","days.fishing","num.dsets","num.ssets","num.lsets","dph.yft","dph.skj","dph.bet","dph.pbf","sch.yft","sch.skj","sch.bet","sch.pbf","log.yft","log.skj","log.bet","log.pbf")
  #
  # subset data by gear and year
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$gear==2 & tmpcae.frm$year>=start.year & tmpcae.frm$year<=end.year,]
  #
  # trim data frame for EPO (and trim data without 5 deg information)
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$latc5>=(-47.5) & tmpcae.frm$latc5<=(47.5) & tmpcae.frm$lonc5<0 & tmpcae.frm$lonc5>=(-147.5) & !is.na(tmpcae.frm$latc5) & !is.na(tmpcae.frm$lonc5),]
  #
  # get dolphin-set information, when there is catch
  tmp.dphtunas<-tmpcae.frm[,c("dph.yft","dph.skj","dph.bet")]
  tmp.dphtunas[is.na(tmp.dphtunas)]<-0
  dph.trop.tunas<-apply(tmp.dphtunas,1,sum)
  dph.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class","dph.bet","dph.yft","dph.skj")])[dph.trop.tunas>0,]
  names(dph.out)[(ncol(dph.out)-2):ncol(dph.out)]<-c("bet","yft","skj")
  dph.out$setype<-rep(1,nrow(dph.out))
  #
  # get unassociated-set information, when there is catch
  tmp.schtunas<-tmpcae.frm[,c("sch.yft","sch.skj","sch.bet")]
  tmp.schtunas[is.na(tmp.schtunas)]<-0
  sch.trop.tunas<-apply(tmp.schtunas,1,sum)
  sch.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class","sch.bet","sch.yft","sch.skj")])[sch.trop.tunas>0,]
  names(sch.out)[(ncol(sch.out)-2):ncol(sch.out)]<-c("bet","yft","skj")
  sch.out$setype<-rep(4,nrow(sch.out))
  #
  # floating object-set information, when there is catch
  tmp.logtunas<-tmpcae.frm[,c("log.yft","log.skj","log.bet")]
  tmp.logtunas[is.na(tmp.logtunas)]<-0
  log.trop.tunas<-apply(tmp.logtunas,1,sum)
  log.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class","log.bet","log.yft","log.skj")])[log.trop.tunas>0,]
  names(log.out)[(ncol(log.out)-2):ncol(log.out)]<-c("bet","yft","skj")
  log.out$setype<-rep(5,nrow(log.out))
  # 
  # make output data frame
  output.frm<-rbind(dph.out,sch.out,log.out)
  # return output
  return(output.frm)
}