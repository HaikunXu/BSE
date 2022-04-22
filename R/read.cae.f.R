#' xxx
#' 
#' \code{read.cae.f} yyy
#' 
#' @export

read.cae.f = function(dir,file,start.year,end.year) {
  # reads in CAE+IDM data from Miscellaneous VB program: CAE data, which is under: Data dump files
  # afile is input file name; path is specified below in read.table call
  # keeps only PS data (BB is gear==1; PS is gear==2), and data from start.year to end.year
  #
  # January 24, 2017: edited version of read.cae.f, to handle new format of CAE VB program output
  #
  # read in data
  tmpcae.frm<-read.table(paste0(dir,file),header=F)
  names(tmpcae.frm)<-c("tripno","vesno","depyr","depmoda","arryr","arrmoda","flag","gear","source","year","month","day","latc1","lonc1","latc5","lonc5","class","vescap.mt","vescap.mcube.current","vescap.mcube.historical","dml.trip","days.fishing","num.dsets","num.ssets","num.lsets","dph.yft","dph.skj","dph.bet","dph.pbf","sch.yft","sch.skj","sch.bet","sch.pbf","log.yft","log.skj","log.bet","log.pbf")
  #
  # subset data by gear and year
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$gear==2 & tmpcae.frm$year>=start.year & tmpcae.frm$year<=end.year,]
  #
  # trim data frame for EPO
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$latc5>=(-47.5) & tmpcae.frm$latc5<=(47.5) & tmpcae.frm$lonc5<0 & tmpcae.frm$lonc5>=(-147.5) & !is.na(tmpcae.frm$latc5) & !is.na(tmpcae.frm$lonc5),]
  #
  # compute sum of three main tuna species - THESE LINES CURRENTLY NOT USED
  # tmptunas<-tmpcae.frm[,c("dph.yft","dph.skj","dph.bet","sch.yft","sch.skj","sch.bet","log.yft","log.skj","log.bet")]
  # tmptunas[is.na(tmptunas)]<-0
  # sum.trop.tunas<-apply(tmptunas,1,sum)
  #
  # compute sum of three main tuna species by set type and vessel size class
  tmp.dphtunas<-tmpcae.frm[,c("dph.yft","dph.skj","dph.bet")]
  tmp.dphtunas[is.na(tmp.dphtunas)]<-0
  dph.trop.tunas<-apply(tmp.dphtunas,1,sum)
  dph.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class")],dph.trop.tunas)[dph.trop.tunas>0,]
  names(dph.out)[ncol(dph.out)]<-"sum.trop.tunas"
  dph.out$setype<-rep(1,nrow(dph.out))
  #
  tmp.schtunas<-tmpcae.frm[,c("sch.yft","sch.skj","sch.bet")]
  tmp.schtunas[is.na(tmp.schtunas)]<-0
  sch.trop.tunas<-apply(tmp.schtunas,1,sum)
  sch.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class")],sch.trop.tunas)[sch.trop.tunas>0,]
  names(sch.out)[ncol(sch.out)]<-"sum.trop.tunas"
  sch.out$setype<-rep(4,nrow(sch.out))
  #
  tmp.logtunas<-tmpcae.frm[,c("log.yft","log.skj","log.bet")]
  tmp.logtunas[is.na(tmp.logtunas)]<-0
  log.trop.tunas<-apply(tmp.logtunas,1,sum)
  log.out<-data.frame(tmpcae.frm[,c("tripno","vesno","flag","gear","source","year","month","latc5","lonc5","class")],log.trop.tunas)[log.trop.tunas>0,]
  names(log.out)[ncol(log.out)]<-"sum.trop.tunas"
  log.out$setype<-rep(5,nrow(log.out))
  # 
  # make output data frame
  output.frm<-rbind(dph.out,sch.out,log.out)
  # return output
  return(output.frm)
}