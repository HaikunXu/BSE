#' xxx
#' 
#' \code{read.caedata.1deg.withvesscap.f} yyy
#' 
#' @export

read.caedata.1deg.withvesscap.f = function(afile,yr.start,yr.end) {
  # reads in CAE data that is output by Data dump VB program CAE data, with option for 1 degree
  # afile includes path
  tmpfrm<-read.table(afile,header=F)
  names(tmpfrm)<-c("tripno","vesno","depyr","depmoda","arryr","arrmoda","flag","gear","source","year","month","day","latc1","lonc1","latc5","lonc5","class","vescap.mt","vescap.mcube.current","vescap.mcube.historical","dml.trip","days.fishing","num.dsets","num.ssets","num.lsets","dph.yft","dph.skj","dph.bet","dph.pbf","sch.yft","sch.skj","sch.bet","sch.pbf","log.yft","log.skj","log.bet","log.pbf")
  # trim for EPO and P-S
  tmpfrm<-tmpfrm[tmpfrm$gear==2 & tmpfrm$latc1<50 & tmpfrm$latc1>(-50) & !is.na(tmpfrm$latc1) & tmpfrm$lonc1>(-150) & tmpfrm$lonc1<(-60) & !is.na(tmpfrm$lonc1),]
  # set NAs in number of days fishing, sets and catch to zero
  tmp.lst<-c("days.fishing","num.dsets","num.ssets","num.lsets","dph.yft","dph.skj","dph.bet","dph.pbf","sch.yft","sch.skj","sch.bet","sch.pbf","log.yft","log.skj","log.bet","log.pbf")
  tmpx<-match(tmp.lst,names(tmpfrm))
  n.match<-length(tmpx)
  for(i in 1:n.match){
    tmpfrm[is.na(tmpfrm[,tmpx[i]]),tmpx[i]]<-0
  }
  # trim to years in yr.start to yr.end
  tmpfrm<-tmpfrm[tmpfrm$year>=yr.start & tmpfrm$year<=yr.end,]
  # drop records with a value 0 (apparently these are for IDM and SetSum data; days fishing are not computed for those sources)
  tmpfrm<-tmpfrm[tmpfrm$days.fishing!=0,]
  #
  # tmpfrm$vescap.mcube.historical[tmpfrm$vescap.mcube.historical==0]<-NA
  #
  return(tmpfrm)
}