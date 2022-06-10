#' xxx
#' 
#' \code{read.cae.cpue.f} yyy
#' 
#' @export

read.cae.cpue.f = function(dir,file,start.year,end.year) {
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
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$gear==2 & tmpcae.frm$year>=start.year & tmpcae.frm$year<=end.year,] # gear = 2 is PS
  #
  # trim data frame for EPO
  tmpcae.frm<-tmpcae.frm[tmpcae.frm$latc5>=(-47.5) & tmpcae.frm$latc5<=(47.5) & tmpcae.frm$lonc5<0 & tmpcae.frm$lonc5>=(-147.5) & !is.na(tmpcae.frm$latc5) & !is.na(tmpcae.frm$lonc5),]
  return(tmpcae.frm)
}