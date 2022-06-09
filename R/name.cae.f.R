#' xxx
#' 
#' \code{name.cae.f} yyy
#' 
#' @export

name.cae.f = function(cae) {
  names(cae)<-c("tripno","vesno","depyr","depmoda","arryr","arrmoda","flag","gear","source","year","month","day","latc1","lonc1","latc5","lonc5","class","vescap.mt","vescap.mcube.current","vescap.mcube.historical","dml.trip","days.fishing","num.dsets","num.ssets","num.lsets","dph.yft","dph.skj","dph.bet","dph.pbf","sch.yft","sch.skj","sch.bet","sch.pbf","log.yft","log.skj","log.bet","log.pbf")
  return(cae)
}