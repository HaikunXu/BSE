#' xxx
#' 
#' \code{check.strat.flg.f} yyy
#' 
#' @export

check.strat.flg.f = function(lat.5deg,lon.5deg,stratflg) {
  # plot area code to double-check
  strat.flg <- data.frame(lat=lat.5deg,lon=lon.5deg,area=stratflg$area)
  strat.flg <- unique(strat.flg)
  
  plot(x=strat.flg$lon[strat.flg$area==1],y=strat.flg$lat[strat.flg$area==1],pch="1",
       xlim=c(min(lon.5deg),max(lon.5deg)),ylim=c(min(lat.5deg),max(lat.5deg)),
       xlab="Lon",ylab="Lat")
  if(length(unique(stratflg$area))>1) {
    for (a in 2:length(unique(stratflg$area))) {
      points(x=strat.flg$lon[strat.flg$area==a],y=strat.flg$lat[strat.flg$area==a],pch=toString(a))
    }
  }
}