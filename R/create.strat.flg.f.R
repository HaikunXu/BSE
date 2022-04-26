#' xxx
#' 
#' \code{create.strat.flg.f} yyy
#' 
#' @export

create.strat.flg.f = function(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS,Species) {
  if(Species=="SKJ") return(create.strat.flg.f.SKJ(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS))
  if(Species=="YFT") return(create.strat.flg.f.YFT(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS))
  if(Species=="BET") return(create.strat.flg.f.BET(lat.5deg,lon.5deg,is.lwrght,month,setype,vessel.class,PS))
}