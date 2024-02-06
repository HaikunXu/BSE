#' xxx
#' 
#' \code{call.fishery.estimates.BET.old.f} yyy
#' 
#' @export

call.fishery.estimates.BET.old.f = function(unlds.bystrat,iyear,PS) {
  # single spp PS
  #
  # this function calls fishery.estimates.f for each of the three species (bet, yft, skj)
  # this function uses create.fishery.flg.f
  #
  # strat.ests.withsamps is output of call.stratum.estimates.f
  # unlds.bystrat is output of get.strat.unloads.f
  # iyear is year, used only to add to output
  #
  # February 8, 2015: edited call.fishery.estimates.f to run only on yft
  #
  # bet
  bet.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$bet.totunlds.bystr,str.defns=unlds.bystrat$bet.str.defns)
  bet.output<-fishery.estimates.catchonly.f(bet.unlds.bystrat,iyear,PS)
  #
  # yft
  # yft.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$yft.totunlds.bystr,str.defns=unlds.bystrat$yft.str.defns)
  # yft.output<-fishery.estimates.f(strat.ests.withsamps$yft,yft.unlds.bystrat,iyear)
  #
  # skj
  # skj.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$skj.totunlds.bystr,str.defns=unlds.bystrat$skj.str.defns)
  # skj.output<-fishery.estimates.f(strat.ests.withsamps$skj,skj.unlds.bystrat,iyear)
  #
  # output results
  return(bet.output)
}