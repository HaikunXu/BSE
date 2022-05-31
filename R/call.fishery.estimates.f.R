#' xxx
#' 
#' \code{call.fishery.estimates.f} yyy
#' 
#' @export

call.fishery.estimates.f = function(strat.ests.withsamps,unlds.bystrat,iyear,PS,Species) {
  # single spp PS
  #
  # this function calls fishery.estimates.f for each of the three species (bet, yft, skj)
  # this function uses create.fishery.flg.f
  #
  # strat.ests.withsamps is output of call.stratum.estimates.f
  # unlds.bystrat is output of get.strat.unloads.f
  # iyear is year, used only to add to output
  #

  if(Species=="BET"){
    bet.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$bet.totunlds.bystr,str.defns=unlds.bystrat$bet.str.defns)
    output<-fishery.estimates.7599.f(strat.ests.withsamps$bet,bet.unlds.bystrat,iyear,PS,Species)
  }

  if(Species=="YFT"){
    yft.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$yft.totunlds.bystr,str.defns=unlds.bystrat$yft.str.defns)
    output<-fishery.estimates.7599.f(strat.ests.withsamps$yft,yft.unlds.bystrat,iyear,PS,Species)
  }
  if(Species=="SKJ"){
    skj.unlds.bystrat<-list(totunlds.bystr=unlds.bystrat$skj.totunlds.bystr,str.defns=unlds.bystrat$skj.str.defns)
    output<-fishery.estimates.7599.f(strat.ests.withsamps$skj,skj.unlds.bystrat,iyear,PS,Species)
  }
  
  #
  # output results
  return(list(bet=output,yft=output,skj=output))
}