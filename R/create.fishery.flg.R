#' xxx
#' 
#' \code{create.fishery.flg.f} yyy
#' 
#' @export

create.fishery.flg.f = function(strat.defns,PS,Species)
{
  if(Species=="SKJ") return(create.fishery.flg.f.SKJ(strat.defns,PS))
  if(Species=="YFT") return(create.fishery.flg.f.YFT(strat.defns,PS))
  if(Species=="BET") return(create.fishery.flg.f.BET(strat.defns,PS))
}
