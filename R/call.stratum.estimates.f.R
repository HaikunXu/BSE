#' xxx
#' 
#' \code{call.stratum.estimates.f} yyy
#' 
#' @export

call.stratum.estimates.f = function(unloads.bystrat,well.estimates,well.strats,min.sampsize) {
  # single spp P-S
  #
  # Calls function stratum.estimates.f for each of three species: bet, yft, skj
  # edited January 8, 2015: added species names print statements
  #
  # bet
  if(nrow(well.estimates$ancillary.info[[1]])>=min.sampsize & !is.null(unloads.bystrat$bet.totunlds.bystr)){
    # then hopefully we have enough well-level summaries in at least one stratum to make estimates for bet... 
    print("running BET")
    bet.unloads<-list(totunlds.bystr=unloads.bystrat$bet.totunlds.bystr,str.defns=unloads.bystrat$bet.str.defns)
    bet.wellests<-list(ancillary.info=well.estimates$ancillary.info[[1]],wbar.j=well.estimates$wbar.j[[1]],Nhat.j=well.estimates$Nhat.j[[1]],Nhat.jk=well.estimates$Nhat.jk[[1]])
    bet.strests<-stratum.estimates.7599.f(bet.unloads,bet.wellests,well.strats$bet,min.sampsize)
  } else {
    bet.strests<-NULL
  }
  # yft
  if(nrow(well.estimates$ancillary.info[[2]])>=min.sampsize & !is.null(unloads.bystrat$yft.totunlds.bystr)){
    # then hopefully we have enough well-level summaries in at least one stratum to make estimates for yft... 
    print("running YFT")
    yft.unloads<-list(totunlds.bystr=unloads.bystrat$yft.totunlds.bystr,str.defns=unloads.bystrat$yft.str.defns)
    yft.wellests<-list(ancillary.info=well.estimates$ancillary.info[[2]],wbar.j=well.estimates$wbar.j[[2]],Nhat.j=well.estimates$Nhat.j[[2]],Nhat.jk=well.estimates$Nhat.jk[[2]])
    yft.strests<-stratum.estimates.7599.f(yft.unloads,yft.wellests,well.strats$yft,min.sampsize)
  } else {
    yft.strests<-NULL
  }
  # skj
  if(nrow(well.estimates$ancillary.info[[3]])>=min.sampsize & !is.null(unloads.bystrat$skj.totunlds.bystr)){
    # then hopefully we have enough well-level summaries in at least one stratum to make estimates for skj... 
    print("running SKJ")
    skj.unloads<-list(totunlds.bystr=unloads.bystrat$skj.totunlds.bystr,str.defns=unloads.bystrat$skj.str.defns)
    skj.wellests<-list(ancillary.info=well.estimates$ancillary.info[[3]],wbar.j=well.estimates$wbar.j[[3]],Nhat.j=well.estimates$Nhat.j[[3]],Nhat.jk=well.estimates$Nhat.jk[[3]])
    skj.strests<-stratum.estimates.7599.f(skj.unloads,skj.wellests,well.strats$skj,min.sampsize)
  } else {
    skj.strests<-NULL
  }
  # output results
  return(list(bet=bet.strests,yft=yft.strests,skj=skj.strests))
}