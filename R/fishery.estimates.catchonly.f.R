#' xxx
#' 
#' \code{fishery.estimates.catchonly.f} yyy
#' 
#' @export

fishery.estimates.catchonly.f = function(spp.unlds.bystrat,iyear,PS)
{
  # single spp P-S
  # creates stock assessment estimates of total catch (mt) 
  #   within stock assessments fisheries (quarter x areas x gears) FOR A SINGLE SPECIES
  #
  # total catch estimates are the sum of total stratum catches for all catch strata in a fishery
  #
  # this function is called by (or an edited version of this): call.fishery.estimates.f
  #
  # strat.ests.withsamps is output of function stratum.estimates.f - SUBSETTED FOR A SPECIES
  # spp.unlds.bystrat is output from get.strat.unloads.f (applied to output of get.corrected.unloads.f) - SUBSETTED FOR A SPECIES
  # iyear the year for which estimates are being made; used only to add column with year to output data frame
  #
  # this function uses: create.fishery.flg.f
  #
  # edited 12.18.2014: added code to return number of wells sampled per strata, for strata with adequate well sample data
  # edited 3.26.2019: only estimate total catch by fishery strata (no size comps)
  #
  # Estimates of total catch by fishery areagear-quarter
  #
  #  make fishery areagear x quarter id
  tmpspp.fshryflg<-create.fishery.flg.f.BET(spp.unlds.bystrat$str.defns,PS)
  tmpspp.qrtrfshry.id<-paste(tmpspp.fshryflg$fishery.areagear,tmpspp.fshryflg$fishery.quarter,sep=":")
  #
  # sum catch by areagear x quarter
  totcatch.byfshry<-tapply(spp.unlds.bystrat$totunlds.bystr,tmpspp.qrtrfshry.id,sum)
  #
  return(list(catchtotals=totcatch.byfshry,catchdefns=dimnames(totcatch.byfshry)[[1]]))
}
