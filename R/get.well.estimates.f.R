#' xxx
#' 
#' \code{get.well.estimates.f} yyy
#' 
#' @export

get.well.estimates.f = function(lfgrpd,lfmm,yr.start,yr.end) {
  for(year in yr.start:yr.end) {
    print(paste0("Getting the well estimates for year ",year))
    well.estimates <- well.estimates.f(lfgrpd[lfgrpd$year.firstset==year,],lfmm)
    assign(paste0("well.estimates.", year), well.estimates,pos=1)
    rm(well.estimates)
  }
}