#' xxx
#' 
#' \code{mark.tripnos.f} yyy
#' 
#' @export

mark.tripnos.f = function(input.frm,tripno.list) {
  tripflg <- rep(F,length(input.frm$tripno))
  ntrip <- length(tripno.list)
  for(i in 1:ntrip){
    tripflg[input.frm$tripno==tripno.list[i] & !is.na(input.frm$tripno)]<-T
  }
  return(tripflg)
}