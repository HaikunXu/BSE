#' xxx
#' 
#' \code{mark.vesnos.f} yyy
#' 
#' @export

mark.vesnos.f = function(input.frm,vesno.list) {
  vesflg<-rep(F,length(input.frm$vesno))
  nves<-length(vesno.list)
  for(i in 1:nves){
    vesflg[input.frm$vesno==vesno.list[i] & !is.na(input.frm$vesno)]<-T
  }
  return(vesflg)
}