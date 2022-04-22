#' xxx
#' 
#' \code{get.strat.unloads.f} yyy
#' 
#' @export

get.strat.unloads.f = function(cae.frm,cae.stratum,total.unloads) {
  # this function prorates annual total P-S unloads to catch estimation strata.
  #
  # cae.frm is the output from read.cae.f (this is a data frame with YFT+SKJ+BET); 
  # cae.stratum is the output from create.strat.flg.f (this has the three variables that define the strata,
  #    and there is a 1-to-1 match of rows between cae.frm and cae.stratum;
  # total.unloads is the output from read.unloads.f (this is the total YFT+SKJ+BET unloads for the year).
  #
  # NOTE: function assumes input data are only for one year.
  # NOTE: function assumes there are three variables that define the strata: area, month, gear. 
  #
  # sum CAE catches of YFT+SKJ+BET by stratum
  sum.cae.bystrat<-tapply(cae.frm$sum.trop.tunas,list(cae.stratum$area,cae.stratum$month,cae.stratum$gear),sum)
  sum.cae.bystrat[is.na(sum.cae.bystrat)]<-0
  #
  # prorate total unloads to strata
  total.unloads.bystrat<-(total.unloads/sum(cae.frm$sum.trop.tunas))*sum.cae.bystrat
  #
  # reformat unloads by strata
  unloads.bystrat.vec<-as.vector(total.unloads.bystrat)
  #
  # get stratum definitions to go with reformatted (vectorized) unloads by strata
  n.areas<-length(dimnames(total.unloads.bystrat)[[1]])
  n.months<-length(dimnames(total.unloads.bystrat)[[2]])
  n.gears<-length(dimnames(total.unloads.bystrat)[[3]])
  #
  # fill arrays with values of area, month and gear
  area.array<-array(rep(as.numeric(dimnames(total.unloads.bystrat)[[1]]),(n.months*n.gears)),dim=c(n.areas,n.months,n.gears))
  #
  month.array<-array(rep(as.vector(matrix(rep(as.numeric(dimnames(total.unloads.bystrat)[[2]]),n.areas),ncol=n.months,nrow=n.areas,byrow=T)),n.gears),dim=c(n.areas,n.months,n.gears))
  #
  gear.array<-array(sort(rep(as.numeric(dimnames(total.unloads.bystrat)[[3]]),(n.areas*n.months))),dim=c(n.areas,n.months,n.gears))
  #
  # reformat stratum definitions to match that of unloads
  strat.defns<-data.frame(as.vector(area.array),as.vector(month.array),as.vector(gear.array))
  names(strat.defns)<-c("area","month","gear")
  #
  # return only those strata with unloads>0
  return(list(totunlds.bystr=unloads.bystrat.vec[unloads.bystrat.vec>0],str.defns=strat.defns[unloads.bystrat.vec>0,]))
}