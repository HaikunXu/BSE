#' xxx
#' 
#' \code{substitute.f} yyy
#' 
#' @export

substitute.f = function(strat.defns,unloads.bystrat.miss,lfgrpd.all.frm,lfgrpd.stratflg,lfmm.all.frm,gear.sub,area.sub,month.sub,min.subsize,growth.increments.array)
{
  # this function computes catch species and size composition for strata with unloads (ie, cae+idm) but no sample data
  # edited version of stratum.estimates.f (see that function for documentation details of equations)
  #
  # lfgrpd.stratflg is output of create.strat.flg.f applied to lfgrpd.all.frm
  #
  # strat.defns and unloads.bystrat.miss are the parts output by get.strat.unloads.f 
  #     but subsetted for missing data strata using output of stratum.estimates.f
  # lfgprd.all.frm is output from read.lengthfreq.f; subsetted for specific year
  # min.subsize is the minimum number of samples per stratum required to make an estimate
  #
  # this function uses the following functions: get.sub.f, well.miss.f
  #
  # edited December 2021 to fix bug in code that searches for a substitution (bug was that number of rows of lfgrpd.all.frm for a cell was used as number of samples, 
  #             which is not correct because records in that data frame are unique tripno x wellsampleno x species (and possibly x sampno if sorted sample)
  #
  # start of code added Jan 25 2022
  # column 1 of fo.fishery.submat is the fishery seeking a sub, and columns 2-5 are the possible subs
  fo.fishery.submat<-matrix(c("FO.A1","FO.A3","FO.A2","FO.A4","FO.A5","FO.A2","FO.A4","FO.A5","FO.A1","FO.A3","FO.A3","FO.A1","FO.A4","FO.A5","FO.A2","FO.A4","FO.A5","FO.A2","FO.A3","FO.A1","FO.A5","FO.A4","FO.A2","FO.A3","FO.A1"),ncol=5,byrow=T)
  # end of code added Jan 25 2022
  #
  # get number of strata 
  nstrats<-length(unloads.bystrat.miss)
  #
  # initialize output storage for stratum weight by species (What.i) and number of fish by 1cm bin by species (Nhat.ik)
  #    and substitution details
  What.i<-matrix(0,nrow=nstrats,ncol=3)
  Nhat.ik<-array(0,dim=c(nstrats,201,3))
  sub.details<-matrix(NA,ncol=9,nrow=nstrats)
  #
  # get sample ids for the data frame lfgrpd.all.frm (id is trip number:well sample number)
  lfgrpd.all.sampid<-paste(lfgrpd.all.frm$tripno,lfgrpd.all.frm$wellsampno,pasate=":")
  #
  # loop over strata, computing What.i, Nhat.ik
  for(nstr in 1:nstrats){
    #
    # find a substitute stratum
    substr.info<-get.sub.f(strat.defns$area[nstr],strat.defns$month[nstr],strat.defns$gear[nstr],strat.defns$fishery[nstr],lfgrpd.stratflg,gear.sub,area.sub,month.sub,min.subsize,lfgrpd.all.sampid)
    sub.details[nstr,]<-unlist(c(strat.defns$area[nstr],strat.defns$month[nstr],strat.defns$gear[nstr],strat.defns$fishery[nstr],substr.info),use.names=F)
    #
    # subset LF grouped data for the substitute stratum
    if(is.na(substr.info$fishery.areagear)){
      # substitute stratum is a specific area, month, gear
      lfgrpd.sub.frm<-lfgrpd.all.frm[lfgrpd.stratflg$area==substr.info$area & lfgrpd.stratflg$month==substr.info$month & lfgrpd.stratflg$gear==substr.info$gear,]
    } else {
      # substitute stratum is the fishery area-gear
      lfgrpd.sub.frm<-lfgrpd.all.frm[lfgrpd.stratflg$fishery.areagear==substr.info$fishery.areagear,]
      #
      # start of code below added Jan 25 2022
      if(nrow(lfgrpd.sub.frm)==0){
        # no data for this fishery for the year so we are desperate to find a substitution option
        fo.type<-substr(substr.info$fishery.areagear,1,2)
        if(fo.type=="FO"){
          fish.found<-F
          ifish.sub<-2
          while(!fish.found & ifish.sub<=5){
            lfgrpd.sub.frm<-lfgrpd.all.frm[lfgrpd.stratflg$fishery.areagear==fo.fishery.submat[fo.fishery.submat[,1]==substr.info$fishery.areagear,ifish.sub],]
            if(nrow(lfgrpd.sub.frm)>=1){
              # this only means there is at least one well sample; need to fix in future when modify all code to use samples from original (if there) plus samples in substitute stratum
              fish.found<-T
            } else {
              ifish.sub<-ifish.sub+1
            }
          } # end of while loop
          #
          # if ifish.sub is 6 no data found
          if(ifish.sub==6){
            print("********* No data found FO fishery-level substitution")
          }
          #
        } # end of if for FO
        #
        if(fo.type=="UN"){
          # NOTE that UN substitution as coded here is garbage; using whole EPO 
          lfgrpd.sub.frm<-lfgrpd.all.frm[substr(lfgrpd.stratflg$fishery.areagear,1,2)==fo.type,]
        }
        #
        if(fo.type=="DP"){
          # NOTE that DP substitution as coded here is garbage; using whole EPO 
          lfgrpd.sub.frm<-lfgrpd.all.frm[substr(lfgrpd.stratflg$fishery.areagear,1,2)==fo.type,]
        }
        # end of code added Jan 25 2022
      }
      #
    }
    #
    # get well-level estimates for samples in this substitute stratum
    #   first determine the shift direction if there is a month change original versus substitute
    delta.col<-NULL
    if(substr.info$grwshrk.flg){
      if((strat.defns$month[nstr]-substr.info$month)==1){
        # grow fish lengths by 1 month
        delta.col<-2
      } else {
        if((strat.defns$month[nstr]-substr.info$month)==(-1)){
          # shrink fish lengths by 1 month
          delta.col<-3
        }
      }
    }
    well.estimates<-well.miss.f(lfgrpd.sub.frm,lfmm.all.frm,substr.info,delta.col,growth.increments.array)
    #
    # now get stratum-level estimates for this stratum
    #(note that code is not totally efficient; could be simplified somewhat, but not done at this point to keep readability)
    #
    # get well stratum and find if there is an unloads stratum that matches
    W<-unloads.bystrat.miss[nstr]   
    #
    # compute What.i
    sum.Wj<-sum(well.estimates$ancillary.info$wellmt.3spp)
    What.i[nstr,]<-W*(apply(well.estimates$What.ij,2,sum)/sum.Wj)
    #
    # compute Nhat.ik
    for(ispp in 1:3){
      sum.Nj<-sum(well.estimates$Nhat.j)
      sum.Nij<-sum(well.estimates$Nhat.ij[,ispp])
      if(sum.Nij>0){
        if(!is.vector(well.estimates$Nhat.ijk[,,ispp])){
          # there is more than one well sample with this species
          Nhat.ik[nstr,,ispp]<-(W/(sum.Wj/sum.Nj))*(sum.Nij/sum.Nj)*(apply(well.estimates$Nhat.ijk[,,ispp],2,sum)/sum.Nij)
        } else {
          # there is only one well sample with this species
          Nhat.ik[nstr,,ispp]<-(W/(sum.Wj/sum.Nj))*(sum.Nij/sum.Nj)*(well.estimates$Nhat.ijk[,,ispp]/sum.Nij)
        }
      }
    }
  }
  # 
  sub.details<-data.frame(sub.details)
  names(sub.details)<-c("old.area","old.month","old.gear","fishery.areagear","get.sub.area","get.sub.month","get.sub.gear","grwshrk.flg","fshareagear.flg")
  #
  return(list(strats=strat.defns,What.i=What.i,Nhat.ik=Nhat.ik,sub.details=sub.details))
}
