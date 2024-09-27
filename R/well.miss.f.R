#' xxx
#' 
#' \code{well.miss.f} yyy
#' 
#' @export

well.miss.f = function(lfgrpd.substrat.frm,lfmm.frm,substr.info,delta.col,growth.increments.array)
{
  # edited version of well.estimates.f that runs on data of only one stratum
  # computes well estimates for a substitution stratum
  # this function is called by substitute.f
  # 
  # lfgrpd.substrat.frm is output from read.lengthfrq.f, subsetted for the substitution stratum 
  # lfmm.frm is output from read.lfmmdata.f (not substituted)
  # substr.info is output from function get.sub.f; it contains the substitution stratum id information
  # delta.col is the value 2 (grow lengths by one month) or 3 (shrink lengths by one month); used in grow.shrink.f
  # growth.increments.array is an array that gives the grow/shrink amounts; used in grow.shrink.f 
  #    (see grow.shrink.f for array description)
  #
  # species look-up table
  spp.lookup<-matrix(c(106,110,111,1,2,3),ncol=2,nrow=3,byrow=F)
  #
  # create well sample id for grouped L-F data: trip number:well sample number
  sampid.grpd<-paste(lfgrpd.substrat.frm$tripno,lfgrpd.substrat.frm$wellsampno,sep=":")
  #
  # get unique sample ids
  sampid.grpd.unq<-unique(sampid.grpd)
  #
  # and number of unique sample ids
  nsamps<-length(sampid.grpd.unq)
  #
  # create well sample id for ungrouped lf mm data
  sampid.lfmm<-paste(lfmm.frm$tripno,lfmm.frm$wellsampno,sep=":")
  #
  # initialize storage for well summary quantities
  # What.ij is the estimated well weight (mt) of each species in each well (page 319, eq(49) ); rows are well samples, columns species (106, 110, 111)
  # Nhat.ij is the estimated number of each species in the well (page 318, eq (45) ); rows are well samples, columns species (106, 110, 111)
  # Nhat.ijk is the estimated number of each species in the well in each 1cm length bin; an array with dim(samples x 1cm bins x species)
  # Nhat.j is the estimated number of all species in the well; vector 
  #
  What.ij<-matrix(0,nrow=nsamps,ncol=3)
  Nhat.ij<-matrix(0,nrow=nsamps,ncol=3)
  Nhat.ijk<-array(0,dim=c(nsamps,201,3))
  Nhat.j<-rep(0,nsamps)
  lf.well.frm<-data.frame(matrix(NA,nrow=nsamps,ncol=5))
  names(lf.well.frm)<-c("tripno","wellsampno","year","wellmt.3spp","sorted")
  #
  #
  # NOTE: looping variables do NOT relate to i,j,k subscripts in SAR4 equations (I could have made better choices ;-(( )
  #
  # process samples of this substratum one at a time
  for(i in 1:nsamps){
    #
    # get all records for this sample
    tmpdat<-lfgrpd.substrat.frm[sampid.grpd==sampid.grpd.unq[i],]
    #
    # get length data for this sample
    lfmm.sub.frm<-lfmm.frm[sampid.lfmm==sampid.grpd.unq[i],]
    #
    # if necessary, grow/shrink the lengths
    if(substr.info$grwshrk.flg){
      lfmm.sub.frm<-grow.shrink.f(lfmm.sub.frm,delta.col,growth.increments.array)
    }
    #
    # store limited ancillary information
    lf.well.frm$tripno[i]<-tmpdat$tripno[1]
    lf.well.frm$wellsampno[i]<-tmpdat$wellsampno[1]
    lf.well.frm$year[i]<-tmpdat$year.firstset[1]
    lf.well.frm$wellmt.3spp[i]<-tmpdat$totalmt.yftskjbet[1]
    #
    # get sorted flag and check if unique for sample; process non-split and split samples separately
    sortid<-unique(tmpdat$sorted.flg)
    # print(tmpdat$sorted.flg)
    if(length(sortid)!=1){
      print(paste("Sample with more than one value for sorted flag: ",sampid.grpd.unq[i]))
    } else {
      # print(sortid)
      # process non-split sample
      if(sortid==2){
        lf.well.frm$sorted[i]<-2
        #
        # initialize temporary storage for this sample (sppmsrd is m_ij; sppcnt is n_ij; sppkg is w_ij; lf.1cm.counts is m_ijk)
        sppcnt<-rep(0,3)
        sppmsrd<-rep(0,3)
        sppkg<-rep(0,3)
        lf.1cm.counts<-matrix(0,ncol=201,nrow=3)
        #
        # get number of species for this well sample
        nspp<-nrow(tmpdat)
        #
        # loop over number of species to fill raw stats variables
        for(k in 1:nspp){
          # raw summaries for species k
          sppcnt[spp.lookup[spp.lookup[,1]==tmpdat$species[k],2]]<-tmpdat$fish.count[k]
          sppmsrd[spp.lookup[spp.lookup[,1]==tmpdat$species[k],2]]<-tmpdat$sample.size[k]
          #
          if(!is.na(tmpdat$sample.size[k]) & tmpdat$sample.size[k]>0){
            sppkg[spp.lookup[spp.lookup[,1]==tmpdat$species[k],2]]<-sum(lfmm.sub.frm$weight.kg[lfmm.sub.frm$spp==tmpdat$species[k]])
            #
            tmpcnts<-table(lfmm.sub.frm$length.1cmbin[lfmm.sub.frm$spp==tmpdat$species[k]])
            lf.1cm.counts[spp.lookup[spp.lookup[,1]==tmpdat$species[k],2],as.numeric(dimnames(tmpcnts)[[1]])]<-tmpcnts
          }
          # end of loop on number of species for non-split sample
        }
        #
        # derive well-level summaries for this non-split sample
        #   first compute some intermediate quantities
        wbar.ij<-sppkg/sppmsrd
        wbar.ij[sppmsrd==0]<-0
        #
        fhat.ij<-sppcnt/sum(sppcnt)
        #
        wbar.j<-sum(wbar.ij*fhat.ij)
        #
        #  now compute well-level estimates
        What.ij[i,]<-(lf.well.frm$wellmt.3spp[i]/wbar.j)*wbar.ij*fhat.ij
        Nhat.j[i]<-(lf.well.frm$wellmt.3spp[i]*1000)/wbar.j
        Nhat.ij[i,]<-Nhat.j[i]*fhat.ij
        #
        for(nn in 1:3){
          if(sppmsrd[nn]>0){
            Nhat.ijk[i,,nn]<-Nhat.ij[i,nn]*lf.1cm.counts[nn,]/sppmsrd[nn]
          }
        }
        #
        # end of if statement for non-split sample
      }
      #
      # process split sample
      if(sortid==1){
        lf.well.frm$sorted[i]<-1
        #
        # get species information for this sample
        sppcdes<-unique(tmpdat$species)
        nspp<-length(sppcdes)
        #
        # loop over species in the sample
        for(mm in 1:nspp){
          #
          # get number of splits for this species in this sample
          tmpdat.spp<-tmpdat[tmpdat$species==sppcdes[mm],]
          nsplits.spp<-nrow(tmpdat.spp)
          # 
          # initialize temporary storage W.ij, N.ij and N_ijk for this species
          tmp.N.ijk<-rep(0,201)         
          tmp.W.ij<-0
          tmp.N.ij<-0
          #
          # loop over number of splits for this species in this sample
          for(kj in 1:nsplits.spp){
            tmp.lf1cm<-rep(0,201)  
            #
            # compute split-level summaries for this species: W_ij, w_ij, m_ij, wbar_ij
            wellwt.split<-tmpdat.spp$totalmt.catch[kj]*(tmpdat.spp$pcnt.totalcatch[kj]/100)
            sampwt.split<-sum(lfmm.sub.frm$weight.kg[lfmm.sub.frm$sampno==tmpdat.spp$sampno[kj] & lfmm.sub.frm$spp==tmpdat.spp$species[1]])
            nmsrd.split<-length(lfmm.sub.frm$length.1cmbin[lfmm.sub.frm$sampno==tmpdat.spp$sampno[kj] & lfmm.sub.frm$spp==tmpdat.spp$species[1]])
            avgwt.split<-sampwt.split/nmsrd.split
            #
            # compute well-level estimates for this species (summed over splits) of: What_ij, Nhat_ij, Nhat_ijk 
            tmp.W.ij<-tmp.W.ij+wellwt.split
            tmp.N.split<-1000*wellwt.split/avgwt.split
            tmp.N.ij<-tmp.N.ij+tmp.N.split
            tmpcnts<-table(lfmm.sub.frm$length.1cmbin[lfmm.sub.frm$sampno==tmpdat.spp$sampno[kj] & lfmm.sub.frm$spp==tmpdat.spp$species[1]])
            tmp.lf1cm[as.numeric(dimnames(tmpcnts)[[1]])]<-tmpcnts
            tmp.N.ijk<-tmp.N.ijk+(tmp.N.split*(tmp.lf1cm/nmsrd.split))
            #
            # end of loop on splits for this species
          }
          #
          # for this species, save well-level estimates
          What.ij[i,spp.lookup[spp.lookup[,1]==sppcdes[mm],2]]<-tmp.W.ij
          Nhat.ij[i,spp.lookup[spp.lookup[,1]==sppcdes[mm],2]]<-tmp.N.ij
          Nhat.ijk[i,,spp.lookup[spp.lookup[,1]==sppcdes[mm],2]]<-tmp.N.ijk
          #
          # end of loop over species for this split sample
        }
        #
        # compute estimate of total fish for the well
        Nhat.j[i]<-sum(Nhat.ij[i,])      
        
        # end of if statement for split sample
      }
      # end of if on test of unique sorted flag for this sample 
    }
    # end of loop on number of samples
  }
  #
  return(list(ancillary.info=lf.well.frm,What.ij=What.ij,Nhat.j=Nhat.j,Nhat.ij=Nhat.ij,Nhat.ijk=Nhat.ijk))
}
