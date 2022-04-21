#' xxx
#' 
#' \code{read.lfmmdata.f} yyy
#' 
#' @export

read.lfmmdata.f = function(dir,file) {
  # reads in LengthMMyyyy.txt file output by Miscellaneous VB program: Length frequency II (May 16, 2008), 
  #   which is located in: Cleridy data files
  # converts length (in mm) to weight (in kg) using formula: W = aL^b, and assigns 1cm length bin
  # afile is the filename, without path
  # NOTE: the output of this function will contain NAs in the weight.kg column for species other than bet, yft, skj
  # an aside: this function would be made more efficient if tmpfrm was subsetted first by gear, but this requires
  #       first linking to the other LF file: LengthFreqyyyy-yyyy.txt
  #
  # edited 12.8.14: add length bin 0 to length bin 1 (because length bins later used to index matrices)
  #
  # read in raw length data
  tmpfrm<-read.table(paste0(dir,file),header=F)
  names(tmpfrm)<-c("tripno","wellsampno","sampno","spp","length.mm")
  #
  # only keep lengths that are for bet, yft or skj
  tmpfrm<-tmpfrm[tmpfrm$spp==106 | tmpfrm$spp==110 | tmpfrm$spp==111,]
  #
  nrecs<-nrow(tmpfrm)
  #
  # conversion constants (a and b) for each of the three tropical tuna species (order is: bet 106, yft 110, skj 111)
  # (constants are from JoyDeLee, who got them from Pat)
  aa<-double(3)
  aa<-c(0.000000045896,0.0000000176631,0.00000000255076)
  bb<-c(2.90182,3.02,3.336)
  #
  # convert length to weight
  weight.kg<-rep(NA,nrecs)
  weight.kg[tmpfrm$spp==106]<-aa[1]*(tmpfrm$length.mm[tmpfrm$spp==106]^bb[1])
  weight.kg[tmpfrm$spp==110]<-aa[2]*(tmpfrm$length.mm[tmpfrm$spp==110]^bb[2])
  weight.kg[tmpfrm$spp==111]<-aa[3]*(tmpfrm$length.mm[tmpfrm$spp==111]^bb[3])
  #
  # assign 1cm length bin (length bin is the lower endpoint of the interval)
  #  each bin is: x.0 to x.999999999999 
  length.1cmbin<-floor(tmpfrm$length.mm/10)
  #
  # put all lengths greater than 201 cm into the 201cm bin
  length.1cmbin[length.1cmbin>201]<-201
  # put all lengths 0 into length bin 1
  length.1cmbin[length.1cmbin==0]<-1 
  #
  outfrm<-data.frame(tmpfrm,weight.kg,length.1cmbin)
  return(outfrm)
}
