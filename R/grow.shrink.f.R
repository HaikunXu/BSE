#' xxx
#' 
#' \code{grow.shrink.f} yyy
#' 
#' @export

grow.shrink.f = function(well.lngths,delta.col,growth.increments.array)
{
  # grows/shrinks yft, skj and bet lengths by one month, according to information in growth.increments.array.
  # this function is called by well.miss.f
  # edited version of read.lfmm.data.f
  #
  # growth.increments.array is an array with the following format: 
  #     1cm length bin centers x change in length *in centimeters* (due to shift by +1 month, -1 month) x species (see spp.lookup for species order)
  #     for example, for 1cm bins from 20.5 to 200.5, this array has dimensions: 181 x 3 x 3.
  #     therefore, for species 1 (bet), the matrix is 181 x 3, where the first column is the 1cm length bin centers, and
  #          column 2 is the amount (cm) added to a length for growth by 1 month, column 3 is the amount (cm) substracted for shrinkage by 1 month
  #     it is assumed that the 1cm length bins span the range of lengths in well.lngths.
  # well.lngths is the output of read.lfmmdata.f for a single trip number-well sample number
  # delta.col is the column number for the month shift: 2 for growth by 1 month, 3 for shrinkage by 1 month
  #
  # edited 12.8.14: length bin 0 added to length bin 1
  #
  # species look-up table
  spp.lookup<-matrix(c(106,110,111,1,2,3),ncol=2,nrow=3,byrow=F)
  #
  # get number of lengths to process
  n.lngths<-nrow(well.lngths)
  #
  # initialize shifted length vector
  new.lngths<-rep(NA,n.lngths)
  #
  # convert input length to 1cm bin midpoint to be able to index growth.increments.array
  cmbin.centers<-floor(well.lngths$length.mm/10)+0.5
  #
  # grow/shrink lengths by cm change in growth.increments.array 
  #    (note: need to multiply increment by 10 because keep lengths are in mm for conversion to weight)
  for(il in 1:n.lngths){
    spp.no<-spp.lookup[well.lngths$spp[il]==spp.lookup[,1],2]
    new.lngths[il]<-well.lngths$length.mm[il]+(10*growth.increments.array[growth.increments.array[,1,spp.no]==cmbin.centers[il],delta.col,spp.no])
  }
  # convert shifted legnths to weight
  # conversion constants (a and b) for each of the three tropical tuna species (order is: bet 106, yft 110, skj 111)
  # (constants are from JoyDeLee, who got them from Pat), to convert from length (mm) to weight (kg)
  aa<-double(3)
  aa<-c(0.000000045896,0.0000000176631,0.00000000255076)
  bb<-c(2.90182,3.02,3.336)
  #
  # convert length to weight
  weight.kg<-rep(NA,n.lngths)
  weight.kg[well.lngths$spp==106]<-aa[1]*(new.lngths[well.lngths$spp==106]^bb[1])
  weight.kg[well.lngths$spp==110]<-aa[2]*(new.lngths[well.lngths$spp==110]^bb[2])
  weight.kg[well.lngths$spp==111]<-aa[3]*(new.lngths[well.lngths$spp==111]^bb[3])
  #
  # assign 1cm length bin (length bin is the lower endpoint of the 1cm interval)
  #  each bin is: x.0 to x.999999999999 
  length.1cmbin<-floor(new.lngths/10)
  #
  # put all lengths greater than 201 cm into the 201cm bin
  length.1cmbin[length.1cmbin>201]<-201
  # put all lengths 0 into length bin 1
  length.1cmbin[length.1cmbin==0]<-1 
  #
  outfrm<-data.frame(well.lngths[,1:4],new.lngths,weight.kg,length.1cmbin)
  names(outfrm)[5]<-"length.mm"
  return(outfrm)
}