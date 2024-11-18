#' xxx
#' 
#' \code{get.summedLF.5deg.f} yyy
#' 
#' @export

get.summedLF.5deg.f = function(well.samps,dph.vesno.flg,nyear,Species) {
  # edited June 3, 2019 to output number of well samples and year
  #
  # get 5 degree square, QUARTER and set type information in data
  
  if(Species=="BET") spp <- 1
  if(Species=="YFT") spp <- 2
  if(Species=="SKJ") spp <- 3
  
  quarter <- rep(1, length(well.samps$ancillary.info$month))
  quarter[well.samps$ancillary.info$month >= 4 &
            well.samps$ancillary.info$month <= 6] <- 2
  quarter[well.samps$ancillary.info$month >= 7 &
            well.samps$ancillary.info$month <= 9] <- 3
  quarter[well.samps$ancillary.info$month >= 10] <- 4
  #
  dg5.setype.qrtr.id <-
    paste(
      well.samps$ancillary.info$lat.5deg[well.samps$What.ij[, spp] > 0 &
                                           dph.vesno.flg],
      well.samps$ancillary.info$lon.5deg[well.samps$What.ij[, spp] > 0 &
                                           dph.vesno.flg],
      well.samps$ancillary.info$setype[well.samps$What.ij[, spp] > 0 &
                                         dph.vesno.flg],
      quarter[well.samps$What.ij[, spp] > 0 & dph.vesno.flg],
      sep = ":"
    )
  unq.setype.dg5.qrtr.id <- unique(dg5.setype.qrtr.id)
  num.setype.dg5.qrtr.id <- length(unq.setype.dg5.qrtr.id)
  num.wellsamps <- rep(NA, length(num.setype.dg5.qrtr.id))
  #
  tmplf <- well.samps$Nhat.ijk[well.samps$What.ij[, spp] > 0 & dph.vesno.flg, , spp]
  yft.summedlf <- matrix(NA, ncol = 201, nrow = num.setype.dg5.qrtr.id)
  # sum yft LF by set type, 5 degree square and quarter
  for (i in 1:num.setype.dg5.qrtr.id) {
    tmplf.sub <- tmplf[dg5.setype.qrtr.id == unq.setype.dg5.qrtr.id[i], ]
    if (is.matrix(tmplf.sub)) {
      yft.summedlf[i, ] <- as.vector(apply(tmplf.sub, 2, sum))
      num.wellsamps[i] <- nrow(tmplf.sub)
    } else {
      yft.summedlf[i, ] <- as.vector(tmplf.sub)
      num.wellsamps[i] <- 1
    }
  }
  #
  xvars <-
    data.frame(matrix(as.numeric(unlist(
      strsplit(unq.setype.dg5.qrtr.id, "\\:")
    )), ncol = 4, byrow = T))
  ancil.frm <- data.frame(xvars, num.wellsamps)
  names(ancil.frm) <-
    c("lat.5deg", "lon.5deg", "setype", "quarter", "num.wellsamps")
  ancil.frm$year <- rep(nyear, length(ancil.frm$setype))
  #
  return(list(ancil.info = ancil.frm, lf = yft.summedlf))
}
