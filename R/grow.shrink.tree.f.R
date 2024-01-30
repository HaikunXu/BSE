#' xxx
#' 
#' \code{grow.shrink.tree.f} yyy
#' 
#' @export

grow.shrink.tree.f = function(well.lngths, Species)
{
  # edited version of grow.shrink.bet.f (January 29 2024)
  # assumes that grow/shrink amounts at lengths below 20cm are the same amounts as a 20cm (4cm)
  # this function grows/shrinks bet to quarter mid-point according to Alex bet growth information
  # input: bet.grwshrk.frm file (growth.increments.array) and lengths file to be grown/shrunk (lengths raised to well; well.lngths)
  # assumes length file is the result of cbind applied to well.estimates.yyyy$ancillary.info and well.estimates.yyyy$Nhat.ijk for species of interest
  # assumes there are no fish to grow/shrink initially below 10 cm, and that growth beyond 200 cm is 0
  # assumes that both the grow and shrink increments in growth.increments.array (column 2 and column 3, respectively) are positive values
  #
  
  if(Species=="BET") growth.increments.array <- grow.increments.betyftskj[20:200,,1]
  if(Species=="YFT") growth.increments.array <- grow.increments.betyftskj[20:200,,2]
  if(Species=="SKJ") growth.increments.array <- grow.increments.betyftskj[20:200,,3]
  
  # compute month of each well sample
  month <- well.lngths$month
  
  # get matrix of length interval lower bounds for each sample (10cm to 201 cm, where 201 cm is a plus group)
  lngths.10.201 <- data.matrix(well.lngths[, 22:213])
  nrecs <- nrow(lngths.10.201)
  nlngths <- ncol(lngths.10.201)
  lngth.intervals <-
    matrix(rep(seq(10, 201, 1), nrecs), byrow = T, ncol = nlngths)
  
  # make matrices of grow/shrink amounts (have to add 0 to each because  table stops at 200 cm, and have to add 4cm for smallest fish)
  grow.mat <-
    matrix(rep(c(rep(4, 10), growth.increments.array[, 2], 0), nrecs), byrow = T, ncol = nlngths)
  shrink.mat <-
    matrix(rep(c(rep(4, 10), growth.increments.array[, 3], 0), nrecs), byrow = T, ncol = nlngths)
  
  # compute new length interval lower bounds for each sample
  change.flg <- rep(F, nrecs)
  change.flg[month == 1 | month == 4 | month == 7 | month == 10] <- T
  lngth.intervals[change.flg, ] <- lngth.intervals[change.flg, ] + grow.mat[change.flg]
  change.flg <- rep(F, nrecs)
  change.flg[month == 3 | month == 6 | month == 9 | month == 12] <- T
  lngth.intervals[change.flg, ] <- lngth.intervals[change.flg, ] + shrink.mat[change.flg]
  
  # map counts to new length intervals (assumption is that new length intervals are 6-201cm => 196 columns, based on growth file)
  tmp.lngths <- matrix(0,
                       ncol = 201,
                       nrow = nrecs,
                       byrow = T)
  
  for (i in 1:nlngths) {
    for (j in 1:nrecs) {
      tmp.lngths[j, (lngth.intervals[j, i])] <-
        lngths.10.201[j, i] + tmp.lngths[j, (lngth.intervals[j, i])]
    }
  }
  # add in smaller fish that were not grown/shrunk
  tmp.lngths[, 1:10] <- tmp.lngths[, 1:10] + data.matrix(well.lngths[, 13:22])
  
  new.lngths <- data.frame(tmp.lngths)
  names(new.lngths) <-
    paste(rep("X", 201), as.character(seq(1, 201)), sep = "")
  
  output.frm <- data.frame(well.lngths[, 1:12], new.lngths)
  return(output.frm)
}