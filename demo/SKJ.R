library(BSE)
# global setup
raw_data_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/Raw data extractions/"
save_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/SKJ/"
yr.start <- 2000
yr.end <- 2021
Species <- "SKJ"
grow.increments <- grow.increments.2cmSKJ.betyftskj # the growth increment for SKJ

# Get the total unloads for the PS fleet
total.unlds <- read.unloads.f(raw_data_dir,"Unloading2000-2021.txt",yr.start,yr.end)
# Get the CAE+IDM data
cae <- read.cae.f(raw_data_dir,"CAE-LatLon2000-2021.txt",yr.start,yr.end)
# Get the length-frequency data (length in millimeters)
lfmm <- read.lfmmdata.f(raw_data_dir,"LengthMM2000-2021.txt")
# Get the grouped length-frequency output
lfgrpd <- read.lengthfreq.f(raw_data_dir,"LengthFreq2000-2021.txt")


# Running SKJ OBJ
PS <- "OBJ"
area.substitution.mat <- area.substitution.mat.SKJ.FLT.SAC2022 # for OBJ

cae.stratflg <- create.strat.flg.f(cae$latc5,cae$lonc5,is.lwrght=F,cae$month,cae$setype,cae$class,PS=PS,Species=Species)
check.strat.flg.f(cae$latc5,cae$lonc5,cae.stratflg)

lfgrpd.stratflg <- create.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,is.lwrght=T,floor(lfgrpd$moda/100),lfgrpd$setype,lfgrpd$class,PS=PS,Species=Species)
check.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,lfgrpd.stratflg)

# Loop to get all years' fishery estimates
for(year in yr.start:yr.end) {
  print(paste0("Year: ",year))
  
  print("Step 1: get well estimates")
  well.estimates <- well.estimates.f(lfgrpd[lfgrpd$year.firstset==year,],lfmm)
  
  print("Step 2: get catch estimates")
  catch.estimates <- get.catch.estimates.f(cae,cae.stratflg,total.unlds,lfgrpd,lfgrpd.stratflg,lfmm,year,2,well.estimates,area.substitution.mat,grow.increments,PS=PS,Species=Species)
  
  print("Step 3: get fishery estimates")
  fishery.estimates <- fishery.estimates.f(catch.estimates$stratum.estimates.withsamps,catch.estimates$stratum.estimates.NOsamps,year,PS=PS,Species=Species)
  
  assign(paste0("fishery.estimates.", year), fishery.estimates, pos=1)
}
save(list=objects(pat="fishery.estimates"),file=paste0(save_dir,"SKJ_",PS,"_2000-2021.RData"))

# get final catch and comp output for the stock assessment
SKJ.OBJ.Catch.20002021<-compile.catch.output.f(yr.start,yr.end,PS=PS,Species=Species,c("A1","A2","A3","A4"))
SKJ.OBJ.Comp.20002021<-compile.sizecomps.output.f(yr.start,yr.end,PS=PS,Species=Species)


# Running SKJ NOA
PS <- "NOA"
area.substitution.mat <- area.substitution.mat.SKJ.UNA.SAC2022 # for NOA

cae.stratflg <- create.strat.flg.f(cae$latc5,cae$lonc5,is.lwrght=F,cae$month,cae$setype,cae$class,PS=PS,Species=Species)
lfgrpd.stratflg <- create.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,is.lwrght=T,floor(lfgrpd$moda/100),lfgrpd$setype,lfgrpd$class,PS=PS,Species=Species)

# Loop to get all years' fishery estimates
for(year in yr.start:yr.end) {
  print(paste0("Year: ",year))
  
  print("Step 1: get well estimates")
  well.estimates <- well.estimates.f(lfgrpd[lfgrpd$year.firstset==year,],lfmm)
  
  print("Step 2: get catch estimates")
  catch.estimates <- get.catch.estimates.f(cae,cae.stratflg,total.unlds,lfgrpd,lfgrpd.stratflg,lfmm,year,2,well.estimates,area.substitution.mat,grow.increments,PS=PS,Species=Species)
  
  print("Step 3: get fishery estimates")
  fishery.estimates <- fishery.estimates.f(catch.estimates$stratum.estimates.withsamps,catch.estimates$stratum.estimates.NOsamps,year,PS=PS,Species=Species)
  
  assign(paste0("fishery.estimates.", year), fishery.estimates, pos=1)
}
save(list=objects(pat="fishery.estimates"),file=paste0(save_dir,"SKJ_",PS,"_2000-2021.RData"))

# get final catch and comp output for the stock assessment
SKJ.NOA.Catch.20002021<-compile.catch.output.f(yr.start,yr.end,PS=PS,Species=Species,c("A1","A2","A3","A4"))
SKJ.NOA.Comp.20002021<-compile.sizecomps.output.f(yr.start,yr.end,PS=PS,Species=Species)


# Running SKJ DEL
PS <- "DEL"
area.substitution.mat <- area.substitution.mat.SKJ.DEL.SAC2022 # for DEL

cae.stratflg <- create.strat.flg.f(cae$latc5,cae$lonc5,is.lwrght=F,cae$month,cae$setype,cae$class,PS=PS,Species=Species)
lfgrpd.stratflg <- create.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,is.lwrght=T,floor(lfgrpd$moda/100),lfgrpd$setype,lfgrpd$class,PS=PS,Species=Species)

# Loop to get all years' fishery estimates
for(year in yr.start:yr.end) {
  print(paste0("Year: ",year))
  
  print("Step 1: get well estimates")
  well.estimates <- well.estimates.f(lfgrpd[lfgrpd$year.firstset==year,],lfmm)
  
  print("Step 2: get catch estimates")
  catch.estimates <- get.catch.estimates.f(cae,cae.stratflg,total.unlds,lfgrpd,lfgrpd.stratflg,lfmm,year,2,well.estimates,area.substitution.mat,grow.increments,PS=PS,Species=Species)
  
  print("Step 3: get fishery estimates")
  fishery.estimates <- fishery.estimates.f(catch.estimates$stratum.estimates.withsamps,catch.estimates$stratum.estimates.NOsamps,year,PS=PS,Species=Species)
  
  assign(paste0("fishery.estimates.", year), fishery.estimates, pos=1)
}
save(list=objects(pat="fishery.estimates"),file=paste0(save_dir,"SKJ_",PS,"_2000-2021.RData"))

# get final catch and comp output for the stock assessment
SKJ.DEL.Catch.20002021<-compile.catch.output.f(yr.start,yr.end,PS=PS,Species=Species,c("A1","A2"))
SKJ.DEL.Comp.20002021<-compile.sizecomps.output.f(yr.start,yr.end,PS=PS,Species=Species)

# save results
write.csv(SKJ.OBJ.Catch.20002021,file=paste0(save_dir,"SKJ.OBJ.Catch.20002021.csv"),row.names = FALSE)
write.csv(SKJ.OBJ.Comp.20002021,file=paste0(save_dir,"SKJ.OBJ.Comp.20002021.csv"),row.names = FALSE)
write.csv(SKJ.NOA.Catch.20002021,file=paste0(save_dir,"SKJ.NOA.Catch.20002021.csv"),row.names = FALSE)
write.csv(SKJ.NOA.Comp.20002021,file=paste0(save_dir,"SKJ.NOA.Comp.20002021.csv"),row.names = FALSE)
write.csv(SKJ.DEL.Catch.20002021,file=paste0(save_dir,"SKJ.DEL.Catch.20002021.csv"),row.names = FALSE)
write.csv(SKJ.DEL.Comp.20002021,file=paste0(save_dir,"SKJ.DEL.Comp.20002021.csv"),row.names = FALSE)