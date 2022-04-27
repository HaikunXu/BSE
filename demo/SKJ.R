# # load data
# 
# area.substitution.mat.DEL <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_DEL_SAC 2022.csv",header=F))
# 
# area.substitution.mat.UNA <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_UNA_SAC 2022.csv",header=F))
# 
# area.substitution.mat.FLT <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_FLT_SAC 2022.csv",header=F))
# 
# # check that each row has all areas (assuming all areas are viable substitutes for other areas)
# apply(area.substitution.mat.DEL,1,sum)
# apply(area.substitution.mat.UNA,1,sum)
# apply(area.substitution.mat.FLT,1,sum)
# 
# load("D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/spp comp_R functions_V3.RData")


library(BSE)
# setup
raw_data_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/Raw data extractions/"
save_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/SKJ/"
yr.start <- 2000
yr.end <- 2021

# area subsitution matrices
area.substitution.mat.DEL <- area.substitution.mat.SKJ.DEL.SAC2022
area.substitution.mat.UNA <- area.substitution.mat.SKJ.UNA.SAC2022
area.substitution.mat.FLT <- area.substitution.mat.SKJ.FLT.SAC2022

# Get the total unloads for the PS fleet
total.unlds <- read.unloads.f(raw_data_dir,"Unloading2000-2021.txt",yr.start,yr.end)
 # Get the CAE+IDM data
cae <- read.cae.f(raw_data_dir,"CAE-LatLon2000-2021.txt",yr.start,yr.end)
# Get the length-frequency data (length in millimeters)
lfmm <- read.lfmmdata.f(raw_data_dir,"LengthMM2000-2021.txt")
# Get the grouped length-frequency output
lfgrpd <- read.lengthfreq.f(raw_data_dir,"LengthFreq2000-2021.txt")

# get the well estimates for all years
get.well.estimates.f(lfgrpd,lfmm,yr.start,yr.end)
save.image(paste0(save_dir,"base files_2000-2021.RData"))

# Running SKJ OBJ
cae.stratflg <- create.strat.flg.f(cae$latc5,cae$lonc5,is.lwrght=F,cae$month,cae$setype,cae$class,PS="OBJ",Species = "SKJ")

lfgrpd.stratflg <- create.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,is.lwrght=T,floor(lfgrpd$moda/100),lfgrpd$setype,lfgrpd$class,PS="OBJ",Species = "SKJ")

# Loop to get all years' fishery estimates
for(year in yr.start:yr.end) {
  print(paste0("Year: ",year))
  
  print("Step 1: get well estimates")
  well.estimates <- well.estimates.f(lfgrpd[lfgrpd$year.firstset==year,],lfmm)
  
  print("Step 2: get catch estimates")
  catch.estimates <- get.catch.estimates.f(cae,cae.stratflg,total.unlds,lfgrpd,lfgrpd.stratflg,lfmm,year,2,well.estimates,area.substitution.mat.SKJ.FLT.SAC2022,grow.increments.2cmSKJ.betyftskj,PS="OBJ",Species = "SKJ")
  
  print("Step 3: get fishery estimates")
  fishery.estimates <- fishery.estimates.f(catch.estimates$stratum.estimates.withsamps,catch.estimates$stratum.estimates.NOsamps,year,PS="OBJ",Species = "SKJ")
  
  assign(paste0("fishery.estimates.", year), fishery.estimates, pos=1)
}

save(list=objects(pat="fishery.estimates"),file=paste0(save_dir,"SKJ_OBJ_2000-2021.RData"))

# get final catch and comp output for the stock assessment

SKJ.OBJ.Catch.20002021<-compile.catch.output.f(yr.start,yr.end,PS="OBJ",Species="SKJ",c("A1","A2","A3","A4"))
SKJ.OBJ.Comp.20002021<-compile.sizecomps.output.f(yr.start,yr.end,PS="OBJ",Species="SKJ")