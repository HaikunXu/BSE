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
yr.start <- 2000
yr.end <- 2021
Species <- "SKJ"
PS <- "OBJ"

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

# get the well estiamtes for all years
get.well.estimates.f(lfgrpd,lfmm,yr.start,yr.end)

save.image("D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/SKJ/base files_2000-2021.RData")

# Running SKJ OBJ
cae.stratflg <- create.strat.flg.f(cae$latc5,cae$lonc5,is.lwrght=F,cae$month,cae$setype,cae$class,PS="OBJ",Species = "SKJ")

lfgrpd.stratflg <- create.strat.flg.f(lfgrpd$lat.5deg,lfgrpd$lon.5deg,is.lwrght=T,floor(lfgrpd$moda/100),lfgrpd$setype,lfgrpd$class,PS="OBJ",Species = "SKJ")

get.catch.estimates.f(cae,cae.stratflg,total.unlds,lfgrpd,lfgrpd.stratflg,lfmm,2000,2,well.estimates.2000,area.substitution.mat.SKJ.FLT.SAC2022,grow.increments.2cmSKJ.betyftskj,PS="OBJ",Species = "SKJ")

fishery.estimates.2000 <- fishery.estimates.f(stratum.estimates.2000.withsamps,stratum.estimates.2000.NOsamps,2000,PS="OBJ",Species = "SKJ")
