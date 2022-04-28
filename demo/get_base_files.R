library(BSE)
# setup
raw_data_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/Raw data extractions/"
save_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/SKJ/"
yr.start <- 2000
yr.end <- 2021

# area subsitution matrices
# area.substitution.mat.DEL <- area.substitution.mat.SKJ.DEL.SAC2022
# area.substitution.mat.UNA <- area.substitution.mat.SKJ.UNA.SAC2022
# area.substitution.mat.FLT <- area.substitution.mat.SKJ.FLT.SAC2022

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