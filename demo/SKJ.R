# load data

area.substitution.mat.DEL <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_DEL_SAC 2022.csv",header=F))

area.substitution.mat.UNA <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_UNA_SAC 2022.csv",header=F))

area.substitution.mat.FLT <-as.matrix(read.csv("data/area_substitution_matrix_SKJ_FLT_SAC 2022.csv",header=F))

# check that each row has all areas (assuming all areas are viable substitutes for other areas)
apply(area.substitution.mat.DEL,1,sum)
apply(area.substitution.mat.UNA,1,sum)
apply(area.substitution.mat.FLT,1,sum)

load("D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/spp comp_R functions_V3.RData")

library(BSE)
BSE::get.VB.output.new.V2.f("Unloading2000-2021.txt","CAE-LatLon2000-2021.txt","LengthMM2000-2021.txt","LengthFreq2000-2021.txt",2000,2021)
