library(BSE)
# global setup
raw_data_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/Raw data extractions/"
save_dir <- "D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/BET/"
yr.start <- 2000
yr.end <- 2021
Species <- "BET"
grow.increments <- grow.increments.betyftskj # the growth increment matrix

# Get the total unloads for the PS fleet
total.unlds <- read.unloads.f(raw_data_dir,"Unloading2000-2021.txt",yr.start,yr.end)
# Get the CAE+IDM data
cae <- read.cae.f(raw_data_dir,"CAE-LatLon2000-2021.txt",yr.start,yr.end)
# Get the length-frequency data (length in millimeters)
lfmm <- read.lfmmdata.f(raw_data_dir,"LengthMM2000-2021.txt")
# Get the grouped length-frequency output
lfgrpd <- read.lengthfreq.f(raw_data_dir,"LengthFreq2000-2021.txt")


# Running BET OBJ
PS <- "OBJ"
area.substitution.mat <- area.substitution.mat.BET.movemodel.ALL # for OBJ

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
save(list=objects(pat="fishery.estimates"),file=paste0(save_dir,"BET_",PS,"_2000-2021.RData"))

# get final catch and comp output for the stock assessment
BET.OBJ.Catch.20002021<-compile.catch.output.f(yr.start,yr.end,PS=PS,Species=Species,c("A1","A2","A3","A4","A5"))
BET.OBJ.Comp.20002021<-compile.sizecomps.output.f(yr.start,yr.end,PS=PS,Species=Species)

# save results
write.csv(BET.OBJ.Catch.20002021,file=paste0(save_dir,"BET.OBJ.Catch.20002021.csv"),row.names = FALSE)
write.csv(BET.OBJ.Comp.20002021,file=paste0(save_dir,"BET.OBJ.Comp.20002021.csv"),row.names = FALSE)

# compare results
library(tidyverse)

BET_OBJ_Catch.SAC12 <- read.csv("D:/OneDrive - IATTC/IATTC/2022/BSE stuff from Cleridy/spp comp programs_from 2000/current_estimates/BET_FLOATING-OBJECT CATCH_FORMATTED_1975-2019_for SAC 2020.csv")
BET.OBJ.Catch.20002021 <- read.csv(paste0(save_dir,"BET.OBJ.Catch.20002021.csv"))

BET_OBJ_Catch.SAC12 <- BET_OBJ_Catch.SAC12[,2:8] %>% mutate(Source="Cleridy")
BET.OBJ.Catch.20002021 <- BET.OBJ.Catch.20002021 %>% mutate(Source="Haikun")

BET_OBJ_Catch <- rbind(BET_OBJ_Catch.SAC12,BET.OBJ.Catch.20002021) %>% 
  mutate(Year=year+quarter/4-1/8) %>%
  filter(year>1999,year<2020) %>%
  gather(3:7,key="Area",value="Catch")

ggplot(data=BET_OBJ_Catch) +
  geom_line(aes(x=Year,y=Catch,color=Source)) +
  facet_wrap(~Area,nrow=5) +
  theme_bw()

ggsave(file=paste0(save_dir,"BET.OBJ.Catch.png"),h=10,w=10)
