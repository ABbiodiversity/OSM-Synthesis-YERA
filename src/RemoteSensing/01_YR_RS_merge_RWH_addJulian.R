
library(rgdal)
library(tidyverse)
library(lattice)


#read and clean data
dat <- read.csv("./data/processed/RemoteSensing/YR_sitesRS.csv", stringsAsFactors=F)
unique(dat$ss) #why only 476 sites?
ext2013 <- read.csv("./data/processed/RemoteSensing/extract2013.csv")
ext2014 <- read.csv("./data/processed/RemoteSensing/extract2014.csv")
ext2015 <- read.csv("./data/processed/RemoteSensing/extract2015.csv")
ext2016 <- read.csv("./data/processed/RemoteSensing/extract2016.csv")
ext2017 <- read.csv("./data/processed/RemoteSensing/extract2017.csv")
ext2018 <- read.csv("./data/processed/RemoteSensing/extract2018.csv")
ext2019 <- read.csv("./data/processed/RemoteSensing/extract2019.csv")
ext <- rbind(ext2013, ext2014, ext2015, ext2016, ext2017, ext2018, ext2019)
ext <- ext %>% select(-system.index, -.geo)
rm(list=c(paste0('ext', 2013:2019)))
length(unique(sapply(strsplit(as.character(ext$UID), '-'),'[[',1)))

#remove extraneous info from dat.
remove = c("ï..OID_", 'region_y', 'wetland_y', 'BUFF_DIST', 'ORIG_FID', 'Shape_Leng', 'Shape_Area')
dat = dat[,!colnames(dat) %in% remove]
rm(remove)

#Collapse dat to one occupancy state per year.

UIDs = sort(unique(dat$UID))
i=1
for(i in 1:length(unique(UIDs))) {
  U=UIDs[i]
  sub = dat[dat$UID==U,]
  occ = suppressWarnings(max(sub$occupied, na.rm=T))
  if(occ == -Inf) {occ=NA}
  sub = sub[1,]
  sub$occupied=occ
  if(i==1) {Occupancy = sub} else {Occupancy = rbind(Occupancy,sub)}
}
rm(list=c('i', 'sub', 'occ'))

i=3
#Collapse ext to one measurement per year by averaging.
for(i in 1:length(unique(UIDs))) {
  U=UIDs[i]
  sub = ext[ext$UID==U,]

  temp = data.frame(matrix(apply(sub[,1:11],2,mean), nrow=1))
  colnames(temp) = colnames(sub)[1:11]
  temp$UID = UIDs[i]
  if(i==1) {RS = temp} else {RS = rbind(RS,temp)}
}
rm(list=c('sub','temp', 'i'))


Occupancy <- merge(Occupancy, RS, by = "UID")

Occupancy$presence = Occupancy$occupied
Occupancy$presence = gsub(0, 'absent', Occupancy$presence)
Occupancy$presence = gsub(1, 'present', Occupancy$presence)
unique(Occupancy$ss)


dat <- Occupancy

doy2013 <- read.csv("./data/processed/RemoteSensing/DOY2013.csv")
doy2014 <- read.csv("./data/processed/RemoteSensing/DOY2014.csv")
doy2015 <- read.csv("./data/processed/RemoteSensing/DOY2015.csv")
doy2016 <- read.csv("./data/processed/RemoteSensing/DOY2016.csv")
doy2017 <- read.csv("./data/processed/RemoteSensing/DOY2017.csv")
doy2018 <- read.csv("./data/processed/RemoteSensing/DOY2018.csv")
doy2019 <- read.csv("./data/processed/RemoteSensing/DOY2019.csv")
alldoy = rbind(doy2013, doy2014, doy2015, doy2016, doy2017, doy2018, doy2019)

alldoy = aggregate(mean ~ UID, data=alldoy, mean)

dat = merge(dat, alldoy, by='UID')
colnames(dat)[ncol(dat)]='julian'

shp = readOGR("./data/processed/RemoteSensing/YR_sites_wetland_buf_wArea.shp")
shp = shp@data
shp = shp[order(shp$ss),]
hist(log(shp$area))
hist(shp$area)
x=shp[shp$area>2000,]
shp = aggregate(area ~ ss, data=shp, mean)

write.csv(dat, './data/processed/rs_annual_julian_2013-19.csv', row.names=F)


