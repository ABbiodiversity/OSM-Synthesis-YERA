library(dplyr)
library(ggplot2)

#read and clean data
dat <- read.csv("./data/processed/RemoteSensing/YR_sitesRS.csv", stringsAsFactors=F)
ext2013 <- read.csv("./data/processed/RemoteSensing/extract2013.csv", stringsAsFactors=F)
ext2014 <- read.csv("./data/processed/RemoteSensing/extract2014.csv", stringsAsFactors=F)
ext2015 <- read.csv("./data/processed/RemoteSensing/extract2015.csv", stringsAsFactors=F)
ext2016 <- read.csv("./data/processed/RemoteSensing/extract2016.csv", stringsAsFactors=F)
ext2017 <- read.csv("./data/processed/RemoteSensing/extract2017.csv", stringsAsFactors=F)
ext2018 <- read.csv("./data/processed/RemoteSensing/extract2018.csv", stringsAsFactors=F)
ext2019 <- read.csv("./data/processed/RemoteSensing/extract2019.csv", stringsAsFactors=F)
ext <- rbind(ext2013, ext2014, ext2015, ext2016, ext2017, ext2018, ext2019)
ext <- ext %>% select(-system.index, -.geo)
ext$ss = sapply(strsplit(ext$UID,'-'), '[[', 1)
dat <- merge(dat,ext, by = "ss")
jt <- data.frame(occupied = c(0,1), presence = c("absent", "present"))
dat <- merge(dat, jt, by = "occupied")

RSvars <- colnames(dat[,16:25])

dat <- dat %>% year

i=1
for (i in 1:length(RSvars)){
  var <- RSvars[i]
  print(
    ggplot(dat, aes_string(x = "presence", y = var)) +
      geom_violin()
  )
}
