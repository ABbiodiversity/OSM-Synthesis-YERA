

#This script will look at consistency as a function of mean habitat quality within the
#250m buffer (clipped to wetlands).

library(rgdal)
library(raster)



ci = read.csv('./src/ConsistentlyOccupiedStations/ConsistentlyOccupiedStations.csv', stringsAsFactors=F)
colnames(ci)[1]='ss'

brt=read.csv('./data/processed/BRT_habitat_predictions_best_2013-19.csv', stringsAsFactors=F)

brt = merge(brt, ci[,c('ss','Consistent')], by='ss')

brt = brt[brt$Consistent != 'Inconclusive',]
#remove those with missing data from some years.
brt = brt[!is.na(brt$pred2013),]
brt = brt[!is.na(brt$pred2014),]
brt = brt[!is.na(brt$pred2015),]
brt = brt[!is.na(brt$pred2016),]
brt = brt[!is.na(brt$pred2017),]
brt = brt[!is.na(brt$pred2018),]
brt = brt[!is.na(brt$pred2019),]

brt$meanPred = apply(brt[,paste0('pred', 2013:2019)],1,mean, na.rm=T)
brt$sdPred = apply(brt[,paste0('pred', 2013:2019)],1,sd, na.rm=T)

#Note this is a local file.
r = raster("D:/School stuff/GIS/YERA/YERA_COLL_SUM_FINAL11.tif")

buf = readOGR("./data/processed/RemoteSensing/YR_sites_wetland_buf_wArea.shp")

#Remove duplicates. Each buffer is represented many times.

buf = buf[order(buf@data$ss),]
head(buf@data,20)
buf@data$keep = NA

buf@data$keep[1]=TRUE
i=2
for(i in 2:nrow(buf@data)) {
  ss = buf@data$ss[i]
  buf@data$keep[i]=ifelse(ss == buf@data$ss[i-1], FALSE, TRUE)
  print(i)
}

buf = buf[buf@data$keep,]

#spatial transform
buf = spTransform(buf, crs(r))

values = extract(r, buf, fun=mean)

#Make column for mean habitat quality
buf@data$quality = values
rm(values)

#merge with brt data prior to plotting.

brt = merge(brt, buf@data[,c('ss','quality')], by='ss')

#View data.
hist(brt$quality)

y=aov(brt$quality ~ brt$Consistent)
summary(y)

jpeg('./results/figures/Hedley2020MapClass_ByConsistency.jpeg', width=8, height=4, res=500, units='in')
par(mar=c(2.5,3,0.5,0.5))
boxplot(brt$quality ~ brt$Consistent, axes=F, col=c('green', 'yellow', 'red'), xlab=NA, ylab=NA, ylim=c(0,8), yaxs='i')

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(0,8,1))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(0,8,1))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "Mean of predicted occupancy rate", line = 2)
box()
dev.off()





