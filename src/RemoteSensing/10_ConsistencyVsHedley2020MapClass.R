

#This script will look at consistency as a function of mean habitat quality within the
#250m buffer (clipped to wetlands).

library(rgdal)
library(raster)



ci = read.csv('./src/ConsistentlyOccupiedStations/ConsistentlyOccupiedStations.csv', stringsAsFactors=F)
colnames(ci)[1]='ss'



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

#merge with ci data prior to plotting.

ci = merge(ci, buf@data[,c('ss','quality')], by='ss')

#View data.
hist(ci$quality)

ci = ci[ci$Consistent %in% c('Consistent', 'Inconsistent', 'Never'),]

y=aov(ci$quality ~ ci$Consistent)
summary(y)
sub = ci[ci$Consistent %in% c('Consistent', 'Inconsistent'),]
y=wilcox.test(sub$quality ~ sub$Consistent)
table(sub$Consistent)
table(ci$Consistent)

jpeg('./results/figures/Hedley2020MapClass_ByConsistency.jpeg', width=8, height=4, res=500, units='in')
par(mar=c(2.5,3,0.5,0.5))
boxplot(ci$quality ~ ci$Consistent, axes=F, col=c('darkgreen', 'orange', 'darkred'), xlab=NA, ylab=NA, ylim=c(0,8), yaxs='i')

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(0,8,1))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(0,8,1))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "Habitat quality score", line = 2)
box()
dev.off()

x=ci[ci$Consistent=='Consistent',]

table(ci$Consistent)


