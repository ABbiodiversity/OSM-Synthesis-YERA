

#First section will examine whether predicted suitability is, on average, higher
#for consistently occupied stations than inconsistently.
#Fundamentally this is a spatial question, about whether better habitat is more
#consistently occupied.



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



y=aov(brt$meanPred ~ brt$Consistent)
summary(y)

jpeg('./results/figures/PredictedSuitability_ByConsistency.jpeg', width=8, height=4, res=500, units='in')
par(mfrow=c(1,2))
par(mar=c(2.5,3,0.5,0.5))
boxplot(brt$meanPred ~ brt$Consistent, axes=F, col=c('green', 'yellow', 'red'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(0,1,0.1))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(0,1,0.1))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "Mean of predicted occupancy rate", line = 2)
box()


boxplot(brt$sdPred ~ brt$Consistent, axes=F, col=c('green', 'yellow', 'red'))
axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(0,1,0.1))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(0,1,0.1))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "SD of predicted occupancy rate", line = 2)
box()
dev.off()

#Commented this out, it was looking at principal component 2.
# jpeg('./results/figures/RS_PrComp2_ConsIncons.jpeg', width=4, height=4, res=500, units='in')
# par(mar=c(2.5,3,0.5,0.5))
# boxplot(-PC2Mean~Consistent, data=noNever, axes=F, col=c('green', 'yellow'))
#
# axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
# axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
# axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
# axis(side = 1, at=c(1,2), labels=c("Consistent", "Inconsistent"),
#      line=-0.2, lwd=0, cex.axis=1)
#
# mtext(side = 2, "Principal Component 2 (veg. health)", line = 2)
# box()
#
# dev.off()




