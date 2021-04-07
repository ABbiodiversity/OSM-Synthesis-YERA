
#Investigate PC values for consistently occupied vs inconsistently occupied vs never.


library(factoextra)

#load rs data.
rs = read.csv('./data/processed/rs_annual_dateCorrected_2013-19.csv', stringsAsFactors=F)
#load consistently occupied classification.
ci = read.csv('./src/ConsistentlyOccupiedStations/ConsistentlyOccupiedStations.csv', stringsAsFactors=F)
colnames(ci)[colnames(ci)=='Station']='ss'

#Remove inconclusive stations that only had one year of data.
ci = ci[ci$Consistent != 'Inconclusive',]

#Run PCA for rs data, and add first two PCs as columns.
cols = colnames(rs)[16:26]

rs2=rs[!is.na(rs$B2),]
pc=prcomp(rs2[,cols], scale=T)
fviz_eig(pc)

res.pt <- get_pca_ind(pc)
rs2$PC1 = res.pt$coord[,1]
rs2$PC2 = res.pt$coord[,2]

#Calculate metrics for each station. Mean, sd most obvious.

colsMean = c('PC1Mean', 'PC2Mean',paste0(cols, 'Mean'))
colsSD = c('PC1SD', 'PC2SD',paste0(cols, 'SD'))

ci[colsMean]=NA
ci[colsSD]=NA

sum(ci$ss %in% rs2$ss)
length(unique(ci$ss))

S="Y:207@1:E"
for(i in 1:nrow(ci)) {
  S = ci$ss[i]
  sub = rs2[rs2$ss==S,]
  ci[i,colsMean]=apply(sub[,c('PC1', 'PC2',cols)],2,mean)
  ci[i,colsSD]=apply(sub[,c('PC1', 'PC2',cols)],2,sd)
}

#splom(ci[,c(colsSD)])

#Now boxplot.

boxplot(ci$PC1Mean ~ ci$Consistent)
boxplot(ci$PC2Mean ~ ci$Consistent)
boxplot(ci$PC1SD ~ ci$Consistent)
boxplot(ci$PC2SD ~ ci$Consistent, ylim=c(0,2))


#Removing stations that were never occupied.

noNever = ci[ci$Consistent %in% c('Consistent', 'Inconsistent'),]

#Rerun PC analysis without Never.

rs2=rs[!is.na(rs$B2) & rs$ss %in% noNever$ss,]
pc=prcomp(rs2[,cols], scale=T)
z=fviz_eig(pc)
z$data$eig[1]+z$data$eig[2]
pc$rotation

res.pt <- get_pca_ind(pc)
rs2$PC1 = res.pt$coord[,1]
rs2$PC2 = res.pt$coord[,2]

#Calculate metrics for each station. Mean, sd most obvious.

colsMean = c('PC1Mean', 'PC2Mean',paste0(cols, 'Mean'))
colsSD = c('PC1SD', 'PC2SD',paste0(cols, 'SD'))


noNever[colsMean]=NA
noNever[colsSD]=NA

sum(noNever$ss %in% rs2$ss)
length(unique(noNever$ss))

S="Y:207@1:E"
for(i in 1:nrow(noNever)) {
  S = noNever$ss[i]
  sub = rs2[rs2$ss==S,]
  noNever[i,colsMean]=apply(sub[,c('PC1', 'PC2',cols)],2,mean)
  noNever[i,colsSD]=apply(sub[,c('PC1', 'PC2',cols)],2,sd)
}

dev.off()
boxplot(PC1Mean~Consistent, data=noNever)
boxplot(PC2Mean~Consistent, data=noNever)
boxplot(PC1SD~Consistent, data=noNever)
boxplot(PC2SD~Consistent, data=noNever)
which(noNever$PC2SD>3)
noNever[3,]

#Consistently occupied sites were higher in B5.

t.test(PC1Mean~Consistent, data=noNever) #Not significant
t.test(PC2Mean~Consistent, data=noNever) #Inconsistent has higher values of PC2.
#The main driver of PC2 is negative B5, which is supposed to "Emphasize biomass content and shorelines"
t.test(PC1SD~Consistent, data=noNever) #Not significant
t.test(PC2SD~Consistent, data=noNever) #Not significant

#Biggest differences in means occur in TCB, B3, B5 and B6. Although these
#differences are not as significant as PC2. See:
par(mfrow=c(4,3))
par(mar=c(2,2,0.5,0))
for(i in 3:length(colsMean)) {
  boxplot(noNever[,colsMean[i]] ~ noNever$Consistent, main=colsMean[i])
  print(colsMean[i])
  print(t.test(noNever[,colsMean[i]] ~ noNever$Consistent)$p.value)
}


#Biggest differences in sd occur in TCB, B3. Interestingly consistent sites had more variability
#in B3, but lower variability in TCB. See:
par(mfrow=c(4,3))
par(mar=c(2,2,0.5,0))
for(i in 3:length(colsSD)) {
  boxplot(noNever[,colsSD[i]] ~ noNever$Consistent, main=colsSD[i])
  print(colsSD[i])
  print(t.test(noNever[,colsSD[i]] ~ noNever$Consistent)$p.value)
}



#Want to check if consistently occupied sites had higher predicted occupancy rates across years.

x=read.csv("./data/processed/BRT_habitat_predictions_2013-19.csv", stringsAsFactors=F)
noNever = merge(noNever, x, by='ss')

noNever$meanPred = apply(noNever[,paste0('pred', 2013:2019)], 1, mean)

dev.off()
boxplot(noNever$meanPred ~ noNever$Consistent) #No apparent difference.



#Make figures.

jpeg('./results/figures/RS_PrCompMean_ByConsistency.jpeg', width=8, height=4, res=500, units='in')
par(mfrow=c(1,2))
par(mar=c(2.5,3,0.5,0.5))
#Panel a: PC1 mean.
boxplot(ci$PC1Mean ~ ci$Consistent, axes=F, col=c('darkgreen', 'orange', 'darkred'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "PC 1 (Brightness)", line = 2)
box()
legend('topleft', legend='a) PC1 Mean', bty='n')

#Panel b: PC2 mean.
boxplot(ci$PC2Mean ~ ci$Consistent, axes=F, col=c('darkgreen', 'orange', 'darkred'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "PC 2 (Veg. health)", line = 2)
box()
legend('topleft', legend='b) PC2 Mean', bty='n')
dev.off()

#Panel c: PC1 SD
jpeg('./results/figures/RS_PrCompSD_ByConsistency.jpeg', width=8, height=4, res=500, units='in')
par(mfrow=c(1,2))
par(mar=c(2.5,3,0.5,0.5))
boxplot(ci$PC1SD ~ ci$Consistent, axes=F, col=c('darkgreen', 'orange', 'darkred'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "PC 1 SD (Brightness)", line = 2)
box()
legend('topleft', legend='a) PC1 SD', bty='n')

#Panel d: PC1 SD

boxplot(ci$PC2SD ~ ci$Consistent, axes=F, col=c('darkgreen', 'orange', 'darkred'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
axis(side = 1, at=c(1,2,3), labels=c("Consistent", "Inconsistent", "Never"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "PC 2 SD (Veg. health)", line = 2)
box()
legend('topleft', legend='b) PC2 SD', bty='n')


dev.off()

#Mann whitney U tests

wilcox.test(noNever$PC1Mean ~ noNever$Consistent)
boxplot(noNever$PC1Mean ~ noNever$Consistent)
wilcox.test(noNever$PC2Mean ~ noNever$Consistent)
wilcox.test(noNever$PC1SD ~ noNever$Consistent)
wilcox.test(noNever$PC2SD ~ noNever$Consistent)
table(noNever$Consistent)

jpeg('./results/figures/RS_PrComp2_ConsIncons.jpeg', width=4, height=4, res=500, units='in')
par(mar=c(2.5,3,0.5,0.5))
boxplot(-PC2Mean~Consistent, data=noNever, axes=F, col=c('darkgreen', 'orange'))

axis(side = 1, tck = -.015, at=c(1,2,3), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-8,8,2))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-8,8,2))
axis(side = 1, at=c(1,2), labels=c("Consistent", "Inconsistent"),
     line=-0.2, lwd=0, cex.axis=1)

mtext(side = 2, "Principal Component 2 (veg. health)", line = 2)
box()

dev.off()












