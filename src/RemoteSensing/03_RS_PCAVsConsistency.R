
#Investigate PC values for consistently occupied vs inconsistently occupied vs never.


library(factoextra)

#load rs data.
rs = read.csv('./data/processed/rs_annual_2013-19.csv', stringsAsFactors=F)
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


noNever = ci[ci$Consistent %in% c('Consistent', 'Inconsistent'),]
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


