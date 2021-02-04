
#Yellow Rail R2 analysis
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(dismo)
library(gbm)
library(ggthemes)

################################################
#load rs data, collapse by station.
################################################
rs = read.csv('./data/processed/rs_annual_2013-19.csv', stringsAsFactors=F)

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

i=1
for(i in 1:nrow(ci)) {
  S = ci$ss[i]
  sub = rs2[rs2$ss==S,]
  ci[i,colsMean]=apply(sub[,c('PC1', 'PC2',cols)],2,mean)
  ci[i,colsSD]=apply(sub[,c('PC1', 'PC2',cols)],2,sd)
}

noNever = ci[ci$Consistent %in% c('Consistent', 'Inconsistent'),]
#Remove stations with no remote sensing data.
noNever = noNever[!is.na(noNever$PC1Mean),]

######################################################
#Boosted Regression tree.
######################################################

response = 'Consistent'
rsVars = c('PC1Mean', 'PC2Mean', 'PC1SD', 'PC2SD')
noNever$Consistent = gsub('Consistent','1', noNever$Consistent)
noNever$Consistent = gsub('Inconsistent','0', noNever$Consistent)
noNever$Consistent = as.numeric(noNever$Consistent)

x=noNever[noNever$Consistent==1,]

fit <- gbm.step(noNever, rsVars, response, family = "bernoulli", tree.complexity = 5,
                learning.rate = 0.001, bag.fraction = 0.5)
dev.off()
dfImportance <- data.frame(summary(fit))
dfImportance <- arrange(dfImportance, rel.inf)

ggplot(dfImportance, aes(x = reorder(var, -rel.inf), y = rel.inf)) + geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Relative importance") + theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


noNever$predicted=predict.gbm(fit, newdata = noNever, type='response')
plot(noNever$predicted,noNever$Consistent)

for (i in 1:length(rsVars)){
  var <- rsVars[i]
  responsePlot <- plot.gbm(fit, i.var = var, return.grid=TRUE, type="response")
  print(
    ggplot(responsePlot, aes_string(x = var, y = "y")) + geom_jitter() + geom_smooth() +
      xlab(var) + ylab("Probability of\nconsistently occupied") + theme_minimal(base_size = 25)
  )
}


































