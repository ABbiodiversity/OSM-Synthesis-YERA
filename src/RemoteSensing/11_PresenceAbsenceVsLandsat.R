
library(lme4)

ci = read.csv('./src/ConsistentlyOccupiedStations/ConsistentlyOccupiedStations.csv', stringsAsFactors=F)

data = read.csv("./data/processed/yera_occupy_2013-19_new.csv", stringsAsFactors = F)

rs = read.csv('./data/processed/rs_annual_julian_2013-19.csv', stringsAsFactors=F)

#Subset data from inconsistently occupied locations.

rs = rs[rs$ss %in% ci$Station[ci$Consistent == 'Inconsistent'],]


#Remove unsurveyed years.

rs = rs[!is.na(rs$occupied),]

vars = colnames(rs)[16:26]

jpeg('./results/figures/PresenceAbsenceVsLandsat.jpeg', width=7, height=8, res=500, units='in')
par(mfrow=c(4,3))
par(mar=c(3.5,4.3,0.5,0.5))
for(i in 1:11) {
  formula=rs[,vars[i]] ~ rs$presence
  boxplot(formula, ylab=NA, axes=F, col='gray', xlab=NA)
  axis(side=1, at=c(1,2), labels=c('absent', 'present'))
  axis(side=2, las=1)
  mtext(side = 2, vars[i], line = 3.2)
  box()
  y=wilcox.test(formula) #Mann whitney U test doesn't account for repeated measures.
  print(y$p.value)
}
dev.off()

#Eleven models to run.

#GLMM with response: 1/0, and predictor each variable, random effect for station.

OUT=data.frame(var=vars,AICwith=rep(NA,11), AICwithout=rep(NA,11))
i=1
for(i in 1:11) {
  sub = rs[,c('occupied',vars[i], 'ss')]
  colnames(sub)[2]='predictor'
  sub$ss = as.factor(sub$ss)
  y1=glmer(occupied ~ predictor + (1|ss), data=sub, family=binomial)
  OUT$AICwith[i]=summary(y1)$AICtab[1]
  y2=glmer(occupied ~ (1|ss), data=sub, family=binomial)
  OUT$AICwithout[i]=summary(y2)$AICtab[1]
}

p = rs[rs$occupied==1,]
a = rs[rs$occupied==0,]

p = aggregate(B5 ~ ss, data=p, FUN = mean)
a = aggregate(B5 ~ ss, data=a, FUN = mean)
both = merge(p, a, by='ss')
hist(both$B5.x-both$B5.y)

x=data[data$ss=='RS:27:E',]
y=rs[rs$ss=='RS:27:E',]

OUT[,2:3]=round(OUT[,2:3],2)
write.csv(OUT, './results/PresenceVsAbsenceLandsatAIC.csv',row.names=F)
