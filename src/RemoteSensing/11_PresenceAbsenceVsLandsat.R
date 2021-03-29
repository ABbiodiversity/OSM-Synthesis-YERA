

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
  y=wilcox.test(formula)
  print(y$p.value)
}
dev.off()








