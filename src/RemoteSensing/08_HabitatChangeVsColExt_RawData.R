


#This script will explore whether transitions from unoccupied to occupied and vice
#versa are associated with positive or negative changes in habitat quality (BRT)


data=read.csv('./data/processed/yera_occupy_2013-19_new.csv', stringsAsFactors=F)

brt=read.csv('./data/processed/BRT_habitat_predictions_best_2013-19.csv', stringsAsFactors=F)
brt2=read.csv('./data/processed/BRT_habitat_predictions_2013-19.csv', stringsAsFactors=F)
colnames(brt2)
plot(as.matrix(brt[,2:8]), as.matrix(brt2[,2:8]))
plot(as.matrix(brt[,9:14]), as.matrix(brt2[,9:14]))
#Remove NAs for no surveys conducted.
data = data[!is.na(data$occupied),]

#Note in year t, if it has a colonization or an extinction, that will mean it experienced
#that in the transition from t-1 to t (not from t to t+1)

data$sy = paste(data$ss, data$year, sep='-')

#Station years with surveys.
sys = unique(data$sy)

#Create output data frame. One entry per station year. Indicator saying if
#the station experienced a colonization or extinction, and previous and current
#BRT predicted probabilities.
OUT = data.frame(stationYear = sys,occStatus=NA, col=NA, ext=NA, prevPred=NA, curPred=NA)

for(i in 1:length(sys)) {
  d = data[data$sy == sys[i],]
  year = d$year[1]
  currocc = max(d$occupied)
  OUT$occStatus[i]=currocc
  if(year==2013) {next} #No col or ext prior to 2013.

  comp = data[data$ss == d$ss[1] & data$year<year,]
  if(nrow(comp)==0) next
  comp = comp[comp$year==max(comp$year),]
  prevocc = max(comp$occupied)
  prevyear = comp$year[1]

  if(currocc==prevocc) next
  if(currocc == 1 & prevocc==0) {OUT$col[i]=1}
  if(currocc == 0 & prevocc==1) {OUT$ext[i]=1}

  if(OUT$col[i] == 1 | OUT$ext[i] == 1) {
    bprev = brt[brt$ss==d$ss[1], paste0('pred',prevyear)]
    bcur = brt[brt$ss==d$ss[1], paste0('pred',year)]
    if(length(bprev) != 0) OUT$prevPred[i]=bprev
    if(length(bcur) != 0) OUT$curPred[i]=bcur
  }
}

OUT$diff=OUT$curPred - OUT$prevPred
OUT$qdiff = qlogis(OUT$curPred)-qlogis(OUT$prevPred)

sum(OUT$occStatus)

exts = OUT[OUT$ext==1 & !is.na(OUT$ext),]

cols = OUT[OUT$col==1 & !is.na(OUT$col),]

hist(exts$diff)
hist(cols$diff)


hist(exts$qdiff)
hist(cols$qdiff)
boxplot(exts$qdiff, cols$qdiff, names = c('Extinctions', 'Colonizations'))

t.test(exts$qdiff, cols$qdiff)
boxplot()



jpeg('./results/figures/colextVsDeltaSuitability.jpeg', width=4, height=4, units='in', res=500)
par(mar=c(3,3,0.5,0.5))
boxplot(exts$diff, cols$diff, names = c('Extinctions', 'Colonizations'), col=c('red', 'green'), axes=F)

axis(side = 1, tck = -.015, at=c(1,2), labels = NA)
axis(side = 2, tck = -.015, labels = NA, at=seq(-2,2,0.1))
axis(side = 2, lwd = 0, line = -.4, las = 1, at=seq(-2,2,0.1))
axis(side = 1, at=c(1,2), labels=c("Extinctions", "Colonizations"),
     line=0.3, lwd=0, cex.axis=1)

mtext(side = 2, "Change in predicted occupancy", line = 2)
box()
dev.off()

