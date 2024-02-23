library(lubridate)
library(lme4)
source('./AccessToWildtrax.R')


long=read.csv('./data/processed/LongWaterMeasurements.csv',stringsAsFactor=F)

wide=read.csv('./data/processed/WaterMeasurementsByPoint.csv', stringsAsFactors=F)



#Remove stations with only one measurement.

MultiStations=wide$SDD[wide$NYears>1]

#Remove stations where all values were 0 (upland).

y=rowSums(wide[,4:(ncol(wide)-1)], na.rm=T)
NonZeroStations=wide$SDD[y>0]

#Reduce the size of the long data to stations with some water and >1 measurement

data=long[long$SDD %in% MultiStations & long$SDD %in% NonZeroStations,]
data2=wide[wide$SDD %in% MultiStations & wide$SDD %in% NonZeroStations,]

#add julian date.
data$julian=as.POSIXlt(data$VegDate)$yday+1

#Now going to subset west data.

SurveyData=read.csv('./data/processed/yera_occupy_2013-19_new.csv', header=T, stringsAsFactors=F)

region='west'
for(region in c('west', 'east', 'unmineable')) {
  Stations=sort(unique(SurveyData$ss[SurveyData$region==region]))

  Subset=data[data$WTStation %in% Stations,]
  Subset2=data2[data2$WTStation %in% Stations,]


  Subset$year=as.factor(Subset$year)
  #Subset$Depth=log(Subset$Depth+0.2)
  #Subset$Depth=log(Subset$Depth+0.2)
  Subset$Depth=Subset$Depth+1
  hist(Subset$Depth)
  Subset$julian=(Subset$julian-mean(Subset$julian))/sd(Subset$julian)

  #Poisson regression. Tried negative binomial, but
  #didn't seem to converge.
  fm1=lmer(log(Depth) ~ year + julian + (1|Station/SDD), data=Subset, REML=T)
  #fm1=glmer(Depth ~ year  + (1|SDD), data=Subset, family='poisson')
  fm1
  plot(fm1)
  VarCorr(fm1)
  y=ranef(fm1)$SDD
  hist(y[,1])

  Ys=sort(unique(Subset$year))
  newdata=Subset2[,c('SDD', 'WTStation', 'Station')]
  newdata2=newdata
  newdata[paste0('x',Ys)]=NA

  i=1
  for(i in 1:length(Ys)) {
    k=Ys[i]
    newdata2$year=k
    newdata2$julian=0
    newdata[,paste0('x',k)]=exp(predict(fm1, newdata2, type='response'))-1
  }
  jpeg(paste0('./results/',region, 'WaterDepthByYear.jpeg'), res=500, width=5, height=5, units='in')
  boxplot(newdata[,paste0('x',Ys)], yaxs='i', ylim=c(0,20), cex.axis=0.8)
  dev.off()

  x=Subset[Subset$year==2015,]
  table(x$Station)
  #hist(Subset$Depth[Subset$year==2016], breaks=seq(0,105,5))
  #boxplot(data$julian ~ data$year)
  #boxplot(Subset$julian ~ Subset$year)
}
?isSingular
