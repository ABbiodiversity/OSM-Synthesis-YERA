

data=read.csv('./data/processed/yera_occupy_2013-19_new.csv', stringsAsFactors=F)

OUT=data.frame(Station=sort(unique(data$ss)), stringsAsFactors=F)

OUT$OccupiedYears=NA

OUT$SurveyedYears=NA

OUT$Consistent=NA

OUT$Extinctions=NA

OUT$Colonizations=NA

vector=Result

TransitionMatrix=function(vector) {
  vector=vector[!is.na(vector)]
  vector=gsub(0, 'Vacant',vector)
  vector=gsub(1, 'Occupied',vector)
  m=matrix(0, nrow=2, ncol=2)
  colnames(m)=c('Occupied', 'Vacant')
  rownames(m)=colnames(m)
  if(length(vector)>1) {
    for(i in 1:(length(vector)-1)) {
      current=vector[i]
      upcoming=vector[i+1]
      m[current,upcoming]=m[current, upcoming]+1
    }
  }
  return(m)
}

for(i in 1:nrow(OUT)) {
  S=OUT$Station[i]
  Subset=data[data$ss==S,]
  Years=unique(Subset$year)
  Result=rep(NA, length(Years))
  names(Result)=Years
  for(j in 1:length(Years)) {
    Y=Years[j]
    Sub=Subset[Subset$year==Y,]
    if(sum(is.na(Sub$occupied))==nrow(Sub)) {next} else {
      Occ=as.numeric(sum(Sub$occupied, na.rm=T)>0)
      Result[names(Result)==Y]=Occ
    }
  }
  Surveyed=sum(!is.na(Result))
  Detected=sum(Result, na.rm=T)
  if(Detected==0) {Consistent='Never'}
  if(Detected>0 & Detected<Surveyed) {Consistent='Inconsistent'}
  if(Detected==Surveyed) {Consistent='Consistent'}
  if(Detected==1 & Surveyed==1) {Consistent='Inconclusive'}
  TM=TransitionMatrix(Result)
  OUT$OccupiedYears[OUT$Station==S]=Detected
  OUT$SurveyedYears[OUT$Station==S]=Surveyed
  OUT$Consistent[OUT$Station==S]=Consistent
  OUT$Extinctions[OUT$Station==S]=TM['Occupied','Vacant']
  OUT$Colonizations[OUT$Station==S]=TM['Vacant', 'Occupied']
}

rm(list=ls()[ls()!='OUT'])

OUT$Proportion=round(OUT$OccupiedYears/OUT$SurveyedYears, 2)

write.csv(OUT, './src/ConsistentlyOccupiedStations/ConsistentlyOccupiedStations.csv', row.names=F)

#Summary info below:
table(OUT$Consistent) #22 consistently occupied stations, 145 inconsistent.

hist(OUT$Proportion[OUT$Consistent=='Inconsistent'])
mean(OUT$Proportion[OUT$Consistent=='Inconsistent']) #When they were inconsistently
#occupied, they were occupied in 0.4 of the years on average. Note, this is biased high
#by the method of observation, I think. I.e. if they were extremely infrequently occupied
# we would likely not have classified as inconsistent (they would be classified as Never).
#Although the same could be said at the upper end, in which sites that are infrequently
#NOT occupied would also not be counted, since they are likely Consistent.

mean(OUT$SurveyedYears[OUT$Consistent=='Inconsistent']) #Inconsistent sites surveyed 4.2
#years on average.

hist(OUT$SurveyedYears[OUT$Consistent=='Consistent'])
mean(OUT$SurveyedYears[OUT$Consistent=='Consistent']) #Consistently occupied sites had
#a very good survey effort, average 4.7 years. So that's good... I wouldn't feel comfortable
#if most of them had only 2 years (with few visits, likely just random chance)

sum(OUT$Extinctions) #142 extinction events.

sum(OUT$Colonizations) #118 colonization events.












