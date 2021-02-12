
#Identify clusters of nearby sites. I'm going to use 10km clusters, just based on the size of our study
#area. So if a site is within 10km of another site, they are classified into the same cluster.
#So these are spatial clusters that should reduce/eliminate the problem of training/testing on nearby
#points.

library(sp)
library(geodist)
library(rgdal)

#Get locations.
locs = read.csv("./data/lookup/yera_ss-meta_2013-18.csv", stringsAsFactors=F)


#Figure out spatial clustering.
locs = SpatialPointsDataFrame(coords = cbind(locs$longit, locs$latit), proj4string=CRS("+proj=longlat"), data=locs)

d = geodist(x = cbind(locs@data$longit, locs@data$latit), measure = 'geodesic')

rownames(d)=locs@data$ss
colnames(d)=locs@data$ss

diag(d)=0

i=24

locs@data$cluster=NA

for(i in 1:nrow(d)) {
  #If it has a cluster already, next
  if(!is.na(locs@data$cluster[i])) {next}
  #If i is 1, set cluster to 1. Otherwise add one.
  if(i==1) {cl = 1} else {cl = max(locs@data$cluster, na.rm=T)+1}

  #Station name
  S = rownames(d)[i]
  print(S)

  #number of stations in cluster always starts at one.
  prev=1

  #Get distances associated with that station.
  set = matrix(d[S,], nrow=1)

  #Find its buddies within 10km.
  buddies = which(set<=10000, arr.ind=T)

  #Find station names of its buddies.
  newS = unique(colnames(d)[buddies[,2]])

  #How many are there?
  current = length(newS)
  print(paste(prev, current))

  #While loop to do the same using all the buddies together, to see if we can grab more nearby.
  #This loops stops when the number of stations stops growing.
  while(current>prev) {
    prev=current
    newset = d[newS,]
    buddies = which(newset<=10000, arr.ind=T)
    newS = unique(colnames(d)[buddies[,2]])
    current = length(newS)
    print(paste(prev, current))
  }
  locs@data$cluster[locs@data$ss %in% newS]=cl

}

plot(locs, col=locs@data$cluster)
max(locs@data$cluster)

y=locs@data

y=y[,c('ss','latit','longit','cluster')]


#Random year selection.

#Get rs data for random year selection.
rs = read.csv("./data/processed/rs_annual_2013-19.csv", stringsAsFactors=F)
#Remove empty station years
rs = rs[!is.na(rs$presence),]
#Remove station years without RS data.
rs = rs[!is.na(rs$B2),]


# y$random=NA
# sum(y$ss %in% rs$ss)
# set.seed(1546)
# for(i in 1:nrow(y)) {
#   S = y$ss[i]
#   if(!S %in% rs$ss) {next}
#   sub = rs[rs$ss == S,]
#   years = as.character(unique(sub$year))
#   chosen = sample(years, 1)
#   y$random[i]=chosen
# }

sum(is.na(y$random))

#Adding "best" year selection rather than total randomness.
y=read.csv('./data/processed/spatial_clusters.csv', stringsAsFactors=F)

y$best=NA
sum(y$ss %in% rs$ss)
set.seed(2051)
i=1
for(i in 1:nrow(y)) {
  S = y$ss[i]
  if(!S %in% rs$ss) {next}
  sub = rs[rs$ss == S,]
  if(max(sub$occupied)==1) {
    sub = sub[sub$occupied==1,] #Throw out unoccupied years.
    years = as.character(unique(sub$year))
    chosen = sample(years, 1)
    y$best[i]=chosen
  } else {
    chosen = y$random[i] #If there are no occupied years, then stick with the original random year.
    y$best[i]=chosen
  }
}
sum(y$random != y$best, na.rm=T) #129 station years changed, so this might actually help things.

write.csv(y, './data/processed/spatial_clusters.csv', row.names=F)


