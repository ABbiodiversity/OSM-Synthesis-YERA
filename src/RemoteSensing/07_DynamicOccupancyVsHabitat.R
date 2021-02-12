#-------------------------------------------------------------------------------

# Title: YERA Occupancy Modeling of habiat trends
# Created: February 8, 2021

# Objectives: Model Yellow Rail extinction and colonization as a function
#of habitat quality from 2013-2019.

# Script attempts to emulate the analysis here:
# https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf
# Note one difference I made is to not code the number of years as T.
# Instead I code it as Y, because T conflicts with TRUE, messed things up.

#-------------------------------------------------------------------------------

# Load packages
library(unmarked)

# Import data

Data=read.table("./data/processed/yera_occupy_2013-19_new.csv", sep=',', header=T, stringsAsFactors=F)

Doy=read.table("./data/processed/yera_doy_2013-19_new.csv", sep=',', header=T, stringsAsFactors=F)

brtData=read.csv('./data/processed/BRT_habitat_predictions_2013-19.csv', stringsAsFactors=F)

#-------------------------------------------------------------------------------

# Analysis. All regions (LAPR), then can break it up later.

Regions=c('LAPR', 'Mine', 'NoMine', 'West', 'East')

table(as.character(Data$region))

ww=1
for(ww in 1:length(Regions)) {
  R=Regions[ww]
  print(paste0('Working on ',R))
  if(R=='LAPR') {Occ=Data}
  if(R=='Mine') {Occ=Data[Data$region %in% c('mineable', 'west', 'east'),]}
  if(R=='NoMine') {Occ=Data[Data$region == 'unmineable',]}
  if(R=='West') {Occ=Data[Data$region == 'west',]}
  if(R=='East') {Occ=Data[Data$region == 'east',]}

  Stations=sort(unique(Occ$ss)) # Stations

  Stations = Stations[Stations %in% brtData$ss] #Filter those without BRT data.

  M <- length(Stations) # Number of stations

  J <- 4 # Visits per station per year

  Years=sort(unique(Occ$year))
  Y <- length(Years) # Number of years

  psi <- rep(NA, Y)

  muZ <- z <- array(dim = c(M, Y))

  y <- array(NA, dim = c(M, J, Y), dimnames=list(Stations,
                                                 1:J, Years))


  # Now to fill the array with the required observations.

  for(i in 1:length(Stations)) {
    S=Stations[i]
    for(j in 1:J) {
      V=j
      for(k in 1:Y) {
        CurrentYear=Years[k]
        Obs=Occ$occupied[Occ$ss==S & Occ$sample==V & Occ$year==CurrentYear]
        y[i,j,k]=Obs
      }
    }
  }

  yy <- matrix(y, M, J*Y, dimnames=list(Stations, paste0(rep(Years,each=4),1:J))) #matrix
  #with Nstation number of rows (476) and visits*years columns (28).

  year <- matrix(as.character(Years),
                 nrow=nrow(yy), ncol=Y, byrow=TRUE)

  #Add brt values as covariates. Going to try first year as 0 (or NA?), years 2-7 have values.
  brt <- year
  class(brt)='numeric'
  brt[!is.na(brt)]=NA #Set to NA.
  rownames(brt)=Stations
  colnames(brt)=Years
  i=1
  j=1
  for(i in 1:nrow(brt)) {
    for(j in 1:ncol(brt)) {
      t = as.numeric(colnames(brt)[j])
      if(t<2019) {
        brt[i,j]=qlogis(brtData[brtData$ss==rownames(brt)[i],paste0('pred', t+1)])-
          qlogis(brtData[brtData$ss==rownames(brt)[i],paste0('pred', t)])
      }

      # if(colnames(brt)[j]=='2013') {
      #   brt[i,j]=0
      # } else {
      #   brt[i,j]=brtData[brtData$ss==rownames(brt)[i],paste0('prob', colnames(brt)[j])]
      # }
    }
  }
  brt[,7]=0
  brt[is.na(brt)]=0
  # cn=colnames(brt)
  # brt=brt[,2:7]
  # brt=cbind(brt, rep(0,nrow(brt)))
  # colnames(brt)=cn
  # brt[is.na(brt)]=0
  #brt=plogis(brt)
  #hist(brt) #Need to change this to orig values on logit scale.
  #brt = brt+0.0001

  simUMF <- unmarkedMultFrame(
    y = yy,
    yearlySiteCovs = list(year = year, brt = brt),
    numPrimary=Y)
  #summary(simUMF)

  #Dynamic occupancy model where many things vary by year. Note
  #I think it's best to fix detection probability across years.
  #This is more like the model we want.

  m1 <- colext(psiformula = ~1, # First-year occupancy
               gammaformula = ~ brt + year, # Colonization
               epsilonformula = ~ brt + year, # Extinction
               pformula = ~ year-1, # Detection
               data = simUMF)
  ?colext
  m1@projected.mean
  #m1

  #Prediction and plotting
  brtvals = seq(-2.5,2.5,0.1)
  nd <- data.frame(year=rep(as.character(Years[1:(length(Years)-1)]), each=length(brtvals)), brt=rep(brtvals, length(Years)-1)) #all but last year for Extinction probability
  E.ext <- predict(m1, type='ext', newdata=nd)
  E.ext$Parameter='Extinction'
  E.ext$Year=nd$year
  E.ext$brt=nd$brt
  plot(E.ext$brt, E.ext$Predicted)
  E.col <- predict(m1, type='col', newdata=nd)
  E.col$Parameter='Colonization'
  E.col$Year=nd$year
  nd <- data.frame(year=as.character(Years))
  E.det <- predict(m1, type='det', newdata=nd)
  E.det$Parameter='Detection'
  E.det$Year=nd$year

  #Early results suggest that the change in habitat quality has effectively no effect on
  #occupancy parameters. Keep in mind, however, this depends on the ranking of habitat quality.

  #Going to take a raw-data approach to this and see if I can agree with the notion that
  #transitions were not significantly associated with habitat parameters.

#
#   #m1@projected.mean
#
#   Results=list(Occupancy=m1@projected.mean,
#                Extinction=E.ext,
#                Colonization=E.col,
#                Detection=E.det)
#   if(ww==1) {
#     OUT=list(Results)
#     names(OUT)=R
#     MODELS=list(m1)
#     names(MODELS)=R
#   } else {
#     OUT=append(OUT, list(Results))
#     names(OUT)[length(OUT)]=R
#     MODELS=append(MODELS, m1)
#     names(MODELS)[length(MODELS)]=R
#   }
#   print(paste0(R, ' Complete!'))
#   if(ww==length(Regions)) {
#     rm(list=ls()[!ls()%in% c('Data', 'Doy', 'OUT', 'MODELS')])
#   }
# }
#
# # Save outputs
#
# save(OUT, file='./results/occupancy/MultiYearOccupancyOutput_2013-19.RData')
#
# # save(MODELS, file='MultiYearOccupancyModels.RData')
