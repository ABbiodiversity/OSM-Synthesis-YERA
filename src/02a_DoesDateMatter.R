#setwd("D:/School stuff/Manuscripts/YERA Trend/MultiYearOccupancy")
library(unmarked)
library(AICcmodavg)

Occ=read.table("./data/processed/yera_occupy_2013-18_new.csv", sep=',', header=T, stringsAsFactors=F)
#manually change data where needed.

Doy=read.table("D:/School stuff/Manuscripts/YERA Trend/yera_doy_2013-18.txt", sep=',', header=TRUE, stringsAsFactors=F)


#Note with the new data we have lost some station-years.
#For now I'm just going to replace these station-years with NA
#This happens in the first bit of the script before moving to the
#analysis.

Stations=sort(unique(Occ$ss))
Years=sort(unique(Occ$year))
for(i in 1:length(Stations)) {
  for(j in 1:length(Years)) {
   Sub=Occ[Occ$ss == Stations[i] & Occ$year == Years[j] & !is.na(Occ$occupied),]
   if(nrow(Sub)>0 & nrow(Sub)<4) {
     print(Sub$ss[1])
     Occ$occupied[Occ$ss == Stations[i] & Occ$year == Years[j]]=NA
   }
  }
}

#Check.
X=table(Occ$ss[!is.na(Occ$occupied)], Occ$year[!is.na(Occ$occupied)])
table(X)
Y=which(X<4 & X>0, arr.ind=T)
rm(list=ls()[!ls()%in% c('Doy', 'Occ')])

#Copy to Doy data frame
if(sum(Doy$ss==Occ$ss)==nrow(Doy) &#Check they're sorted the same way
   sum(Doy$year==Occ$year)==nrow(Doy)) {
  Doy$stdsdoy[is.na(Occ$occupied)]=NA
}
#Check.
sum(is.na(Doy$stdsdoy)==is.na(Occ$occupied))==nrow(Doy)


#In the following script I'm going to explore using AIC whether there is
#reason to include date as detection covariate.
#I'm only going to use the full data set (LAPR) for simplicity.
#This script follows the crossbill example here:
#https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf




Stations=sort(unique(Occ$ss)) #Stations

M <- length(Stations) #Number of stations

J <- 4 #Visits per station per year

Years=sort(unique(Occ$year))
Y <- length(Years) #Number of years

psi <- rep(NA, Y)

muZ <- z <- array(dim = c(M, Y))

y <- array(NA, dim = c(M, J, Y), dimnames=list(Stations,
                                               1:J, Years))

DATE=y

#Now to fill the array with the required observations.

for(i in 1:length(Stations)) {
  S=Stations[i]
  for(j in 1:J) {
    V=j
    for(k in 1:Y) {
      CurrentYear=Years[k]
      Obs=Occ$occupied[Occ$ss==S & Occ$sample==V & Occ$year==CurrentYear]
      y[i,j,k]=Obs
      Julians=sort(Doy$stdsdoy[Occ$ss==S & Occ$year==CurrentYear])
      if(length(Julians)==4) {DATE[i,j,k]=Julians[j]}
    }
  }
}

yy <- matrix(y, M, J*Y, dimnames=list(Stations, paste0(rep(Years,each=4),1:J)))
SDOY=matrix(DATE, M, J*Y, dimnames=list(Stations, paste0(rep(Years,each=4),1:J)))


year <- matrix(as.character(Years),
               nrow(yy), Y, byrow=TRUE)


umf <- unmarkedMultFrame(
  y = yy,
  yearlySiteCovs = list(year = year),
  obsCovs = list(date = SDOY),
  numPrimary=Y)

#Now do model selection. I'm only considering three models,
#though the example I'm working from does much more.
#Here I'm only interested in does detection vary with Julian date.


fm0 <- colext(~1, ~year-1, ~year-1, ~year, umf)
fm1 <- colext(~1, ~year-1, ~year-1, ~year + date,
              umf)
fm2 <- colext(~1, ~year-1, ~year-1, ~year + date + I(date^2),
              umf)

nd <- data.frame(year=as.character(Years))
E.det0 <- predict(fm0, type='det', newdata=nd)

nd<- data.frame(year=rep(as.character(Years),each=401), date=seq(-2,2,0.01))
E.det1 <- predict(fm1, type='det', newdata=nd)
E.det2 <- predict(fm2, type='det', newdata=nd)


plot(nd$date, E.det1$Predicted, col=nd$year, ylim=c(0.2,0.9))
abline(h=E.det0$Predicted)


