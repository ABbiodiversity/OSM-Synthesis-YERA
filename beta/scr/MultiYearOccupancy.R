#-------------------------------------------------------------------------------

# setwd("D:/School stuff/Manuscripts/YERA Trend/MultiYearOccupancy")
library(unmarked)

Occ=read.table("./data/processed/yera_occupy_2013-18.csv", sep=',', header=T, stringsAsFactors=F)

Doy=read.table("./data/processed/yera_doy_2013-18.csv", sep=',', header=T, stringsAsFactors=F)

#Script attempts to emulate the analysis here:
# https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf

Stations=sort(unique(Occ$ss)) #Stations

M <- length(Stations) #Number of stations

J <- 4 #Visits per station per year

Years=sort(unique(Occ$year))
T <- length(Years) #Number of years

psi <- rep(NA, T)

muZ <- z <- array(dim = c(M, T))

y <- array(NA, dim = c(M, J, T), dimnames=list(Stations,
                                               1:J, Years))


#Now to fill the array with the required observations.

for(i in 1:length(Stations)) {
  S=Stations[i]
  for(j in 1:J) {
    V=j
    for(k in 1:T) {
      Y=Years[k]
      Obs=Occ$occupied[Occ$ss==S & Occ$sample==V & Occ$year==Y]
      y[i,j,k]=Obs
    }
  }
}

yy <- matrix(y, M, J*T, dimnames=list(Stations, paste0(rep(Years,each=4),1:J)))

year <- matrix(as.character(Years),
               nrow(yy), T, byrow=TRUE)


simUMF <- unmarkedMultFrame(
  y = yy,
  yearlySiteCovs = list(year = year),
  numPrimary=T)
summary(simUMF)

#Simple occupancy model where all parameters don't depend on time.
# I interpret this as assuming a constant extinction probability???
#Not entirely sure on interpretation,
#but I don't think this is the model we want.


m0 <- colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1,
             pformula = ~ 1, data = simUMF, method="BFGS")
summary(m0)

names(m0)

backTransform(m0, type="psi")

#Dynamic occupancy model where many things vary by year. Note
#I think it's best to fix detection probability across years.
#This is more like the model we want.

m1 <- colext(psiformula = ~1, # First-year occupancy
             gammaformula = ~ year-1, # Colonization
             epsilonformula = ~ year-1, # Extinction
             pformula = ~ year-1, # Detection
             data = simUMF)

m1

#Prediction and plotting

nd <- data.frame(year=as.character(Years[1:(length(Years)-1)])) #all but last year for Extinction probability
E.ext <- predict(m1, type='ext', newdata=nd)
E.col <- predict(m1, type='col', newdata=nd)
nd <- data.frame(year=as.character(Years))
E.det <- predict(m1, type='det', newdata=nd)

#Plotting

op <- par(mfrow=c(3,1), mai=c(0.6, 0.6, 0.1, 0.1))
with(E.ext, { # Plot for extinction probability
  plot(1:(T-1), Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Extinction probability ( ', epsilon, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:(T-1), labels=nd$year[1:(T-1)])
  arrows(1:(T-1), lower, 1:(T-1), upper, code=3, angle=90, length=0.03, col=4)
})

with(E.col, { # Plot for colonization probability
  plot(1:(T-1), Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Colonization probability ( ', gamma, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:(T-1), labels=nd$year[1:(T-1)])
  arrows(1:(T-1), lower, 1:(T-1), upper, code=3, angle=90, length=0.03, col=4)
})
with(E.det, { # Plot for detection probability: note 10 years
  plot(1:T, Predicted, pch=1, xaxt='n', xlab='Year',
       ylab=expression(paste('Detection probability ( ', p, ' )')),
       ylim=c(0,1), col=4)
  axis(1, at=1:T, labels=nd$year)
  arrows(1:T, lower, 1:T, upper, code=3, angle=90, length=0.03, col=4)
})
par(op)



