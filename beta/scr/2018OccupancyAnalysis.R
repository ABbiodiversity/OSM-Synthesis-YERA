
setwd("D:/School stuff/GIS/YERA/Occupancy")
library(unmarked)

data=read.csv('20182019OccupancyData.csv', header=T, stringsAsFactors=F)


y=data[,2:5]


Dates=data[,14:(14+3)] #Going to use quadratic function of date.


DatesCentered=Dates-mean(as.numeric(as.matrix(Dates)), na.rm=T)
rm(Dates)



DatesCenteredQuadratic=as.data.frame(DatesCentered^2)
colnames(DatesCenteredQuadratic)=paste0('QuadDate', 1:4)

siteCovs=data[,6:13]

obsCovs=list(date=DatesCentered,
            quaddate=DatesCenteredQuadratic)

?occu
?unmarkedFrameOccu
wt=unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obsCovs)

fm2 <- occu(~ date + quaddate ~ ., wt)

fm2

fm3 = occu(~ 1 ~ ., wt)
fm3 #Note that the AIC is similar. Date as covariate not needed.

fm4 = occu(~ date ~ ., wt)
fm4

#fm3 is the best model based on data.

save(fm3,file='20182019PointEstimateModel_NoSupplement.RData')


backTransform(linearComb(fm3, coefficients = c(1,0), type = 'det')) #Detection Probability on June 10 is 0.64

backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,1,0,0,0,0,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,1,0,0,0,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,1,0,0,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,1,0,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,1,0,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,1,0,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,1,0), type = 'state'))
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,0,1), type = 'state'))


Occs=c(
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,1,0,0,0,0,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,1,0,0,0,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,1,0,0,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,1,0,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,1,0,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,1,0,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,1,0), type = 'state'))@estimate,
backTransform(linearComb(fm3, coefficients = c(1,0,0,0,0,0,0,0,1), type = 'state'))@estimate)

round(Occs, 2)



