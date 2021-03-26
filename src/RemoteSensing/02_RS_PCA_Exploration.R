
#Overall, this script shows that the RS values are highly correlated. If you skip to the
#last PCA, you see that for the full dataset, the first two principal components explain 95%
# of the variance (PC1: 77% and PC2: 19%)


library(lattice)
library(factoextra)
library(lme4)
rs = read.csv('./data/processed/rs_annual_julian_2013-19.csv', stringsAsFactors=F)

#476 stations, each with 7 measurements.

boxplot(rs$julian ~ rs$year) #There is some variation between years.

#Have a look at trends for McClelland west. West was best from 2014 to 2017, especially 2015-2017.
unique(rs$region_x)
west = rs[rs$region_x=='west',]

cols = colnames(west)[16:26]

splom(rs[,cols])


par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:length(cols)) {
  boxplot(west[,cols[i]]~west$year)
}
dev.off()


y=prcomp(west[,cols], scale=T)
fviz_eig(y)
res.pt <- get_pca_ind(y)
boxplot(res.pt$coord[,1] ~ west$year)

#Have a look at trends for McClelland east. East was best from 2016-2017
unique(rs$region_x)
east = rs[rs$region_x=='east',]

cols = colnames(east)[16:26]



par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:length(cols)) {
  boxplot(east[,cols[i]]~east$year)
}
dev.off()


y=prcomp(east[,cols], scale=T)
fviz_eig(y)
res.pt <- get_pca_ind(y)
boxplot(res.pt$coord[,1] ~ east$year)


#Have a look at trends for McClelland as a whole.

mc = rs[rs$region_x %in% c('east', 'west'),]

cols = colnames(mc)[16:26]



par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:length(cols)) {
  boxplot(mc[,cols[i]]~mc$year)
}
dev.off()


y=prcomp(mc[,cols], scale=T)
fviz_eig(y)
res.pt <- get_pca_ind(y)
boxplot(res.pt$coord[,1] ~ mc$year) #Note pc is flipped.


#Have a look at trends for non-mineable as a whole.
table(rs$region_x)
un = rs[rs$region_x == 'unmineable',]

cols = colnames(un)[16:26]

un = un[!is.na(un$B2),]


par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:length(cols)) {
  boxplot(un[,cols[i]]~un$year)
}
dev.off()


y=prcomp(un[,cols], scale=T)
fviz_eig(y)
res.pt <- get_pca_ind(y)
boxplot(res.pt$coord[,1] ~ un$year)

#All together.
rs2=rs
cols = colnames(rs2)[16:26]

rs2 = rs2[!is.na(rs2$B2),]


par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:length(cols)) {
  boxplot(rs2[,cols[i]]~rs2$year)
}
dev.off()


y=prcomp(rs2[,cols], scale=T)
z=fviz_eig(y)
sum(z$data$eig[1:2])
y$rotation

res.pt <- get_pca_ind(y)
boxplot(res.pt$coord[,1] ~ rs2$year) #Variation in PC across years
plot( rs2$julian,res.pt$coord[,1])
plot( rs2$julian,res.pt$coord[,2])
plot(res.pt$coord[,2], rs2$julian)
#Look at scatterplots.
par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:11) {
  plot(rs2$julian, rs2[,cols[i]])
}
dev.off()

#Look at one station at a time.
s = rs2$ss[1]
sub = rs2[rs2$ss == s,]
plot(sub$julian, sub$B2)

#This doesn't look like RS measurements are strongly dependent on date. Maybe because
#these are median date among all usable images, so don't really represent the date of the median
#pixel image.

#It is possible there is hidden variation here, driven by station (some are naturally higher than others)
#and by year (some years higher than others).
#What I should probably do is: glmm with RE for station and julian as main effect. I won't
#use year, since I want to maintain the interannual variation. Then I will use the residuals as the data.
rs2$julian = scale(rs2$julian)
rs3=rs2
for(i in 1:length(cols)) {
  response = rs2[,cols[i]] #Response variable
  predictor = rs2$julian #Julian date
  re = rs2$ss #random effect
  mod=lmer(response ~ predictor + (1|re)) #model
  #Correct for date effect. In this case, get the residuals, but normalize to the original mean.
  y=response - predict(mod, newdata = data.frame(response=response, predictor=predictor, re=re)) + mean(response)
  #Store new values in rs3
  rs3[,cols[i]] = y
}

par(mfrow=c(4,3))
par(mar=c(2,2,0,0))
for(i in 1:11) {
  plot(rs2[,cols[i]], rs3[,cols[i]])
}

write.csv(rs3, './data/processed/rs_annual_dateCorrected_2013-19.csv', row.names=F)



















