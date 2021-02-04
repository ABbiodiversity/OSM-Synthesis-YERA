
#Overall, this script shows that the RS values are highly correlated. If you skip to the
#last PCA, you see that for the full dataset, the first two principal components explain 95%
# of the variance (PC1: 77% and PC2: 19%)


library(lattice)
library(factoextra)
rs = read.csv('./data/processed/rs_annual_2013-19.csv', stringsAsFactors=F)

#476 stations, each with 7 measurements.

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
boxplot(res.pt$coord[,1] ~ rs2$year)
par(cex.axis=0.5) # is for x-axis
boxplot(res.pt$coord[,1] ~ rs2$region_x+rs2$year, cex.names=0.5)







