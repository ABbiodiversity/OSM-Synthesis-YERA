
#This script is an updated version of the HabitatBRT script.
#The main difference from previous versions is that rather than selecting a random year for surveys,
#I prioritized years with rails.
#If a station had rails in at least one year, the random year was selected among those years.
#If the station had no rails ever, then the random year was selected in a true random fashion.

#In practice what this should do is give our model a better ability to distinguish good from
#bad habitat, because it removes instances where "bad" habitat may simply have been
#unoccupied in a particular year. Statistical power is improved, because we increase the
#number of stations that have YERA, compared with the old approach.



#Going to try to explain the idea here.

#1) Spatial clusters have been identified as points that are nearby one another. Each cluster
#was >10km from all other clusters to ensure spatial separation.

#2) leave out one cluster at a time as a test set. Use remainder as training/CV.

#3) Fit BRT model using training data, then apply it to each year of the test set. From that
#we can calculate "deltas" for whether the habitat quality improved or worsened between years.
#That will then be fed into a multi-season occupancy model as a covariate on both colonization and
#extinction probability. Expect that positive deltas associated with colonization, and negative
#deltas associated with extinction.

#4) iterate through each cluster so that all data is used.

#library(devtools)
#install_github("fabsig/gpboost") #Couldn't get this to work.
library(dismo)
library(gbm)
library(factoextra)
library(sjstats)
library(pROC)

##########################
#Load the data.
##########################

data = read.csv("./data/processed/rs_annual_dateCorrected_2013-19.csv", stringsAsFactors = F)
#remove stations-years without RS data.
sum(is.na(data$B2)) #15 such station-years.
data = data[!is.na(data$B2),]
#don't want to remove station-years without surveys because deltas will still be
#needed for the missing years.
#data = data[!is.na(data$presence),]


clusters = read.csv('./data/processed/spatial_clusters.csv', stringsAsFactors=F)

data = merge(data, clusters[,c('ss', 'cluster', 'best')], by='ss')

#select only data in the randomly chosen year. Prediction will still occur across years.
rand = data[data$year == data$best,]


table(clusters$cluster) #clusters contained between 4 and 216 points. The 216 point cluster is
#McClelland fen.

table(rand$best, rand$presence) #Pretty even ratio of presence:absence in each randomly selected year.
#Which seems like a good thing.
table(rand$cluster, rand$presence) #Some clusters have no yellow rails, which is fine.
#Importantly, no single cluster totally dominates the detections. There are most detections
#in the cluster 2, but there are still significant numbers outside that cluster to work with (i.e.
#when it is excluded from fitting, we should still have lots of data to work with).
#Still need to check that after selecting only random years.

rm(list=c('clusters'))


###########################
#Analysis.
###########################

#Select test data.

response = 'presence'
rand$presence = gsub('present','1', rand$presence)
rand$presence = gsub('absent','0', rand$presence)
rand$presence = as.numeric(rand$presence)


rsVars = c("B2", "B3", "B4", "B5", "B6", "B7", "NBR", "NDVI", "NDWI", "TCB", "TCwet")

outputVars = paste0('pred', 2013:2019)
rand[outputVars]=NA

#Prior to the loop, going to select the best tree complexity and use it throughout.
# Commented out, because tc was 6. Note however, that this number was not stable, and
# in fact varied from one run to the next. I conclude that it probably doesn't much matter.
# Tree complexity indicates the complexity of interactions allowable in the data.

# lr=0.0005
# OUT=data.frame(ntrees=rep(NA,10), deviance=rep(NA,10))
# set.seed(1)
# for(i in 1:10) {
#   fit <- gbm.step(rand, rsVars, response, family = "bernoulli", tree.complexity = i,
#                   learning.rate = lr, bag.fraction = 0.5)
#   length(fit$trees) #ensure >1000
#   OUT$ntrees[i]=fit$n.trees
#   OUT$deviance[i]=fit$cv.statistics$deviance.mean
# }
# plot(OUT$deviance)
# plot(OUT$ntrees) #Greater complexity requires fewer trees. Makes sense.
# plot(OUT)

#Selected tc is 8.
tc=8

i=1
set.seed(1)
rand$cvAUC = NA
for(i in 1:max(rand$cluster)) {
  print(paste('Cluster', i))
  test = rand[rand$cluster==i,]

  #Select train data.
  train = rand[rand$cluster!=i,]

  #Filter training data to randomly chosen years.
  train = train[train$year == train$best,]

  #Train the BRT. Need to identify the best learning rate and tree complexity for the problem.
  #Learning rate should be as low as possible, as I understand it.

  #I couldn't get package GPBoost (not on CRAN) to install, so I don't have a  way to
  #add random effects.
  #Instead I randomly selected one year per point for analysis.

  lr = 0.001
  trees = 0
  while(trees <1000) {
    fit <- gbm.step(train, rsVars, response, family = "bernoulli", tree.complexity = tc,
                    learning.rate = lr, bag.fraction = 0.5)
    trees = fit$n.trees
    if(trees<1000) {lr=lr-0.0001}
  }

  #predict on the test data, then put it back into the original data.
  j=1
  for(j in 1:nrow(test)) {
    for(k in 2013:2019) {
      selected = data[data$ss==test$ss[j] & data$year==k,]
      if(nrow(selected)>0) {
        selected$pred = predict.gbm(fit, newdata=selected, fit$gbm.call$best.trees,type='response')
        test[j,paste0('pred',k)]=selected$pred
      }
    }
  }

  if(i==1) {forest = list(list(model=fit, cluster=i))} else {
    forest = append(forest, list(list(model=fit, cluster=i)))
  }
  test$cvAUC = mean(fit$cv.roc.matrix)
  rand[rand$cluster==i,] = test
}

dev.off()
plot(2013:2019,rand[1,paste0('pred', 2013:2019)], xlim=c(2012,2020), ylim=c(0,1), type='l', yaxs='i')
for(i in 2:nrow(rand)) {
  points(2013:2019,rand[i,paste0('pred', 2013:2019)], type='l')
}

boxplot(rand[,paste0('pred', 2013:2019)])

#Looking good, but can we rule out that date of images is a confound?

#Calculate differences.
rand$diffs2014 = rand$pred2014-rand$pred2013
rand$diffs2015 = rand$pred2015-rand$pred2014
rand$diffs2016 = rand$pred2016-rand$pred2015
rand$diffs2017 = rand$pred2017-rand$pred2016
rand$diffs2018 = rand$pred2018-rand$pred2017
rand$diffs2019 = rand$pred2019-rand$pred2018
boxplot(rand[,paste0('diffs', 2014:2019)])

#Now calculate the AUC for each cluster.

rand$bestPred=NA
for(i in 1:nrow(rand)) {
  b = rand$best[i]
  char = paste0('pred',b)
  val = rand[i,char]
  rand$bestPred[i]=val
}
boxplot(rand$bestPred~rand$occupied)
boxplot(apply(rand[,paste0('pred',2013:2019)],1,mean) ~ rand$occupied)
names(fit)
mean(aggregate(cvAUC ~ cluster, data=rand, mean)[,2]) #Average cv AUC was 0.70.

vals = c()
for(i in 1:max(rand$cluster)) {
  sub = rand[rand$cluster==i,]
  y=roc(data=sub, response=presence, predictor=bestPred)
}
boxplot(bestPred ~ cluster, data=rand)
#x=rand[rand$cluster==16,]
boxplot(bestPred ~ occupied, data=rand)
y=roc(data=rand, response=presence, predictor=bestPred) #56
rand$AUC = y$auc

rand = rand[,c('ss', paste0('pred', 2013:2019), paste0('diffs', 2014:2019), 'cvAUC','AUC')]

#write.csv(rand, './data/processed/BRT_habitat_predictions_best_2013-19b.csv', row.names=F)


# # Want to check something: Pred values seem to show similar patterns to PC1 values
# # over time.
# # So what is going on here? Want to check if the two are directly correlated, in which
# # case the BRT would really be doing nothing fancy.
# # I checked this, and the conclusion was that the BRT is correlated, but loosely, so it seems to
# # be doing more than PC regression would.
#
# rand[paste0('PC1',2013:2019)]=NA
# rand[paste0('PC2',2013:2019)]=NA
#
# y=prcomp(data[,rsVars], scale=T)
# z=fviz_eig(y)
# sum(z$data$eig[1:2])
# y$rotation
#
# res.pt <- get_pca_ind(y)
# data$PC1 = res.pt$coord[,1]
# data$PC2 = res.pt$coord[,2]
#
# for(i in 1:nrow(data)) {
#   S= data$ss[i]
#   year = data$year[i]
#   PCs = c(data$PC1[i], data$PC2[i])
#   rand[rand$ss==S,paste0('PC1',year)]=PCs[1]
#   rand[rand$ss==S,paste0('PC2',year)]=PCs[2]
# }
#
#
# plot(as.matrix(rand[,paste0('pred', 2013:2019)]), as.matrix(rand[,paste0('PC1', 2013:2019)]))
#
# plot(as.matrix(rand[,paste0('pred', 2013:2019)]), as.matrix(rand[,paste0('PC2', 2013:2019)]))
#
# #The above plots show that in fact the predicted values and PC values are only loosely correlated.
# #So that's good - it means the BRT appears to be doing more than simply exploiting the PC scores.
#
# #Below here looking at the amount of variation attributable to between years vs between stations.
# #Long story short, about 2x as much variation between stations as between years.
# boxplot(rand[,'PC12013'] ~ rand$ss)
#
# stacked = stack(rand[,paste0('PC1', 2013:2019)])
# stacked$ss = rep(rand$ss, 7)
# ANOVA=aov(values ~ ind + ss, data = stacked)
# anova_stats(ANOVA)
# summary(ANOVA)













