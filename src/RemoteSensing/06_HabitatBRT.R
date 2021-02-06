
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

##########################
#Load the data.
##########################

data = read.csv("./data/processed/rs_annual_2013-19.csv", stringsAsFactors = F)
#remove stations-years without RS data.
sum(is.na(data$B2)) #15 such station-years.
data = data[!is.na(data$B2),]
#don't want to remove station-years without surveys because deltas will still be
#needed for the missing years.
#data = data[!is.na(data$presence),]


clusters = read.csv('./data/processed/spatial_clusters.csv', stringsAsFactors=F)

data = merge(data, clusters[,c('ss', 'cluster', 'random')], by='ss')

#select only data in the randomly chosen year. Prediction will still occur across years.
rand = data[data$year == data$random,]


table(clusters$cluster) #clusters contained between 4 and 216 points. The 216 point cluster is
#McClelland fen.

table(rand$random, rand$presence) #Pretty even ratio of presence:absence in each randomly selected year.
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
#Commented out, because tc was 6. Note however, that this number was not stable, and
#in fact varied from one run to the next. I conclude that it probably doesn't much matter.
#Tree complexity indicates the complexity of interactions allowable in the data.
#
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

#Selected tc is 6.
tc=6


i=1
set.seed(1)
for(i in 1:max(rand$cluster)) {
  print(paste('Cluster', i))
  test = rand[rand$cluster==i,]

  #Select train data.
  train = rand[rand$cluster!=i,]

  #Filter training data to randomly chosen years.
  train = train[train$year == train$random,]

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

  rand[rand$cluster==i,] = test
}

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






















