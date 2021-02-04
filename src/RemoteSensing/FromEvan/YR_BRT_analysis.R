#Yellow Rail R2 analysis
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(dismo)
library(gbm)
library(ggthemes)

######################################################################
#read in spatial data
######################################################################
sites <- readOGR(".", "siteOccupancy")
occupancy <- raster("occupancy.tif")
pts <- readOGR(".", "pts")
######################################################################
#
######################################################################



######################################################################
#read in needed R2 tifs
######################################################################
setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/R2_data/Variables3")
R2 <- c("R2_20160628_CoPr", "R2_20160628_CrossPr", "R2_20160628_FD_DB", "R2_20160628_FD_RS", "R2_20160628_FD_VS",
        "R2_20160628_HH", "R2_20160628_HV", "R2_20160628_VV", "B2_L8_2016", "B3_L8_2016", "B4_L8_2016",
        "B5_L8_2016", "B6_L8_2016", "B7_L8_2016", "NDVI_L8_2016", "NDWI_L8_2016", "NBR_L8_2016")
R2files <- paste0(R2, ".tif")
######################################################################
#
######################################################################


######################################################################
#extract R2 data for each site and clean data frame
######################################################################
dat <- data.frame(row=1:length(sites))
for (i in 1:length(R2files)){
  r <- raster(R2files[i])
  ext <- extract(r,sites, fun = mean)
  dat <- cbind(dat,ext)
  print(R2files[i])
}
ext <- extract(occupancy, pts)
dat <- cbind(dat,ext)
dat <- dat[,-1]
colnames(dat) <- R2
dat <- na.omit(dat)
dat <- as.data.frame(dat)
colnames(dat) <- c(R2, "occupyNum")
jt <- data.frame(occupyNum = c(1,2), occupy = c("absent", "present"))
dat <- merge(dat, jt, by = "occupyNum")

R2names <- c("CoPolRatio","CrossPolRatio", "DoubleBounce_FD", "RoughSurface_FD",
             "VolumeScattering_FD", "HH", "HV", "VV", "B2_L8", "B3_L8", "B4_L8", "B5_L8",
             "B6_L8", "B7_L8", "NDVI_L8", "NDWI_L8", "NBR_L8")
colnames(dat) <- c("occupyNum", R2names, "occupy")
######################################################################
#
######################################################################


######################################################################
#generate boxplots
######################################################################
setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/Publication/Figures")
for (i in 1:length(R2names)){
  var <- R2names[i]
  png(paste0(var,"_BP.png"), width = 1000, height = 800)
  print(ggplot(dat, aes_string(x = "occupy", y = var)) +
    geom_boxplot(fill = "#bdbdbd") + xlab("") + ylab(var) + theme_minimal(base_size = 25))
  dev.off()
}
######################################################################
#
######################################################################




######################################################################
#BRT model and plots
######################################################################
dat <- dat %>% mutate(BRTbinary = occupyNum - 1)
fit <- gbm.step(dat, 2:(length(R2names)+1), length(R2names)+3, family = "bernoulli", tree.complexity = 5,
                     learning.rate = 0.01, bag.fraction = 0.5)
dfImportance <- data.frame(summary(fit))
dfImportance <- arrange(dfImportance, rel.inf)
png("BRT_importance.png", width = 1200, heigh = 900)
ggplot(dfImportance, aes(x = reorder(var, -rel.inf), y = rel.inf)) + geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Relative importance") + theme_minimal(base_size = 25) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))
dev.off()

for (i in 1:length(R2names)){
  var <- R2names[i]
  responsePlot <- plot.gbm(fit, i.var = var, return.grid=TRUE, type="response")
  png(paste0("responsePlot_", var, ".png"), width = 1000, height = 800)
  print(
    ggplot(responsePlot, aes_string(x = var, y = "y")) + geom_jitter() + geom_smooth() +
      xlab(var) + ylab("Occupany probability") + theme_minimal(base_size = 25)
        )
  dev.off()
}

png("dualResponse_RS_CP.png", width = 800, height = 800)
gbm.perspec(fit, x=2, y=4)
dev.off()
######################################################################
#
######################################################################





######################################################################
#yearly change
######################################################################
setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/Publication/Data")
sites2014 <- readOGR(".", "sites2014")
sites2014Pts <- readOGR(".", "sites2014_pts")
occupancyChange <- raster("occupancyChange1.tif")

setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/R2_data/Variables3")
R2 <- c("R2_20160628_CrossPr", "R2_20160628_FD_DB", "R2_20160628_FD_RS", "R2_20160628_FD_VS","NDVI_L8_2016",
        "R2_20140806_CrossPr", "R2_20140806_FD_DB", "R2_20140806_FD_RS", "R2_20140806_FD_VS","NDVI_L8_2014")
R2files <- paste0(R2, ".tif")

datChange <- data.frame(row=1:length(sites2014))
for (i in 1:length(R2files)){
  r <- raster(R2files[i])
  ext <- extract(r,sites2014, fun = mean)
  datChange <- cbind(datChange,ext)
  print(R2files[i])
}
ext <- extract(occupancyChange, sites2014Pts)
datChange <- cbind(datChange,ext)
datChange <- datChange[,-1]
colnames(datChange) <- c(R2, "occupancyChange")
datChange <- na.omit(datChange)
datChange <- as.data.frame(datChange)

datChangeSummarise <- datChange %>% group_by(occupancyChange) %>% summarise_at(R2, mean, na.rm = TRUE)
occupancyChangeText <- c("loss", "none", "gain")
datChangeSummarise <- datChangeSummarise %>% mutate(occupancyChangeText = occupancyChangeText)


#cross pol ratio
d1 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,2]), year = rep("2016", 3))
colnames(d1) <- c("occupancyChangeText", "RS", "year")
d2 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,7]), year = rep("2014", 3))
colnames(d2) <- c("occupancyChangeText", "RS", "year")
dplot <- rbind(d2, d1)

setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/Publication/Figures")
png("yearlyTrend_CrossPol.png", width = 1000, height = 800)
ggplot(dplot, aes(x=year, y=RS, group = occupancyChangeText, colour = occupancyChangeText)) + geom_point(size = 2) +
  geom_line(size = 2) + theme_minimal(base_size = 25) + xlab("Year") + ylab("Cross pol ratio")
dev.off()

#NDVI
d1 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,6]), year = rep("2016", 3))
colnames(d1) <- c("occupancyChangeText", "RS", "year")
d2 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,11]), year = rep("2014", 3))
colnames(d2) <- c("occupancyChangeText", "RS", "year")
dplot <- rbind(d2, d1)

setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/Publication/Figures")
png("yearlyTrend_NDVI.png", width = 1000, height = 800)
ggplot(dplot, aes(x=year, y=RS, group = occupancyChangeText, colour = occupancyChangeText)) + geom_point(size = 2) +
  geom_line(size = 2) + theme_minimal(base_size = 25) + xlab("Year") + ylab("NDVI")
dev.off()


#flooded vegetation
d1 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,3]), year = rep("2016", 3))
colnames(d1) <- c("occupancyChangeText", "RS", "year")
d2 <- data.frame(occupancyChange = c(datChangeSummarise[,12]), RS = c(datChangeSummarise[,8]), year = rep("2014", 3))
colnames(d2) <- c("occupancyChangeText", "RS", "year")
dplot <- rbind(d2, d1)

setwd("C:/Users/Evan/Desktop/ALPHA/YellowRail/Publication/Figures")
png("yearlyTrend_DB.png", width = 1000, height = 800)
ggplot(dplot, aes(x=year, y=RS, group = occupancyChangeText, colour = occupancyChangeText)) + geom_point(size = 2) +
  geom_line(size = 2) + theme_minimal(base_size = 25) + xlab("Year") + ylab("Double bounce")
dev.off()
