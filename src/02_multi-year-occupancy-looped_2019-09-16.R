#-------------------------------------------------------------------------------

# Title: YERA Occupancy Modeling
# Created: September 16, 2019

# Objectives: Model Yellow Rail occupancy 2013-2019.

# Script attempts to emulate the analysis here:
# https://cran.r-project.org/web/packages/unmarked/vignettes/colext.pdf
# Note one difference I made is to not code the number of years as T.
# Instead I code it as Y, because T conflicts with TRUE, messed things up.

#-------------------------------------------------------------------------------

# Load packages
library(unmarked)
library(readr)

# Import data

Data_og=read.table("./data/processed/yera_occupy_2013-21_new.csv", sep=',', header=T, stringsAsFactors=F)

Data = read_csv("./data/processed/yera_occupy_2013-21_new_eb.csv")

# Need to expand out all of the options in Data (not included by Erin)
missing_combos <- Data %>%
  select(embSS, region, wetland) %>%
  distinct() %>%
  crossing(sample = 1:4, year = c(2013:2019, 2021)) %>%
  anti_join(Data, by = c("embSS", "region", "wetland", "sample", "year")) %>%
  mutate(occupied = NA)

# Join together (well, bind)
Data <- Data %>%
  select(-`richardSS(mess)`) %>%
  bind_rows(missing_combos) %>%
  arrange(embSS, year, sample, region, wetland) %>%
  # Weird thing. Some duplicated rows, because wetland field wasn't filled out properly. Oh well.
  group_by(embSS, year, sample) %>%
  add_count() %>%
  filter(!(n > 1 & is.na(wetland))) %>%
  select(-n)

Doy = read.table("./data/processed/yera_doy_2013-19_new.csv", sep=',', header=T, stringsAsFactors=F)

#-------------------------------------------------------------------------------

# Analysis

# Looping through the five different regional analyses that Erin did.
#   1) LAPR region
#   2) Mineable
#   3) Non-mineable
#   4) Western McLelland
#   5) Eastern McLelland

# Define regions
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

  Stations=sort(unique(Occ$embSS)) # Stations

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
        Obs=Occ$occupied[Occ$embSS==S & Occ$sample==V & Occ$year==CurrentYear]
        y[i,j,k]=Obs
      }
    }
  }

  yy <- matrix(y, M, J*Y, dimnames=list(Stations, paste0(rep(Years,each=4),1:J)))

  year <- matrix(as.character(Years),
                 nrow(yy), Y, byrow=TRUE)


  simUMF <- unmarkedMultFrame(
    y = yy,
    yearlySiteCovs = list(year = year),
    numPrimary=Y)
  #summary(simUMF)

  # Simple occupancy model where all parameters don't depend on time.
  # I interpret this as assuming a constant extinction probability???
  # Not entirely sure on interpretation, but I don't think this is the model we want.


  #m0 <- colext(psiformula= ~1, gammaformula = ~ 1, epsilonformula = ~ 1,
  #            pformula = ~ 1, data = simUMF, method="BFGS")
  #summary(m0)

  #names(m0)

  #backTransform(m0, type="psi")

  #Dynamic occupancy model where many things vary by year. Note
  #I think it's best to fix detection probability across years.
  #This is more like the model we want.

  m1 <- colext(psiformula = ~1, # First-year occupancy
               gammaformula = ~ year-1, # Colonization
               epsilonformula = ~ year-1, # Extinction
               pformula = ~ year-1, # Detection
               data = simUMF)

  #m1

  #Prediction and plotting

  nd <- data.frame(year=as.character(Years[1:(length(Years)-1)])) #all but last year for Extinction probability
  E.ext <- predict(m1, type='ext', newdata=nd)
  E.ext$Parameter='Extinction'
  E.ext$Year=nd$year
  E.col <- predict(m1, type='col', newdata=nd)
  E.col$Parameter='Colonization'
  E.col$Year=nd$year
  nd <- data.frame(year=as.character(Years))
  E.det <- predict(m1, type='det', newdata=nd)
  E.det$Parameter='Detection'
  E.det$Year=nd$year

  #m1@projected.mean

  Results=list(Occupancy=m1@projected.mean,
               Extinction=E.ext,
               Colonization=E.col,
               Detection=E.det)
  if(ww==1) {
    OUT=list(Results)
    names(OUT)=R
    MODELS=list(m1)
    names(MODELS)=R
  } else {
      OUT=append(OUT, list(Results))
      names(OUT)[length(OUT)]=R
      MODELS=append(MODELS, m1)
      names(MODELS)[length(MODELS)]=R
  }
  print(paste0(R, ' Complete!'))
  if(ww==length(Regions)) {
    rm(list=ls()[!ls()%in% c('Data', 'Doy', 'OUT', 'MODELS')])
  }
}

# Save outputs

save(OUT, file='./results/occupancy/MultiYearOccupancyOutput_2013-21_eb.RData')

# save(MODELS, file='MultiYearOccupancyModels.RData')
